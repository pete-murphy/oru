{-# LANGUAGE OverloadedStrings #-}

module Oru.Main where

import Control.Arrow ((<<<), (>>>))
import Control.Monad qualified as Monad
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable qualified as Traversable
import Debug.Trace qualified as Debug
import Network.HTTP.Types qualified as HTTP.Types
import Network.HTTP.Types.Header qualified as Header
import Network.Wai (Application)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors qualified as Cors
import Numeric qualified
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Prelude

type Filename = String

type OruIndex =
  HashMap Filename TermFrequency

type InvertedOruIndex =
  HashMap Text (HashMap Filename Double)

type TermCount =
  HashMap Text Int

type TermFrequency =
  HashMap Text Double

main :: IO ()
main = do
  cwd <- Directory.getCurrentDirectory
  let commentsDirectory = cwd </> "comments"
  files <- Directory.listDirectory commentsDirectory
  oruIndex' <-
    HashMap.fromList
      <$> Traversable.for files \file -> do
        contents <- Text.readFile (commentsDirectory </> file)
        let count = termCount contents
        pure (file, termFrequency count)

  let idf = inverseDocumentFrequency (HashMap.elems oruIndex')
      oruIndex =
        oruIndex'
          <&> HashMap.mapWithKey \term frequency ->
            frequency * idf term

      invertedOruIndex = HashMap.fromListWith HashMap.union do
        (filename, frequencyMap) <- HashMap.toList oruIndex
        (term, frequency) <- HashMap.toList frequencyMap
        pure (term, HashMap.singleton filename frequency)
      tfidf' = tfidf invertedOruIndex
  Warp.run 3000 (Cors.simpleCors (app tfidf'))

app :: (Text -> HashMap Filename Double) -> Application
app tfidf' request response = do
  Debug.traceShowM request
  let result =
        Wai.queryString request
          & List.lookup "q"
          & Monad.join
          <&> (Text.decodeUtf8 >>> tfidf')
          & Maybe.fromMaybe mempty
          & HashMap.toList
          & List.sortOn (Down <<< snd)
          & take 20
          <&> ( \(filename, score) ->
                  Text.pack filename <> ": " <> Text.pack (Numeric.showFFloat (Just 3) score "")
              )
          & Text.unlines
          & Text.encodeUtf8
          & ByteString.Lazy.fromStrict
  response do
    Wai.responseLBS
      HTTP.Types.status200
      [(Header.hContentType, "text/plain")]
      result

normalize :: Text -> Maybe Text
normalize =
  Text.toLower >>> Text.dropAround (not <<< Char.isAlphaNum) >>> \case
    "" -> Nothing
    term -> Just term

termCount :: Text -> TermCount
termCount =
  Text.words
    >>> Maybe.mapMaybe normalize
    >>> map (\term -> (term, 1))
    >>> HashMap.fromListWith (+)

termFrequency :: TermCount -> TermFrequency
termFrequency count = do
  let total = Foldable.sum count
  count & HashMap.map (\frequency -> fromIntegral frequency / fromIntegral total)

inverseDocumentFrequency :: [TermFrequency] -> Text -> Double
inverseDocumentFrequency frequencies term = do
  let total = length frequencies
      totalWithTerm = length (filter (HashMap.member term) frequencies)
  log (fromIntegral (total - totalWithTerm) / fromIntegral totalWithTerm)

tfidf ::
  InvertedOruIndex ->
  -- | search
  Text ->
  -- | score
  HashMap Filename Double
tfidf index search = do
  let terms = Maybe.mapMaybe normalize (Text.words search)
      fnmap = terms <&> \term -> HashMap.lookupDefault mempty term index
  foldr (HashMap.unionWith (+)) mempty fnmap
