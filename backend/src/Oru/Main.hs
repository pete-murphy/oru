module Oru.Main where

import Control.Arrow ((<<<), (>>>))
import Control.Monad qualified as Monad
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
import Data.Text.IO qualified as Text
import Data.Traversable qualified as Traversable
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
  let assetsDirectory = cwd </> "assets"
  files <- Directory.listDirectory assetsDirectory
  oruIndex' <-
    HashMap.fromList
      <$> Traversable.for files \file -> do
        contents <- Text.readFile (assetsDirectory </> file)
        let count = termCount contents
        pure (file, termFrequency count)

  let idf = inverseDocumentFrequency (HashMap.elems oruIndex')
      oruIndex =
        oruIndex'
          <&> HashMap.mapWithKey \term frequency ->
            frequency * idf term

  let invertedOruIndex = HashMap.fromListWith HashMap.union do
        (filename, frequencyMap) <- HashMap.toList oruIndex
        (term, frequency) <- HashMap.toList frequencyMap
        pure (term, HashMap.singleton filename frequency)
  let tfidf' = tfidf invertedOruIndex

  Monad.void do
    Monad.forever do
      Text.putStrLn "Enter search term:"
      searchTerm <- Text.getLine
      tfidf' searchTerm
        & HashMap.toList
        & List.sortOn (Down <<< snd)
        & take 20
        & Foldable.traverse_ \(filename, score) -> do
          Text.putStrLn (Text.pack filename <> ":\t" <> Text.pack (Numeric.showFFloat (Just 3) score ""))

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
  let terms = Text.words search
      fnmap = terms <&> \term -> HashMap.lookupDefault mempty term index
  foldr (HashMap.unionWith (+)) mempty fnmap
