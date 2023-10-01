{-# LANGUAGE OverloadedStrings #-}

module Oru.Main where

import Control.Arrow ((<<<), (>>>))
import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
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
import Data.Void (Void)
import Debug.Trace qualified as Debug
import Network.HTTP.Types qualified as HTTP.Types
import Network.HTTP.Types.Header qualified as Header
import Network.Wai (Application)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors qualified as Cors
import Oru.Comment (Comment (..), Internals (..), Preview (..), PreviewComment (..))
import Oru.Comment qualified as Comment
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Megaparsec.Char.Lexer
import Prelude

type Filename = String

type OruIndex =
  HashMap Filename TermFrequency

type TermCount =
  HashMap Text Int

type TermFrequency =
  HashMap Text Double

main :: IO ()
main = do
  cwd <- Directory.getCurrentDirectory
  let commentsDirectory = cwd </> "comments"
  filePaths <- Directory.listDirectory commentsDirectory
  fileDataByFilePath <- do
    HashMap.fromList
      <$> Traversable.for filePaths \filePath -> do
        contents <- Text.readFile (commentsDirectory </> filePath)
        let frontmatter =
              Megaparsec.runParser frontmatterParser filePath contents
                & onLeft \parseError -> error ("[ERROR] Failed to parse frontmatter: " <> filePath <> "\n" <> Megaparsec.errorBundlePretty parseError)
            count = termCount contents
        pure (filePath, (frontmatter, termFrequency count))

  let oruIndex' = snd <$> fileDataByFilePath
      frontmatterByFilePath = fst <$> fileDataByFilePath
      idf = inverseDocumentFrequency (HashMap.elems oruIndex')
      oruIndex =
        oruIndex'
          <&> HashMap.mapWithKey \term frequency ->
            frequency * idf term

      invertedOruIndex = HashMap.fromListWith HashMap.union do
        (filename, frequencyMap) <- HashMap.toList oruIndex
        (term, frequency) <- HashMap.toList frequencyMap
        pure (term, HashMap.singleton filename frequency)
      tfidf' = tfidf (fmap (HashMap.intersectionWith (,) frontmatterByFilePath) invertedOruIndex)
  Warp.run 3000 (Cors.simpleCors (app tfidf'))

onLeft :: (forall a. e -> a) -> Either e success -> success
onLeft handle = \case
  Left err -> handle err
  Right success -> success

app :: (Text -> HashMap Filename (Comment.Frontmatter, Double)) -> Application
app tfidf' request response = do
  Debug.traceShowM request

  let result =
        Wai.queryString request
          & List.lookup "q"
          & Monad.join
          <&> (Text.decodeUtf8 >>> tfidf')
          & Maybe.fromMaybe mempty
          & HashMap.toList
          & List.sortOn (Down <<< snd <<< snd)
          & take 20
          & map
            ( \(slug, (Comment.Frontmatter {..}, score)) ->
                -- TODO: Clean up???
                ( PreviewComment
                    ( Comment
                        ( Internals
                            { commentTitle = frontmatterCommentTitle,
                              commentMovieTitle = frontmatterMovieTitle,
                              commentSlug = Text.pack slug,
                              commentRating = frontmatterRating
                            }
                        )
                        Preview
                    ),
                  realToFrac score :: Float
                )
            )
          & Aeson.encode

  response do
    Wai.responseLBS
      HTTP.Types.status200
      [(Header.hContentType, "application/json")]
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
  HashMap Text (HashMap Filename (a, Double)) ->
  -- | search
  Text ->
  -- | score
  HashMap Filename (a, Double)
tfidf index search = do
  let terms = Maybe.mapMaybe normalize (Text.words search)
      fnmap = terms <&> \term -> HashMap.lookupDefault mempty term index
  foldr (HashMap.unionWith (\(k, v) (_, v') -> (k, v + v'))) mempty fnmap

type Parser = Parsec Void Text

frontmatterParser :: Parser Comment.Frontmatter
frontmatterParser = do
  _ <- "---" *> Megaparsec.Char.newline
  frontmatterMovieTitle <- fmap Text.pack do
    "movie title: " *> Megaparsec.manyTill (Megaparsec.anySingleBut '\n') Megaparsec.Char.newline
  frontmatterCommentTitle <- fmap Text.pack do
    "comment title: " *> Megaparsec.manyTill (Megaparsec.anySingleBut '\n') Megaparsec.Char.newline
  frontmatterRating <-
    ("rating: " *> Megaparsec.optional Megaparsec.Char.Lexer.decimal)
      <* Megaparsec.Char.newline
  pure Comment.Frontmatter {..}
