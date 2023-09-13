module Main (main) where

import Control.Arrow ((>>>))
import Data.Data (Proxy (..))
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.JsonSpec.Elm (Named)
import Data.JsonSpec.Elm qualified as Elm
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Language.Elm.Name (Module)
import Language.Elm.Pretty qualified as Elm.Pretty
import Oru.Comment (CommentSpec)
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified as Render.Text
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Prelude

main :: IO ()
main = do
  exists <- Directory.doesDirectoryExist frontendDirectory
  if exists
    then do
      let actual :: HashMap Module Text
          actual =
            Elm.elmDefs (Proxy @(Named "Comment" CommentSpec))
              & Set.toList
              & Elm.Pretty.modules
              <&> ( Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
                      >>> Render.Text.renderStrict
                      >>> (<> "\n")
                  )
      Foldable.traverse_ writeModule (HashMap.toList actual)
    else putStrLn ("frontend directory does not exist at: " <> frontendDirectory)

frontendDirectory :: FilePath
frontendDirectory = ".." </> "frontend" </> "src"

writeModule :: (Module, Text) -> IO ()
writeModule (module_, content) = do
  Directory.createDirectoryIfMissing True dirname
  Text.IO.writeFile filename content
  where
    pathName :: [Text] -> FilePath
    pathName = (frontendDirectory </>) . Text.unpack . Text.intercalate "/"

    filename :: FilePath
    filename = pathName module_ <> ".elm"

    dirname :: FilePath
    dirname = pathName (init module_)