{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

--------------------------------------------------------------------------------
import           Control.Monad       (liftM)
import           Data.List           (isSuffixOf)
import           Data.List           (intersperse, sortBy)
import           Data.Monoid         (mappend)
import           Data.Ord            (comparing)
import           Hakyll
import           System.FilePath     (splitExtension)
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyllWith myConfiguration $
    -- copy all images
   do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "static/*" $ do
      route idRoute
      compile copyFileCompiler
    -- css files
    match "src/lessc/hack.less" $ do
      route $ customRoute $ const "css/hack.css"
      compile $
        getResourceString >>=
        withItemBody
          (unixFilter "lessc" ["-", "--include-path=./src/lessc/page/"]) >>=
        return . fmap compressCss
    matchMetadata
      "articles/**"
      (\meta ->
         case lookupString "draft" meta of
           Just "false" -> True
           Nothing      -> True
           _            -> False) $ do
      route $
        directorizeRoute `composeRoutes` gsubRoute "articles/" (const "") `composeRoutes`
        setExtension "html"
      compile $
        pandocCompilerWithToc >>=
        loadAndApplyTemplate
          "templates/layout.html"
          (createDefaultIndex "articles") >>=
        relativizeUrls >>=
        deIndexURLs
    match "index.markdown" $ do
      route $ setExtension "html"
      compile $ do
        articles <- loadAll "articles/**"
        let indexCtx =
              listField "posts" postCtx (sortByOrder $ articles) `mappend`
              createDefaultIndex "main"
        pandocCompilerWithoutToc >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/layout.html" indexCtx >>=
          relativizeUrls >>=
          deIndexURLs
    match "templates/*" $ compile templateBodyCompiler

sortByOrder :: MonadMetadata m => [Item a] -> m [Item a]
sortByOrder =
  let getOrder id' = getMetadataField' id' "order"
   in sortByM (getOrder . itemIdentifier)

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs =
  liftM (map fst . sortBy (comparing snd)) $ mapM (\x -> liftM (x, ) (f x)) xs

postCtx :: Context String
postCtx = dateField "date" "%Y-%m-%d" `mappend` defaultContext

-- https://svejcar.dev/posts/2019/11/27/table-of-contents-in-hakyll/
pandocCompilerWithToc =
  pandocCompilerWith
    defaultHakyllReaderOptions
    (defaultHakyllWriterOptions
       { writerNumberSections = False
       , writerTableOfContents = True
       , writerTOCDepth = 3
       , writerTemplate = Just "<nav id=\"TableOfContents\">$toc$</nav>$body$"
       })

pandocCompilerWithoutToc =
  pandocCompilerWith
    defaultHakyllReaderOptions
    (defaultHakyllWriterOptions
       { writerNumberSections = False
       , writerTableOfContents = False
       , writerTOCDepth = 3
       , writerTemplate = Just "$body$"
       })

createDefaultIndex :: String -> Context String
createDefaultIndex groupName =
  let navigationItems = [("/", "main")]
      listItem (path, label) active =
        let linkPart =
              if active
                then linkPart' "nav-item active"
                else linkPart' "nav-item"
            linkPart' cssClass url content =
              "<a class=\"" ++
              cssClass ++ "\" href=\"" ++ url ++ "\">" ++ content ++ "</a>"
         in linkPart path label
      navigation =
        foldl
          (\result item@(_, name) -> result ++ listItem item (name == groupName))
          ""
          navigationItems
   in constField "navigation" navigation `mappend` defaultContext

myConfiguration :: Configuration
myConfiguration =
  defaultConfiguration
    { deployCommand =
        "rsync -avz --delete _site/ root@sputnik.private:/srv/www/terranix/"
    }

-- | /file.<ext>-> /file/index.<ext>
directorizeRoute :: Routes
directorizeRoute =
  let directorize path = dirs ++ "/index" ++ ext
        where
          (dirs, ext) = splitExtension path
   in customRoute (directorize . toFilePath)

-- | Strips "index.html" from given URL string.
stripIndex :: String -> String
stripIndex url =
  if "index.html" `isSuffixOf` url
    then take (length url - 10) url
    else url

-- | remove index.html from links
deIndexURLs :: Item String -> Compiler (Item String)
deIndexURLs item = return $ fmap (withUrls stripIndex) item
