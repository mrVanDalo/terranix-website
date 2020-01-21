{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Monad       (liftM, filterM)
import           Data.List           (isSuffixOf)
import           Data.List           (intersperse, sortBy)
import           Data.Monoid         (mappend)
import           Data.Ord            (comparing)
import           Hakyll
import           System.FilePath     (splitExtension)
import           Text.Pandoc.Options

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
      "documentation/**"
      (\meta ->
         case lookupString "draft" meta of
           Just "false" -> True
           Nothing      -> True
           _            -> False) $ do
      route $
        directorizeRoute `composeRoutes` gsubRoute "documentation/" (const "") `composeRoutes`
        setExtension "html"
      compile $
        pandocCompilerWithToc >>=
        loadAndApplyTemplate
          "templates/layout.html"
          (createDefaultIndex "documentation") >>=
        relativizeUrls >>=
        deIndexURLs
    match "documentation.markdown" $ do
      route $ setExtension "html"
      compile $ do
        documentations <- loadAll "documentation/**"
        let indexCtx =
              listField "posts" postCtx (sortByOrder documentations) `mappend`
              createDefaultIndex "documentation"
        pandocCompilerWithoutToc >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/layout.html" indexCtx >>=
          relativizeUrls >>=
          deIndexURLs
    match "options.html" $ do
      route $ setExtension "html"
      compile $ do
        getResourceString >>=
          loadAndApplyTemplate "templates/layout.html" (createDefaultIndex "options") >>=
          relativizeUrls >>=
          deIndexURLs
    match "index.markdown" $ do
      route $ setExtension "html"
      compile $ do
        documentations <- loadAll "documentation/**"
        a <- sortByOrder documentations
        let indexCtx =
              listField "posts" postCtx ( filterByPreview a ) `mappend`
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

filterByPreview :: MonadMetadata m => [Item a] -> m [Item a]
filterByPreview = let
  hasPreview id' = do
    result <- getMetadataField id' "preview"
    case result of
      Just "true" -> return True
      _ -> return False
  in filterM ( hasPreview . itemIdentifier )


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
  let navigationItems = [ ("/", "main")
                        , ("/documentation.html", "documentation")
                        , ("/options.html", "options")]
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
