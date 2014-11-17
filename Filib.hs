{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll

postContext :: Context String
postContext = dateField "date" "%B %e %Y" <> defaultContext

indexContext :: Compiler [Item String] -> Context String
indexContext posts = listField "posts" postContext posts <> defaultContext

main :: IO ()
main = hakyll $ do
  match "index.html" $ do
    route idRoute
    compile $ do
      let posts = recentFirst =<< loadAll "posts/*.md"
      getResourceBody
        >>= applyAsTemplate (indexContext posts)
        >>= loadAndApplyTemplate "templates/default.html" (indexContext posts)
        >>= relativizeUrls

  match (fromList ["about.md", "contact.md"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postContext
      >>= loadAndApplyTemplate "templates/default.html" postContext
      >>= relativizeUrls

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateCompiler
