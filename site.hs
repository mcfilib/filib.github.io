{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll

postContext :: Context String
postContext =
  dateField "date" "%B %e %Y" <> defaultContext

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

  match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

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

  match "index.html" $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveContext =
                  listField "posts" postContext (return posts) <>
                  constField "title" "Archives"                <>
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/index.html" archiveContext
              >>= loadAndApplyTemplate "templates/default.html" archiveContext
              >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
