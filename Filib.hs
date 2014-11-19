{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid                     (mconcat)
import Hakyll
import Text.Blaze.Html                 (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)

import qualified Data.Map                    as M
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

indexContext :: Tags -> Compiler [Item String] -> Context String
indexContext tags posts = mconcat
  [ constField "rootTitle" "Home"
  , listField "posts" (postContext tags) posts
  , defaultContext
  ]

postContext :: Tags -> Context String
postContext tags = mconcat
  [ dateField "date" "%B %e %Y"
  , tagsField "tags" tags
  , defaultContext
  ]

tagsContext :: Tags -> Compiler [Item String] -> String -> Context String
tagsContext tags posts title = mconcat
  [ constField "title" title
  , listField "posts" (postContext tags) posts
  , defaultContext
  ]

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*.md" $ fromCapture "tags/*.html"

  tagsRules tags $ \tag pattern -> do
    route   $ setExtension "html"
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" (tagsContext tags (return posts) tag)
        >>= loadAndApplyTemplate "templates/default.html"   (tagsContext tags (return posts) tag)
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.md"
      getResourceBody
        >>= applyAsTemplate                               (indexContext tags (return posts))
        >>= loadAndApplyTemplate "templates/default.html" (indexContext tags (return posts))
        >>= relativizeUrls

  match "posts/*.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    (postContext tags)
      >>= loadAndApplyTemplate "templates/default.html" (postContext tags)
      >>= relativizeUrls

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "favicon.ico" $ do
    route   idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler
