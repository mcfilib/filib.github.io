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
  [ constField "topLevelTitle" "Home"
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
    let context = tagsContext tags posts tag
    route $ setExtension "html"
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" context
        >>= loadAndApplyTemplate "templates/default.html"   context
        >>= relativizeUrls

  match "index.html" $ do
    let context = indexContext tags posts
    route idRoute
    compile $ do
      getResourceBody
        >>= applyAsTemplate                               context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

  match "posts/*.md" $ do
    let context = postContext tags
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    context
      >>= loadAndApplyTemplate "templates/default.html" context
      >>= relativizeUrls

  match "favicon.ico" $ do
    route   idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateCompiler

  where posts = recentFirst =<< loadAll "posts/*.md"
