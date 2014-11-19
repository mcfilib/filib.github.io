{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid                     (mconcat)
import Hakyll
import Text.Blaze.Html                 (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)

import qualified Data.Map                    as M
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

type Title = String

indexCtx :: Tags -> Compiler [Item String] -> Context String
indexCtx tags posts = mconcat
  [ constField "rootTitle" "Home"
  , listField "posts" (postCtx tags) posts
  , defaultContext
  ]

postCtx :: Tags -> Context String
postCtx tags = mconcat
  [ dateField "date" "%B %e %Y"
  , tagsField "tags" tags
  , defaultContext
  ]

tagsCtx :: Title -> Tags -> Compiler [Item String] -> Context String
tagsCtx title tags posts = mconcat
  [ constField "title" title
  , listField "posts" (postCtx tags) posts
  , defaultContext
  ]

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*.md" $ fromCapture "tags/*.html"

  tagsRules tags $ \tag pattern -> do
    route   $ setExtension "html"
    compile $ do
      let posts = recentFirst =<< loadAll pattern
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" (tagsCtx tag tags posts)
        >>= loadAndApplyTemplate "templates/default.html"   (tagsCtx tag tags posts)
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      let posts = recentFirst =<< loadAll "posts/*.md"
      getResourceBody
        >>= applyAsTemplate                               (indexCtx tags posts)
        >>= loadAndApplyTemplate "templates/default.html" (indexCtx tags posts)
        >>= relativizeUrls

  match "posts/*.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
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
