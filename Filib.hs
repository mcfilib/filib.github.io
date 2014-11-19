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
    let posts   = recentFirst =<< loadAll pattern
        context = tagsCtx tag tags posts
    route   $ setExtension "html"
    compile $ makeItem ""
      >>= loadAndApplyTemplate "templates/post-list.html" context
      >>= loadAndApplyTemplate "templates/default.html"   context
      >>= relativizeUrls

  match "index.html" $ do
    let posts   = recentFirst =<< loadAll "posts/*.md"
        context = indexCtx tags posts
    compile $ getResourceBody
      >>= applyAsTemplate                               context
      >>= loadAndApplyTemplate "templates/default.html" context
      >>= relativizeUrls

  match "posts/*.md" $ do
    let context = postCtx tags
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    context
      >>= loadAndApplyTemplate "templates/default.html" context
      >>= relativizeUrls

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match ("favicon.ico" .||. "images/*") $ do
    route   idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler
