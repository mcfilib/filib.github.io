{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import           Control.Applicative ((<$>))
import           Control.Monad (forM, forM_)
import           Data.List (intersperse)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Monoid (mconcat)
import           Hakyll
import           Hakyll.Web.Feed
import           Text.Blaze.Html (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------

-- TYPES

type Field = String
type Title = String

--------------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = hakyll $ do
  -- Compress CSS into one file
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  -- Static files
  match ("favicon.ico" .||. "images/*") $ do
    route   idRoute
    compile copyFileCompiler

  -- Build tags
  tags <- buildTags "posts/*.md" $ fromCapture "tags/*.html"

  -- Tags rules
  tagsRules tags $ \tag pattern -> do
    let posts   = recentFirst =<< loadAll pattern
        context = tagsCtx tag tags posts
    route   $ setExtension "html"
    compile $ makeItem ""
      >>= loadAndApplyTemplate "templates/post-list.html" context
      >>= loadAndApplyTemplate "templates/default.html"   defaultContext
      >>= relativizeUrls

  -- Read templates
  match "templates/*" $
    compile templateCompiler

  -- Index
  match "index.html" $ do
    let posts   = recentFirst =<< loadAll "posts/*.md"
        context = indexCtx tags posts
    route idRoute
    compile $ getResourceBody
      >>= applyAsTemplate context
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  -- About
  match "about.html" $ do
    route idRoute
    compile $ getResourceBody
      >>= applyAsTemplate defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  -- CV
  match "cv.html" $ do
    route idRoute
    compile $ getResourceBody
      >>= applyAsTemplate defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  -- Render each and every post
  match "posts/*.md" $ do
    let context = postCtx tags
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" context
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  -- Render RSS feed
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx tags `mappend` bodyField "description"
          snapshots = loadAllSnapshots "posts/*" "content"
      posts <- snapshots >>= fmap (take 10) . recentFirst
      renderRss feedConfig feedCtx posts

--------------------------------------------------------------------------------

-- CTX

indexCtx :: Tags -> Compiler [Item String] -> Context String
indexCtx tags posts = mconcat
  [ constField "rootTitle" "Home"
  , listField "posts" (postCtx tags) posts
  , defaultContext
  ]

postCtx :: Tags -> Context String
postCtx tags = mconcat
  [ dateField "date" "%Y-%m-%d"
  , htmlTags "tags" tags
  , defaultContext
  ]

tagsCtx :: Title -> Tags -> Compiler [Item String] -> Context String
tagsCtx title tags posts = mconcat
  [ constField "title" title
  , listField "posts" (postCtx tags) posts
  , defaultContext
  ]

htmlTags :: Field -> Tags -> Context a
htmlTags key tags = field key $ \item -> do
  tags' <- getTags $ itemIdentifier item
  links <- forM tags' $ \tag -> renderLink tag <$> getRoute (tagsMakeId tags tag)
  return . renderHtml . mconcat . intersperse " " . catMaybes $ links
  where
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
      H.a ! A.href (toValue . toUrl $ filePath) ! A.class_ "post-category green" $ toHtml tag

--------------------------------------------------------------------------------

-- FEED

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Philip Cunningham: music, code and stuff."
    , feedDescription = "Philip is a Ruby and Haskell developer currently working remotely from Penarth."
    , feedAuthorName  = "Philip Cunningham"
    , feedAuthorEmail = "hello@filib.io"
    , feedRoot        = "https://filib.io"
    }
