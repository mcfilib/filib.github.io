{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mconcat)
import Hakyll

postContext :: Tags -> Context String
postContext tags = mconcat
  [ dateField "date" "%B %e %Y"
  , tagsField "tags" tags
  , defaultContext
  ]

indexContext :: Tags -> Compiler [Item String] -> Context String
indexContext tags posts = mconcat
  [ listField "posts" (postContext tags) posts
  , defaultContext
  ]

tagsContext :: Tags -> Compiler [Item String] -> Context String
tagsContext tags posts = mconcat
  [ listField "posts" (postContext tags) posts
  , defaultContext
  ]

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*.md" $ fromCapture "tags/*.html"

  tagsRules tags $ \tag pattern -> do
    let posts = recentFirst =<< loadAll "posts/*.md"
    route $ setExtension "html"
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" (tagsContext tags posts)
        >>= loadAndApplyTemplate "templates/default.html" (tagsContext tags posts)
        >>= relativizeUrls

  match "index.html" $ do
    let posts = recentFirst =<< loadAll "posts/*.md"
    route idRoute
    compile $ do
      getResourceBody
        >>= applyAsTemplate (indexContext tags posts)
        >>= loadAndApplyTemplate "templates/default.html" (indexContext tags posts)
        >>= relativizeUrls

  match (fromList ["about.md", "contact.md"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*.md" $ do
    route $ setExtension "html"
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

  match "templates/*" $ compile templateCompiler
