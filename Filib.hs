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
  [ constField "topLevelTitle" "Home"
  , listField "posts" (postContext tags) posts
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
    route $ setExtension "html"
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" (tagsContext tags posts tag)
        >>= loadAndApplyTemplate "templates/default.html" (tagsContext tags posts tag)
        >>= relativizeUrls

  match "index.html" $ do
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

  where posts = recentFirst =<< loadAll "posts/*.md"
