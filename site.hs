--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM)
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "static/*" $ do
    route idRoute
    compile copyFileCompiler

  match (fromList ["robots.txt"]) $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ compile compressCssCompiler
  create ["style.css"] $ do
    route idRoute
    compile $ do
      csses <- loadAll "css/*.css"
      makeItem $ unlines $ map itemBody csses

  match (fromList ["index.md"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< filterPublished =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

filterPublished :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
filterPublished = filterM $ isPublished . itemIdentifier
  where
    isPublished :: (MonadMetadata m, MonadFail m) => Identifier -> m Bool
    isPublished id' = do
      metadata <- getMetadata id'
      pure $ isJust $ lookupString "date" metadata
