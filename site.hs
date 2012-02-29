#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-
website build script
ghc --make hakyll.hs && ./hakyll build
-}

-- import Prelude hiding (id)
-- import Control.Category (id)
import Control.Arrow ((>>>)) --, (***), arr)
import Hakyll

-- siteurl = "http://joyful.com/fungen"

main:: IO ()
main = hakyll $ do
    match "*.md" $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "site.hamlet"
            >>> relativizeUrlsCompiler

    -- match "images/*" $ do
    --     route   idRoute
    --     compile copyFileCompiler

    -- match "files/*" $ do
    --     route   idRoute
    --     compile copyFileCompiler

    -- match "css/*" $ do
    --     route   idRoute
    --     compile compressCssCompiler

    match "*.hamlet" $ compile templateCompiler

    -- match "robots.txt" $ do
    --     route   idRoute
    --     compile copyFileCompiler

