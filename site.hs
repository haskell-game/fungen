#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{- hakyll script, builds website in ./_site/ -}

import Control.Applicative
import Data.List
import Data.Monoid
import Debug.Trace
import Hakyll
import qualified Text.Blaze.Html5                 as H
import qualified Text.Blaze.Html5.Attributes      as A
import           Text.Blaze.Html                  (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String  (renderHtml)

main:: IO ()
main = hakyll $ do
  match "site.html" $ compile templateCompiler
  match "*.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler >>= loadAndApplyTemplate "site.html" defaultContext >>= relativizeUrls
  match "old-site/**" $ do
    route   idRoute
    compile copyFileCompiler
