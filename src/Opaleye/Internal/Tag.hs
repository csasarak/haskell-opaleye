{-# LANGUAGE OverloadedStrings #-}
module Opaleye.Internal.Tag where

import Data.Text

-- | Tag is for use as a source of unique IDs in QueryArr
newtype Tag = UnsafeTag Int deriving (Read, Show)

start :: Tag
start = UnsafeTag 1

next :: Tag -> Tag
next = UnsafeTag . (+1) . unsafeUnTag

unsafeUnTag :: Tag -> Int
unsafeUnTag (UnsafeTag i) = i

tagWith :: Tag -> Text -> Text
tagWith t s = s <> "_" <> (pack . show $ unsafeUnTag t)
