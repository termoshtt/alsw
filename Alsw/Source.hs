{-# LANGUAGE OverloadedStrings #-}
module Alsw.Source
  ( Source
  , StaticSource
  , getSource
  , getSourceName
  , holyquintet
  )
where

import qualified Data.Text as T

class Source a where
  getSource :: a -> [T.Text]
  getSourceName :: a -> T.Text

data StaticSource = StaticSource { source :: [T.Text]
                                 , name :: T.Text
                                 }

instance Source StaticSource where
  getSource ss = source ss
  getSourceName ss = name ss

holyquintet :: StaticSource
holyquintet = StaticSource { source = ["mami", "madoka", "homhom", "sayaka", "kyoko"]
                           , name = "holyquintet"
                           }

