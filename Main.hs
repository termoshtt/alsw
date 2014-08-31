{-# LANGUAGE OverloadedStrings #-}

import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput
import Graphics.Vty.Attributes
import System.Exit
-- import qualified Data.Text as T

main :: IO()
main = do
  -- Header
  title <- (plainText "alsw: anything.el-like selector widget")
           <++> (hFill ' ' 1)
  setNormalAttribute title (black `on` white)

  prompt <- plainText "> "
  e <- editWidget
  header <- (return title) <--> (hBox prompt e)

  -- Footer
  footer <- (plainText "footer") <++> (hFill ' ' 1)
  setNormalAttribute footer (black `on` white)

  -- List
  lst <- newList (fgColor white) 1
  let addPlainText = \ l s -> addToList l s =<< plainText s

  addPlainText lst "homhom"
  addPlainText lst "madoka"
  addPlainText lst "saki"

  ui <- (return header)
        <--> (return lst)
        <--> (return footer)

  -- Focus Group
  fg <- newFocusGroup
  _ <- addToFocusGroup fg e
  _ <- addToFocusGroup fg lst

  fg `onKeyPressed` \ _ key _ ->
    if key == KASCII 'q'
      then exitSuccess
      else return False

  -- Collection
  c <- newCollection
  _ <- addToCollection c ui fg

  -- Run
  runUi c defaultContext
