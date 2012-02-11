module Main where

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI

  window <- windowNew
  window `onDestroy` mainQuit

  set window [containerBorderWidth := 10]

  hButtonBox <- hButtonBoxNew
  set window [containerChild := hButtonBox]

  bts <- mapM buttonNewWithLabel ["One", "Two", "Three"]

  set hButtonBox [containerChild := button | button <- bts ]
  set hButtonBox [ buttonBoxLayoutStyle := ButtonboxStart
                 , buttonBoxChildSecondary (bts !! 2) := True ]

  widgetShowAll window

  mainGUI


