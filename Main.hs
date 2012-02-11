module Main where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI

  window <- windowNew
  window `onDestroy` mainQuit

  set window [containerBorderWidth := 10]

  topBox <- vBoxNew False 3
  set window [containerChild := topBox]

  titleLabel <- labelNew (Just "Dragonfruit")
  set topBox [containerChild := titleLabel]
  set topBox [boxChildPacking titleLabel := PackNatural]

  mainScroller <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy mainScroller PolicyAutomatic PolicyAutomatic
  set topBox [containerChild := mainScroller]

  evoBox <- vBoxNew False 1
  scrolledWindowAddWithViewport mainScroller evoBox
  set evoBox [containerBorderWidth := 5]

  canvas <- drawingAreaNew
  canvasF <- frameNew
  set canvasF [containerChild := canvas]
  set evoBox [containerChild := canvasF]
  widgetSetSizeRequest canvas 480 480
  onExpose canvas $ const (updateCanvas canvas)

  hButtonBox <- hButtonBoxNew
  set evoBox [containerChild := hButtonBox]

  bts <- mapM buttonNewWithLabel ["One", "Two", "Three"]

  set hButtonBox [containerChild := button | button <- bts ]
  set hButtonBox [ buttonBoxLayoutStyle := ButtonboxStart
                 , buttonBoxChildSecondary (bts !! 2) := True ]

  widgetShowAll window

  updateCanvas canvas

  mainGUI

updateCanvas :: DrawingArea -> IO Bool
updateCanvas canvas = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  renderWithDrawable win (example width height)
  return True

example :: Int -> Int -> Render ()
example width0 height0 = do
  let (width, height) = (fromIntegral width0, fromIntegral height0)
  save
  setSourceRGBA 0 0 0 0.7
  stroke
  rectangle 10 10 (width - 10) (height - 10)
  restore
