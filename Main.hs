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

  canvas <- drawingAreaNew
  canvasF <- frameNew
  set canvasF [containerChild := canvas]
  set topBox [containerChild := canvasF]
  widgetSetSizeRequest canvas 480 480
  onExpose canvas $ const (updateCanvas canvas)

  hButtonBox <- hButtonBoxNew
  set topBox [containerChild := hButtonBox]

  bts <- mapM buttonNewWithLabel ["One", "Two", "Three"]

  set hButtonBox [containerChild := button | button <- bts ]
  set hButtonBox [ buttonBoxLayoutStyle := ButtonboxStart
                 , buttonBoxChildSecondary (bts !! 2) := True ]

  widgetShowAll window

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
      x = 0
      y = 0
  save
  setSourceRGBA 0 0 0 0.7
  stroke
  translate (x + width / 2) (y + height / 2)
  scale (1 / (height / 2)) (1 / (width / 2))
  arc 0 0 0.4 0 (2 * pi)
  restore
