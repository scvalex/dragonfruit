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

  mainGUI

updateCanvas :: DrawingArea -> IO Bool
updateCanvas canvas = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  renderWithDrawable win (example width height)
  return True

example :: Int -> Int -> Render ()
example width height = do
  setupCanvas width height

setupCanvas :: Int -> Int -> Render ()
setupCanvas wWidth wHeight = do
  let width   = 10
      height  = 10
      xmax    = width / 2
      xmin    = - xmax
      ymax    = height / 2
      ymin    = - ymax
      scaleX  = realToFrac wWidth  / width
      scaleY  = realToFrac wHeight / height

  setLineCap LineCapRound
  setLineJoin LineJoinRound
  setLineWidth $ 4 / max scaleX scaleY

  -- Set up user coordinates
  scale scaleX scaleY
  -- Centre origin
  translate (width / 2) (height / 2)

  -- white canvas
  setSourceRGBA 1.0 1.0 1.0 1.0
  paint

  keepState $ do
    setSourceRGBA 0.0 0.0 0.0 0.4
    rectangle (xmin + (width * 0.1)) (ymin + (height * 0.1)) (width * 0.8) (height * 0.8)
    stroke

keepState :: Render t -> Render ()
keepState render = save >> render >> restore
