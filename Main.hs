module Main where

import Control.Monad
import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as M
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
  prologue width height

-- Set up stuff
prologue :: Int -> Int -> Render ()
prologue wWidth wHeight = do
  let width   = 10
      height  = 10
      xmax    = width / 2
      xmin    = - xmax
      ymax    = height / 2
      ymin    = - ymax
      scaleX  = realToFrac wWidth  / width
      scaleY  = realToFrac wHeight / height

  -- style and color
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  setLineWidth $ 1 / max scaleX scaleY
  setSourceRGBA 0.5 0.7 0.5 0.5

  -- Set up user coordinates
  scale scaleX scaleY
  -- center origin
  translate (width / 2) (height / 2)
  -- positive y-axis upwards
  let flipY = M.Matrix 1 0 0 (-1) 0 0
  transform flipY

  grid xmin xmax ymin ymax

grid :: Double -> Double -> Double -> Double -> Render ()
grid xmin xmax ymin ymax =
  keepState $ do
  setSourceRGBA 0 0 0 0.7
  -- axes
  moveTo 0 ymin; lineTo 0 ymax; stroke
  moveTo xmin 0; lineTo xmax 0; stroke
  -- grid
  setDash [0.01, 0.99] 0
  forM [xmin .. xmax] $ \ x ->
      do moveTo x ymin
         lineTo x ymax
         stroke

keepState :: Render t -> Render ()
keepState render = do
  save
  _ <- render
  restore
