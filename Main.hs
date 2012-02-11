module Main where

import Control.Concurrent.MVar
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

import Instances ()
import Types
import Skyline

type State = (Skyline, SkylineParameters)

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

  let params = SkylineParameters { getMutationRate = 0.05 }
  sky <- spawn params
  stateMV <- newMVar (sky, params)

  canvas <- drawingAreaNew
  canvasF <- frameNew
  set canvasF [containerChild := canvas]
  set evoBox [containerChild := canvasF]
  set evoBox [boxChildPacking canvasF := PackNatural]
  widgetSetSizeRequest canvas 480 200
  onExpose canvas $ const (updateCanvas canvas stateMV)

  window `on` keyPressEvent $ tryEvent $ do
         "space" <- eventKeyName
         liftIO $ do
           (w, h) <- widgetGetSize canvas
           mutateOnce stateMV
           widgetQueueDrawArea canvas 0 0 w h

  widgetShowAll window

  mainGUI

updateCanvas :: DrawingArea -> MVar State -> IO Bool
updateCanvas canvas stateMV = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  state <- readMVar stateMV
  renderWithDrawable win (drawSkyline width height state)
  return True

mutateOnce :: MVar State -> IO ()
mutateOnce stateMV = do
  modifyMVar_ stateMV $ \(sky, params) -> do
                       sky' <- mutate params sky
                       return (sky', params)

drawSkyline :: Int -> Int -> State -> Render ()
drawSkyline width height (sky, params) = do
  setupCanvas width height
  visualize params sky

setupCanvas :: Int -> Int -> Render ()
setupCanvas wWidth wHeight = do
  let width   = 10
      height  = 10
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

keepState :: Render t -> Render ()
keepState render = save >> render >> restore
