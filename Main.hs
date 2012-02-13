module Main where

import Common
import Control.Concurrent.MVar
import Control.Monad
import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as M
import Graphics.UI.Gtk

import Instances ()
import Types
import Skyline

data State = State { getCurrent :: (MVar Skyline, Frame)
                   , getParams  :: SkylineParameters
                   , getCurrentContainer :: Frame
                   , getHistoryContainer :: VBox
                   , getNextHandler :: MVar (ConnectId DrawingArea) }

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

  (canvas, canvasF) <- newFramedCanvas
  containerAdd evoBox canvasF
  set evoBox [boxChildPacking canvasF := PackNatural]

  let params = SkylineParameters { getMutationRate = 0.05 }
  sky <- spawn params
  skyMV <- newMVar sky
  nextHandlerMV <- newEmptyMVar
  stateMV <- newMVar $ State { getCurrent = (skyMV, canvasF)
                             , getParams  = params
                             , getCurrentContainer = canvasF
                             , getHistoryContainer = evoBox
                             , getNextHandler = nextHandlerMV }

  onExpose canvas $ const (updateCanvas canvas skyMV params)

  hdlNext <- canvas `on` buttonPressEvent $ tryEvent $ do
               LeftButton <- eventButton
               liftIO $ newSaveCanvas stateMV
  putMVar nextHandlerMV hdlNext

  widgetShowAll window

  mainGUI

newFramedCanvas :: IO (DrawingArea, Frame)
newFramedCanvas = do
  canvas <- drawingAreaNew
  canvasF <- frameNew
  containerAdd canvasF canvas
  widgetSetSizeRequest canvas 200 100
  return (canvas, canvasF)

updateCanvas :: DrawingArea -> MVar Skyline -> SkylineParameters -> IO Bool
updateCanvas canvas skyMV params = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  sky <- readMVar skyMV
  renderWithDrawable win (drawSkyline width height sky params)
  return True

mutateOnce :: MVar Skyline -> SkylineParameters -> IO ()
mutateOnce skyMV params = modifyMVar_ skyMV $ mutate params

newSaveCanvas :: MVar State -> IO ()
newSaveCanvas stateMV = do
  s@State { getCurrent = (skyMV, canvasF)
          , getParams  = params
          , getHistoryContainer = evoBox
          , getNextHandler = nextHandlerMV } <- takeMVar stateMV
  nextHandler <- takeMVar nextHandlerMV
  signalDisconnect nextHandler

  containerRemove evoBox canvasF
  containerAdd evoBox canvasF
  set evoBox [boxChildPacking canvasF := PackNatural]

  (newCanvas, newCanvasF) <- newFramedCanvas
  containerAdd evoBox newCanvasF
  set evoBox [boxChildPacking newCanvasF := PackNatural]
  sky <- readMVar skyMV
  newSkyMV <- newMVar sky
  onExpose newCanvas $ const (updateCanvas newCanvas newSkyMV params)

  hdlNext <- newCanvas `on` buttonPressEvent $ tryEvent $ do
               LeftButton <- eventButton
               liftIO $ newSaveCanvas stateMV
  putMVar nextHandlerMV hdlNext

  mutateOnce newSkyMV params
  widgetShowAll evoBox

  putMVar stateMV s{getCurrent = (newSkyMV, newCanvasF)}

drawSkyline :: Int -> Int -> Skyline -> SkylineParameters -> Render ()
drawSkyline width height sky params = do
  setupCanvas width height
  inUserCoordinates width height $ visualize params sky

setupCanvas :: Int -> Int -> Render ()
setupCanvas devWidth devHeight = do
  -- White-out canvas
  setSourceRGBA 1.0 1.0 1.0 1.0
  paint

  when debug $ keepState (boundingBox devWidth devHeight)

boundingBox :: Int -> Int -> Render ()
boundingBox devWidth devHeight = do
  inUserCoordinates devWidth devHeight $ do
    setSourceRGBA 0.0 0.0 0.0 1.0
    rectangle 0.1 0.1 7.8 0.8
    stroke

inUserCoordinates :: Int -> Int -> Render a -> Render a
inUserCoordinates devWidth devHeight render =
    keepState $ do
      -- Switch to user coordinates
      let scaleX  = realToFrac devWidth / 8.0
          scaleY  = realToFrac devHeight

      setLineCap LineCapRound
      setLineJoin LineJoinRound
      setLineWidth $ 4 / max scaleX scaleY

      -- Set up user coordinates
      scale scaleX scaleY
      -- Positive y-axis upwards
      translate 0.0 1.0
      let flipY = M.Matrix 1 0 0 (-1) 0 0
      transform flipY

      render

keepState :: Render t -> Render t
keepState render = save >> render >>= \x -> restore >> return x

redrawCanvas :: DrawingArea -> IO ()
redrawCanvas canvas = do
  (w, h) <- widgetGetSize canvas
  widgetQueueDrawArea canvas 0 0 w h
