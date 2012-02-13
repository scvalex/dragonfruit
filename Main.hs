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

data State = State { getCurrent :: (Skyline, DrawingArea)
                   , getHistory :: [(Skyline, DrawingArea)]
                   , getParams  :: SkylineParameters }

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
  set evoBox [boxChildPacking canvasF := PackNatural]
  widgetSetSizeRequest canvas 200 100

  let params = SkylineParameters { getMutationRate = 0.05 }
  sky <- spawn params
  stateMV <- newMVar $ State { getCurrent = (sky, canvas)
                             , getHistory = []
                             , getParams  = params }

  onExpose canvas $ const (updateCanvas canvas stateMV)

  window `on` keyPressEvent $ tryEvent $ do
         "space" <- eventKeyName
         liftIO $ do
           mutateOnce stateMV
           redrawCanvas canvas

  canvas `on` buttonPressEvent $ tryEvent $ do
         LeftButton <- eventButton
         liftIO $ newSaveCanvas stateMV

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
  modifyMVar_ stateMV $ \s@State{ getCurrent = (sky, skyCanvas)
                                , getParams = params} -> do
                       sky' <- mutate params sky
                       return s{ getCurrent = (sky', skyCanvas)
                               , getParams = params }

newSaveCanvas :: MVar State -> IO ()
newSaveCanvas stateMV = do
  modifyMVar_ stateMV $ \s@State{ getCurrent = (sky, skyCanvas)
                                , getHistory = hist
                                , getParams  = params} -> do
                          newSky <- spawn params
                          return s{ getCurrent = (newSky, skyCanvas)
                                  , getHistory = (sky, skyCanvas):hist }
  State{getCurrent = (_, canvas)} <- readMVar stateMV
  redrawCanvas canvas

drawSkyline :: Int -> Int -> State -> Render ()
drawSkyline width height State{getCurrent = (sky, _), getParams = params} = do
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
