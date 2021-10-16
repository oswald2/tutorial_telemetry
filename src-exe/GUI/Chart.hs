{-# LANGUAGE
    OverloadedLabels
#-}
module GUI.Chart
    ( initChartWidget
    , ChartWidget
    , chartWidgetAddValues
    ) where

import           RIO                     hiding ( (.~) )
import qualified RIO.Map                       as M
import qualified RIO.Text                      as T

import           Data.GI.Gtk.Threading
import           GI.Cairo                      as Cairo
import           GI.Gtk                        as Gtk

import           GI.Cairo.Render.Connector      ( renderWithContext )
import           Graphics.Rendering.Chart      as Ch
import           Graphics.Rendering.Chart.Backend.GI.Cairo
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (^.) )

import           Data.Time.Clock

import           Data.Sequence                 as S

import           Data.List                      ( cycle )




data ChartWidget = ChartWidget
    { cwDrawingArea :: !DrawingArea
    , cwValues      :: TVar ValueMap
    }

initChartWidget :: (MonadIO m) => DrawingArea -> m ChartWidget
initChartWidget da = do
    let values = M.fromList [("S2KTP501", S.empty), ("S2KTP502", S.empty)]

    var <- newTVarIO values

    let gui = ChartWidget { cwDrawingArea = da, cwValues = var }

    void $ Gtk.on da #draw (drawingFunction gui)

    pure gui


type ValueMap = Map Text (Seq (UTCTime, Double))

chartWidgetAddValues :: (MonadIO m) => ChartWidget -> [(Text, UTCTime, Double)] -> m () 
chartWidgetAddValues chart values = do 
    atomically $ do 
        modifyTVar (cwValues chart) ins
    redrawChart chart 
    where 
        ins :: ValueMap -> ValueMap
        ins valMap = foldl' singleIns valMap values

        singleIns valMap (name, t, pv) = M.adjust (S.|> (t, pv)) name valMap



redrawChart :: (MonadIO m) => ChartWidget -> m ()
redrawChart chart =
    liftIO $ postGUIASync $ Gtk.widgetQueueDraw (cwDrawingArea chart)


chartContent :: ValueMap -> Renderable ()
chartContent valueMap =
    let plots = RIO.zipWith plotVal (M.toList valueMap) chartColors
    in  toRenderable (layout plots)
  where
    chartColors = cycle [opaque green, opaque orange, opaque darkblue]

    plotVal (name, vals) color =
        plot_lines_values
            .~ [toList vals]
            $  plot_lines_style
            .  line_color
            .~ color
            $  plot_lines_style
            .  line_width
            .~ 2
            $  plot_lines_title
            .~ T.unpack name
            $  def

    layout ps =
        layout_title
            .~ "TM Parameter Values"
            $  layout_plots
            .~ map toPlot ps
            $  def


drawingFunction :: ChartWidget -> Cairo.Context -> IO Bool
drawingFunction gui context = do
    let drawingArea = cwDrawingArea gui

    width   <- fromIntegral <$> #getAllocatedWidth drawingArea
    height  <- fromIntegral <$> #getAllocatedHeight drawingArea

    content <- readTVarIO (cwValues gui)

    let rndr = runBackend (defaultEnv bitmapAlignmentFns)
                          (render (chartContent content) (width, height))

    void $ renderWithContext rndr context
    pure True
