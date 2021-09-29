{-# LANGUAGE
    OverloadedLabels
#-}
module GUI.Chart
    ( initChartWidget
    , ChartWidget
    ) where

import           RIO                     hiding ( (.~) )

import           GI.Cairo                      as Cairo
import           GI.Gtk                        as Gtk

import           GI.Cairo.Render.Connector      ( renderWithContext )
import           Graphics.Rendering.Chart      as Ch
import           Graphics.Rendering.Chart.Backend.GI.Cairo
import           Graphics.Rendering.Chart.Easy as Ch
                                         hiding ( (^.) )


data ChartWidget = ChartWidget
    { cwDrawingArea :: !DrawingArea
    }

initChartWidget :: (MonadIO m) => DrawingArea -> m ChartWidget
initChartWidget da = do
    let gui = ChartWidget { cwDrawingArea = da }

    void $ Gtk.on da #draw (drawingFunction gui)

    pure gui



chartContent :: Renderable ()
chartContent = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x * 3.14159 / 45) + 1) / 2 * (sin (x * 3.14159 / 5))

    sinusoid1 =
        plot_lines_values
            .~ [[ (x, (am x)) | x <- [0, (0.5) .. 400] ]]
            $  plot_lines_style
            .  line_color
            .~ opaque blue
            $  plot_lines_title
            .~ "am"
            $  def

    sinusoid2 =
        plot_points_style
            .~ filledCircles 2 (opaque red)
            $  plot_points_values
            .~ [ (x, (am x)) | x <- [0, 7 .. 400] ]
            $  plot_points_title
            .~ "am points"
            $  def

    layout =
        layout_title
            .~ "Amplitude Modulation"
            $  layout_plots
            .~ [toPlot sinusoid1, toPlot sinusoid2]
            $  def


drawingFunction :: ChartWidget -> Cairo.Context -> IO Bool 
drawingFunction gui context = do 
    let drawingArea = cwDrawingArea gui 

    width <- fromIntegral <$> #getAllocatedWidth drawingArea 
    height <- fromIntegral <$> #getAllocatedHeight drawingArea 

    let rndr = runBackend (defaultEnv bitmapAlignmentFns) (render chartContent (width, height))

    void $ renderWithContext rndr context
    pure True 