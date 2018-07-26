import System.Time
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour

prices = [(1,3,2010,v1,v2) | (v1,v2) <- zip [10..50][20..60]]

date dd mm yyyy = dd + 10*mm + 100*yyyy


chart = layout 
  where
    price1 = defaultPlotFillBetween {
        plot_fillbetween_style = solidFillStyle (black 33),
        plot_fillbetween_values = [ (date d m y,(0,v1)) | (d,m,y,v1,v2) <- prices]
    }
    layout = defaultLayout1 {
        layout1_title="Price History",			   
        layout1_right_axis=linkAxes (months defaultAxisLineStyle),
        layout1_left_axis=linkAxes (autoScaledAxis defaultAxisLineStyle),
        layout1_plots_ = [("price 1", toPlot price1)]
    }

main = do
    renderableToWindow (toRenderable chart) 640 480
    renderableToPNGFile (toRenderable chart) 640 480 "test3.png"

