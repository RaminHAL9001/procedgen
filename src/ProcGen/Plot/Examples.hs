-- | Example plot functions which look nice when you run them, and also show you how to use the
-- 'ProcGen.Plot' API if you choose to read the source code.
module ProcGen.Plot.Examples where

import           ProcGen.Plot
import           ProcGen.Types

import           Control.Lens

import           Happlets.Draw.Color

----------------------------------------------------------------------------------------------------

-- | A plot with reasonable default values for inspecting functions in the range between -1.0 and
-- 1.0.
smallPlotAxis :: PlotAxis ProcGenFloat
smallPlotAxis = makePlotAxis &~
  (do plotAxisBounds    .= (-2.5, 2.5)
      plotAxisMajor  .=
        ( makeGridLines &~ do
            gridLinesSpacing .= (0.5)
            lineWeight       .= (1.0)
            lineColor        .= packRGBA32 0x7F 0x7F 0x7F 0xBF )
      plotAxisMinor  .= Just
        ( makeGridLines &~ do
            gridLinesSpacing .= (0.125)
            lineWeight       .= (1.0)
            lineColor        .= packRGBA32 0xBF 0xBF 0xBF 0xBF )
      plotAxisDrawOrigin .= Just
        ( makeLineStyle &~ do
            lineWeight .= (2.0)
            lineColor  .= packRGBA32 0x00 0x00 0x00 0xBF ))

-- | An unpopulated cartesian plot with a 'smallPlotAxis', with reasonable defaults for inspecting
-- functions in the range between -1.0 and 1.0.
smallCart :: PlotCartesian ProcGenFloat
smallCart = plotCartesian &~ do
  plotWindow .=
    ( makePlotWindow &~ do
        xAxis   .= smallPlotAxis
        yAxis   .=
          ( smallPlotAxis &~ do
               plotAxisBounds .= (-1.5, 1.5) )
        bgColor .= packRGBA32 0xFF 0xFF 0xFF 0xBF )
  plotFunctionList .= []

-- | Example 'PlotCartesian' function which plots a sigmoid and a sine-squared curve, typcially used
-- as activation functions in neural networks.
activation :: PlotCartesian ProcGenFloat
activation = smallCart &~ do
  plotWindow %= flip (&~)
    (do xAxis .= smallPlotAxis
        yAxis . plotAxisMin .= (0.0) )
  plotFunctionList .=
    [ makeCartesian &~ do
        plotLabel    .= "sigmoid"
        lineColor    .= blue
        lineWeight   .= 3.0
        cartFunction .= sigmoid     TimeWindow{ timeStart = (-1), timeEnd = 1 }
    , makeCartesian &~ do
        plotLabel    .= "sineSquared"
        lineColor    .= red
        lineWeight   .= 3.0
        cartFunction .= sineSquared TimeWindow{ timeStart = (-1), timeEnd = 1 } ]
