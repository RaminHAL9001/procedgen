-- | Example plot functions which look nice when you run them, and also show you how to use the
-- 'Procedgen.Plot' API if you choose to read the source code.
module Procedgen.Plot.Examples where

import           Procedgen.Plot
import           Procedgen.Types

import           Control.Lens

import           Happlets.Draw.Color

----------------------------------------------------------------------------------------------------

-- | A plot with reasonable default values for inspecting functions in the range between -1.0 and
-- 1.0.
smallPlotAxis :: PlotAxis ProcGenFloat
smallPlotAxis = makePlotAxis &~
  (do axisBounds     .= (-2.5, 2.5)
      axisMajor      .=
        ( makeGridLines &~ do
            gridLinesSpacing .= (0.5)
            lineWeight       .= (1.5)
            lineColor        .= packRGBA32 0x7F 0x7F 0x7F 0xBF )
      axisMinor      .= Just
        ( makeGridLines &~ do
            gridLinesSpacing .= (0.125)
            lineWeight       .= (0.5)
            lineColor        .= packRGBA32 0xBF 0xBF 0xBF 0xBF )
      axisDrawOrigin .= Just
        ( makeLineStyle &~ do
            lineWeight .= (3.0)
            lineColor  .= packRGBA32 0x00 0x00 0x00 0xBF ))

-- | An unpopulated cartesian plot with a 'smallPlotAxis', with reasonable defaults for inspecting
-- functions in the range between -1.0 and 1.0.
smallCart :: PlotCartesian ProcGenFloat
smallCart = plotCartesian &~ do
  plotWindow .=
    ( makePlotWindow &~ do
        xDimension .= smallPlotAxis
        yDimension .=
          ( smallPlotAxis &~ do
               axisBounds .= (-1.5, 1.5) )
        bgColor    .= packRGBA32 0xFF 0xFF 0xFF 0xBF )
  plotFunctionList .= []

-- | Example 'PlotCartesian' function which plots a sigmoid and a sine-squared curve, typcially used
-- as activation functions in neural networks.
activation :: PlotCartesian ProcGenFloat
activation = smallCart &~ do
  plotWindow %= flip (&~)
    (do xDimension .= smallPlotAxis
        yDimension . axisMin .= (0.0) )
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
