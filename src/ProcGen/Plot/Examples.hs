-- | Example plot functions which look nice when you run them, and also show you how to use the
-- 'ProcGen.Plot' API if you choose to read the source code.
module ProcGen.Plot.Examples where

import           ProcGen.Plot
import           ProcGen.Types

import           Control.Lens

import           Happlets.Draw.Color

----------------------------------------------------------------------------------------------------

-- | Example 'PlotCartesian' function which plots a sigmoid and a sine-squared curve, typcially used
-- as activation functions in neural networks.
activation :: PlotCartesian ProcGenFloat
activation = plotCartesian &~ do
  plotFunctionList .=
    [ makeCartesian &~ do
        cartFunction .= sigmoid TimeWindow{ timeStart = (-1), timeEnd = 1 }
        plotLabel    .= "sigmoid"
        lineColor    .= blue
        lineWeight   .= 3.0
    , makeCartesian &~ do
        cartFunction .= sineSquared TimeWindow{ timeStart = (-1), timeEnd = 1 }
        plotLabel    .= "sineSquared"
        lineColor    .= red
        lineWeight   .= 3.0
    ]
  let axis = makePlotAxis &~ do
        plotAxisOffset .= 0.0
        plotAxisMin    .= (-1.0)
        plotAxisMax    .= 1.0
        plotAxisMajor  %= flip (&~)
          (do gridLinesSpacing .= 0.5
              lineColor        .= packRGBA32 0x40 0x40 0x40 0xA0
              lineWeight       .= 2.0
          )
        plotAxisMinor  .= Just
          ( makeGridLines &~ do
              gridLinesSpacing .= 0.1
              lineColor        .= packRGBA32 0x80 0x80 0x80 0x80
              lineWeight       .= 1.0
          )
  plotWindow %= flip (&~)
    (do xAxis .= axis
        yAxis .= (plotAxisMin .~ 0.0) axis
    )
