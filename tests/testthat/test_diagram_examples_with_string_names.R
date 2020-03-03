library(testthat)
library(IOHanalyzer)

context("Test the Diagram Examples with String Names")



test_that("Test the diagram examples with string names",{
  path1 <- system.file("extdata", "ONE_PLUS_LAMDA_EA_ws", package="IOHanalyzer")
  expect_equal(check_format(path1), "IOHprofiler")
  dsl1 <- DataSetList(path1)

  Plot.RT.Single_Func(subset(dsl1, funcId == 'bla'))
  Plot.FV.Single_Func(subset(dsl1, funcId == 'bla'))
  Plot.RT.PMF(subset(dsl1, funcId == 'bla'), 14)
  Plot.RT.Histogram(subset(dsl1, funcId == 'bla'), 14)
  Plot.RT.ECDF_Per_Target(subset(dsl1, funcId == 'bla'), 14)
  Plot.RT.ECDF_Single_Func(subset(dsl1, funcId == 'bla'))
  suppressWarnings(Plot.RT.ECDF_AUC(subset(dsl1, funcId == 'bla')))
  Plot.FV.PDF(subset(dsl1, funcId == 'bla'), 100)
  Plot.FV.Histogram(subset(dsl1, funcId == 'bla'), 100)
  Plot.FV.ECDF_Per_Target(subset(dsl1, funcId == 'bla'), 10)
  Plot.FV.ECDF_Single_Func(subset(dsl1, funcId == 'bla'))
  suppressWarnings(Plot.FV.ECDF_AUC(subset(dsl1, funcId == 'bla')))
  Plot.RT.Parameters(subset(dsl1, funcId == 'bla'))
  Plot.FV.Parameters(subset(dsl1, funcId == 'bla'))
  Plot.RT.ECDF_Multi_Func(dsl1)
  Plot.RT.Multi_Func(dsl1)
#  Plot.RT.Aggregated(dsl1) this will fail also with the original data
#  Plot.FV.Aggregated(dsl1) this will fail also with the original data
  Plot.FV.Multi_Func(dsl1)
  Plot.Stats.Significance_Heatmap(subset(dsl1, funcId == 'bla'), 16)
  Plot.Stats.Significance_Graph(subset(dsl1, funcId == 'bla'), 16)
#  Plot.Stats.Glicko2_Candlestick(dsl1, nr_rounds=2)  this will fail also with the original data
})
