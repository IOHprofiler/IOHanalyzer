library(testthat)
library(IOHanalyzer)

context("Basic DataSetList functionality, but with String-Named Functions")



test_that("Validate reading of files",{
  path1 <- system.file("extdata", "ONE_PLUS_LAMDA_EA_ws", package="IOHanalyzer")
  expect_equal(check_format(path1), "IOHprofiler")
  dsl1 <- DataSetList(path1)

  expect_true(any(match(class(dsl1), "DataSetList")))
  expect_equal(get_dim(dsl1), 100)
  expect_equal(get_algId(dsl1), "ONE_PLUS_LAMDA_EA")
  expect_equal(get_funcId(dsl1), c('bla', 'blubb'))

  ds1 <- dsl1[[1]]
  expect_true(any(match(class(ds1), "DataSet")))
  expect_equal(attr(ds1,'DIM'), 100)
  expect_equal(attr(ds1,'algId'), "ONE_PLUS_LAMDA_EA")
  #expect_equal(attr(ds1,'suite'), "PBO")
  expect_equal(attr(ds1,'funcId'), 'bla')
  expect_equal(attr(ds1,'instance'), c(1,1,1,1,1,1,1,1,1,1))
  expect_equal(attr(ds1,'format'), "IOHprofiler")
  expect_equal(attr(ds1,'maximization'), TRUE)

  ds1 <- dsl1[[2]]
  expect_true(any(match(class(ds1), "DataSet")))
  expect_equal(attr(ds1,'DIM'), 100)
  expect_equal(attr(ds1,'algId'), "ONE_PLUS_LAMDA_EA")
  #expect_equal(attr(ds1,'suite'), "PBO")
  expect_equal(attr(ds1,'funcId'), 'blubb')
  expect_equal(attr(ds1,'instance'), c(1,1,1,1,1,1,1,1,1,1))
  expect_equal(attr(ds1,'format'), "IOHprofiler")
  expect_equal(attr(ds1,'maximization'), TRUE)
})

test_that("Validate overview, summary and sample functions",{
  path1 <- system.file("extdata", "ONE_PLUS_LAMDA_EA_ws", package="IOHanalyzer")
  expect_equal(check_format(path1), "IOHprofiler")
  dsl1 <- DataSetList(path1)

  path2 <- system.file("extdata", "ONE_PLUS_LAMDA_EA", package="IOHanalyzer")
  expect_equal(check_format(path2), "IOHprofiler")
  dsl2 <- DataSetList(path2)

  expect_equal(get_FV_summary(dsl1[1], 12)$"98%", get_FV_summary(dsl2[1], 12)$"98%")
  expect_equal(get_FV_summary(dsl1[[1]], 12)$"98%", get_FV_summary(dsl2[[1]], 12)$"98%")
  expect_equal(get_RT_summary(dsl1[1], 12)$"98%", get_RT_summary(dsl2[1], 12)$"98%")
  expect_equal(get_RT_summary(dsl1[[1]], 12)$"98%", get_RT_summary(dsl2[[1]], 12)$"98%")
  expect_equal(get_FV_overview(dsl1[1])$"mean reached", get_FV_overview(dsl2[1])$"mean reached")
  expect_equal(get_FV_overview(dsl1[[1]])$"mean reached", get_FV_overview(dsl2[[1]])$"mean reached")
  expect_equal(get_RT_overview(dsl1[[1]])$"runs", get_RT_overview(dsl2[[1]])$"runs")
  expect_equal(get_RT_overview(dsl1[1])$"runs", get_RT_overview(dsl2[1])$"runs")
  expect_equal(get_FV_sample(dsl1[[1]],12)$"run.5",get_FV_sample(dsl2[[1]],12)$"run.5")
  expect_equal(get_FV_sample(dsl1[1],12)$"run.5",get_FV_sample(dsl2[1],12)$"run.5")
  expect_equal(get_RT_sample(dsl1[[1]],12)$"run.5",get_RT_sample(dsl2[[1]],12)$"run.5")
  expect_equal(get_RT_sample(dsl1[1],12)$"run.5",get_RT_sample(dsl2[1],12)$"run.5")
  expect_equal(min(get_funvals(dsl1[1])),min(get_funvals(dsl2[1])))
  expect_equal(min(get_runtimes(dsl1[1])),min(get_runtimes(dsl2[1])))
})
