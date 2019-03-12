context("Basic DataSetList functionality")

test_that("Can DataSetLists be loaded?",{
  expect_true(any(match(class(dsl), "DataSetList")))
  expect_true(any(match(class(dsl[[1]]), "DataSet")))
})

test_that("Validate reading of files",{
  path <- system.file("extdata", "ONE_PLUS_LAMDA_EA", package="IOHProfiler")
  expect_equal(check_format(path),"IOHprofiler")
})
