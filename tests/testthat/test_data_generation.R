context("Data generation for plotting")

test_that("Single-function info",{
  dt <- generate_data.Single_Function(subset(dsl, funcId == 1), which = 'by_RT')
  expect_true(is.data.table(dt))
  expect_true(all(c("DIM", "funcId", "ID", "target", "ERT", 
                    "lower", "upper", "mean", "median") %in% colnames(dt)))
  
  dt <- generate_data.Single_Function(subset(dsl, funcId == 1), which = 'by_FV')
  expect_true(is.data.table(dt))
  expect_true(all(c("DIM", "funcId", "ID", "runtime", 
                    "lower", "upper", "mean", "median") %in% colnames(dt)))
  expect_false("ERT" %in% colnames(dt))
  expect_error(generate_data.Single_Function(dsl))
})

test_that("PMF-data", {
  dt <- generate_data.PMF(subset(dsl, funcId == 1), target = 15, which = 'by_RT')
  expect_true(is.data.table(dt))
  expect_true(all(c("DIM", "funcId", "ID", "target", "RT") %in% colnames(dt)))
  expect_false("f(x)" %in% colnames(dt))
  
  dt <- generate_data.PMF(subset(dsl, funcId == 1), target = 100, which = 'by_FV')
  expect_true(is.data.table(dt))
  expect_true(all(c("DIM", "funcId", "ID", "runtime", "f(x)") %in% colnames(dt)))
  expect_false("RT" %in% colnames(dt))
  expect_error(generate_data.PMF(dsl))
})

test_that("Histogram-data", {
  dt <- generate_data.hist(subset(dsl, funcId == 1), target = 15, which = 'by_RT')
  expect_true(is.data.table(dt))
  expect_true(all(c("x", "y", "width", "text", "ID") %in% colnames(dt)))
  
  dt <- generate_data.hist(subset(dsl, funcId == 1), target = 100, which = 'by_FV')
  expect_true(is.data.table(dt))
  expect_true(all(c("x", "y", "width", "text", "ID") %in% colnames(dt)))  
  expect_error(generate_data.hist(dsl))
})

test_that("ECDF-data (single function)", {
  dt <- generate_data.ECDF(subset(dsl, funcId == 1), c(10, 15, 16))
  expect_true(is.data.table(dt))
  expect_true(all(c("x", "mean", "ID") %in% colnames(dt)))
  expect_true(all(dt[['mean']] <= 1) && all(dt[['mean']] >= 0) )
  
  dt <- generate_data.ECDF(subset(dsl, funcId == 1), c(1, 10, 100), which = 'by_FV')
  expect_true(is.data.table(dt))
  expect_true(all(c("x", "mean", "ID") %in% colnames(dt)))
  expect_true(all(dt[['mean']] <= 1) && all(dt[['mean']] >= 0) )
  expect_error(generate_data.ECDF(dsl, c(10, 15, 16)))
})

test_that("ECDF-data (multiple functions, auto-generated targets)", {
  targets <- get_ECDF_targets(dsl, 'linear', 3)
  expect_true(is.data.table(targets))
  
  dt <- generate_data.ECDF(dsl, targets)
  expect_true(is.data.table(dt))
  expect_true(all(c("x", "mean", "ID") %in% colnames(dt)))
  expect_true(all(dt[['mean']] <= 1) && all(dt[['mean']] >= 0) )
  
  dt <- generate_data.ECDF(dsl, c(1, 10, 100), which = 'by_FV')
  expect_true(is.data.table(dt))
  expect_true(all(c("x", "mean", "ID") %in% colnames(dt)))
  expect_true(all(dt[['mean']] <= 1) && all(dt[['mean']] >= 0) )
})

test_that("AUC-data", {
  dt <- generate_data.AUC(subset(dsl, funcId == 1), c(10, 13, 16))
  expect_true(is.data.table(dt))
  expect_true(all(c("x", "auc", "ID") %in% colnames(dt)))
  expect_true(all(dt[['auc']] <= 1) && all(dt[['auc']] >= 0) )
  
  dt <- generate_data.AUC(subset(dsl, funcId == 1), c(1, 10, 100), which = 'by_FV')
  expect_true(is.data.table(dt))
  expect_true(all(c("x", "auc", "ID") %in% colnames(dt)))
  expect_true(all(dt[['auc']] <= 1) && all(dt[['auc']] >= 0) )
  
  dt <- generate_data.AUC(dsl, get_ECDF_targets(dsl))
  expect_true(is.data.table(dt))
  subset(dsl, funcId == 1)
  expect_true(all(dt[['auc']] <= 1) && all(dt[['auc']] >= 0) )
})

test_that("Parameter-data", {
  dt <- generate_data.Parameters(subset(dsl, funcId == 1))
  expect_true(is.data.table(dt))
  expect_true(all(c("ID", "runtime", "parId", 
                    "lower", "upper", "mean", "median") %in% colnames(dt)))
  
  dt <- generate_data.Parameters(subset(dsl, funcId == 1), which = 'by_FV')
  expect_true(is.data.table(dt))
  expect_true(all(c("ID", "target", "parId", 
                    "lower", "upper", "mean", "median") %in% colnames(dt)))
  
  expect_error(generate_data.Parameters(dsl))
})

test_that("Aggregated data for multiple functions / dimensions", {
  targets <- get_target_dt(dsl)
  expect_true(is.data.table(targets))
  expect_true(all(c("funcId", "DIM", "target") %in% colnames(targets)))
  
  dt <- generate_data.Aggr(dsl, targets = targets)
  expect_true(is.data.table(dt))
  expect_true(all(c("ID", "target", "rank", "DIM", "funcId", "value",
                    "median") %in% colnames(dt)))
  
  dt <- generate_data.Aggr(dsl, which = 'by_FV')
  expect_true(is.data.table(dt))
  expect_true(all(c("ID", "runtime", "rank", "DIM", "funcId", "value",
                    "median") %in% colnames(dt)))
  
  expect_error(generate_data.Aggr(dsl, targets = c(12, 16)))
})
