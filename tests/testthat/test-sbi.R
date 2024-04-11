library(testthat)


testthat::test_that("One argument missing", {
  expect_error(BlindingIndex(50, 0, 0), "Invalid input format.")
})

testthat::test_that("Data frame as an argument", {
  expect_error(BlindingIndex(as.data.frame(cbind(c(10,10), c(10,10)))), "Invalid input format.")
})

testthat::test_that("Input table with wrong dimensions", {
  expect_error(BlindingIndex(as.table(cbind(c(10,10,10), c(10,10,10)))), "Invalid input format.")
})

testthat::test_that("Group A is empty", {
  expect_error(BlindingIndex(0, 0, 50, 50), "No one is in group A or no one from group A provided data.")
})

testthat::test_that("Group B is empty", {
  expect_error(BlindingIndex(50, 50, 0, 0), "No one is in group B or no one from group B provided data.")
})




testthat::test_that("Testing the z value for quadratic equation, m=1", {
  expect_equal(as.numeric(BlindingIndex(1/2, 1/2 ,0 ,1)["z"]), z_calc_special_case_quadratic(n_BA=1/2))
})

testthat::test_that("Testing the z value for quadratic equation, m=2", {
  expect_equal(as.numeric(BlindingIndex(1, 1, 0, 2)["z"]), z_calc_special_case_quadratic(n_BA=1))
})

testthat::test_that("Testing the z value for quadratic equation, m=10", {
  expect_equal(as.numeric(BlindingIndex(5, 5, 0, 10)["z"]), z_calc_special_case_quadratic(n_BA=5))
})

testthat::test_that("Testing the z value for quadratic equation, m=100", {
  expect_equal(as.numeric(BlindingIndex(50, 50, 0, 100)["z"]), z_calc_special_case_quadratic(n_BA=50))
})

testthat::test_that("Testing the z value for quadratic equation, m=1000", {
  expect_equal(as.numeric(BlindingIndex(500, 500, 0, 1000)["z"]), z_calc_special_case_quadratic(n_BA=500))
})

testthat::test_that("Testing the z value for cubic equation, m=1, n=2", {
  expect_equal(as.numeric(BlindingIndex(1/2, 1/2, 0, 2)["z"]), z_calc_special_case_cubic(n_BA=1/2, n_BB=2))
})

testthat::test_that("Testing the z value for cubic equation, m=1, n=10", {
  expect_equal(as.numeric(BlindingIndex(1/2, 1/2, 0, 10)["z"]), z_calc_special_case_cubic(n_BA=1/2, n_BB=10))
})

testthat::test_that("Testing the z value for cubic equation, m=1, n=100", {
  expect_equal(as.numeric(BlindingIndex(1/2,1/2,0,100)["z"]), z_calc_special_case_cubic(n_BA=1/2, n_BB=100))
})

testthat::test_that("Testing the z value for cubic equation, m=1, n=1000", {
  expect_equal(as.numeric(BlindingIndex(1/2, 1/2, 0, 1000)["z"]), z_calc_special_case_cubic(n_BA=1/2, n_BB=1000))
})

testthat::test_that("Testing the z value for cubic equation, m=2, n=1", {
  expect_equal(as.numeric(BlindingIndex(1, 1, 0, 1)["z"]), z_calc_special_case_cubic(n_BA=1, n_BB=1))
})

testthat::test_that("Testing the z value for cubic equation, m=2, n=10", {
  expect_equal(as.numeric(BlindingIndex(1, 1, 0, 10)["z"]), z_calc_special_case_cubic(n_BA=1, n_BB=10))
})

testthat::test_that("Testing the z value for cubic equation, m=2, n=100", {
  expect_equal(as.numeric(BlindingIndex(1, 1, 0, 100)["z"]), z_calc_special_case_cubic(n_BA=1, n_BB=100))
})

testthat::test_that("Testing the z value for cubic equation, m=2, n=1000", {
  expect_equal(as.numeric(BlindingIndex(1, 1, 0, 1000)["z"]), z_calc_special_case_cubic(n_BA=1, n_BB=1000))
})

testthat::test_that("Testing the z value for cubic equation, m=10, n=1", {
  expect_equal(as.numeric(BlindingIndex(5, 5, 0, 1)["z"]), z_calc_special_case_cubic(n_BA=5, n_BB=1))
})

testthat::test_that("Testing the z value for cubic equation, m=10, n=100", {
  expect_equal(as.numeric(BlindingIndex(5, 5, 0, 100)["z"]), z_calc_special_case_cubic(n_BA=5, n_BB=100))
})

testthat::test_that("Testing the z value for cubic equation, m=10, n=1000", {
  expect_equal(as.numeric(BlindingIndex(5, 5, 0, 1000)["z"]), z_calc_special_case_cubic(n_BA=5, n_BB=1000))
})

testthat::test_that("Testing the z value for cubic equation, m=100, n=1", {
  expect_equal(as.numeric(BlindingIndex(50, 50, 0, 1)["z"]), z_calc_special_case_cubic(n_BA=50, n_BB=1))
})

testthat::test_that("Testing the z value for cubic equation, m=100, n=10", {
  expect_equal(as.numeric(BlindingIndex(50, 50, 0, 10)["z"]), z_calc_special_case_cubic(n_BA=50, n_BB=10))
})

testthat::test_that("Testing the z value for cubic equation, m=100, n=1000", {
  expect_equal(as.numeric(BlindingIndex(50, 50, 0, 1000)["z"]), z_calc_special_case_cubic(n_BA=50, n_BB=1000))
})

testthat::test_that("Testing the z value for cubic equation, m=1000, n=1", {
  expect_equal(as.numeric(BlindingIndex(500, 500, 0, 1)["z"]), z_calc_special_case_cubic(n_BA=500, n_BB=1))
})

testthat::test_that("Testing the z value for cubic equation, m=1000, n=10", {
  expect_equal(as.numeric(BlindingIndex(500, 500, 0, 10)["z"]), z_calc_special_case_cubic(n_BA=500, n_BB=10))
})

testthat::test_that("Testing the z value for cubic equation, m=1000, n=100", {
  expect_equal(as.numeric(BlindingIndex(500, 500, 0, 100)["z"]), z_calc_special_case_cubic(n_BA=500, n_BB=100))
})

testthat::test_that("Perfect blinding has index = 0", {
  expect_equal(as.numeric(BlindingIndex(50, 50, 50, 50)["est"]), 0)
})

testthat::test_that("Perfect blinding has index = 0", {
  expect_equal(as.numeric(BlindingIndex(as.table(cbind(c(50, 50), c(50, 50))))["est"]), 0)
})

testthat::test_that("Perfect blinding has p-value = 1", {
  expect_equal(as.numeric(BlindingIndex(50, 50, 50, 50)["p.value"]), 1)
})

testthat::test_that("Zero blinding has index = 1", {
  expect_equal(as.numeric(BlindingIndex(100, 0, 0, 100)["est"]), 1)
})

testthat::test_that("Zero blinding has p-value = 1", {
  expect_equal(as.numeric(BlindingIndex(100, 0, 0, 100)["p.value"]), 1)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (a), Estimate.", {
  expect_equal(as.numeric(BlindingIndex(56, 14, 48, 32)["est"]), 0.2)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (a), Estimate.", {
  expect_equal(as.numeric(BlindingIndex(as.table(cbind(c(56, 14), c(48, 32))))["est"]), 0.2)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (a), Lower CI.", {
  expect_equal(round(as.numeric(BlindingIndex(56, 14, 48, 32)["lwr.ci"]), 4), 0.0524)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (a), Upper CI.", {
  expect_equal(round(as.numeric(BlindingIndex(56, 14, 48, 32)["upr.ci"]), 4), 0.3339)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (b), Estimate.", {
  expect_equal(as.numeric(BlindingIndex(9, 1, 3, 7)["est"]), 0.6)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (b), Lower CI.", {
  expect_equal(round(as.numeric(BlindingIndex(9, 1, 3, 7)["lwr.ci"]), 4), 0.1705)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (b), Upper CI.", {
  expect_equal(round(as.numeric(BlindingIndex(9, 1, 3, 7)["upr.ci"]), 4), 0.8090)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (c), Estimate.", {
  expect_equal(round(as.numeric(BlindingIndex(6, 1, 2, 5)["est"]), 4), 0.5714)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (c), Lower CI.", {
  expect_equal(round(as.numeric(BlindingIndex(6, 1, 2, 5)["lwr.ci"]), 4), 0.0582)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (c), Upper CI.", {
  expect_equal(round(as.numeric(BlindingIndex(6, 1, 2, 5)["upr.ci"]), 4), 0.8062)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (d), Estimate.", {
  expect_equal(round(as.numeric(BlindingIndex(5, 51, 0, 29)["est"]), 4), 0.0893)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (d), Lower CI.", {
  expect_equal(round(as.numeric(BlindingIndex(5, 51, 0, 29)["lwr.ci"]), 4), -0.0381)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (d), Upper CI.", {
  expect_equal(round(as.numeric(BlindingIndex(5, 51, 0, 29)["upr.ci"]), 4), 0.1926)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (e), Estimate.", {
  expect_equal(as.numeric(BlindingIndex(0, 10, 0, 20)["est"]), 0)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (e), Lower CI.", {
  expect_equal(round(as.numeric(BlindingIndex(0, 10, 0, 20)["lwr.ci"]), 4), -0.1611)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (e), Upper CI.", {
  expect_equal(round(as.numeric(BlindingIndex(0, 10, 0, 20)["upr.ci"]), 4), 0.2775)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (f), Estimate.", {
  expect_equal(as.numeric(BlindingIndex(0, 10, 0, 10)["est"]), 0)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (f), Lower CI.", {
  expect_equal(round(as.numeric(BlindingIndex(0, 10, 0, 10)["lwr.ci"]), 4), -0.2775)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (f), Upper CI.", {
  expect_equal(round(as.numeric(BlindingIndex(0, 10, 0, 10)["upr.ci"]), 4), 0.2775)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (g), Estimate.", {
  expect_equal(as.numeric(BlindingIndex(10, 0, 0, 20)["est"]), 1)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (g), Lower CI.", {
  expect_equal(round(as.numeric(BlindingIndex(10, 0, 0, 20)["lwr.ci"]), 4), 0.6791)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (g), Upper CI.", {
  expect_equal(as.numeric(BlindingIndex(10, 0, 0, 20)["upr.ci"]), 1)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (h), Estimate.", {
  expect_equal(as.numeric(BlindingIndex(10, 0, 0, 10)["est"]), 1)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (h), Lower CI.", {
  expect_equal(round(as.numeric(BlindingIndex(10, 0, 0, 10)["lwr.ci"]), 4), 0.6075)
})

testthat::test_that("Example from Newcombe 1998, PMID: 9595617, Table II, Method 10, Column (h), Upper CI.", {
  expect_equal(as.numeric(BlindingIndex(10, 0, 0, 10)["upr.ci"]), 1)
})


