test_that("It is a default clockPlot",{
  testPlot <- clockPlot(Sys.time())
  expect_true(ggplot2::is.ggplot(testPlot))
  expect_length(testPlot$layers, 8)
  expect_equal(testPlot$layers[[6]]$geom_params$lineend, "round")
  expect_equal(testPlot$layers[[7]]$geom_params$lineend, "round")
})

test_that("Parsing a tibble without a column gives errror",{
  testTibble <- tibble::tibble(times = Sys.time() - sample(1:100000, 5))
  expect_error(clockPlot(testTibble), "Must assign value to 'column' if time is of type tibble")
})

test_that("Receive a warning if column is unused", {
  expect_warning(clockPlot(Sys.time(), column = "time"), "'column' not in use when 'time' is of type POSIX. Ignoring value.")
})

test_that("Tibble maintains original columns as well as ClockPlot column", {
  testTibble <- tibble::tibble(index = c(1:5), times = Sys.time() - sample(1:100000, 5), letters = sample(c("a", "b", "c", "d", "e"), 5))
  expect_equal(colnames(clockPlot(testTibble, column = "times")), c("clockPlots", "index", "times", "letters"))
})
