context("unit")

options("testthat.junit.output_file" = "result/model.xml")
test_file("unit/test_model.R", JunitReporter$new())

options("testthat.junit.output_file" = "result/landscape.xml")
test_file("unit/test_landscape.R", JunitReporter$new())