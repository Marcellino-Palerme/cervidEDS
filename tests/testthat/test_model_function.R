options("testthat.junit.output_file" = "result/model_function.xml")
with_reporter("junit",{
context('model unit tests')
#------test dist_expr-----#

# nominal case
test_that("test.dist_expr_nom", {
  expr_dist = dist_expr(1, 1, 2, 2)
  x = 0
  y = 1
  result = eval(parse(text = expr_dist))
  expect_equal(1, result)
})

# segment is one point case
test_that("test.dist_expr_one_point", {
  expr_dist = dist_expr(1, 1, 1, 1)
  x = 0
  y = 1
  result = eval(parse(text = expr_dist))
  expect_equal(1, result)
})
},T)