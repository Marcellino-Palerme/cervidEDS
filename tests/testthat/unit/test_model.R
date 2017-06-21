context('model unit tests')
#------test dist_expr-----#

# nominal case
test_that("test.dist_expr_nom", {
  expr = dist_expr(1, 1, 2, 2)
  x = 0
  y = 1
  result = eval(parse(text = expr))
  expect_equal((1/sqrt(2)), result)
})

# segment is one point case
test_that("test.dist_expr_one_point", {
  expr = dist_expr(1, 1, 1, 1)
  x = 0
  y = 1
  result = eval(parse(text = expr))
  expect_equal(1, result)
})