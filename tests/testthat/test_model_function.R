options("testthat.junit.output_file" = "result/model_function.xml")
with_reporter("junit",{
context('model unit tests')
#------test dist_expr-----#

# nominal case
test_that("test.dist_expr_nom", {
  expr_dist = dist_expr(1, 1, 2, 2)
  x = 0
  y = 1
  result = Eval(yacas(expr_dist))
  expect_equal(1, result)
})

# segment is one point case
test_that("test.dist_expr_one_point", {
  expr_dist = dist_expr(1, 1, 1, 1)
  x = 0
  y = 1
  result = Eval(yacas(expr_dist))
  expect_equal(1, result)
})

#------test dist_expr-----#
# point down segment case
test_that("test.dist_expr_down", {
  expr_dist = dist_expr(1.5,8,4.5,3)
  x = 0.5
  y = 6
  result = Eval(yacas(expr_dist))
  expect_true( 1.92 > result,
               info = paste("a)", result, "sup or equal 1.92"))
  expect_true( result > 1.85,
               info = paste("a)", result, "inf or equal 1.85"))

  expr_dist = dist_expr(5,5,10.5,9.5)
  x = 9
  y = 7
  result = Eval(yacas(expr_dist))
  expect_true( 1.1 > result,
               info = paste("b)", result, "sup or equal 1.1"))
  expect_true( result > 0.9,
               info = paste("b)", result, "inf or equal 0.9"))
})

# point up segment case
test_that("test.dist_expr_up", {
  expr_dist = dist_expr(1.5,8,4.5,3)
  x = 5
  y = 8
  result = Eval(yacas(expr_dist))
  expect_true( 3.02 > result,
               info = paste("a)", result, "sup or equal 3.02"))
  expect_true( result > 2.98,
               info = paste("a)", result, "inf or equal 2.98"))

  expr_dist = dist_expr(5,5,10.5,9.5)
  x = 5
  y = 8
  result = Eval(yacas(expr_dist))
  expect_true( 2.35 > result,
               info = paste("b)", result, "sup or equal 2.35"))
  expect_true( result > 2.27,
               info = paste("b)", result, "inf or equal 2.27"))
})

# distance point and a extrem of segment case
test_that("test.dist_expr_point_extrem", {
  expr_dist = dist_expr(1.5,8,4.5,3)
  x = 3
  y = 0
  result = Eval(yacas(expr_dist))
  dist = sqrt(1.5^2 + 9)
  expect_true( dist == result,
               info = paste("a)", result, "not equal", dist))

  expr_dist = dist_expr(5,5,10.5,9.5)
  x = 3
  y = 0
  result = Eval(yacas(expr_dist))
  dist = sqrt(4 + 25)
  expect_true( dist == result,
               info = paste("b)", result, "not equal", dist))

  expr_dist = dist_expr(1.5,8,4.5,3)
  x = 6.5
  y = 17
  result = Eval(yacas(expr_dist))
  dist = sqrt(25 + 81)
  expect_true( dist == result,
               info = paste("c)", result, "not equal", dist))

  expr_dist = dist_expr(5,5,10.5,9.5)
  x = 6.5
  y = 17
  result = Eval(yacas(expr_dist))
  dist = sqrt(16 + 7.5^2)
  expect_true( dist == result,
               info = paste("d)", result, "not equal", dist))

})

# segment is a point case
test_that("test.dist_expr_point", {
  expr_dist = dist_expr(1.5,8,1.5,8)
  x = 3
  y = 0
  result = Eval(yacas(expr_dist))
  dist = sqrt(1.5^2 + 64)
  expect_true( dist == result,
               info = paste("a)", result, "not equal", dist))

  # only one point
  expr_dist = dist_expr(1.5,8,1.5,8)
  x = 1.5
  y = 8
  result = Eval(yacas(expr_dist))
  expect_true( 0 == result,
               info = paste("b)", result, "not equal 0"))
})

# segment vertical case
test_that("test.dist_expr_vertical", {
  expr_dist = dist_expr(9,3.5,9,0.5)
  x = 3
  y = 0
  result = Eval(yacas(expr_dist))
  dist = sqrt(0.5^2 + 36)
  expect_true( dist == result,
               info = paste("a)", result, "not equal", dist))

  # align to segment
  expr_dist = dist_expr(9,3.5,9,0.5)
  x = 9
  y = 7
  result = Eval(yacas(expr_dist))
  dist = 3.5
  expect_true( dist == result,
               info = paste("b)", result, "not equal", dist))

  #orthogonal to segment
  expr_dist = dist_expr(9,3.5,9,0.5)
  x = 11.5
  y = 2.5
  result = Eval(yacas(expr_dist))
  dist = 2.5
  expect_true( dist == result,
               info = paste("c)", result, "not equal", dist))
})

# segment horizontal case
test_that("test.dist_expr_horizontal", {
  expr_dist = dist_expr(9.5,8,12.5,8)
  x = 9
  y = 7
  result = Eval(yacas(expr_dist))
  dist = sqrt(0.5^2 + 1)
  expect_true( dist == result,
               info = paste("a)", result, "not equal", dist))

    # align to segment
  expr_dist = dist_expr(9.5,8,12.5,8)
  x = 5
  y = 8
  result = Eval(yacas(expr_dist))
  dist = 4.5
  expect_true( dist == result,
               info = paste("b)", result, "not equal", dist))

  #orthogonal to segment
  expr_dist = dist_expr(9.5,8,12.5,8)
  x = 11.5
  y = 2.5
  result = Eval(yacas(expr_dist))
  dist = 5.5
  expect_true( dist == result,
               info = paste("c)", result, "not equal", dist))
})
},T)