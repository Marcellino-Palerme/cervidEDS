options("testthat.output_file" = "result/model_function.xml")
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
  expect_equal( dist, result,
                tolerance = 1e-15,
                info = paste("b)", result, "not equal", dist))

  expr_dist = dist_expr(1.5,8,4.5,3)
  x = 6.5
  y = 17
  result = Eval(yacas(expr_dist))
  dist = sqrt(25 + 81)
  expect_equal( dist, result,
                tolerance = 1e-15,
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

#------test potentiel_0-----#
# nominal case
test_that("test.potentiel_0_nominal", {
  expr_pot = potentiel_0(4, 8, 9, 9.5, 8, 12.5, 8)
  x = 9
  y = 7
  result = Eval(yacas(expr_pot))
  pot = 4 * exp(-8 * (0.5^2 + 1)) + 9
  expect_equal(pot, result)
}) 

#------test agglo_0-----#
# nominal case
test_that("test.agglo_0_nominal", {
  expr_pot_a = potentiel_0(4, 8, 9, 9.5, 8, 12.5, 8)
  expr_pot_b = potentiel_0(-1.8, 1, 1.2, 9, 3.5, 9, 0.5)
  exp_agglo = agglo_0(list(expr_pot_a, expr_pot_b))
  x = 9
  y = 7
  result = Eval(yacas(exp_agglo))
  pot = 4 * exp(-8 * (0.5^2 + 1)) + 9 +
        -1.8 * exp(-1 * (3.5 ^ 2) ) + 1.2
  expect_equal( pot, result)
}) 

#------test Interact-----#

check_Interact <- function(id,
                           func,
                           params,
                           result)
{
  expect_equal(result$get_id(), id)
  expect_equal(result$get_func_interact(), func)
  expect_equal(result$get_params(), params)
}
# nominal case
test_that("test.Interact_nominal", {
  result = Interact$new(4, mean, list(7, 4))
  check_Interact(4, mean, list(7, 4), result)

  # modify id
  expect_equal(result$set_id(7), 0)
  check_Interact(7, mean, list(7, 4), result)
  
  # modify function
  expect_equal(result$set_func_interact(sqrt), 0)
  check_Interact(7, sqrt, list(7, 4), result)
  
  # modify params
  expect_equal(result$set_params(list(25)), 0)
  check_Interact(7, sqrt, list(25), result)
})

# error case
test_that("test.Interact_error", {
  # id isn't a numeric
  expect_error(Interact$new('4', mean, list(7, 4)))
  
  # func_interact isn't a function
  expect_error(Interact$new(4, 5, list(7, 4)))
  
  # params isn't a list
  expect_error(Interact$new(4, mean, c(7, 4)))
  
  result = Interact$new(4, mean, list(7, 4))
  check_Interact(4, mean, list(7, 4), result)
  
  # id isn't a numeric
  expect_equal(result$set_id(mean), 1)
  check_Interact(4, mean, list(7, 4), result)
  
  # func_interact isn't a function
  expect_equal(result$set_func_interact("sqrt"), 1)
  check_Interact(4, mean, list(7, 4), result)
  
  # modify params
  expect_equal(result$set_params(25), 1)
  check_Interact(4, mean, list(7, 4), result)
})

#------test TypeInteract-----#

check_TypeInteract <- function(id,
                               name,
                               func,
                               interacts,
                               result)
{
  expect_equal(result$get_id(), id)
  expect_equal(result$get_name(), name)
  expect_equal(result$get_func_agglo(), func)
  ids = c()
  for (interact in interacts)
  {
    id_interact = interact$get_id()
    check_Interact(id_interact,
                   interact$get_func_interact(),
                   interact$get_params(),
                   result$get_interact(id_interact))
    
    expect_equal(result$get_func_interact(id_interact),
                 interact$get_func_interact())
    expect_equal(result$get_params(id_interact),
                 interact$get_params())
    ids = c(ids, id_interact)
  }
  expect_equal(length(ids), length(result$get_id_interacts()))
  expect_true(all(ids %in% result$get_id_interacts()))

}

# nominal case
test_that("test.TypeInteract_nominal", {
  Inter4 = Interact$new(4, mean, list(7, 4))
  Inter2 = Interact$new(2, sqrt, list(4))
  
  result = TypeInteract$new(8, "huit", agglo_0, list(Inter2, Inter4))
  check_TypeInteract(8, "huit", agglo_0, list(Inter2, Inter4), result)
  
  # modify id
  expect_equal(result$set_id(7), 0)
  check_TypeInteract(7, "huit", agglo_0, list(Inter2, Inter4), result)
  
  # modify function
  expect_equal(result$set_func_agglo(sqrt), 0)
  check_TypeInteract(7, "huit", sqrt, list(Inter2, Inter4), result)
  
  # modify name
  expect_equal(result$set_name("sept"), 0)
  check_TypeInteract(7, "sept", sqrt, list(Inter2, Inter4), result)
  
  # Delete interact
  expect_equal(result$remove_interact(4), 0)
  check_TypeInteract(7, "sept", sqrt, list(Inter2), result)
  
  Inter8 = Interact$new(8, mean, list(28, 2))
  # Add interact
  expect_equal(result$set_interact(Inter8), 0)
  check_TypeInteract(7, "sept", sqrt, list(Inter2, Inter8), result)
  
  Inter2_modif = Interact$new(2, all, list(4))
  # Modify interaction function
  expect_equal(result$set_func_interact(2, all), 0)
  check_TypeInteract(7, "sept", sqrt, list(Inter2_modif, Inter8), result)
  
  Inter8_modif = Interact$new(8, mean, list(28, 2, 5))
  # Modify params of interact
  expect_equal(result$set_params(8, list(28, 2, 5)), 0)
  check_TypeInteract(7, "sept", sqrt, list(Inter2_modif, Inter8_modif), result)
})

# error case
test_that("test.TypeInteract_error", {
  Inter4 = Interact$new(4, mean, list(7, 4))
  Inter2 = Interact$new(2, sqrt, list(4))
  
  # Id isn't a numeric
  expect_error(TypeInteract$new("8", "huit", agglo_0, list(Inter2, Inter4)))
  
  # name isn't a characters
  expect_error(TypeInteract$new(8, 8, agglo_0, list(Inter2, Inter4)))
  
  # func_agglo isn't a function
  expect_error(TypeInteract$new(8, "huit", "agglo_0", list(Inter2, Inter4)))
  
  result = TypeInteract$new(8, "huit", agglo_0, list(Inter2, Inter4))
  check_TypeInteract(8, "huit", agglo_0, list(Inter2, Inter4), result)
  
  # Id isn't a numeric
  expect_equal(result$set_id("7"), 1)
  check_TypeInteract(8, "huit", agglo_0, list(Inter2, Inter4), result)
  
  # func_agglo isn't a function
  expect_equal(result$set_func_agglo(4), 1)
  check_TypeInteract(8, "huit", agglo_0, list(Inter2, Inter4), result)
  
  # name isn't a characters
  expect_equal(result$set_name(agglo_0), 1)
  check_TypeInteract(8, "huit", agglo_0, list(Inter2, Inter4), result)
  
  # Delete interact
  expect_equal(result$remove_interact(14), 1)
  check_TypeInteract(8, "huit", agglo_0, list(Inter2, Inter4), result)
  
  # interact isn't interact
  expect_equal(result$set_interact(mean), 1)
  check_TypeInteract(8, "huit", agglo_0, list(Inter2, Inter4), result)
  
  Inter2_modif = Interact$new(2, all, list(4))
  # interaction function isn't function
  expect_equal(result$set_func_interact(2, "all"), 1)
  check_TypeInteract(8, "huit", agglo_0, list(Inter2, Inter4), result)
  
  # params isn't a list
  expect_equal(result$set_params(4, c(28, 2, 5)), 1)
  check_TypeInteract(8, "huit", agglo_0, list(Inter2, Inter4), result)
})

},T)