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

    check_Interact(id_interact,
                   result$get_func_interact(id_interact),
                   result$get_params(id_interact),
                   interact)

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
  
  Inter8_same = Interact$new(8, sum, list(28, 2, 8))
  # Add interact with same id
  expect_equal(result$set_interact(Inter8_same), 0)
  check_TypeInteract(7, "sept", sqrt, list(Inter2, Inter8_same), result)
  
  Inter2_modif = Interact$new(2, all, list(4))
  # Modify interaction function
  expect_equal(result$set_func_interact(2, all), 0)
  check_TypeInteract(7, "sept", sqrt, list(Inter2_modif, Inter8_same), result)
  
  Inter8_modif = Interact$new(8, sum, list(28, 2, 5))
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

#------test TypeInteractModel-----#

check_TypeInteractModel <- function(Typeinteracts,
                                    result)
{
  # take list of TypeInteract
  Typeinteracts_model = result$get_model()
  # Verify lists have same length
  expect_equal(length(Typeinteracts_model), length(Typeinteracts))
  ids = c()
  # Verify same TypeInteracts in model
  for (Typeinteract in Typeinteracts)
  {
    # Take id of TypeInteract
    id_Typeinteract = Typeinteract$get_id()
    # Take ids of interact
    id_interacts = Typeinteract$get_id_interacts()
    # Verify if same ids interact in model
    expect_equal(length(id_interacts), 
                 length(result$get_id_interacts(id_Typeinteract)))
    expect_true(all(id_interacts %in% result$get_id_interacts(id_Typeinteract)))
    
    # Verify if value of TypeInteract return by model are same the TypeInteract
    check_TypeInteract(id_Typeinteract,
                       Typeinteract$get_name(),
                       Typeinteract$get_func_agglo(),
                       lapply(Typeinteract$get_id_interacts(),
                              Typeinteract$get_interact),
                       result$get_typeinteract(id_Typeinteract))
    
    check_TypeInteract(id_Typeinteract,
                       result$get_name(id_Typeinteract),
                       result$get_func_agglo(id_Typeinteract),
                       lapply(result$get_id_interacts(id_Typeinteract),
                              result$get_interact,
                              id_host = id_Typeinteract),
                       Typeinteract)

    
    for (id_neighbor in id_interacts)
    {
      check_Interact(id_neighbor,
                     result$get_func_interact(id_Typeinteract, id_neighbor),
                     result$get_params(id_Typeinteract, id_neighbor),
                     Typeinteract$get_interact(id_neighbor))

      check_Interact(id_neighbor,
                     Typeinteract$get_func_interact(id_neighbor),
                     Typeinteract$get_params(id_neighbor),
                     result$get_interact(id_Typeinteract, id_neighbor))
    }

    ids = c(ids, id_Typeinteract)
  }
  expect_equal(length(ids), length(result$get_id_typeinteract()))
  expect_true(all(ids %in% result$get_id_typeinteract()))
  
}

# nominal case
test_that("test.TypeInteractModel_nominal", {
  Inter4 = Interact$new(4, mean, list(7, 4))
  Inter2 = Interact$new(2, sqrt, list(4))
  TI8 = TypeInteract$new(8, "huit", agglo_0, list(Inter2, Inter4))
  Inter14 = Interact$new(14, potentiel_0, list(7, 4, 6))
  Inter12 = Interact$new(12, sum, list(1, 2, 3, 4))
  TI24 = TypeInteract$new(24, "24", agglo_0, list(Inter14, Inter12))
  TI32 = TypeInteract$new(32, "TD", sum, list(Inter4, Inter12, Inter2, Inter14))
  
  result = TypeInteractModel$new(list(TI32, TI8, TI24))
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # Add TypeInteract
  TIAdd = TypeInteract$new(85, "add", mean, list(Inter2, Inter14))
  expect_equal(result$set_type_interact(TIAdd), 0)
  check_TypeInteractModel(list(TI32, TI8, TI24, TIAdd), result)

  # Modify TypeInteract
  TI24_modif = TypeInteract$new(24, "23", sum, list(Inter4, Inter2))
  expect_equal(result$set_type_interact(TI24_modif), 0)
  check_TypeInteractModel(list(TI32, TI8, TI24_modif, TIAdd), result)

  # Modify agglomeration function
  TI8_agglo = TypeInteract$new(8, "huit", sample, list(Inter2, Inter4))
  expect_equal(result$set_func_agglo(8, sample), 0)
  check_TypeInteractModel(list(TI32, TI8_agglo, TI24_modif, TIAdd), result)
  
  # Add Interact
  Inter24 = Interact$new(24, potentiel_0, list(0.7, 1.4, 2.6))
  TI32add = TypeInteract$new(32, "TD", sum, list(Inter4, Inter12, Inter2,
                                                 Inter14, Inter24))
  expect_equal(result$set_interact(32, Inter24), 0)
  check_TypeInteractModel(list(TI32add, TI8_agglo, TI24_modif, TIAdd), result)

  # Modify Interact
  Inter12_modif = Interact$new(12, mean, list(1.7, 2.4, 4.3, 0.24))
  TI32modif = TypeInteract$new(32, "TD", sum, list(Inter4, Inter12_modif, Inter2,
                                                   Inter14, Inter24))
  expect_equal(result$set_interact(32, Inter12_modif), 0)
  check_TypeInteractModel(list(TI32modif, TI8_agglo, TI24_modif, TIAdd), result)

  # Modify interact function
  Inter14_func = Interact$new(14, is.character, list(7, 4, 6))
  TIAdd_func = TypeInteract$new(85, "add", mean, list(Inter2, Inter14_func))
  expect_equal(result$set_func_interact(85, 14, is.character), 0)
  check_TypeInteractModel(list(TI32modif, TI8_agglo, TI24_modif, TIAdd_func),
                          result)

  # Modify parameters of a interact
  Inter2_params = Interact$new(2, sqrt, list(4,7,"test"))
  TI8_params = TypeInteract$new(8, "huit", sample, list(Inter2_params, Inter4))
  expect_equal(result$set_params(8, 2, list(4,7,"test")), 0)
  check_TypeInteractModel(list(TI32modif, TI8_params, TI24_modif, TIAdd_func),
                          result)

  # remove TypeInteract
  expect_equal(result$remove_type_interact(32), 0)
  check_TypeInteractModel(list(TI8_agglo, TI24_modif, TIAdd_func), result)

  # remove Interact
  TI24_del = TypeInteract$new(24, "23", sum, list(Inter4))
  expect_equal(result$remove_interact(24, 2), 0)
  check_TypeInteractModel(list(TI8_agglo, TI24_del, TIAdd_func), result)

  #empty Model
  result = TypeInteractModel$new(list())
  check_TypeInteractModel(list(), result)

  #initialize with vector
  result = TypeInteractModel$new(c(TI8, TI32, TI24))
  check_TypeInteractModel(list(TI32, TI8, TI24), result)
})

# error case
test_that("test.TypeInteractModel_error", {
  Inter4 = Interact$new(4, mean, list(7, 4))
  Inter2 = Interact$new(2, sqrt, list(4))
  TI8 = TypeInteract$new(8, "huit", agglo_0, list(Inter2, Inter4))
  Inter14 = Interact$new(14, potentiel_0, list(7, 4, 6))
  Inter12 = Interact$new(12, sum, list(1, 2, 3, 4))
  TI24 = TypeInteract$new(24, "24", agglo_0, list(Inter14, Inter12))
  TI32 = TypeInteract$new(32, "TD", sum, list(Inter4, Inter12, Inter2, Inter14))
  
  expect_error(TypeInteractModel$new())

  result = TypeInteractModel$new(list(TI32, TI8, TI24))
  check_TypeInteractModel(list(TI32, TI8, TI24), result)
  
  # Add Interact instead of TypeInteract
  expect_equal(result$set_type_interact(Inter14), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)
  
  # Modify agglomeration function by integer
  expect_equal(result$set_func_agglo(8, 5), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)
  
  # Add Interact for TypeInteract don't exist
  Inter24 = Interact$new(24, potentiel_0, list(0.7, 1.4, 2.6))
  expect_equal(result$set_interact(2, Inter24), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)
  
  # Modify interact function of TypeInteract don't exist
  expect_equal(result$set_func_interact(2, 14, is.character), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # Modify interact function of Interact don't in TypeInteract
  expect_equal(result$set_func_interact(24, 4, is.character), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # Modify interact function of Interact don't exist
  expect_equal(result$set_func_interact(24, 44, is.character), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # Modify interact function by character
  expect_equal(result$set_func_interact(24, 14, "plouf"), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # Modify parameters of a interact of TypeInteract don't exist
  expect_equal(result$set_params(1, 2, list(4,7,"test")), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # Modify parameters of a interact of Interact don't exist
  expect_equal(result$set_params(24, 25, list(4,7,"test")), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # Modify parameters of a interact of Interact don't in TypeInteract
  expect_equal(result$set_params(24, 2, list(4,7,"test")), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # Modify parameters of a interact by function
  expect_equal(result$set_params(24, 12, eval), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # remove TypeInteract don't exist
  expect_equal(result$remove_type_interact(132), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # remove Interact of TypeInteract don't exit
  expect_equal(result$remove_interact(224, 12), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # remove Interact  don't in Type Interact
  expect_equal(result$remove_interact(24, 2), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)

  # remove Interact  don't exit
  expect_equal(result$remove_interact(24, 20), 1)
  check_TypeInteractModel(list(TI32, TI8, TI24), result)
  
})
},T)