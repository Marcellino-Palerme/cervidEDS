options("testthat.output_file" = "result/landscape.xml")
with_reporter("junit",{
context('landscape unit tests')

#------test PotentialPolygon-----#

check_PotentialPolygon <- function(id,
                                   func_pot,
                                   func_dx,
                                   func_dy,
                                   result)
{
  expect_equal(result$get_id(), id)
  x = LearnBayes::rigamma(1,1,1)
  y = LearnBayes::rigamma(1,1,1)
  expect_equal(result$get_potential()(x, y), func_pot(x, y))
  expect_equal(as.double(result$get_dx()(x, y)), as.double(func_dx(x, y)))
  expect_equal(as.double(result$get_dy()(x, y)), as.double(func_dy(x, y)))
} 

# nominal case
test_that("test.PotentialPolygon_nominal", {
  result = PotentialPolygon$new("3e","x * x + y * y")
  func_pot <- function(x,y) x * x + y * y
  func_dx <- function(x,y) 2*x
  func_dy <- function(x,y) 2*y
  check_PotentialPolygon("3e", func_pot, func_dx, func_dy, result)

  # modify id with id character
  expect_equal(result$set_id("7"), 0)
  check_PotentialPolygon("7", func_pot, func_dx, func_dy, result)

  # modify id with id numeric
  expect_equal(result$set_id(3.8), 0)
  check_PotentialPolygon("3.8", func_pot, func_dx, func_dy, result)
  
  # modify function
  expect_equal(result$set_potential('sqrt(x + y)'), 0)
  func_pot <- function(x,y) sqrt(x + y)
  func_dx <- function(x,y) 1/(2 * sqrt(x + y)) 
  func_dy <- function(x,y) 1/(2 * sqrt(x + y))
  check_PotentialPolygon("3.8", func_pot, func_dx, func_dy, result)

})

# error case
test_that("test.PotentialPolygon_error", {
  # initialize with id is a function
  expect_error(PotentialPolygon$new(mean,"x * x + y * y"))
  
  # initialize with function not litteral
  expect_error(PotentialPolygon$new("1", all))

  result = PotentialPolygon$new("3e","x * x + y * y")
  func_pot <- function(x,y) x * x + y * y
  func_dx <- function(x,y) 2*x
  func_dy <- function(x,y) 2*y
  check_PotentialPolygon("3e", func_pot, func_dx, func_dy, result)
  
  # modify id by a list
  expect_equal(result$set_id(list("7")), 1)
  check_PotentialPolygon("3e", func_pot, func_dx, func_dy, result)

  # modify function by function not litteral
  expect_equal(result$set_potential(sum), 4)
  check_PotentialPolygon("3e", func_pot, func_dx, func_dy, result)

})

#------test PotentialPolygons-----#
check_PotentialPolygons <- function(lt_PotentialPolygon,
                                    result)
{
  ids = c()
  for (PotentialPolygon in lt_PotentialPolygon)
  {
    id = PotentialPolygon$get_id()
    ids = c(ids,id)
    check_PotentialPolygon(id,
                           PotentialPolygon$get_potential(),
                           PotentialPolygon$get_dx(),
                           PotentialPolygon$get_dy(),
                           result$get_potentialpolygon(id))
    check_PotentialPolygon(id,
                           result$get_potentials(list(id))[[1]],
                           result$get_dxs(list(id))[[1]],
                           result$get_dys(list(id))[[1]],
                           PotentialPolygon)
  }
  expect_equal(length(ids), length(result$get_ids()))
  expect_true(all(ids %in% result$get_ids()))
} 

# nominal case
test_that("test.PotentialPolygons_nominal", {
  PP0 = PotentialPolygon$new("3e","x * x + y * y")
  PP1 = PotentialPolygon$new("a","(x / y) + (y / x) ")
  PP2 = PotentialPolygon$new("div","(x - y) / (y + x) ")

  # initialize PotentialPolygon
  result = PotentialPolygons$new(list(PP0, PP2, PP1))
  check_PotentialPolygons(list(PP1, PP0, PP2), result)

  # add a PotentialPolygon
  PP4 = PotentialPolygon$new("5", "x**y")
  result$set_potentialpolygons(list(PP4))
  check_PotentialPolygons(list(PP1, PP4, PP0, PP2), result)

  # modify a PotentialPolygon
  PP1_modif = PotentialPolygon$new("a","2*x * 2*y ")
  result$set_potentialpolygons(list(PP1_modif))
  check_PotentialPolygons(list(PP4, PP1_modif, PP0, PP2), result)

  # add a PotentialPolygon
  PP5 = PotentialPolygon$new("14", "y / x")
  result$set_potential("14", "y / x")
  check_PotentialPolygons(list(PP1_modif, PP4, PP0, PP5, PP2), result)

  # modify a PotentialPolygon
  PP0_modif = PotentialPolygon$new("3e", "y / x")
  result$set_potential("3e", "y / x")
  check_PotentialPolygons(list(PP1_modif, PP4, PP0_modif, PP5, PP2), result)
})

# error case
test_that("test.PotentialPolygons_error", {
  PP0 = PotentialPolygon$new("3e","x * x + y * y")
  PP1 = PotentialPolygon$new("a","(x / y) + (y / x) ")
  PP2 = PotentialPolygon$new("div","(x - y) / (y + x) ")
  
  # initialize withoutt list
  expect_error(PotentialPolygons$new(PP0))

  # initialize with list not sith only PotentialPolygon
  expect_error(PotentialPolygons$new(list(PP0, PP2, PP1, 7)))

  # initialize with empty list
  expect_error(PotentialPolygons$new(list()))

  # initialize PotentialPolygon
  result = PotentialPolygons$new(list(PP0, PP2, PP1))
  check_PotentialPolygons(list(PP1, PP0, PP2), result)

})
},T)
