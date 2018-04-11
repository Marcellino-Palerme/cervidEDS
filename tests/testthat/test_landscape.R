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

  # modify function to zero
  expect_equal(result$set_potential(''), 0)
  func_pot <- function(x,y) 0
  func_dx <- function(x,y) 0
  func_dy <- function(x,y) 0
  check_PotentialPolygon("3.8", func_pot, func_dx, func_dy, result)

})

# zero case
test_that("test.PotentialPolygon_nominal", {
  result = PotentialPolygon$new("O","")
  func_pot <- function(x,y) 0
  func_dx <- function(x,y) 0
  func_dy <- function(x,y) 0
  check_PotentialPolygon("O", func_pot, func_dx, func_dy, result)
  
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
  func_div = function(x, y) (x - y) / (y + x)

  # initialize PotentialPolygon
  result = PotentialPolygons$new(list(PP0, PP2, PP1))
  check_PotentialPolygons(list(PP1, PP0, PP2), result)

  # add a PotentialPolygon
  PP4 = PotentialPolygon$new("5", "x**y")
  dx_5 = function(x, y) y*x**(y - 1)
  dy_5 = function(x, y) x^y * log(x)
  result$set_potentialpolygons(list(PP4))
  check_PotentialPolygons(list(PP1, PP4, PP0, PP2), result)

  # modify a PotentialPolygon
  PP1_modif = PotentialPolygon$new("a","")
  result$set_potentialpolygons(list(PP1_modif))
  check_PotentialPolygons(list(PP4, PP1_modif, PP0, PP2), result)

  # add a PotentialPolygon
  PP5 = PotentialPolygon$new("14", "y / x")
  func_14 = function(x,y) y / x
  result$set_potential("14", "y / x")
  check_PotentialPolygons(list(PP1_modif, PP4, PP0, PP5, PP2), result)

  # modify a PotentialPolygon
  PP0_modif = PotentialPolygon$new("3e", "y / x")
  dx_3e = function(x, y) -y/(x**2)
  dy_3e = function(x, y) 1/x
  result$set_potential("3e", "y / x")
  check_PotentialPolygons(list(PP1_modif, PP4, PP0_modif, PP5, PP2), result)
  
  # Ask several potential of Polygon exist and not
  zero_func = function(x,y) 0
  potential = result$get_potentials(list("div","6", "14"))
  expect_equal(length(potential),3)
  expect_equal(potential, list(func_div, zero_func, func_14))

  x = LearnBayes::rigamma(1,1,1)
  y = LearnBayes::rigamma(1,1,1)

  # Ask several dx of Polygon exist and not
  dx = result$get_dxs(list("5","3e", "6"))
  expect_equal(length(dx),3)
  expect_equal(as.double(dx[[1]](x, y)), as.double(dx_5(x, y)))
  expect_equal(as.double(dx[[2]](x, y)), as.double(dx_3e(x, y)))
  expect_equal(as.double(dx[[3]](x, y)), as.double(zero_func(x, y)))

  # Ask several dy of Polygon exist and not
  dy = result$get_dys(list("6", "5","3e"))
  expect_equal(length(dy),3)
  expect_equal(as.double(dy[[2]](x, y)), as.double(dy_5(x, y)))
  expect_equal(as.double(dy[[3]](x, y)), as.double(dy_3e(x, y)))
  expect_equal(as.double(dy[[1]](x, y)), as.double(zero_func(x, y)))

  # initialize with empty list
  check_PotentialPolygons(list(), PotentialPolygons$new(list()))
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

  # initialize PotentialPolygon
  result = PotentialPolygons$new(list(PP0, PP2, PP1))
  check_PotentialPolygons(list(PP1, PP0, PP2), result)

  # Ask PotentialPolygon not exist
  expect_equal(result$get_potentialpolygon("1"), NULL)
  
  x = LearnBayes::rigamma(1,1,1)
  y = LearnBayes::rigamma(1,1,1)
  
  # Ask potential of Polygon not exist
  potential = result$get_potentials(list("6"))
  expect_equal(length(potential),1)
  expect_equal(potential[[1]](x, y), 0)

  # Ask potential out list
  expect_equal(result$get_potentials("a"), -1)

  # Ask dx of Polygon not exist
  dx = result$get_dxs(list("2"))
  expect_equal(length(dx),1)
  expect_equal(dx[[1]](x, y), 0)

  # Ask dx without list
  expect_equal(result$get_dxs("div"), -1)

  # Ask dy of Polygon not exist
  dy = result$get_dys(list("2"))
  expect_equal(length(dy),1)
  expect_equal(dy[[1]](x, y), 0)
  
  # Ask dy without list
  expect_equal(result$get_dys("div"), -1)
})

#------test PotentialLandscape-----#
# Function to validate PotentialLandscape
check_PotentialLandscape <- function(land_poly,
                                     land_lines,
                                     interaction_model,
                                     list_neighbours,
                                     list_potential,
                                     list_dx,
                                     list_dy,
                                     coord_poly,
                                     result)
{
  expect_equal(result$get_landscape(), land_poly)
  expect_equal(result$get_interaction_model(), interaction_model)

  for (id in 1:length(list_neighbours))
  {
    expect_equal(sort(result$get_neighbours_id(id)), list_neighbours[[id]])
  }
  for (coord in coord_poly)
  {
    for (x in runif(5, coord[1], coord[2]))
    {
      for (y in runif(5, coord[3], coord[4]))
      {
        expect_equal(result$get_potential_coord(x, y),
                     list_potential[[coord[5]]](x, y),
                     info = paste("x", x, "y", y))
        expect_equal(as.double(result$get_dx_coord(x, y)),
                     as.double(list_dx[[coord[5]]](x, y)),
                     info = paste("x", x, "y", y))
        expect_equal(as.double(result$get_dy_coord(x, y)),
                     as.double(list_dy[[coord[5]]](x, y)),
                     info = paste("x", x, "y", y))
      }
    }
  }
}

potentiel_1 <- function(a, b, c, x0, y0, x1, y1)
{
  return(paste("(",a,"*x*x*",b,") + ",c,"*y*y"))
}
P1 <- NULL
P2 <- NULL
P3 <- NULL
P4 <- NULL
neutre <- NULL
border <- NULL
int1_1 <- NULL
int1_2 <- NULL
int1_3 <- NULL
int1_4 <- NULL
int1_5 <- NULL
type_1 <- NULL
type_2 <- NULL
type_3 <- NULL
type_4 <- NULL
type_5 <- NULL


init_val_landscape <- function()
{
  # create the landscape
  coords = matrix(c(0, 0, 25, 0, 25, 20, 0, 20, 0, 0 ),
                  ncol = 2,byrow = T)
  P1 <<- sp::Polygon(coords)
  P1 <<- sp::Polygons(list(P1),ID = c(1))
  coords = matrix(c(0, 20, 25, 20, 25, 25, 25, 50, 0, 50, 0, 20 ),
                  ncol = 2,byrow = T)
  P2 <<- sp::Polygon(coords)
  P2 <<- sp::Polygons(list(P2),ID = c(2))
  coords = matrix(c(25, 0, 50, 0, 50, 25, 25, 25, 25, 20, 25, 0),
                  ncol = 2,byrow = T)
  P3 <<- sp::Polygon(coords)
  P3 <<- sp::Polygons(list(P3),ID = c(3))
  coords = matrix(c(25, 25, 50, 25, 50, 50, 25, 50, 25, 25),
                  ncol = 2,byrow = T)
  P4 <<- sp::Polygon(coords)
  P4 <<- sp::Polygons(list(P4),ID = c(4))
}

init_val_model <- function()
{
  #Interact border
  border <<- Interact$new(0,potentiel_1,list(2, 4, 0))
  
  neutre <<- list(0, 0, 0)
  attractive = list(0.1, 0.5, 0.24)
  repulsive = list(1, 2, 3)
  #Interact hote type 1 neighbour type 1
  int1_1 <<- Interact$new(1,potentiel_1, neutre)
  #Interact hote type 1 neighbour type 2
  int1_2 <<- Interact$new(2,potentiel_1, repulsive)
  #Interact hote type 1 neighbour type 3
  int1_3 <<- Interact$new(3,potentiel_1, repulsive)
  #Interact hote type 1 neighbour type 4
  int1_4 <<- Interact$new(4,potentiel_1, neutre)
  #Interact hote type 1 neighbour type 5
  int1_5 <<- Interact$new(5,potentiel_1, repulsive)
  
  #Interact hote type 2 neighbour type 1
  int2_1 = Interact$new(1,potentiel_1, attractive)
  #Interact hote type 2 neighbour type 2
  int2_2 = Interact$new(2,potentiel_1, neutre)
  #Interact hote type 2 neighbour type 3
  int2_3 = Interact$new(3,potentiel_1, neutre)
  #Interact hote type 2 neighbour type 4
  int2_4 = Interact$new(4,potentiel_1, attractive)
  #Interact hote type 1 neighbour type 5
  int2_5 = Interact$new(5,potentiel_1, repulsive)
  
  #Interact hote type 3 neighbour type 1
  int3_1 = Interact$new(1,potentiel_1, attractive)
  #Interact hote type 3 neighbour type 2
  int3_2 = Interact$new(2,potentiel_1, neutre)
  #Interact hote type 3 neighbour type 3
  int3_3 = Interact$new(3,potentiel_1, neutre)
  #Interact hote type 3 neighbour type 4
  int3_4 = Interact$new(4,potentiel_1, attractive)
  #Interact hote type 1 neighbour type 5
  int3_5 = Interact$new(5,potentiel_1, attractive)
  
  #Interact hote type 4 neighbour type 1
  int4_1 = Interact$new(1,potentiel_1, neutre)
  #Interact hote type 4 neighbour type 2
  int4_2 = Interact$new(2,potentiel_1, repulsive)
  #Interact hote type 4 neighbour type 3
  int4_3 = Interact$new(3,potentiel_1, repulsive)
  #Interact hote type 4 neighbour type 4
  int4_4 = Interact$new(4,potentiel_1, neutre)
  #Interact hote type 4 neighbour type 5
  int4_5 = Interact$new(5,potentiel_1, repulsive)
  
  #Interact hote type 5 neighbour type 1
  int5_1 = Interact$new(1,potentiel_1, attractive)
  #Interact hote type 5 neighbour type 2
  int5_2 = Interact$new(2,potentiel_1, attractive)
  #Interact hote type 5 neighbour type 3
  int5_3 = Interact$new(3,potentiel_1, repulsive)
  #Interact hote type 5 neighbour type 4
  int5_4 = Interact$new(4,potentiel_1, attractive)
  #Interact hote type 5 neighbour type 5
  int5_5 = Interact$new(5,potentiel_1, neutre)
  
  #def type 1
  type_1 <<- TypeInteract$new(1, "type_1", agglo_0,list(border,
                                                        int1_1,
                                                        int1_2,
                                                        int1_3,
                                                        int1_4,
                                                        int1_5))
  
  #def type 2
  type_2 <<- TypeInteract$new(2, "type_2", agglo_0,list(border,
                                                        int2_1,
                                                        int2_2,
                                                        int2_3,
                                                        int2_4,
                                                        int2_5))
  
  #def type 3
  type_3 <<- TypeInteract$new(3, "type_3", agglo_0,list(border,
                                                        int3_1,
                                                        int3_2,
                                                        int3_3,
                                                        int3_4,
                                                        int3_5))
  
  #def type 4
  type_4 <<- TypeInteract$new(4, "type_4", agglo_0,list(border,
                                                        int4_1,
                                                        int4_2,
                                                        int4_3,
                                                        int4_4,
                                                        int4_5))
  
  #def type 5
  type_5 <<- TypeInteract$new(5, "type_5", agglo_0,list(border,
                                                        int5_1,
                                                        int5_2,
                                                        int5_3,
                                                        int5_4,
                                                        int5_5))
}

# nominal case
test_that("test.PotentialLandscape_nominal", {
  init_val_landscape()

  land_poly = sp::SpatialPolygons(list(P1, P2, P3, P4))
  # get lines of landscape
  land_line = extract_lines(land_poly)

  land_poly = affect_polygons_type(land_poly, 2)
  land_poly$id_type = c(1, 2, 3, 1)
  land_line = affect_lines_type(land_line,2)
  land_line$id_type = c(0, 4, -1, 0, 0, 0, 5, 5, 4, 0, 0, 0, 0)
  
 
  #def model
  init_val_model()
  model = TypeInteractModel$new(list(type_1, type_2, type_3, type_4,type_5))

  func_border = function(x, y) 2 * x * x * 4
  func_attractive = function(x, y) 0.1 * x * x * 0.5 + 0.24 * y * y
  func_repulsive = function(x, y) 1 * x * x * 2 + 3 * y * y
  dx_border = function(x, y) 2 * 2 * x * 4
  dx_attractive = function(x, y) 0.1 * 2 * x * 0.5 
  dx_repulsive = function(x, y) 1 * 2 * x * 2 
  dy_border = function(x, y) 0
  dy_attractive = function(x, y) 0.24 * 2 * y
  dy_repulsive = function(x, y) 3 * 2 * y
  result = PotentialLandscape$new(land_poly, land_line, model)
  neighbour = list(c(2, 3), c(1, 3, 4), c(1, 2, 4), c(2, 3))
  potential = list(function(x, y) 2 * func_repulsive(x,y) + 
                                  2 * func_border(x, y),
                   function(x, y) 3 * func_attractive(x,y) + 
                                  2 * func_repulsive(x,y) + 
                                  2 * func_border(x, y),
                   function(x, y) 4 * func_attractive(x,y) + 
                                  2 * func_border(x, y),
                   function(x, y) 3 * func_repulsive(x,y) + 
                                  2 * func_border(x, y))
  dx = list(function(x, y) 2 * dx_repulsive(x,y) + 2 * dx_border(x, y),
            function(x, y) 3 * dx_attractive(x,y) + 2 * dx_repulsive(x,y) +
                           2 * dx_border(x, y),
            function(x, y) 4 * dx_attractive(x,y) + 2 * dx_border(x, y),
            function(x, y) 3 * dx_repulsive(x,y) + 2 * dx_border(x, y))

  dy = list(function(x, y) 2 * dy_repulsive(x,y) + 2 * dy_border(x, y),
            function(x, y) 3 * dy_attractive(x,y) + 2 * dy_repulsive(x,y) +
                           2 * dy_border(x, y),
            function(x, y) 4 * dy_attractive(x,y) + 2 * dy_border(x, y),
            function(x, y) 3 * dy_repulsive(x,y) + 2 * dy_border(x, y))
  coord = list(c(1, 24, 1, 19, 1),
               c(1, 24, 21, 49, 2),
               c(26, 49, 1, 24, 3),
               c(26, 49, 26, 49, 4))
  
  check_PotentialLandscape(land_poly,
                           land_line,
                           model,
                           neighbour,
                           potential,
                           dx,
                           dy,
                           coord,
                           result)
  
  # ask neighbours of no exist polygon
  expect_equal(result$get_neighbours_id(8), list())
  
  # ask potential out of landscape
  expect_equal(result$get_potential_coord(43,54), NA)

  # ask dx out of landscape
  expect_equal(result$get_dx_coord(73,24), NA)

  # ask dy out of landscape
  expect_equal(result$get_dy_coord(73,84), NA)

  coords = matrix(c(0, 0, 25, 0, 25, 25, 0, 25, 0, 0 ),
                  ncol = 2,byrow = T)
  P1 <<- sp::Polygon(coords)
  P1 <<- sp::Polygons(list(P1),ID = c(1))
  coords = matrix(c(0, 25, 25, 25, 25, 50, 0, 50, 0, 25 ),
                  ncol = 2,byrow = T)
  P2 <<- sp::Polygon(coords)
  P2 <<- sp::Polygons(list(P2),ID = c(2))
  coords = matrix(c(25, 0, 50, 0, 50, 25, 25, 25, 25, 0),
                  ncol = 2,byrow = T)
  P3 <<- sp::Polygon(coords)
  P3 <<- sp::Polygons(list(P3),ID = c(3))

  land_poly = sp::SpatialPolygons(list(P1, P2, P3, P4))
  # get lines of landscape
  land_line = extract_lines(land_poly)
  
  land_poly = affect_polygons_type(land_poly, 2)
  land_poly$id_type = c(2, 2, 1, 3)
  land_line = affect_lines_type(land_line,2)
  land_line$id_type = c(0, -1, 5, 0, 0, 0, 4, 4, 0, 0, 0, 0)

  # modify landscape
  expect_silent(result$set_landscape(land_poly, land_line))
  neighbour = list(c(2, 3, 4), c(1, 3, 4), c(1, 2, 4), c(1, 2, 3))
  potential = list(function(x, y) func_repulsive(x,y) + 
                                  func_attractive(x,y) +
                                  2 * func_border(x, y),
                   function(x, y) func_attractive(x,y) + 2 * func_border(x, y),
                   function(x, y) 3 * func_repulsive(x,y) + 
                                  2 * func_border(x, y),
                   function(x, y) 3 * func_attractive(x,y) + 
                                  2 * func_border(x, y))
  dx = list(function(x, y) dx_repulsive(x,y) + dx_attractive(x,y) + 
                           2 * dx_border(x, y),
            function(x, y) dx_attractive(x,y) + 2 * dx_border(x, y),
            function(x, y) 3 * dx_repulsive(x,y) + 2 * dx_border(x, y),
            function(x, y) 3 * dx_attractive(x,y) + 2 * dx_border(x, y))
  
  dy = list(function(x, y) dy_repulsive(x,y) + dy_attractive(x,y) +
                           2 * dy_border(x, y),
            function(x, y) dy_attractive(x,y) + 2 * dy_border(x, y),
            function(x, y) 3 * dy_repulsive(x,y) + 2 * dy_border(x, y),
            function(x, y) 3 * dy_attractive(x,y) + 2 * dy_border(x, y))
  coord = list(c(1, 24, 1, 24, 1),
               c(1, 24, 26, 49, 2),
               c(26, 49, 1, 24, 3),
               c(26, 49, 26, 49, 4))

  check_PotentialLandscape(land_poly,
                           land_line,
                           model,
                           neighbour,
                           potential,
                           dx,
                           dy,
                           coord,
                           result)

  #Interact hote type 1 neighbour type 1
  int1_1 <<- Interact$new(1,potentiel_1, neutre)
  #Interact hote type 1 neighbour type 2
  int1_2 <<- Interact$new(2,potentiel_1, neutre)
  #Interact hote type 1 neighbour type 3
  int1_3 <<- Interact$new(3,potentiel_1, neutre)
  #Interact hote type 1 neighbour type 4
  int1_4 <<- Interact$new(4,potentiel_1, neutre)
  #Interact hote type 1 neighbour type 5
  int1_5 <<- Interact$new(5,potentiel_1, neutre)

  #def type 1
  type_1 <<- TypeInteract$new(1, "type_1", agglo_0,list(border,
                                                        int1_1,
                                                        int1_2,
                                                        int1_3,
                                                        int1_4,
                                                        int1_5))

  #def model
  model = TypeInteractModel$new(list(type_1, type_2, type_3, type_4, type_5))

  potential[[3]] = function(x, y) 2 * func_border(x, y)
  dx[[3]] = function(x, y) 2 * dx_border(x, y)
  dy[[3]] = function(x, y) 2 * dy_border(x, y)
  
  # Modify interaction model
  (result$set_interaction_model(model))

  check_PotentialLandscape(land_poly,
                           land_line,
                           model,
                           neighbour,
                           potential,
                           dx,
                           dy,
                           coord,
                           result)

  # Plot potentials
  expect_silent(result$plot_potential())
})
},T)
