options("testthat.junit.output_file" = "result/landscape.xml")
with_reporter("junit",{
context('landscape unit tests')
library("sp")
library("spatstat")
#-------test on gen_land function-------#

#all element check on landscape
#
#Parameters
#land (SpatialPolygons): landscape
#poly (int): numbre of polygon
#width (float): width of landscape
#heigth (float): heigth of landscape
check_land <- function(land, poly, width, heigth)
{
  print("check_land")
  # check return is SpatialPolygons or SpatialPolygons*.
  expect_equal('SpatialPolygons', substr(class(land)[1],1,15))
  # check numbers of polygons
  expect_equal(poly, length(land))
  # check numbers of id
  expect_equal(poly, length(getIdsSpatialPolygons(land)))
  
  bbox_land <- bbox(land)
  # check width
  expect_equal(width, bbox_land[1,2] - bbox_land[1,1])
  # check height
  expect_equal(heigth, bbox_land[2,2] - bbox_land[2,1])
}

# nominal case
test_that("test.gen_land_nom", {
  land <- gen_land(20, 80, 75)
  check_land(land, 20, 80, 75)
})

# minimal case
test_that("test.gen_land_min", {
  land <- gen_land(10, 60, 60)
  check_land(land, 10, 60, 60)
})

# out minimal case
test_that("test.gen_land_omin", {
  land <- gen_land(0, 0, 0)
  check_land(land, 10, 60, 60)
})

# negatif case
test_that("test.gen_land_neg", {
  land <- gen_land(-256, -8840, -755550)
  check_land(land, 10, 60, 60)
})

# high case
# test.gen_land_hi <- function()
# {
#   land <- gen_land(2566, 884455550, 7655550)
#   
#   # validate the created land
#   check_land(land, 2566, 884455550, 7655550)
#   
# }

# float value case
test_that("test.gen_land_float", {
  land <- gen_land(45.23, 545.36, 843.9)
  check_land(land, 45, 545.36, 843.9)
})

# mix case
# test.gen_land_mix0 <- function()
# {
#   land <- gen_land(-256, 54686536, 90)
#   
#   # validate the created land
#   check_land(land, 10, 54686536, 90)
#   
# }
# 
# test.gen_land_mix1 <- function()
# {
#   land <- gen_land(256, -4, 48614530)
#   
#   # validate the created land
#   check_land(land, 256, 60, 48614530)
#   
# }


#------test on affect_type----------#

#all element check on landscape with type
#
#Parameters
#land (SpatialPolygonsDataFrame): landscape
#poly (int): numbre of polygon
#width (float): width of landscape
#heigth (float): heigth of landscape
#nb_type (int): number of type
check_land_type <- function(land, poly, width, heigth, nb_type)
{
  print("check_land_type")
  #verify if affect type no modify the landscape
  check_land(land, poly, width, heigth)
  
  # check numbers of element in id_type
  expect_equal(poly, length(land$id_type))
  
  # check if each value of id_type is a type
  is_type = unique(land$id_type %in% 1:nb_type)
  expect_equal(1, length(is_type))
  expect_true(is_type)
  
}

#nominal case
test_that("test.affect_type_nom", {
  land <- gen_land(38, 457, 965)
  land <- affect_type(land, 10)
  check_land_type(land, 38, 457, 965, 10)
})

#more types than polygons 
test_that("test.affect_type_more_poly", {
  land <- gen_land(38, 457, 965)
  land <- affect_type(land, 150)
  check_land_type(land, 38, 457, 965, 150)
})

#number of type is negative
test_that("test.affect_type_neg", {
  land <- gen_land(38, 457, 965)
  land <- affect_type(land, -25)
  check_land_type(land, 38, 457, 965, 1)
})

#------test on get_neighbours----------#

#check list of neighbours
#
#Parameters
#land (SpatialPolygons): landscape
#lt_nei (list of list): list of neighbours of each polygon

check_nei <- function(land,lt_nei)
{
  print("check_nei")
  # Verify if function create a liste with all polygone
  expect_equal(length(land), length(lt_nei))
  
  # Verify ex: poly 1 have poly 4 as neighbour. 
  # poly 4 have to poly 1 as neighbour
  index = 1
  for (nei in lt_nei)
  {
    for (id in nei)
    {
      expect_true(index %in% lt_nei[[id]])
      print(index)
    }
    index = index + 1
  }
}

#nominal case
test_that("test.get_neighbours_nom", {
  land <- gen_land(30, 450, 680)
  lt_nei <- get_neighbours(land)
  check_nei(land, lt_nei)
})

#minimal case
test_that("test.get_neighbours_min", {
  land <- gen_land()
  lt_nei <- get_neighbours(land)
  check_nei(land, lt_nei)
})

#High case
test_that("test.get_neighbours_hi", {
  land <- gen_land(75, 3450, 2680)
  lt_nei <- get_neighbours(land)
  check_nei(land, lt_nei)
})

#------test on commun_coords----------#

#check commun coordonate
#
#Parameters
#land (SpatialPolygonsDataFrame): landscape
#lt_nei (list of list): list of neighbours of each polygon

check_coords <- function(land, id, lt_ccoords)
{
  print("check_coords")
  # take index of polygone with id
  my_id = match(TRUE, getIdsSpatialPolygons(land) == id)
  # take coordonates
  my_coords = getCoordsSpatialPolygons(land, my_id)
  
  # counting number of commun coordinates
  nb_commun = 0
  for (index_poly in 1:(length(my_coords[,1]) - 1))
  {
    for (index_list in 1:length(lt_ccoords[,1]))
    {
      same = unique(my_coords[index_poly,] == lt_ccoords[index_list,])
      if (isTRUE(same) && length(same) == 1)
      {
        nb_commun = nb_commun + 1
      }
    }
  }
  # It has to find the same number of cummon coordonate
  expect_equal(length(lt_ccoords[,1]), nb_commun)
}

# nominal case
test_that("test.commun_coords_nom", {
  land = gen_land()
  nei = get_neighbours(land)
  lt_ccoords = commun_coords(land, c(2, nei[[2]][1]))
  check_coords(land, 2, lt_ccoords)
  check_coords(land, nei[[2]][1], lt_ccoords)
})

# no commun coords case
test_that("test.commun_coords_no", {
  land = gen_land()
  nei = get_neighbours(land)
  no_nei = c(1:10)[-c(4, nei[[4]])][1]
  lt_ccoords = commun_coords(land, c(4, no_nei))
  expect_true(is.empty(lt_ccoords))
})

# more two lines commun
test_that("test.more_two", {
  coords = matrix(c(0, 0, 30, 0, 30, 15, 30, 30, 30, 45, 30, 
                    60, 0, 60, 0, 0), 8, 2, byrow = T)
  p = Polygon(coords)
  coords = matrix(c(30, 0, 30, 15, 30, 30, 30, 45, 30, 60, 
                    60, 60, 60, 0, 30, 0), 8, 2, byrow = T)
  p1 = Polygon(coords)
  ps = Polygons(list(p), ID = c(1))
  ps1 = Polygons(list(p1), ID = c(2))
  land = SpatialPolygons(list(ps, ps1))
  lt_ccoords = commun_coords(land, c(1, 2))
  expect_true(length(lt_ccoords[, 1]) == 5)
  check_coords(land, 1, lt_ccoords)
  check_coords(land, 2, lt_ccoords)
})

# just one point commun
test_that("test.one_point", {
  coords = matrix(c(0, 0, 30, 0, 30, 15, 30, 30, 0, 30, 0, 
                    0), 6, 2, byrow = T)
  p = Polygon(coords)
  coords = matrix(c(30, 30, 30, 45, 30, 60, 60, 60, 60, 30, 
                    30, 30), 6, 2, byrow = T)
  p1 = Polygon(coords)
  ps = Polygons(list(p), ID = c(1))
  ps1 = Polygons(list(p1), ID = c(2))
  land = SpatialPolygons(list(ps, ps1))
  lt_ccoords = commun_coords(land, c(1, 2))
  expect_true(length(lt_ccoords[, 1]) == 2)
  check_coords(land, 1, lt_ccoords)
  check_coords(land, 2, lt_ccoords)
})

},T)