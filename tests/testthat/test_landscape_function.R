options("testthat.junit.output_file" = "result/landscape_function.xml")

with_reporter("silent",{
land_test_type <- gen_land(38, 457, 965)
land_default <- gen_land()
land_high <- gen_land(75, 3450, 2680)
land_nominal <- gen_land(20, 80, 75)
land_min <- gen_land(10, 60, 60)
land_omin <- gen_land(0, 0, 0)
land_neg <- gen_land(-256, -8840, -755550)
land_float <- gen_land(45.23, 545.36, 843.9)  
})

with_reporter("junit",{
context('landscape unit tests')

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
  expect_equal('SpatialPolygons', substr(class(land)[1],1,15),
               info = "wrong class")
  # check numbers of polygons
  expect_equal(poly, length(land), info = " wrong number of polygon")
  # check numbers of id
  expect_equal(poly, length(getIdsSpatialPolygons(land)),
               info = "wrong number of ID polygons")
  
  bbox_land <- bbox(land)
  # check width
  expect_equal(width, bbox_land[1,2] - bbox_land[1,1],
               info = "bad width")
  # check height
  expect_equal(heigth, bbox_land[2,2] - bbox_land[2,1],
               info = "bad heigth")
}

# nominal case
test_that("test.gen_land_nom", {
  land <- land_nominal
  check_land(land, 20, 80, 75)
})

# minimal case
test_that("test.gen_land_min", {
  land <- land_min
  check_land(land, 10, 60, 60)
})

# out minimal case
test_that("test.gen_land_omin", {
  land <- land_omin
  check_land(land, 10, 60, 60)
})

# negatif case
test_that("test.gen_land_neg", {
  land <- land_neg 
  check_land(land, 10, 60, 60)
})

# float value case
test_that("test.gen_land_float", {
  land <- land_float 
  check_land(land, 45, 545.36, 843.9)
})


#-------test on extract_line function-------#

# Verify if the lines are in SpatialPolygonsDataFrame
check_lines <- function(land, line)
{
  ids_poly = getIdsSpatialPolygons(land)
  ids_lines = getIdsSpatialLines(line)
  for (id_poly in ids_poly)
  {
    # take index of all line of polygon
    keep_ids = ids_lines[c(which(line$id_poly1 %in% id_poly),
                           which(line$id_poly2 %in% id_poly))]
    print(keep_ids)
    # Verify if line exist
    expect_false(unique(is.na(keep_ids)))
  
    # take coordinate of polygon
    coords = getCoordsSpatialPolygons(land, id_poly)
    for (id_line in keep_ids)
    {
      line_coords = getCoordsSpatialLines(line, id_line)
      index_x0 = line_coords[1,1] == coords[,1]
      index_x1 = line_coords[2,1] == coords[,1]
      index_y0 = line_coords[1,2] == coords[,2]
      index_y1 = line_coords[2,2] == coords[,2]
      print(coords)
      # Verify if points of line are in polygon
      index_p0 = match(1,index_x0 * index_y0)
      index_p1 = match(1,index_x1 * index_y1)

      # Verify if the line is in polygon
      expect_true(index_p0 != 0, 
                  info = paste("[",line$x0[id_line],line$y0[id_line],
                               "] points dont belong to polygon", id_poly))
      expect_true(index_p1 != 0, 
                  info = paste("[",line$x1[id_line],line$y1[id_line],
                               "] points dont belong to polygon", id_poly))
      
      #case line forming by the first and last point of polygone
      flp = abs(index_p0 - index_p1) == (length(coords[,1]) - 2)
      #case line forming by two points are following
      follow = abs(index_p0 - index_p1) == 1
      # The line can only forming in one of two cases
      expect_true(xor(flp, follow), info = paste("position of x0y0 point",
                                                 index_p0, 
                                                 "and position of x1y1 point",
                                                 index_p1, "not follow" ))
    }
  }
}

# nominal case
test_that("test.extract_line_nominal",{
  land <- land_nominal
  ext_lin <- extract_lines(land)
  check_lines(land, ext_lin)
})
#-----test on affect_polygons_type---------#

#all element check on landscape with type
#
#Parameters
#land (SpatialPolygonsDataFrame): landscape
#poly (int): numbre of polygon
#width (float): width of landscape
#heigth (float): heigth of landscape
#nb_type (int): number of type
check_poly_type <- function(land, poly, width, heigth, nb_type)
{
  print("check_poly_type")
  #verify if affect type no modify the landscape
  check_land(land, poly, width, heigth)
  
  # check numbers of element in id_type
  expect_equal(poly, length(land$id_type))
  
  # check if each value of id_type is a type
  is_type = unique(land$id_type %in% 1:nb_type)
  expect_equal(1, length(is_type), info = "all type not used 1/2")
  expect_true(is_type, info = "all type not used 2/2")
}

#nominal case
test_that("test.affect_polygons_type_nom", {
  land <- affect_polygons_type(land_test_type, 10)
  check_poly_type(land, 38, 457, 965, 10)
})

#more types than polygons 
test_that("test.affect_polygons_type_more_poly", {
  land <- affect_polygons_type(land_test_type, 150)
  check_poly_type(land, 38, 457, 965, 150)
})

#number of type is negative
test_that("test.affect_polygons_type_neg", {
  land <- affect_polygons_type(land_test_type, -25)
  check_poly_type(land, 38, 457, 965, 1)
})

#-----test on affect_lines_type---------#

lt_lines_test_type <- extract_lines(land_test_type)

check_lines_type <- function(typed_element, nb_type)
{
  print("check_type")
  # check if each value of id_type is a type
  is_type = unique(typed_element$id_type %in% -1:nb_type)
  expect_equal(1, length(is_type), info = "all type not used 1/2")
  expect_true(is_type, info = "all type not used 2/2")
}

#nominal case
test_that("test.affect_lines_type_nom", {
  lt_lines <- affect_lines_type(lt_lines_test_type, 10)
  check_lines_type(lt_lines, 10)
})

#more types than polygons 
test_that("test.affect_lines_type_more_line", {
  lt_lines <- affect_lines_type(lt_lines_test_type, 1444)
  check_lines_type(lt_lines, 1444)
})

#number of type is negative
test_that("test.affect_lines_type_neg", {
  lt_lines <- affect_lines_type(lt_lines_test_type, -25)
  check_lines_type(lt_lines, 1)
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
  expect_equal(length(land), length(lt_nei), info = "size oflist of neighbour is
               different of number of polygons")
  
  ids = attr(lt_nei, "region.id")
  # Verify ex: poly 1 have poly 4 as neighbour. 
  # poly 4 have to poly 1 as neighbour
  index = 1
  for (nei in lt_nei)
  {
    for (id in nei)
    {
      expect_true(index %in% lt_nei[[id]], info = paste("polygon", ids[index],
                                                        "have polygon", ids[id],
                                                        "but not inverse"))
    }
    index = index + 1
  }
}

#nominal case
test_that("test.get_neighbours_nom", {
  land <- land_nominal
  lt_nei <- get_neighbours(land)
  check_nei(land, lt_nei)
})

#minimal case
test_that("test.get_neighbours_min", {
  land = land_default
  lt_nei <- get_neighbours(land)
  check_nei(land, lt_nei)
})

#High case
test_that("test.get_neighbours_hi", {
  land <- land_high
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
  land = land_default
  nei = get_neighbours(land)
  lt_ccoords = commun_coords(land, c(2, nei[[2]][1]))
  check_coords(land, 2, lt_ccoords)
  check_coords(land, nei[[2]][1], lt_ccoords)
})

# no commun coords case
test_that("test.commun_coords_no", {
  land = land_default
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