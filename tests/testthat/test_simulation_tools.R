options("testthat.junit.output_file" = "result/simulation_tools.xml")

with_reporter("silent",{
  my_landscape = gen_land()
  info_types = matrix(c(1, 1, 1, 11, 111,
                        2, -1, 2, 22, 222,
                        3, 1, 3, 33, 333),
                      ncol = 5, byrow = TRUE)
  require("stringr")
})

check_element <- function(my_poly,
                          my_lines,
                          elements)
{
  # Take 
  list_type = as.character(c(my_poly@data$id_type, my_lines@data$id_type))
  for (id_type in as.character(-1:3))
  {
    expect_equal(sum(str_count(list_type, id_type)) ,
                 sum(str_count(elements[,4],id_type)))
  }
}

with_reporter("junit",{
  context('simulation tools unit tests')

  #------extract_elements-----#

    # nominal case
  test_that("test.extract_elements_nominal", {
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_poly = affect_polygons_type(my_landscape, 3)

    elements = extract_elements(25, 10, my_poly, my_lines)
    check_element(my_poly,
                  my_lines,
                  elements)
    
  })

  # no attractive case
  test_that("test.extract_elements_no_att", {
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_lines$id_type = str_replace(my_lines$id_type, "[13]", "-1")
    my_lines$id_type = str_replace(my_lines$id_type, "--1", "-1")
    my_poly = affect_polygons_type(my_landscape, 3)
    my_poly$id_type = str_replace(my_poly$id_type, "[13]", "-1")
    my_poly$id_type = str_replace(my_poly$id_type, "--1", "-1")

    elements = extract_elements(12, 47, my_poly, my_lines)

    check_element(my_poly,
                  my_lines,
                  elements)
  })

  # no repulsive case
  test_that("test.extract_elements_no_repul", {
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_lines$id_type = str_replace(my_lines$id_type, "[2]", "-1")
    my_poly = affect_polygons_type(my_landscape, 3)
    my_poly$id_type = str_replace(my_poly$id_type, "[2]", "-1")

    elements = extract_elements(45, 1, my_poly, my_lines)

    check_element(my_poly,
                  my_lines,
                  elements)
  })

  # no typed lines case
  test_that("test.extract_elements_no_lines",{
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_lines$id_type = seq(length = length(my_lines$id_type), from = -1, by = 0)
    my_poly = affect_polygons_type(my_landscape, 3)

    elements = extract_elements(0, 0, my_poly, my_lines)

    check_element(my_poly,
                  my_lines,
                  elements)
  })

  # only border lines case
  test_that("test.extract_elements_no_lines",{
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_lines$id_type = seq(length = length(my_lines$id_type), from = 0, by = 0)
    my_poly = affect_polygons_type(my_landscape, 3)

    elements = extract_elements(1, 1, my_poly, my_lines)

    check_element(my_poly,
                  my_lines,
                  elements)
  })

  # out of landscape case
  test_that("test.extract_elements_out_landscape", {
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_poly = affect_polygons_type(my_landscape, 3)
    
    elements = extract_elements(80, 40, my_poly, my_lines)
    check_element(my_poly,
                  my_lines,
                  elements)
    
  })
  
  #------plot_potential-----#
  # nominal case
  test_that("test.plot_potential_nomina",{
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_poly = affect_polygons_type(my_landscape, 3)

    expect_silent(mat_pot <- plot_potential(my_poly,
                                            my_lines,
                                            info_types))
    expect_equal(length(mat_pot), 61*61)
  })
  # no repulsive case
  test_that("test.plot_potential_no_repul", {
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_lines$id_type = as.integer(str_replace(my_lines$id_type, "[2]", "-1"))
    my_poly = affect_polygons_type(my_landscape, 3)
    my_poly$id_type = as.integer(str_replace(my_poly$id_type, "[2]", "-1"))
    
    expect_silent(mat_pot <- plot_potential(my_poly,
                                            my_lines,
                                            info_types))

    expect_equal(length(mat_pot), 61*61)
  })
  
  # no typed lines case
  test_that("test.plot_potential_no_lines",{
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_lines$id_type = seq(length = length(my_lines$id_type), from = -1, by = 0)
    my_poly = affect_polygons_type(my_landscape, 3)
    
    expect_silent(mat_pot <- plot_potential(my_poly,
                                            my_lines,
                                            info_types))
    expect_equal(length(mat_pot), 61*61)
  })

  
  #------test next_coord-----#
  #nominal case
  test_that("test.next_coord_nominal", {
    coords = matrix(c(0,0,15,0,15,15,0,15,0,0),5,2,byrow = T)
    p = sp::Polygon(coords)
    ps = sp::Polygons(list(p),ID = c(1))
    land_test = sp::SpatialPolygons(list(ps))
    land_test = sp::SpatialPolygonsDataFrame(land_test,
                                             data.frame(id_type = c(-1)))
    line_test = extract_lines(land_test)
    coord_attrac = matrix(c(5,5,10.5,9.5),
                          ncol = 2,nrow = 2, byrow = TRUE)
    line_attrac = sp::Line(coord_attrac)
    line_attrac = sp::Lines(list(line_attrac),"5")
    coord_repul = matrix(c(1.5,8,4.5,3),
                         ncol = 2,nrow = 2, byrow = TRUE)
    line_repul = sp::Line(coord_repul)
    line_repul = sp::Lines(list(line_repul),"6")
    
    line_test = c(line_test@lines,line_attrac,line_repul)
    line_test = sp::SpatialLines(line_test)
    line_test = sp::SpatialLinesDataFrame(line_test,
                                          data.frame(id_type = c(0, 0, 0, 0, 14, 1)))
    coord_point = c("x" = 5, "y" = 8)
    infos_type = matrix(c(5, 1, 4, 0.1, 2,
                          7, 1, 78, 0.1, 2,
                          14, 1, 6, 0.1, 2,
                          2, 1, 0.2, 0.1, 2,
                          1, -1, 6, 0.1, 2,
                          8, 1, 48, 0.1, 2),
                        nrow = 6, byrow = TRUE)
    result = next_coord(5,
                        8,
                        land_test,
                        line_test,
                        infos_type,
                        0.2,
                        0.1)
    expect_true(result["x"] > 5, info = paste(result["x"], "inf or equal 5"))
    expect_true(result["y"] < 8, info = paste(result["y"], "sup or equal 8"))
  })
  
  
},T)