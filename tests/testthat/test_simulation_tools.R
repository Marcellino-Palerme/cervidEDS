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
                 sum(str_count(elements[,5],id_type)))
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

    elements = extract_elements(my_poly, my_lines)
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

    elements = extract_elements(my_poly, my_lines)

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

    elements = extract_elements(my_poly, my_lines)

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

    elements = extract_elements(my_poly, my_lines)

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

    elements = extract_elements(my_poly, my_lines)

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

    elements = extract_elements(my_poly, my_lines)

    expect_silent(mat_pot <- plot_potential(my_landscape,
                                            elements,
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
    
    elements = extract_elements(my_poly, my_lines)
    
    expect_silent(mat_pot <- plot_potential(my_landscape,
                                            elements,
                                            info_types))
    expect_equal(length(mat_pot), 61*61)
  })
  
  # no typed lines case
  test_that("test.plot_potential_no_lines",{
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_lines$id_type = seq(length = length(my_lines$id_type), from = -1, by = 0)
    my_poly = affect_polygons_type(my_landscape, 3)
    
    elements = extract_elements(my_poly, my_lines)
    
    expect_silent(mat_pot <- plot_potential(my_landscape,
                                            elements,
                                            info_types))
    expect_equal(length(mat_pot), 61*61)
  })
},T)