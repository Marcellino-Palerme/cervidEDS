options("testthat.junit.output_file" = "result/simulation_tools.xml")

with_reporter("silent",{
  my_landscape = gen_land()
  info_types = matrix(c(1, 1, 1, 11, 111,
                        2, 0, 2, 22, 222,
                        3, 1, 3, 33, 333),
                      ncol = 5, byrow = TRUE)
  require("stringr")
})

check_param <- function(val_type,
                        nb_type,
                        kind_element)
{
  params = list("alpha" = c(1, 5),
                "beta"  = c(11, 6),
                "power" = c(111, 7))
  for (name_param in names(params))
  {
    val_param = as.character(val_type * params[[name_param]][1])
    col_element = as.character(kind_element[,params[[name_param]][2]])
    expect_true( val_param %in% col_element,
                 info = paste("Not correct value of",
                              name_param,
                              "for attractive"))
    expect_equal(nb_type,
                 sum(str_count(col_element, val_param)))
  }
}

check_element <- function(my_poly,
                          my_lines,
                          elements)
{
  # Take 
  list_type = as.character(c(my_poly@data$id_type, my_lines@data$id_type))
  nb_one = sum(str_count(list_type, "1") - str_count(list_type, "-1"))
  nb_two = sum(str_count(list_type, "2"))
  nb_three = sum(str_count(list_type, "3"))
  
  # Verify empty attractive element
  if (nb_one + nb_three == 0)
  {
    expect_equal(elements$attractive[1,1], NA)
    expect_equal(length(elements$attractive), 1)
  }
  else
  {
    if (nb_one > 0)
    {
      check_param(1, nb_one, elements$attractive)
    }
    if (nb_two > 0)
    {
      check_param(2, nb_two, elements$repulsive)
    }
    else
    {
      expect_equal(elements$repulsive[1,1], NA)
      expect_equal(length(elements$repulsive), 1)
    }
    if (nb_three > 0)
    {
      check_param(3, nb_three, elements$attractive)
    }
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

    elements = extract_elements(my_poly, my_lines, info_types)
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

    elements = extract_elements(my_poly, my_lines, info_types)

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

    elements = extract_elements(my_poly, my_lines, info_types)

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

    elements = extract_elements(my_poly, my_lines, info_types)

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

    elements = extract_elements(my_poly, my_lines, info_types)

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

    elements = extract_elements(my_poly, my_lines, info_types)

    expect_silent(plot_potential(my_landscape,
                                 elements))
  })
  # no repulsive case
  test_that("test.plot_potential_no_repul", {
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_lines$id_type = str_replace(my_lines$id_type, "[2]", "-1")
    my_poly = affect_polygons_type(my_landscape, 3)
    my_poly$id_type = str_replace(my_poly$id_type, "[2]", "-1")
    
    elements = extract_elements(my_poly, my_lines, info_types)
    
    expect_silent(plot_potential(my_landscape,
                                 elements))
  })
  
  # no typed lines case
  test_that("test.plot_potential_no_lines",{
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 3)
    my_lines$id_type = seq(length = length(my_lines$id_type), from = -1, by = 0)
    my_poly = affect_polygons_type(my_landscape, 3)
    
    elements = extract_elements(my_poly, my_lines, info_types)
    
    expect_silent(plot_potential(my_landscape,
                                 elements))
  })
},T)