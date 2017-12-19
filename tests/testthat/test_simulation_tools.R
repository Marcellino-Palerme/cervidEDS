options("testthat.junit.output_file" = "result/simulation_tools.xml")

with_reporter("silent",{
  my_landscape = gen_land()
})

with_reporter("junit",{
  context('simulation tools unit tests')
  # TODO test la taille pour tous les cas de la liste de matrix
  #------extract_elements-----#

    # nominal case
  test_that("test.extract_elements_nominal", {
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 2)
    my_poly = affect_polygons_type(my_landscape, 3)
    info_types = matrix(c(1, 1, 1, 11, 111,
                          2, 0, 2, 22, 222,
                          3, 1, 3, 33, 333),
                        ncol = 5, byrow = TRUE)
    elements = extract_elements(my_poly, my_lines, info_types)
    # Verify alpha value correct for attractive
    expect_true((1 %in% elements$attractive[,5]) || 
                (3 %in% elements$attractive[,5]),
                info = "Not correct value of alpha for attractive")
    # Verify beta value correct for attractive
    expect_true((11 %in% elements$attractive[,6]) || 
                (33 %in% elements$attractive[,6]),
                info = "Not correct value of beta for attractive")
    # Verify power value correct for attractive
    expect_true((111 %in% elements$attractive[,7]) || 
                (333 %in% elements$attractive[,7]),
                info = "Not correct value of power for attractive")
    # Verify alpha value correct for repulsive
    expect_true((2 %in% elements$repulsive[,5]),
                info = "Not correct value of alpha for repulsive")
    # Verify beta value correct for repulsive
    expect_true((22 %in% elements$repulsive[,6]),
                info = "Not correct value of beta for repulsive")
    # Verify power value correct for repulsive
    expect_true((222 %in% elements$repulsive[,7]),
                info = "Not correct value of power for repulsive")
    
  })

  # no attractive case
  test_that("test.extract_elements_no_att", {
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 2)
    my_poly = affect_polygons_type(my_landscape, 3)
    info_types = matrix(c(1, 0, 1, 11, 111,
                          2, 0, 2, 22, 222,
                          3, 0, 3, 33, 333),
                        ncol = 5, byrow = TRUE)
    elements = extract_elements(my_poly, my_lines, info_types)
    # Verify alpha value correct for repulsive
    expect_true((1 %in% elements$repulsive[,5]) || 
                (2 %in% elements$repulsive[,5]) || 
                (3 %in% elements$repulsive[,5]),
                info = "Not correct value of alpha for repulsive")
    # Verify beta value correct for repulsive
    expect_true((11 %in% elements$repulsive[,6]) || 
                (22 %in% elements$repulsive[,6]) || 
                (33 %in% elements$repulsive[,6]),
                info = "Not correct value of beta for repulsive")
    # Verify power value correct for repulsive
    expect_true((111 %in% elements$repulsive[,7]) || 
                (222 %in% elements$repulsive[,7]) || 
                (333 %in% elements$repulsive[,7]),
                info = "Not correct value of power for repulsive")
    # Verify there aren't attractive element
    expect_equal(length(elements$attractive), 0)
  })

  # no repulsive case
  test_that("test.extract_elements_no_repul", {
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 2)
    my_poly = affect_polygons_type(my_landscape, 3)
    info_types = matrix(c(1, 1, 1, 11, 111,
                          2, 1, 2, 22, 222,
                          3, 1, 3, 33, 333),
                        ncol = 5, byrow = TRUE)
    elements = extract_elements(my_poly, my_lines, info_types)
    # Verify alpha value correct for attractive
    expect_true((1 %in% elements$attractive[,5]) || 
                (2 %in% elements$attractive[,5]) || 
                (3 %in% elements$attractive[,5]),
                info = "Not correct value of alpha for attractive")
    # Verify beta value correct for attractive
    expect_true((11 %in% elements$attractive[,6]) || 
                (22 %in% elements$attractive[,6]) || 
                (33 %in% elements$attractive[,6]),
                info = "Not correct value of beta for attractive")
    # Verify power value correct for attractive
    expect_true((111 %in% elements$attractive[,7]) || 
                (222 %in% elements$attractive[,7]) || 
                (333 %in% elements$attractive[,7]),
                info = "Not correct value of power for attractive")
    # Verify there aren't repulsive element
    expect_equal(length(elements$repulsive), 0)
  })

  # no typed lines case
  test_that("test.extract_elements_no_lines",{
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 2)
    my_lines$id_type = seq(length = length(my_lines$id_type), from = -1, by = 0)
    my_poly = affect_polygons_type(my_landscape, 3)
    info_types = matrix(c(1, 1, 1, 11, 111,
                          2, 0, 2, 22, 222,
                          3, 1, 3, 33, 333),
                        ncol = 5, byrow = TRUE)
    elements = extract_elements(my_poly, my_lines, info_types)
    # Verify alpha value correct for attractive
    expect_true((1 %in% elements$attractive[,5]) || 
                (3 %in% elements$attractive[,5]),
                info = "Not correct value of alpha for attractive")
    # Verify beta value correct for attractive
    expect_true((11 %in% elements$attractive[,6]) || 
                (33 %in% elements$attractive[,6]),
                info = "Not correct value of beta for attractive")
    # Verify power value correct for attractive
    expect_true((111 %in% elements$attractive[,7]) || 
                (333 %in% elements$attractive[,7]),
                info = "Not correct value of power for attractive")
    # Verify alpha value correct for repulsive
    expect_true((2 %in% elements$repulsive[,5]),
                info = "Not correct value of alpha for repulsive")
    # Verify beta value correct for repulsive
    expect_true((22 %in% elements$repulsive[,6]),
                info = "Not correct value of beta for repulsive")
    # Verify power value correct for repulsive
    expect_true((222 %in% elements$repulsive[,7]),
                info = "Not correct value of power for repulsive")
  })

  # only border lines case
  test_that("test.extract_elements_no_lines",{
    my_lines = extract_lines(my_landscape)
    my_lines = affect_lines_type(my_lines, 2)
    my_lines$id_type = seq(length = length(my_lines$id_type), from = 0, by = 0)
    my_poly = affect_polygons_type(my_landscape, 3)
    info_types = matrix(c(1, 1, 1, 11, 111,
                          2, 0, 2, 22, 222,
                          3, 1, 3, 33, 333),
                        ncol = 5, byrow = TRUE)
    elements = extract_elements(my_poly, my_lines, info_types)
    # Verify alpha value correct for attractive
    expect_true((1 %in% elements$attractive[,5]) || 
                  (3 %in% elements$attractive[,5]),
                info = "Not correct value of alpha for attractive")
    # Verify beta value correct for attractive
    expect_true((11 %in% elements$attractive[,6]) || 
                  (33 %in% elements$attractive[,6]),
                info = "Not correct value of beta for attractive")
    # Verify power value correct for attractive
    expect_true((111 %in% elements$attractive[,7]) || 
                  (333 %in% elements$attractive[,7]),
                info = "Not correct value of power for attractive")
    # Verify alpha value correct for repulsive
    expect_true((2 %in% elements$repulsive[,5]),
                info = "Not correct value of alpha for repulsive")
    # Verify beta value correct for repulsive
    expect_true((22 %in% elements$repulsive[,6]),
                info = "Not correct value of beta for repulsive")
    # Verify power value correct for repulsive
    expect_true((222 %in% elements$repulsive[,7]),
                info = "Not correct value of power for repulsive")
  })
},T)