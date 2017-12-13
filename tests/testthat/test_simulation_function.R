options("testthat.junit.output_file" = "result/simulation_function.xml")
with_reporter("junit",{
  context('simulation unit tests')

  #------test border_effect-----#

  # center of landscape case
  test_that("test.border_effect_center", {
    result = border_effect(30,72,0.5,15,36)
    print(result)
    print(result["x"])
    expect_true(result["x"] == 0, info = paste(result["x"], " <> 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], " <> 0"))
  })

  # center left of landscape case
  test_that("test.border_effect_centerleft", {
    result = border_effect(30,72,0.5,1,36)
    
    # border effect push to rigth 
    expect_true(result["x"] > 0, info = "wrong effect border")
    expect_true(result["y"] == 0, info = paste(result["y"], " <> 0"))
  })

  # compare two center left of landscape case
  test_that("test.border_effect_cl_compare", {
    result = border_effect(30,72,0.5,1,36)
    # save result of x
    old_res = result["x"]
    # border effect push to rigth 
    expect_true(result["x"] > 0, info = "wrong effect border")
    expect_true(result["y"] == 0, info = paste(result["y"], " <> 0"))
    
    result = border_effect(30,72,0.5,7,36)
    # border effect push to rigth 
    expect_true(result["x"] > 0, info = "wrong effect border")
    # border effect reduce if it get distance of border
    expect_true(result["x"] < old_res, info = "too effect border")
    expect_true(result["y"] == 0, info = paste(result["y"], " <> 0"))
  })

  # center rigth of landscape case
  test_that("test.border_effect_center_right", {
    result = border_effect(30,72,0.5,20,36)
    
    # border effect push to left 
    expect_true(result["x"] < 0, info = "wrong effect border")
    expect_true(result["y"] == 0, info = paste(result["y"], " <> 0"))
  })
  
  # compare two center rigth of landscape case
  test_that("test.border_effect_cr_compare", {
    result = border_effect(30,72,0.5,20,36)
    # save result of x
    old_res = result["x"]
    # border effect push to left 
    expect_true(result["x"] < 0, info = "wrong effect border")
    expect_true(result["y"] == 0, info = paste(result["y"], " <> 0"))
    
    result = border_effect(30,72,0.5,27,36)
    # border effect push to left 
    expect_true(result["x"] < 0, info = "wrong effect border")
    # border effect grow up if it reduce distance of border
    expect_true(result["x"] < old_res, info = "miss effect border")
    expect_true(result["y"] == 0, info = paste(result["y"], " <> 0"))
  })

  # center up of landscape case
  test_that("test.border_effect_centerup", {
    result = border_effect(30,72,0.5,15,60)
    
    # border effect push to down 
    expect_true(result["y"] < 0, info = "wrong effect border")
    expect_true(result["x"] == 0, info = paste(result["x"], " <> 0"))
  })

  # compare two center up of landscape case
  test_that("test.border_effect_cu_compare", {
    result = border_effect(30,72,0.5,15,60)
    
    # save result of y
    old_res = result["y"]
    
    # border effect push to down 
    expect_true(result["y"] < 0, info = "wrong effect border")
    expect_true(result["x"] == 0, info = paste(result["x"], " <> 0"))
    
    result = border_effect(30,72,0.5,15,50)
    # border effect push to down 
    expect_true(result["y"] < 0, info = "wrong effect border")
    # border effect reduce if it get distance of border
    expect_true(result["y"] > old_res, info = "too effect border")
    expect_true(result["x"] == 0, info = paste(result["y"], " <> 0"))
  })
  
  # center down of landscape case
  test_that("test.border_effect_centerdown", {
    result = border_effect(30,72,0.5,15,10)
    
    # border effect push to up 
    expect_true(result["y"] > 0, info = "wrong effect border")
    expect_true(result["x"] == 0, info = paste(result["x"], " <> 0"))
  })

  # compare two center down of landscape case
  test_that("test.border_effect_cd_compare", {
    result = border_effect(30,72,0.5,15,20)
    
    # save result of y
    old_res = result["y"]
    
    # border effect push to down 
    expect_true(result["y"] > 0, info = "wrong effect border")
    expect_true(result["x"] == 0, info = paste(result["x"], " <> 0"))
    
    result = border_effect(30,72,0.5,15,10)
    # border effect push to down 
    expect_true(result["y"] > 0, info = "wrong effect border")
    # border effect grown up if it reduce distance of border
    expect_true(result["y"] > old_res, info = "too effect border")
    expect_true(result["x"] == 0, info = paste(result["y"], " <> 0"))
  })

  # border rigth of landscape case
  test_that("test.border_effect_border_rigth", {
    result = border_effect(56,21,0.12,56,10)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], "diff 0"))
    expect_true(result["y"] > 0, info = paste(result["y"], "inf or equal 0"))
  })

  # border left of landscape case
  test_that("test.border_effect_border_left", {
    result = border_effect(56,21,0.12,0,10)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], "diff 0"))
    expect_true(result["y"] > 0, info = paste(result["y"], "inf or equal 0"))
  })

  # border up of landscape case
  test_that("test.border_effect_border_up", {
    result = border_effect(56,21,0.12,30.75,21)
    print(result)
    expect_true(result["x"] < 0, info = paste(result["x"], "sup or equal 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], "diff 0"))
  })
  
  # border down of landscape case
  test_that("test.border_effect_border_down", {
    result = border_effect(56,21,0.12,30.75,0)
    print(result)
    expect_true(result["x"] < 0, info = paste(result["x"], "sup or equal 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], "diff 0"))
  })

  # corner down of landscape case
  test_that("test.border_effect_corner_down", {
    # corner down rigth
    result = border_effect(451,96,0.024,0,0)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], "diff 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], "diff 0"))
    
    # corner down left
    result = border_effect(451,96,0.024,451,0)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], "diff 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], "diff 0"))
  })

  # corner up of landscape case
  test_that("test.border_effect_corner_up", {
    # corner up rigth
    result = border_effect(451,96,0.024,0,96)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], "diff 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], "diff 0"))
    
    # corner up left
    result = border_effect(451,96,0.024,451,96)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], "diff 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], "diff 0"))
  })
  
  # nominal case
  test_that("test.border_effect_nominal", {
    # up rigth
    result = border_effect(85,14,0.02,31.42,10.4)
    print(result)
    expect_true(result["x"] > 0, info = paste(result["x"], "inf or equal 0"))
    expect_true(result["y"] < 0, info = paste(result["y"], "sup or equal 0"))
    
    # down rigth
    result = border_effect(85,14,0.02,31.42,4.153)
    print(result)
    expect_true(result["x"] > 0, info = paste(result["x"], "inf or equal 0"))
    expect_true(result["y"] > 0, info = paste(result["y"], "inf or equal 0"))

    # up left
    result = border_effect(85,14,0.02,74.21,10.4)
    print(result)
    expect_true(result["x"] < 0, info = paste(result["x"], "sup or equal 0"))
    expect_true(result["y"] < 0, info = paste(result["y"], "sup or equal 0"))
    
    # down left
    result = border_effect(85,14,0.02,65.123,4.153)
    print(result)
    expect_true(result["x"] < 0, info = paste(result["x"], "sup or equal 0"))
    expect_true(result["y"] > 0, info = paste(result["y"], "inf or equal 0"))
    
  })
  
  #------test distance2segment-----#
  # point down segment case
  test_that("test.distance2segment_down", {
    result = distance2segment(0.5,6,1.5,8,4.5,3)
    expect_true( 1.92 > result["dist"],
                info = paste("a)", result["dist"], "sup or equal 1.92"))
    expect_true( result["dist"] > 1.85,
                 info = paste("a)", result["dist"], "inf or equal 1.85"))
    expect_true( 0 > result["dy"],
                 info = paste("a)", result["dy"], "sup or equal 0"))
    expect_true( 0 > result["dx"],
                 info = paste("a)", result["dx"], "sup or equal 0"))
    
    result = distance2segment(9,7,5,5,10.5,9.5)
    expect_true( 1.1 > result["dist"],
                 info = paste("b)", result["dist"], "sup or equal 1.1"))
    expect_true( result["dist"] > 0.9,
                 info = paste("b)", result["dist"], "inf or equal 0.9"))
    expect_true( 0 > result["dy"],
                 info = paste("b)", result["dy"], "sup or equal 0"))
    expect_true( 0 < result["dx"],
                 info = paste("b)", result["dx"], "inf or equal 0"))
  })

  # point up segment case
  test_that("test.distance2segment_up", {
    result = distance2segment(5,8,1.5,8,4.5,3)
    expect_true( 3.02 > result["dist"],
                 info = paste("a)", result["dist"], "sup or equal 3.02"))
    expect_true( result["dist"] > 2.98,
                 info = paste("a)", result["dist"], "inf or equal 2.98"))
    expect_true( 0 < result["dy"],
                 info = paste("a)", result["dy"], "inf or equal 0"))
    expect_true( 0 < result["dx"],
                 info = paste("a)", result["dx"], "inf or equal 0"))
    
    result = distance2segment(5,8,5,5,10.5,9.5)
    expect_true( 2.35 > result["dist"],
                 info = paste("b)", result["dist"], "sup or equal 2.35"))
    expect_true( result["dist"] > 2.27,
                 info = paste("b)", result["dist"], "inf or equal 2.27"))
    expect_true( 0 < result["dy"],
                 info = paste("b)", result["dy"], "inf or equal 0"))
    expect_true( 0 > result["dx"],
                 info = paste("b)", result["dx"], "sup or equal 0"))
  })

  # distance point and a extrem of segment case
  test_that("test.distance2segment_point_extrem", {
    result = distance2segment(3,0,1.5,8,4.5,3)
    dist = sqrt(1.5^2 + 9)
    expect_true( dist == result["dist"],
                 info = paste("a)", result["dist"], "not equal", dist))
    expect_true( 0 > result["dy"],
                 info = paste("a)", result["dy"], "sup or equal 0"))
    expect_true( 0 > result["dx"],
                 info = paste("a)", result["dx"], "sup or equal 0"))

    result = distance2segment(3,0,5,5,10.5,9.5)
    dist = sqrt(4 + 25)
    expect_true( dist == result["dist"],
                 info = paste("b)", result["dist"], "not equal", dist))
    expect_true( 0 > result["dy"],
                 info = paste("b)", result["dy"], "sup or equal 0"))
    expect_true( 0 > result["dx"],
                 info = paste("b)", result["dx"], "sup or equal 0"))

    result = distance2segment(6.5,17,1.5,8,4.5,3)
    dist = sqrt(25 + 81)
    expect_true( dist == result["dist"],
                 info = paste("c)", result["dist"], "not equal", dist))
    expect_true( 0 < result["dy"],
                 info = paste("c)", result["dy"], "inf or equal 0"))
    expect_true( 0 < result["dx"],
                 info = paste("c)", result["dx"], "inf or equal 0"))

    result = distance2segment(6.5,17,5,5,10.5,9.5)
    dist = sqrt(16 + 7.5^2)
    expect_true( dist == result["dist"],
                 info = paste("d)", result["dist"], "not equal", dist))
    expect_true( 0 < result["dy"],
                 info = paste("d)", result["dy"], "inf or equal 0"))
    expect_true( 0 > result["dx"],
                 info = paste("d)", result["dx"], "sup or equal 0"))
  })

  # segment is a point case
  test_that("test.distance2segment_point", {
    result = distance2segment(3,0,1.5,8,1.5,8)
    dist = sqrt(1.5^2 + 64)
    expect_true( dist == result["dist"],
                 info = paste("a)", result["dist"], "not equal", dist))
    expect_true( 0 > result["dy"],
                 info = paste("a)", result["dy"], "sup or equal 0"))
    expect_true( 0 < result["dx"],
                 info = paste("a)", result["dx"], "inf or equal 0"))
    
    # only one point
    result = distance2segment(1.5,8,1.5,8,1.5,8)
    expect_true( 0 == result["dist"],
                 info = paste("b)", result["dist"], "not equal 0"))
    expect_true( 0 == result["dx"],
                 info = paste("b)", result["dx"], "not equal 0"))
    expect_true( 0 == result["dy"],
                 info = paste("b)", result["dy"], "not equal 0"))
  })

  # segment vertical case
  test_that("test.distance2segment_vertical", {
    result = distance2segment(3,0,9,3.5,9,0.5)
    dist = sqrt(0.5^2 + 36)
    expect_true( dist == result["dist"],
                 info = paste("a)", result["dist"], "not equal", dist))
    expect_true( 0 > result["dy"],
                 info = paste("a)", result["dy"], "sup or equal 0"))
    expect_true( 0 > result["dx"],
                 info = paste("a)", result["dx"], "sup or equal 0"))
    # align to segment
    result = distance2segment(9,7,9,3.5,9,0.5)
    dist = 3.5
    expect_true( dist == result["dist"],
                 info = paste("b)", result["dist"], "not equal", dist))
    expect_true( 0 == result["dx"],
                 info = paste("b)", result["dx"], "not equal 0"))
    expect_true( 0 < result["dy"],
                 info = paste("b)", result["dy"], "inf or equal 0"))

    #orthogonal to segment
    result = distance2segment(11.5,2.5,9,3.5,9,0.5)
    dist = 2.5
    expect_true( dist == result["dist"],
                 info = paste("c)", result["dist"], "not equal", dist))
    expect_true( 0 < result["dx"],
                 info = paste("c)", result["dx"], "inf or equal 0"))
    expect_true( 0 == result["dy"],
                 info = paste("c)", result["dy"], "not equal 0"))
  })

  # segment horizontal case
  test_that("test.distance2segment_horizontal", {
    result = distance2segment(9,7,9.5,8,12.5,8)
    dist = sqrt(0.5^2 + 1)
    expect_true( dist == result["dist"],
                 info = paste("a)", result["dist"], "not equal", dist))
    expect_true( 0 > result["dy"],
                 info = paste("a)", result["dy"], "sup or equal 0"))
    expect_true( 0 > result["dx"],
                 info = paste("a)", result["dx"], "sup or equal 0"))
    # align to segment
    result = distance2segment(5,8,9.5,8,12.5,8)
    dist = 4.5
    expect_true( dist == result["dist"],
                 info = paste("b)", result["dist"], "not equal", dist))
    expect_true( 0 > result["dx"],
                 info = paste("b)", result["dx"], "sup or equal 0"))
    expect_true( 0 == result["dy"],
                 info = paste("b)", result["dy"], "not equal 0"))
    
    #orthogonal to segment
    result = distance2segment(11.5,2.5,9.5,8,12.5,8)
    dist = 5.5
    expect_true( dist == result["dist"],
                 info = paste("c)", result["dist"], "not equal", dist))
    expect_true( 0 == result["dx"],
                 info = paste("c)", result["dx"], "not equal 0"))
    expect_true( 0 > result["dy"],
                 info = paste("c)", result["dy"], "inf or equal 0"))
  })

  #------test potential_func-----#
  expr_pot = expression(alpha*exp(-beta*(dist^puis)))
  #nominal case
  test_that("test.potential_func_nominal",{
    alpha = 6
    beta = 7
    puis = 2
    dist = 1.23
    expect_equal(potential_func(alpha, beta, dist, puis),
                 eval(parse(text = expr_pot)))

    alpha = -3
    beta = 7
    puis = 2
    dist = 1.23
    expect_equal(potential_func(alpha, beta, dist, puis),
                 eval(parse(text = expr_pot)))

    alpha = 3
    beta = 7
    puis = -5
    dist = 7.96
    expect_equal(potential_func(alpha, beta, dist, puis),
                 eval(parse(text = expr_pot)))

    alpha = 3
    beta = -0.23
    puis = 1
    dist = 7.96
    expect_equal(potential_func(alpha, beta, dist, puis),
                 eval(parse(text = expr_pot)))
  })
  #------test grad_potential_func-----#
  expr_deriv = deriv(~alpha*exp(-beta*(sqrt((x1-x)^2+(y1-y)^2)^puis)),
                     c("x","y"))
  #nominal case
  test_that("test.grad_potential_func_nominal",{
    alpha = 6
    beta = 7
    x1 = 4
    y1 = 3
    puis = 2
    x = 9
    y = 1
    dist_deriv = distance2point(x,y,x1,y1)
    eval_deriv = eval(parse(text = expr_deriv))
    grad_in_x = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dx'])
    grad_in_y = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dy'])
    expect_equal(grad_in_x, attr(eval_deriv,"gradient")[1])
    expect_equal(grad_in_y, attr(eval_deriv,"gradient")[2])
  })
  #derivate in x equal 0 case
  test_that("test.grad_potential_func_dx0",{
    alpha = 6
    beta = 7
    x1 = 4
    y1 = 5
    puis = 2
    x = 4
    y = 3
    dist_deriv = distance2point(x,y,x1,y1)
    eval_deriv = eval(parse(text = expr_deriv))
    grad_in_x = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dx'])
    grad_in_y = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dy'])
    expect_equal(grad_in_x, 0)
    expect_equal(grad_in_y, attr(eval_deriv,"gradient")[2])
  })
  #derivate in y equal 0 case
  test_that("test.grad_potential_func_dy0",{
    alpha = 6
    beta = 7
    x1 = 7
    y1 = 3
    puis = 2
    x = 4
    y = 3
    dist_deriv = distance2point(x,y,x1,y1)
    eval_deriv = eval(parse(text = expr_deriv))
    grad_in_x = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dx'])
    grad_in_y = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dy'])
    expect_equal(grad_in_x, attr(eval_deriv,"gradient")[1])
    expect_equal(grad_in_y, 0)
  })
    #distance nul case
  test_that("test.grad_potential_func_nul",{
    alpha = 6
    beta = 7
    x1 = 4
    y1 = 3
    puis = 2
    x = 4
    y = 3
    dist_deriv = distance2point(x,y,x1,y1)
    grad_in_x = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dx'])
    grad_in_y = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dy'])
    expect_equal(grad_in_x, 0)
    expect_equal(grad_in_y, 0)
  })
  #beta negative case
  test_that("test.grad_potential_func_beta_neg",{
    alpha = 6
    beta = -7
    x1 = 4
    y1 = 3
    puis = 2
    x = 9
    y = 1
    dist_deriv = distance2point(x,y,x1,y1)
    eval_deriv = eval(parse(text = expr_deriv))
    grad_in_x = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dx'])
    grad_in_y = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dy'])
    expect_equal(grad_in_x, attr(eval_deriv,"gradient")[1])
    expect_equal(grad_in_y, attr(eval_deriv,"gradient")[2])
  })
  #puis negative case
  test_that("test.grad_potential_func_puis_neg",{
    alpha = 6
    beta = 7
    x1 = 4
    y1 = 3
    puis = -3
    x = 9
    y = 1
    dist_deriv = distance2point(x,y,x1,y1)
    eval_deriv = eval(parse(text = expr_deriv))
    grad_in_x = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dx'])
    grad_in_y = grad_potential_func(alpha, beta, dist_deriv['dist'], puis,
                                    dist_deriv['dy'])
    expect_equal(grad_in_x, attr(eval_deriv,"gradient")[1])
    expect_equal(grad_in_y, attr(eval_deriv,"gradient")[2])
  })

  #------test alpha_func-----#
  expr_alpha = expression(alpha_1 * exp(-0.5 * (log(t / alpha_3) / alpha_2)^2))
  #nominal case
  test_that("test.alpha_func_nominal",{
    alpha_1 = 5
    alpha_2 = 45
    alpha_3 = 71
    t = 90
    eval_alpha = eval(parse(text = expr_alpha))
    alpha = alpha_func(alpha_1, alpha_2, alpha_3, t)
    expect_equal(alpha, eval_alpha)
  })
  #alpha_2 = zero case
  test_that("test.alpha_func_alpha_2_zero",{
    alpha_1 = 5
    alpha_2 = 0
    alpha_3 = 71
    t = 90
    alpha = alpha_func(alpha_1, alpha_2, alpha_3, t)
    expect_equal(alpha, 0)
  })
  #alpha_3 = zero case
  test_that("test.alpha_func_alpha_3_zero",{
    alpha_1 = 5
    alpha_2 = 7
    alpha_3 = 0
    t = 90
    alpha = alpha_func(alpha_1, alpha_2, alpha_3, t)
    expect_equal(alpha, 0)
  })
  #t = zero case
  test_that("test.alpha_func_t_zero",{
    alpha_1 = 5
    alpha_2 = 7
    alpha_3 = 74
    t = 0
    alpha = alpha_func(alpha_1, alpha_2, alpha_3, t)
    expect_equal(alpha, 0)
  })
  #log negative case
  test_that("test.alpha_func_log_negative",{
    alpha_1 = 5
    alpha_2 = 7
    alpha_3 = -74
    t = 45
    alpha = alpha_func(alpha_1, alpha_2, alpha_3, t)
    expect_equal(alpha, 0)
    alpha_3 = 74
    t = -45
    alpha = alpha_func(alpha_1, alpha_2, alpha_3, t)
    expect_equal(alpha, 0)
  })
  
  #------test repulsive_effect-----#
  #nominal case
  test_that("test.repulsive_effect_nominal",{
    coord_element = matrix(c(0,0,30,0,0,30,0,0),ncol = 4,nrow = 2, byrow = TRUE)
    coord_point = c("x" = 15, "y" = 15)
    result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_true(as.double(result["x"]) > 0)
  })
  #effect abort case
  test_that("test.repulsive_effect_abort",{
    coord_element = matrix(c(0,0,30,0,0,30,0,0,30,0,30,30,30,30,0,30,0,30,0,0),
                           ncol = 4,nrow = 4, byrow = TRUE)
    coord_point = c("x" = 15, "y" = 15)
    result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_equal(as.double(result["x"]), 0)
  })
  #to up case
  test_that("test.repulsive_effect_up",{
    coord_element = matrix(c(0,0,30,0),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 4, "y" = 15)
    result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), 0)
    expect_true(as.double(result["y"]) > 0)
print(result)
    # effect grow up if reduce distance
    coord_point = c("x" = 4, "y" = 1)
    new_result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) > 0)
    expect_true(as.double(new_result["y"]) > as.double(result["y"]))
print(new_result)
    # effect reduce if grow up distance
    coord_point = c("x" = 4, "y" = 25)
    new_result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) > 0)
    expect_true(as.double(new_result["y"]) < as.double(result["y"]))
  })
  #to down case
  test_that("test.repulsive_effect_down",{
    coord_element = matrix(c(0,30,30,30),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 4, "y" = 15)
    result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), 0)
    expect_true(as.double(result["y"]) < 0)
    
    # effect grow up if reduce distance
    coord_point = c("x" = 4, "y" = 25)
    new_result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) < 0)
    expect_true(as.double(new_result["y"]) < as.double(result["y"]))
    
    # effect reduce if grow up distance
    coord_point = c("x" = 4, "y" = 7)
    new_result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) < 0)
    expect_true(as.double(new_result["y"]) > as.double(result["y"]))
  })

  #to right case
  test_that("test.repulsive_effect_right",{
    coord_element = matrix(c(0,30,0,0),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 9, "y" = 15)
    result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["y"]), 0)
    expect_true(as.double(result["x"]) > 0)
    
    # effect grow up if reduce distance
    coord_point = c("x" = 4, "y" = 15)
    new_result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["y"]), 0)
    expect_true(as.double(new_result["x"]) > 0)
    expect_true(as.double(new_result["x"]) > as.double(result["x"]))
    
    # effect reduce if grow up distance
    coord_point = c("x" = 14, "y" = 7)
    new_result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["y"]), 0)
    expect_true(as.double(new_result["x"]) > 0)
    expect_true(as.double(new_result["x"]) < as.double(result["x"]))
  })

  #to mix case
  test_that("test.repulsive_effect_mix",{
    coord_element = matrix(c(5,5,10.5,9.5),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 3, "y" = 2)
    result = repulsive_effect(coord_element, coord_point, 4)
    expect_true(as.double(result["y"]) < 0)
    expect_true(as.double(result["x"]) < 0)
    
    coord_point = c("x" = 7, "y" = 10.5)
    new_result = repulsive_effect(coord_element, coord_point, 0.2)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["y"]) > 0)
    
    coord_point = c("x" = 9, "y" = 7)
    new_result = repulsive_effect(coord_element, coord_point, 48)
    expect_true(as.double(new_result["x"]) > 0)
    expect_true(as.double(new_result["y"]) < 0)

    coord_point = c("x" = 12.5, "y" = 10)
    new_result = repulsive_effect(coord_element, coord_point, 78)
    expect_true(as.double(new_result["x"]) > 0)
    expect_true(as.double(new_result["y"]) > 0)
  })

  #no element case
  test_that("test.repulsive_effect_no_element",{
    coord_element = matrix()
    coord_point = c("x" = 15, "y" = 15)
    result = repulsive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_equal(as.double(result["x"]), 0)
  })
  #------test attractive_effect-----#
  #nominal case
  test_that("test.attractive_effect_nominal",{
    coord_element = matrix(c(0,0,30,0,0,30,0,0),ncol = 4,nrow = 2, byrow = TRUE)
    coord_point = c("x" = 15, "y" = 15)
    result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_true(as.double(result["x"]) < 0)
  })
  #effect abort case
  test_that("test.attractive_effect_abort",{
    coord_element = matrix(c(0,0,30,0,0,30,0,0,30,0,30,30,30,30,0,30,0,30,0,0),
                           ncol = 4,nrow = 4, byrow = TRUE)
    coord_point = c("x" = 15, "y" = 15)
    result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_equal(as.double(result["x"]), 0)
  })
  #to down case
  test_that("test.attractive_effect_down",{
    coord_element = matrix(c(0,0,30,0),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 4, "y" = 15)
    result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), 0)
    expect_true(as.double(result["y"]) < 0)
    print(result)
    # effect grow up if reduce distance
    coord_point = c("x" = 4, "y" = 1)
    new_result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) < 0)
    expect_true(as.double(new_result["y"]) < as.double(result["y"]))
    print(new_result)
    # effect reduce if grow up distance
    coord_point = c("x" = 4, "y" = 25)
    new_result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) < 0)
    expect_true(as.double(new_result["y"]) > as.double(result["y"]))
  })
  #to up case
  test_that("test.attractive_effect_up",{
    coord_element = matrix(c(0,30,30,30),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 4, "y" = 15)
    result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), 0)
    expect_true(as.double(result["y"]) > 0)
    
    # effect grow up if reduce distance
    coord_point = c("x" = 4, "y" = 25)
    new_result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) > 0)
    expect_true(as.double(new_result["y"]) > as.double(result["y"]))
    
    # effect reduce if grow up distance
    coord_point = c("x" = 4, "y" = 7)
    new_result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) > 0)
    expect_true(as.double(new_result["y"]) < as.double(result["y"]))
  })
  
  #to left case
  test_that("test.attractive_effect_left",{
    coord_element = matrix(c(0,30,0,0),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 9, "y" = 15)
    result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["y"]), 0)
    expect_true(as.double(result["x"]) < 0)
    
    # effect grow up if reduce distance
    coord_point = c("x" = 4, "y" = 15)
    new_result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["y"]), 0)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["x"]) < as.double(result["x"]))
    
    # effect reduce if grow up distance
    coord_point = c("x" = 14, "y" = 7)
    new_result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(new_result["y"]), 0)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["x"]) > as.double(result["x"]))
  })
  
  #to mix case
  test_that("test.attractive_effect_mix",{
    coord_element = matrix(c(5,5,10.5,9.5),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 3, "y" = 2)
    result = attractive_effect(coord_element, coord_point, 4)
    expect_true(as.double(result["y"]) > 0)
    expect_true(as.double(result["x"]) > 0)
    
    coord_point = c("x" = 7, "y" = 10.5)
    new_result = attractive_effect(coord_element, coord_point, 0.2)
    expect_true(as.double(new_result["x"]) > 0)
    expect_true(as.double(new_result["y"]) < 0)
    
    coord_point = c("x" = 9, "y" = 7)
    new_result = attractive_effect(coord_element, coord_point, 48)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["y"]) > 0)
    
    coord_point = c("x" = 12.5, "y" = 10)
    new_result = attractive_effect(coord_element, coord_point, 78)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["y"]) < 0)
  })

  #no element case
  test_that("test.attractive_effect_no_element",{
    coord_element = matrix()
    coord_point = c("x" = 44, "y" = 10.21)
    result = attractive_effect(coord_element, coord_point, 4)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_equal(as.double(result["x"]), 0)
  })
  
  #------test all_effect-----#
  #nominal case
  test_that("test.all_effect_nominal", {
    coord_attrac = matrix(c(5,5,10.5,9.5),ncol = 4,nrow = 1, byrow = TRUE)
    coord_repul = matrix(c(1.5,8,4.5,3),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 5, "y" = 8)
    result = all_effect(coord_point,
                        15,
                        15,
                        0.2,
                        coord_attrac,
                        6,
                        coord_repul,
                        6,
                        0.1)
    expect_true(result["x"] > 0, info = paste(result["x"], "inf or equal 0"))
    expect_true(result["y"] < 0, info = paste(result["y"], "sup or equal 0"))
  })

  #------test diffusion-----#
  #nominal case
  test_that("test.diffusion_nominal", {
    #verify for same value generated different value
    value_1 = diffusion(5,0.1)
    value_2 = diffusion(5,0.1)
    expect_true(value_1 != value_2)
    value_1 = diffusion(5,0.1)
    expect_true(value_1 != value_2)
  })

  #------test next_coord-----#
  #nominal case
  test_that("test.next_coord_nominal", {
    coord_attrac = matrix(c(5,5,10.5,9.5),ncol = 4,nrow = 1, byrow = TRUE)
    coord_repul = matrix(c(1.5,8,4.5,3),ncol = 4,nrow = 1, byrow = TRUE)
    coord_point = c("x" = 5, "y" = 8)
    result = next_coord(coord_point,
                        15,
                        15,
                        0.2,
                        coord_attrac,
                        6,
                        coord_repul,
                        6,
                        0.1)
    expect_true(result["x"] > 5, info = paste(result["x"], "inf or equal 5"))
    expect_true(result["y"] < 8, info = paste(result["y"], "sup or equal 8"))
  })
},T)