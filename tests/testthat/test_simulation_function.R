options("testthat.junit.output_file" = "result/simulation_function.xml")

with_reporter("silent",{

  require("stringr")
})

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

  # point on segment horizontal 
  test_that("test.distance2segment_on_segment", {
    result = distance2segment(5,5,1,1,10,10)
    expect_equal(result["dist"],0)
    expect_equal(result["dy"], 0)
    expect_equal(result["dx"], 0)
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
  
  #------test potential_effect-----#
  # potential_effect repulsive nominal case
  test_that("test.potential_effect_repulsive_nominal",{
    dist_element = matrix(c(distance2segment(15, 15, 0, 0, 30, 0), 1,
                            distance2segment(15, 15, 0, 30, 0, 0), 1),
                           nrow = 2, byrow = TRUE)
    infos_type = matrix(c(1, -1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_true(as.double(result["x"]) > 0)
  })
  # potential_effect repulsive effect abort case
  test_that("test.potential_effect_repulsive_abort",{
    dist_element = matrix(c(distance2segment(15, 15, 0, 0, 30, 0), 5,
                            distance2segment(15, 15, 0, 30, 0, 0), 5,
                            distance2segment(15, 15, 30, 0, 30, 30), 5,
                            distance2segment(15, 15, 30, 30, 0, 30), 5), 
                           nrow = 4, byrow = TRUE)
    infos_type = matrix(c(5, -1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_equal(as.double(result["x"]), 0)
  })
  # potential_effect repulsive to up case
  test_that("test.potential_effect_repulsive_up",{
    dist_element = matrix(c(distance2segment(4, 15, 0, 0, 30, 0), 5),
                           nrow = 1, byrow = TRUE)
    infos_type = matrix(c(5, -1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), 0)
    expect_true(as.double(result["y"]) > 0)
print(result)
    # effect grow up if reduce distance
    dist_element = matrix(c(distance2segment(4, 1, 0, 0, 30, 0), 5),
                      nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) > 0)
    expect_true(as.double(new_result["y"]) > as.double(result["y"]))
print(new_result)
    # effect reduce if grow up distance
    dist_element = matrix(c(distance2segment(4, 25, 0, 0, 30, 0), 5),
                      nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) > 0)
    expect_true(as.double(new_result["y"]) < as.double(result["y"]))
  })
  #to down case
  test_that("test.potential_effect_repulsive_down",{
    dist_element = matrix(c(distance2segment(4, 15, 0, 30, 30, 30), 5),
                          nrow = 1, byrow = TRUE)
    infos_type = matrix(c(5, -1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), 0)
    expect_lt(as.double(result["y"]), 0)
    
    # effect grow up if reduce distance
    dist_element = matrix(c(distance2segment(4, 25, 0, 30, 30, 30), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["x"]), 0)
    expect_lt(as.double(new_result["y"]), 0)
    expect_lt(as.double(new_result["y"]), as.double(result["y"]))
    
    # effect reduce if grow up distance
    dist_element = matrix(c(distance2segment(4, 7, 0, 30, 30, 30), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["x"]), 0)
    expect_lt(as.double(new_result["y"]), 0)
    expect_gt(as.double(new_result["y"]), as.double(result["y"]))
  })

  #to right case
  test_that("test.potential_effect_repulsive_right",{
    dist_element = matrix(c(distance2segment(9, 15, 0, 30, 0, 0), 5),
                          nrow = 1, byrow = TRUE)
    infos_type = matrix(c(5, -1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["y"]), 0)
    expect_gt(as.double(result["x"]), 0)
    
    # effect grow up if reduce distance
    dist_element = matrix(c(distance2segment(4, 15, 0, 30, 0, 0), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["y"]), 0)
    expect_gt(as.double(new_result["x"]), 0)
    expect_gt(as.double(new_result["x"]), as.double(result["x"]))
    
    # effect reduce if grow up distance
    dist_element = matrix(c(distance2segment(14, 7, 0, 30, 0, 0), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["y"]), 0)
    expect_gt(as.double(new_result["x"]), 0)
    expect_lt(as.double(new_result["x"]), as.double(result["x"]))
  })

  #to mix case
  test_that("test.potential_effect_repulsive_mix",{
    dist_element = matrix(c(distance2segment(3, 2, 5, 5, 10.5, 9.5), 5),
                          nrow = 1, byrow = TRUE)
    infos_type = matrix(c(5, -1, 4, 0.1, 2,
                          7, -1, 78, 0.1, 2,
                          2, -1, 0.2, 0.1, 2,
                          8, -1, 48, 0.1, 2),
                        nrow = 4, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_lt(as.double(result["y"]), 0)
    expect_lt(as.double(result["x"]), 0)
    
    dist_element = matrix(c(distance2segment(7, 10.5, 5, 5, 10.5, 9.5), 2),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["y"]) > 0)
    
    dist_element = matrix(c(distance2segment(9, 7, 5, 5, 10.5, 9.5), 8),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_true(as.double(new_result["x"]) > 0)
    expect_true(as.double(new_result["y"]) < 0)

    dist_element = matrix(c(distance2segment(12.5, 10, 5, 5, 10.5, 9.5), 7),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_true(as.double(new_result["x"]) > 0)
    expect_true(as.double(new_result["y"]) > 0)
  })

  #no element case
  test_that("test.potential_effect_repulsive_no_element",{
    dist_element = matrix()
    infos_type = matrix(c(5, -1, 4, 0.1, 2,
                          7, -1, 78, 0.1, 2,
                          2, -1, 0.2, 0.1, 2,
                          8, -1, 48, 0.1, 2),
                        nrow = 4, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_equal(as.double(result["x"]), 0)
  })

  # potential_effect_attractive nominal case
  test_that("test.potential_effect_attractive_nominal",{
    dist_element = matrix(c(distance2segment(15, 15, 0, 0, 30, 0), 5,
                            distance2segment(15, 15, 0, 30, 0, 0), 5),
                          nrow = 2, byrow = TRUE)
    infos_type = matrix(c(5, 1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_true(as.double(result["x"]) < 0)
  })
  #effect abort case
  test_that("test.potential_effect_attractive_abort",{
    dist_element = matrix(c(distance2segment(15, 15, 0, 0, 30, 0), 5,
                            distance2segment(15, 15, 0, 30, 0, 0), 5,
                            distance2segment(15, 15, 30, 0, 30, 30), 5,
                            distance2segment(15, 15, 30, 30, 0, 30), 5),
                          nrow = 4, byrow = TRUE)
    infos_type = matrix(c(5, 1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_equal(as.double(result["x"]), 0)
  })
  #to down case
  test_that("test.potential_effect_attractive_down",{
    dist_element = matrix(c(distance2segment(4, 15, 0, 0, 30, 0), 5),
                          nrow = 1, byrow = TRUE)
    infos_type = matrix(c(5, 1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), 0)
    expect_true(as.double(result["y"]) < 0)
    print(result)
    # effect grow up if reduce distance
    dist_element = matrix(c(distance2segment(4, 1, 0, 0, 30, 0), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) < 0)
    expect_true(as.double(new_result["y"]) < as.double(result["y"]))
    print(new_result)
    # effect reduce if grow up distance
    dist_element = matrix(c(distance2segment(4, 25, 0, 0, 30, 0), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) < 0)
    expect_true(as.double(new_result["y"]) > as.double(result["y"]))
  })
  #to up case
  test_that("test.potential_effect_attractive_up",{
    dist_element = matrix(c(distance2segment(4, 15, 0, 30, 30, 30), 5),
                          nrow = 1, byrow = TRUE)
    infos_type = matrix(c(5, 1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), 0)
    expect_true(as.double(result["y"]) > 0)
    
    # effect grow up if reduce distance
    dist_element = matrix(c(distance2segment(4, 25, 0, 30, 30, 30), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) > 0)
    expect_true(as.double(new_result["y"]) > as.double(result["y"]))
    
    # effect reduce if grow up distance
    dist_element = matrix(c(distance2segment(4, 7, 0, 30, 30, 30), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["x"]), 0)
    expect_true(as.double(new_result["y"]) > 0)
    expect_true(as.double(new_result["y"]) < as.double(result["y"]))
  })
  
  #to left case
  test_that("test.potential_effect_attractive_left",{
    dist_element = matrix(c(distance2segment(9, 15, 0, 30, 0, 0), 5),
                          nrow = 1, byrow = TRUE)
    infos_type = matrix(c(5, 1, 4, 0.1, 2),
                        nrow = 1, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["y"]), 0)
    expect_true(as.double(result["x"]) < 0)
    
    # effect grow up if reduce distance
    dist_element = matrix(c(distance2segment(4, 15, 0, 30, 0, 0), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["y"]), 0)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["x"]) < as.double(result["x"]))
    
    # effect reduce if grow up distance
    dist_element = matrix(c(distance2segment(14, 7, 0, 30, 0, 0), 5),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_equal(as.double(new_result["y"]), 0)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["x"]) > as.double(result["x"]))
  })
  
  #to mix case
  test_that("test.potential_effect_attractive_mix",{
    dist_element = matrix(c(distance2segment(3, 2, 5, 5, 10.5, 9.5), 5),
                          nrow = 1, byrow = TRUE)
    infos_type = matrix(c(5, 1, 4, 0.1, 2,
                          7, 1, 78, 0.1, 2,
                          2, 1, 0.2, 0.1, 2,
                          8, 1, 48, 0.1, 2),
                        nrow = 4, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_true(as.double(result["y"]) > 0)
    expect_true(as.double(result["x"]) > 0)
    
    dist_element = matrix(c(distance2segment(7, 10.5, 5, 5, 10.5, 9.5), 2),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_true(as.double(new_result["x"]) > 0)
    expect_true(as.double(new_result["y"]) < 0)
    
    dist_element = matrix(c(distance2segment(9, 7, 5, 5, 10.5, 9.5), 8),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["y"]) > 0)
    
    dist_element = matrix(c(distance2segment(12.5, 10, 5, 5, 10.5, 9.5), 7),
                          nrow = 1, byrow = TRUE)
    new_result = potential_effect(dist_element,
                                  infos_type)
    expect_true(as.double(new_result["x"]) < 0)
    expect_true(as.double(new_result["y"]) < 0)
  })

  #no element case
  test_that("test.potential_effect_attractive_no_element",{
    dist_element = matrix()
    infos_type = matrix(c(5, 1, 4, 0.1, 2,
                          7, 1, 78, 0.1, 2,
                          2, 1, 0.2, 0.1, 2,
                          8, 1, 48, 0.1, 2),
                        nrow = 4, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["x"]), as.double(result["y"]))
    expect_equal(as.double(result["x"]), 0)
  })
  
  # potential effect bad size of parameter  case
  test_that("test.potential_value_bad_size", {
    # size element too short
    dist_element = matrix(c(distance2segment(2.5, 1.5, 1, 3, 2, 1),
                            distance2segment(2.5, 1.5, 2, 5, 4, 1)),
                          nrow = 2, byrow = TRUE)
    
    infos_type = matrix(c(7, 1, 0.4, 1, 3,
                          4, -1, 0.4, 1, 3),
                        nrow = 2, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["y"]), 0)
    expect_equal(as.double(result["x"]), 0)
    
    # size element too long
    dist_element = matrix(c(distance2segment(2, 4, 1, 3, 2, 1), 4, 5,
                            distance2segment(2, 4, 2, 5, 4, 1), 7, 5),
                          nrow = 2, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["y"]), 0)
    expect_equal(as.double(result["x"]), 0)
    
    # size info type too short
    infos_type = matrix(c(7, 1, 0.4, 1,
                          4, -1, 0.4, 1),
                        nrow = 2, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["y"]), 0)
    expect_equal(as.double(result["x"]), 0)
    
    # size info type too long
    infos_type = matrix(c(7, 1, 0.4, 1, 4,
                          4, -1, 0.4, 1, 4),
                        nrow = 2, byrow = TRUE)
    result = potential_effect(dist_element,
                              infos_type)
    expect_equal(as.double(result["y"]), 0)
    expect_equal(as.double(result["x"]), 0)
  })
  
  #------test all_effect-----#
  #nominal case
  test_that("test.all_effect_nominal", {
    dist_element = matrix(c(distance2segment(5, 8, 5, 5, 10.5, 9.5), 14,
                             distance2segment(5, 8, 1.5, 8, 4.5, 3), 1),
                           nrow = 2, byrow = TRUE)
    infos_type = matrix(c(5, 1, 4, 0.1, 2,
                          7, 1, 78, 0.1, 2,
                          14, 1, 6, 0.1, 2,
                          2, 1, 0.2, 0.1, 2,
                          1, -1, 6, 0.1, 2,
                          8, 1, 48, 0.1, 2),
                        nrow = 6, byrow = TRUE)
    coord_point = c("x" = 5, "y" = 8)
    result = all_effect(coord_point,
                        15,
                        15,
                        0.2,
                        dist_element,
                        infos_type,
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

  #------test potential value-----#
  #potential repulsive nominal case
  test_that("test.potential_value_repulsive_nominal", {
    dist_element = matrix(c(distance2segment(2, 4, 5, 5, 10.5, 9.5), 1),
                          nrow = 1, byrow = TRUE)
    infos_type = matrix(c(1, -1, 0.4, 1, 3,
                          4, -1, 9, 0.74, 1),
                        nrow = 2, byrow = TRUE)

    result = potential_value(dist_element,
                             infos_type)
    expect_gt(result, 0)

    dist_element = matrix(c(distance2segment(2, 4, 5, 5, 10.5, 9.5), 1,
                            distance2segment(2, 4, 8, 4, 0, 1), 4),
                          nrow = 2, byrow = TRUE)

    new_result = potential_value(dist_element,
                                 infos_type)
    expect_gt(new_result, 0)
    expect_gt(new_result, result)
  })

  #potential attractive nominal case
  test_that("test.potential_value_attractive_nominal", {
    dist_element = matrix(c(distance2segment(2, 4, 5, 5, 10.5, 9.5), 1),
                          nrow = 1, byrow = TRUE)

    infos_type = matrix(c(1, 1, 0.4, 1, 3,
                          4, 1, 9, 0.74, 1),
                        nrow = 2, byrow = TRUE)
    result = potential_value(dist_element,
                             infos_type)
    expect_lt(result, 0)

    dist_element = matrix(c(distance2segment(2, 4, 5, 5, 10.5, 9.5), 1,
                            distance2segment(2, 4, 8, 4, 0, 1), 4),
                          nrow = 2, byrow = TRUE)

    new_result = potential_value(dist_element,
                                 infos_type)
    expect_lt(new_result, 0)
    expect_lt(new_result, result)
  })

  # potential value nominal case
  test_that("test.potential_value_nominal", {
    dist_element = matrix(c(distance2segment(2.5, 1.5, 1, 3, 2, 1), 4,
                            distance2segment(2.5, 1.5, 2, 5, 4, 1), 7),
                           nrow = 2, byrow = TRUE)

    infos_type = matrix(c(7, 1, 0.4, 1, 3,
                          4, -1, 0.4, 1, 3),
                        nrow = 2, byrow = TRUE)
    result = potential_value(dist_element,
                             infos_type)
    expect_gt(result, 0)
    
    dist_element = matrix(c(distance2segment(2, 4, 1, 3, 2, 1), 4,
                            distance2segment(2, 4, 2, 5, 4, 1), 7),
                          nrow = 2, byrow = TRUE)
    new_result = potential_value(dist_element,
                                 infos_type)
    expect_lt(new_result, 0)
    expect_lt(new_result, result)
  })

  # potential value bad size of parameter  case
  test_that("test.potential_value_bad_size", {
    # size element too short
    dist_element = matrix(c(distance2segment(2.5, 1.5, 1, 3, 2, 1),
                            distance2segment(2.5, 1.5, 2, 5, 4, 1)),
                          nrow = 2, byrow = TRUE)
    
    infos_type = matrix(c(7, 1, 0.4, 1, 3,
                          4, -1, 0.4, 1, 3),
                        nrow = 2, byrow = TRUE)
    result = potential_value(dist_element,
                             infos_type)
    expect_equal(result, 0)
    
    # size element too long
    dist_element = matrix(c(distance2segment(2, 4, 1, 3, 2, 1), 4, 5,
                            distance2segment(2, 4, 2, 5, 4, 1), 7, 5),
                          nrow = 2, byrow = TRUE)
    result = potential_value(dist_element,
                             infos_type)
    expect_equal(result, 0)

    # size info type too short
    infos_type = matrix(c(7, 1, 0.4, 1,
                          4, -1, 0.4, 1),
                        nrow = 2, byrow = TRUE)
    result = potential_value(dist_element,
                             infos_type)
    expect_equal(result, 0)

    # size info type too long
    infos_type = matrix(c(7, 1, 0.4, 1, 4,
                          4, -1, 0.4, 1, 4),
                        nrow = 2, byrow = TRUE)
    result = potential_value(dist_element,
                             infos_type)
    expect_equal(result, 0)
  })

  #------test bound-----#
  #bound nominal case
  test_that("test.bound_nominal", {
    result = bound(25,-7,40)
    expect_equal(result, 25)

    result = bound(-10,-7,40)
    expect_equal(result, -7)

    result = bound(100,-7,40)
    expect_equal(result, 40)
  })

  #bound negative case
  test_that("test.bound_negative", {
    result = bound(-5,-6,-2)
    expect_equal(result, -5)

    result = bound(-7.2,-6,-2)
    expect_equal(result, -6)

    result = bound(-1.2,-6,-2)
    expect_equal(result, -2)
  })

  #bound positive case
  test_that("test.bound_positive", {
    result = bound(75,14.2,265)
    expect_equal(result, 75)
    
    result = bound(7.5,14.2,265.1)
    expect_equal(result, 14.2)
    
    result = bound(755,14.2,265.1)
    expect_equal(result, 265.1)
  })
},T)