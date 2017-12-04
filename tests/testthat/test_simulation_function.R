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
},T)