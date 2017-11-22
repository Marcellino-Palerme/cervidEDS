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
    expect_true(result["x"] == 0, info = paste(result["x"], " diff 0"))
    expect_true(result["y"] > 0, info = paste(result["y"], " inf or equal 0"))
  })

  # border left of landscape case
  test_that("test.border_effect_border_left", {
    result = border_effect(56,21,0.12,0,10)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], " diff 0"))
    expect_true(result["y"] > 0, info = paste(result["y"], " inf or equal 0"))
  })

  # border up of landscape case
  test_that("test.border_effect_border_up", {
    result = border_effect(56,21,0.12,30.75,21)
    print(result)
    expect_true(result["x"] < 0, info = paste(result["x"], " sup or equal 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], " diff 0"))
  })
  
  # border down of landscape case
  test_that("test.border_effect_border_down", {
    result = border_effect(56,21,0.12,30.75,0)
    print(result)
    expect_true(result["x"] < 0, info = paste(result["x"], " sup or equal 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], " diff 0"))
  })

  # corner down of landscape case
  test_that("test.border_effect_corner_down", {
    # corner down rigth
    result = border_effect(451,96,0.024,0,0)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], " diff 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], " diff 0"))
    
    # corner down left
    result = border_effect(451,96,0.024,451,0)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], " diff 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], " diff 0"))
  })

  # corner up of landscape case
  test_that("test.border_effect_corner_up", {
    # corner up rigth
    result = border_effect(451,96,0.024,0,96)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], " diff 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], " diff 0"))
    
    # corner up left
    result = border_effect(451,96,0.024,451,96)
    print(result)
    expect_true(result["x"] == 0, info = paste(result["x"], " diff 0"))
    expect_true(result["y"] == 0, info = paste(result["y"], " diff 0"))
  })
  
  # nominal case
  test_that("test.border_effect_nominal", {
    # up rigth
    result = border_effect(85,14,0.02,31.42,10.4)
    print(result)
    expect_true(result["x"] > 0, info = paste(result["x"], " inf or equal 0"))
    expect_true(result["y"] < 0, info = paste(result["y"], " sup or equal 0"))
    
    # down rigth
    result = border_effect(85,14,0.02,31.42,4.153)
    print(result)
    expect_true(result["x"] > 0, info = paste(result["x"], " inf or equal 0"))
    expect_true(result["y"] > 0, info = paste(result["y"], " inf or equal 0"))

    # up left
    result = border_effect(85,14,0.02,74.21,10.4)
    print(result)
    expect_true(result["x"] < 0, info = paste(result["x"], " sup or equal 0"))
    expect_true(result["y"] < 0, info = paste(result["y"], " sup or equal 0"))
    
    # down left
    result = border_effect(85,14,0.02,65.123,4.153)
    print(result)
    expect_true(result["x"] < 0, info = paste(result["x"], " sup or equal 0"))
    expect_true(result["y"] > 0, info = paste(result["y"], " inf or equal 0"))
    
  })
},T)