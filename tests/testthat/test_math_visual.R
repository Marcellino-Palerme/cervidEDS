options("testthat.output_file" = "result/math_visual.xml")

with_reporter("junit",{
context('math visual perception')

  get_trajectory = function(X, Y, beta, sigma, Tmax, Nb_point)
  {
    ## create Interaction model ##
    #Interact border
    border = Interact$new(0,potentiel_0,list(2,4,0))
    #param potentiel_0 to attractive
    attractive = list(1, beta, 1)
    #param potentiel_0 to repulsive
    repulsive = list(-1, 1, 8)
    #param potentiel_0 to neutre
    neutre = list(0, 0, 0)
    
    #Interact hote open neightbour close
    h_open_n_close = Interact$new(1, potentiel_0, attractive)
    #Interact hote open neightbour open
    h_open_n_open = Interact$new(2, potentiel_0, neutre)
    
    #Interact hote close neightbour close
    h_close_n_close = Interact$new(1, potentiel_0, neutre)
    #Interact hote close neightbour open
    h_close_n_open = Interact$new(2, potentiel_0, repulsive)
    
    
    #define type_close
    type_close = TypeInteract$new(1, "close", agglo_0,list(border, h_close_n_close,
                                                           h_close_n_open))
    #define type_open
    type_open = TypeInteract$new(2, "open", agglo_0,list(border, h_open_n_close,
                                                         h_open_n_open))
    
    #define model
    model_maths = TypeInteractModel$new(list(type_close, type_open))
    
    ## Create landscapes ##
    coords = matrix(c(0,0,10,0,10,5,0,5,0,0),5,2,byrow = T)
    p = Polygon(coords)
    coords = matrix(c(0,5,10,5,10,10,0,10,0,5),5,2,byrow = T)
    p1 = Polygon(coords)
    ps = Polygons(list(p),ID = c(1))
    ps1 = Polygons(list(p1),ID = c(2))
    land_maths = SpatialPolygons(list(ps,ps1))
    lines_maths = extract_lines(land_maths)
    # affect type of polygons and lines
    land_maths$id_type = c(2,2)
    lines_maths$id_type = c(0,1,0,0,0,0,0)
    
    ## Create Potential Landscapes ##
    pl_maths = PotentialLandscape$new(land_maths, lines_maths, model_maths)
    pl_maths$plot_potential(0.5)
    ## Put a animal and move it ##
    trajx = c(X)
    trajy = c(Y)
    dt = Tmax/Nb_point
    for (i in seq(Nb_point))
    {
      X = X - pl_maths$get_dx_coord(X, Y)*dt + sigma * sqrt(dt) * rnorm(1)
      trajx = c(trajx, X)
      Y = Y - pl_maths$get_dy_coord(X, Y)*dt + sigma * sqrt(dt) * rnorm(1)
      trajy = c(trajy, Y)
    }
    lines(trajx, trajy)
    return(list(trajx, trajy, pl_maths))
  }
  
  log_likelihood = function(param, sigma, dt, dataX, dataY, pot_land)
  {
    # get model
    model = pot_land$get_interaction_model()
    # modify parameter of interaction between 2 and 1
    model$set_params(2, 1, param)
    pot_land$set_interaction_model(model)
    
    log_prob = c()
    for (j in seq(length(dataX)))
    {
      for (i in seq(length(dataX[[j]]) - 1))
      {
        # estimate mean of gaussian 2D
        mu = as.vector(c(-pot_land$get_dx_coord(dataX[[j]][i],
                                                dataY[[j]][i])*dt,
                         -pot_land$get_dy_coord(dataX[[j]][i],
                                                dataY[[j]][i])*dt))
        # the observed moving of animal
        depX = dataX[[j]][i + 1] - dataX[[j]][i]
        depY = dataY[[j]][i + 1] - dataY[[j]][i]
        
        mat_sigma = matrix(c(sigma^2*dt, 0, 0, sigma^2*dt), 2, 2)
        # probability observed moving of animal in gaussian
        log_prob = c(log_prob, 
                     dmvnorm(x = as.vector(c(depX, depY)), 
                             mean = mu, sigma = mat_sigma,
                             log = T))
      }
    }
    return(sum(log_prob))
  }
  
# nominal case
test_that("test.nominal",{
# Inference test on visual perception
  
  require(numDeriv)
  require(mvtnorm)
  
  Nb_point = 200
  Tmax = 20
  dt = Tmax/Nb_point
  traj = get_trajectory(4, 8, -0.2, 0.2, Tmax, Nb_point)
  trajx = traj[[1]]
  trajy = traj[[2]]
  pl_maths = traj[[3]]


  esti_sigma = sqrt(mean((trajx[2:(Nb_point + 1)] - trajx[1:Nb_point])^2) / dt )
  
  # We cant search only b 
  result = optim(c(0, esti_sigma),function(x)  {return(-1*log_likelihood(
                                                 param = list(1, x[1], 1),
                                                 sigma = x[2], dt = dt, 
                                                 dataX = list(trajx),
                                                 dataY = list(trajy),
                                                 pot_land = pl_maths))},
                control = list(trace = 1))

  expect_equal(result$par[1], -0.2, tolerance = 0.01)
  expect_equal(abs(result$par[2]), 0.2, tolerance = 0.02)
})

# deux beta case
test_that("test.deux_beta",{
  # Inference test on visual perception
  
  require(numDeriv)
  require(mvtnorm)
  
  Nb_point = 200
  Tmax = 20
  dt = Tmax/Nb_point
  traj = get_trajectory(4, 8, -0.2, 0.2, Tmax, Nb_point)
  f_trajx = traj[[1]]
  f_trajy = traj[[2]]
  f_pl_maths = traj[[3]]
  
  traj = get_trajectory(8, 3, -0.11, 0.2, Tmax, Nb_point)
  s_trajx = traj[[1]]
  s_trajy = traj[[2]]
  s_pl_maths = traj[[3]]
  
  esti_sigma = sqrt(mean((f_trajx[2:(Nb_point + 1)] - f_trajx[1:Nb_point])^2) / dt )
  
  # We cant search only b 
  # We search a common b for two trajectory 
  result = optim(c(0, esti_sigma),function(x)  {return(-1*log_likelihood(
                                                      param = list(1, x[1], 1),
                                                      sigma = x[2], dt = dt, 
                                                      dataX = list(f_trajx,
                                                                   s_trajx), 
                                                      dataY = list(f_trajy,
                                                                   s_trajy),
                                                      pot_land = f_pl_maths))},
                                                      control = list(trace = 1))

  expect_equal(abs(result$par[2]), 0.2, tolerance = 0.02)
  # We search b for each trajectory
  f_result = optim(c(0, esti_sigma),function(x)  {return(-1*log_likelihood(
                                                      param = list(1, x[1], 1),
                                                      sigma = x[2], dt = dt, 
                                                      dataX = list(f_trajx), 
                                                      dataY = list(f_trajy),
                                                      pot_land = f_pl_maths))},
                                                      control = list(trace = 1))
  expect_equal(f_result$par[1], -0.2, tolerance = 0.01)
  expect_equal(abs(f_result$par[2]), 0.2, tolerance = 0.02)
  s_result = optim(c(0, esti_sigma),function(x)  {return(-1*log_likelihood(
                                                      param = list(1, x[1], 1),
                                                      sigma = x[2], dt = dt, 
                                                      dataX = list(s_trajx), 
                                                      dataY = list(s_trajy),
                                                      pot_land = s_pl_maths))},
                                                      control = list(trace = 1))  
  expect_equal(s_result$par[1], -0.11, tolerance = 0.01)
  expect_equal(abs(s_result$par[2]), 0.2, tolerance = 0.02)
  
  #Verify log likelihood better when we search b for each trajectory
  expect_lt(abs(result$value), abs(f_result$value + s_result$value))
})
},T)

