options("testthat.output_file" = "result/math_olfactory.xml")
with_reporter("junit",{
  context('math olfactory perception')
  
  # Inference test on olfactory perception
  
  require(numDeriv)
  require(mvtnorm)
  
  type_math = matrix(c(1, 1, 1, 0.1, 2,
                       2, 0, 1, 0.1, 2),
                     nrow = 2, byrow = TRUE)
  
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
  land_maths$id_type = c(1,2)
  lines_maths$id_type = c(0,-1,0,0,0,0,0)
  
  plot_potential(land_maths,
                 lines_maths,
                 type_math,
                 precision = 1)

  ## Put a animal and move it ##
  X = 8
  Y = 9 
  sigma = 0.5
  trajx = X
  trajy = Y
  dt = 0.01
  Nb_point = 50
  for (i in seq(Nb_point))
  {
    traj = next_coord(X,
                      Y,
                      land_maths,
                      lines_maths,
                      type_math,
                      sigma,
                      dt)
    trajx = c(trajx, traj[1])
    X = traj[1]
    trajy = c(trajy, traj[2])
    Y = traj[2]
  }
  lines(trajx, trajy)
  
  
  
  log_likelihood = function(param, sigma, dt, dataX, dataY, land,
                                                            lines,
                                                            type_info)
  {
    # modify parameter of type 2
    type_info[2, 3] = param[1]
    type_info[2, 4] = param[2]
    
    log_prob = c()
    
    for (i in seq(length(dataX) - 1))
    {
      # estimate mean of gaussian 2D
      mu = as.vector(next_coord(dataX[i],
                                dataY[i],
                                land,
                                lines,
                                type_info,
                                sigma,
                                dt))
      # the observed moving of animal
      depX = dataX[i + 1] - dataX[i]
      depY = dataY[i + 1] - dataY[i]
      
      mat_sigma = matrix(c(sigma^2*dt, 0, 0, sigma^2*dt), 2, 2)
      # probability observed moving of animal in gaussian
      log_prob = c(log_prob, 
                   dmvnorm(x = as.vector(c(depX, depY)), 
                           mean = mu, sigma = mat_sigma,
                           log = T))
    }
    return(sum(log_prob))
  }
  
  esti_sigma = sqrt(mean((trajx[2:(Nb_point + 1)] - trajx[1:Nb_point])^2) / dt ) 
  
  result = optim(c(0.1, esti_sigma),function(x)  {return(-1*log_likelihood(
                                                 param = c(1, x[1]),
                                                 sigma = x[2], dt = dt, 
                                                 dataX = trajx, dataY = trajy,
                                                 land = land_maths,
                                                 lines = lines_maths,
                                                 type_info = type_math))},
                 control = list(trace = 1))
  
  expect_equal(result$par[1], 1, tolerance = 0.1)
  expect_equal(result$par[2], 0.1, tolerance = 0.01)
  expect_equal(result$par[3], 2, tolerance = 0.2)
  expect_equal(abs(result$par[4]), 0.5, tolerance = 0.05)
  
},T)
