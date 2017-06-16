# Visualisation et 


require("sp")
require("R6")
source("model.R",local = TRUE)
source("functions/utility.R",local = TRUE)



#Generer un paysage de polygone convexe
#Parameters:
#nb_poly : number of polygon
#width : width of landscape
#height : height of landscape
#
#Return:
#SpatialPolygonDataFrame : landscape with convex ploygons
gen_land <- function(nb_poly=10, width=60, height=60)
{
  # size of the landscape in meters (min is 60*60)
  xlim <- c(0,max(width,60))
  ylim <- c(0,max(height,60)) 
  # number of polygone (min is 10)
  n <- max(nb_poly,10)
  
  # regularity of the polygonal seeds pattern
  iterations <- 100
  
  source('functions/landscape_functions.R')
  
  # 6 metrics can be controled
  target.metrics <- c(ENN_l = 100, # intra aggregation for laying sites (polygonal)
                      ENN_f = 45, # intra aggregation for feeding sites (linear)
                      ENN_fl = 80, # inter aggreg between feeding and laying resources (ISOLATION METRIC)
                      IL = .1, # interface length between laying and feeding resources (CONTIGUITY METRIC)
                      LCP_l = .1, # proportion of laying resource
                      LCP_f = .04) # proportion of feeding resource
  # some metrics can be relaxed like the first two here (intra ressource agreggation),
  # the other need a scale factor to have equivalent impact on the GA convergence
  scale <- c(0, # ENN_l
             0, # ENN_f
             1, # ENN_fl , used as the scaling unit here
             pmax(1, target.metrics[3] / pmax(target.metrics[4], .1)), # interface length IL
             pmax(1, target.metrics[3] / target.metrics[5]), # LCP_f (more constraint is applied on proportions)
             pmax(1, target.metrics[3] / target.metrics[6])) # LCP_l
  
  # Genetic algorithm parameters, see function ls.gen.GA in functionsLSGA.R for detailed meanings of the parameters
  # increase N, max.gen and max.gen.fail to increase the computational effort to reach the target metrics
  # modifying the other parameters could also improve convergence, but in a less straitforward way
  N <- 300
  max.gen <- 2
  max.gen.fail <- 25 
  r <- .5
  tau <- .001
  crs.ovr <- 3
  tournament <- TRUE
  k.tourn <- pmax(3, round(N/5))
  p.tourn <- .2
  p.trunc <- .3
  plot <- FALSE ##
  verbose <- 1000
  
  # run the genetic algorithm
  landscape.res <- ls.gen.GA(n = n, ls.metrics = target.metrics[1:4],
                             props = target.metrics[5:6],
                             sd.max = target.metrics[7], xlim=xlim, 
                             scale = scale, ylim = ylim, N = N, 
                             max.gen = max.gen, max.gen.fail = max.gen.fail,
                             r = r, tau = tau, crs.ovr = crs.ovr,
                             tournament = tournament, k.tourn = k.tourn, 
                             iterations = iterations, p.tourn = p.tourn,
                             p.trunc = p.trunc, plot = plot, verbose = verbose)
  #Delete one element of data 
  landscape.res$spdf$marks <- NULL
  #add id for each polygon
  #landscape.res$spdf$id = 1:length(landscape.res$spdf)
  
  return(landscape.res$spdf)
}

#Give Id of all polygons
#
#Parameters
#land (SpatialPolygons*): the kandscape
#Return
#list of id
getIdsSpatialPolygons <- function(land)
{
  return(sapply(slot(land, "polygons"),
                function(x) slot(x, "ID")))
}

# Give a type at each polygon
#
#Parameters:
#landscape (SpatialPolygoneDataFrame): the landscape whose affect types
#nb_type (int) : number types are affect
#
#Return:
#landscape (SpatialPolygoneDataFrame): the landscape with types
affect_type <- function(landscape, nb_type)
{
  # min number type is 1
  nb_type = max(1,nb_type)
  # create random list of type
  lt_id_types = sample(1:nb_type,length(landscape),replace = T)
  # affect type 
  landscape$id_type = lt_id_types
  
  return(landscape)
}

#give neighbours of each polygon

get_neighbours <- function(landscape)
{
  library(spdep)  
  return(poly2nb(landscape, queen = TRUE,
                 row.names = getSpPPolygonsIDSlots(landscape)))
}

# get coordonnate of polygon in SpatialPolygons
#
#Parameters:
#landscape (SpatialPolygons)
#id (int): id of polygon
#
#Return 
# (matrix X*2): coordinates of polygon
getCoordsSpatialPolygons <- function(landscape, id)
{
  # get position of polygon with this id
  pos_id = match(TRUE, getIdsSpatialPolygons(landscape) == id)
  # get list of polygons
  polys = attr(landscape,"polygons")
  # get the polygon 
  poly = attr(polys[[pos_id]],"Polygons")
  # a polygon is a list of polygons
  coords = NULL
  for (pol in poly)
  {
    # concatenate all coordinate of each polygon of polygon
    coords = rbind(coords,attr(pol,"coords"))
  }
  #return coordinates
  return(coords)
}

# know commun coordonate between two polygons
#
#Parameters:
#landscape (SpatialPolygonsDataFrame): SIG
#ids (tupple of int): id of two polygons
#
#Return 
# (matrix X*2): coordonate of commun points
commun_coords <- function(landscape, ids)
{
  #transform polygons in poins
  coords_0 = getCoordsSpatialPolygons(landscape, ids[1])
  coords_0 <- SpatialPoints(getCoordsSpatialPolygons(landscape, ids[1]))
  coords_1 <- SpatialPoints(getCoordsSpatialPolygons(landscape, ids[2]))

  #search points overlap
  cc = over(coords_0, coords_1)

  #Delete first colum because is the same of last
  cc = cc[-1]

  #keep only colum without na
  cc = cc[complete.cases(cc)]
  
  #one points
  if (length(cc) == 1)
  {
    cc = c(cc, cc)
  }
  
  return(coords_1@coords[cc,])

}

#----------PotentialPolygon class-------------#
#Define the potential function of polygon
PotentialPolygon <- R6Class('PotentialPolygon',
  public = list(
    #initialize PotentialPolygon object
    #Parameters
    #id : (str) identifiant of polygon
    #str_func :(char) litteral potential function (eg:"5*exp(-x*2)")
    #Return
    #(int)
    #0 : sucess
    #1 : impossible to transform str_func in function
    #2 : impossible to create derivate in x
    #3 : impossible to create derivate in y
    initialize = function(id = 0, str_func = "")
    {
      #initialize functions to modify
      private$potential = function(x,y) x + y
      private$deriv_x = function(x,y) x + y
      private$deriv_y = function(x,y) x + y
      
      #Add id
      self$set_id(id)
      #Add potential function
      return(self$set_potential(str_func))
    },
    #Give id of polygone
    #return
    #(int)
    get_id = function()
    {
      return(private$id)
    },
    #Give potential function
    #Return
    #(function)
    get_potential = function()
    {
      return(private$potential)
    },
    #Give derivate of potential function in x
    #Return
    #(function)
    get_dx = function()
    {
      return(private$dx)
    },
    #Give derivate of potential function in y
    #Return
    #(function)
    get_dy = function()
    {
      return(private$dy)
    },
    #Modify id of polygon
    #Parameter
    #id :(char) id of polygon
    set_id = function(in_id)
    {
      private$id = as.character(in_id)
    },
    #Modify potential function and derivate in x and y
    #Parameter
    #str_func :(char) litteral potential function (eg:"5*exp(-x*2)")
    #Return
    #(int)
    #0 : sucess
    #1 : impossible to transform str_func in function
    #2 : impossible to create derivate in x
    #3 : impossible to create derivate in y
    set_potential = function(str_func)
    {
      if (str_func == "")
      {
        body(private$potential) <- 0
        private$deriv_x = function(x,y)
        {
          result = 0
          attr(result, "gradient") = 0
          result
        }
        private$deriv_y = function(x,y)
        {
          result = 0
          attr(result, "gradient") = 0
          result
        }
        return(0)
      }
      
      #Modify function potential
      tryCatch(
          {body(private$potential) <- parse(text = str_func)},
          error = function(err) 
          {return(1)}
        )

      #Modify derivate in x
      tryCatch(
          {body(private$deriv_x) <- deriv(parse(text = str_func),'x')},
          error = function(err) 
          {return(2)}
        )
      
      #Modify derivate in y
      tryCatch(
          {body(private$deriv_y) <- deriv(parse(text = str_func),'y')},
          error = function(err) 
          {return(3)}
        )
      
      return(0)
    }
  ),
  private = list(
    id = "0",
    potential = NULL,
    deriv_x = NULL,
    #derivate in x return only the gradient
    dx = function(x,y) attr(private$deriv_x(x,y),"gradient"),
    deriv_y = NULL,
    #derivate in x return only the gradient
    dy = function(x,y) attr(private$deriv_y(x,y),"gradient")
  )
  
)

#Indicate if element is a PotentialPolygon
is_PotentialPolygon = function(x)
{
  return(class(x)[1] == "PotentialPolygon")
}

#----------PotentialPolygons class-------------#
#Define list of PotentialPolygon
PotentialPolygons = R6Class("PotentialPolygons",
  public = list(
    #initialize PotentialPolygons object
    #Parameter
    #lt_pot_poly :(list) list of PotentialPolygon
    initialize = function(lt_pot_poly)
    {
      self$set_potentialpolygons(lt_pot_poly)
    },
    #Give a PotentialPolygon from this id
    #Parameter
    #id :(char) id of PotentialPolygon
    #Return
    #(PotentialPolygon)
    #Null : if polygon not find
    get_potentialpolygon = function(id)
    {
      index_pp = get_index(id, private$ids)
      #There aren t polygon with this id
      if (index_pp == 0)
      {
        return(NULL)
      }
      
      return(private$pot_polys[[index_pp]])
    },
    #Give list of ids
    #Return
    #(list)
    get_ids = function()
    {
      return(private$ids)
    },
    #Give potential function  one or sereral polygons
    #Parameter
    #ids(optional): (list) list of ids of polygons
    #if ids is empty is egal ask all ids
    #Return
    #(list of function)
    #if a polygon not exist, we add a NULL value in the list
    get_potentials = function(ids = list())
    {
      return(private$get_potentials_(ids, "pot"))
    },
    #Give derivate in x of potential function  one or sereral polygons
    #Parameter
    #ids(optional): (list) list of ids of polygons
    #if ids is empty is egal ask all ids
    #Return
    #(list of function)
    #if a polygon not exist, we add a NULL value in the list
    get_dxs = function(ids = list())
    {
      return(private$get_potentials_(ids, "dx"))
    },
    #Give derivate in y of potential function  one or sereral polygons
    #Parameter
    #ids(optional): (list) list of ids of polygons
    #if ids is empty is egal ask all ids
    #Return
    #(list of function)
    #if a polygon not exist, we add a NULL value in the list
    get_dys = function(ids = list())
    {
      return(private$get_potentials_(ids, "dy"))
    },
    #Add or modify one or several  PotentialPolygon
    #Parameter
    #lt_pot_poly :(list) list of PotentialPolygon
    #Return
    #(int): number of element of list not added 
    set_potentialpolygons = function(lt_pot_poly)
    {
      nb_error = 0 
      for (pot_poly in lt_pot_poly)
      {
        nb_error = nb_error + private$set_potentialpolygon(pot_poly)
      }
      
      return(nb_error)
    },
    #Modify potential function of polygone. From this function derivate in x and
    #y are calculated. If id isn't in list, we 'll create.
    #Parameter
    #id: identifiant of polygon
    #str_func :(char) litteral potential function (eg:"5*exp(-x*2)")
    #Return
    #(int)
    #0 : sucess
    #1 : impossible to transform str_func in function
    #2 : impossible to create derivate in x
    #3 : impossible to create derivate in y
    set_potential = function(id, str_func)
    {
      index = get_index(id, private$ids)
      
      if (index == 0)
      {
        #create a new potentialpolygon
        pot_poly = PotentialPolygon$new(id, str_func)
        #Verify if creating is correct
        if (is_PotentialPolygon(pot_poly))
        {
          private$pot_polys = append(private$pot_polys, pot_poly)
          private$ids = append(private$ids, id)
        }
        else
        {
          #return error
          return(pot_poly)
        }
      }
      else
      {
        #Modify the potential function
        return(private$pot_polys[[index]]$set_potential(str_func))
      }
      return(0)
    }
  ),
  private = list(
    pot_polys = list(),
    ids = list(),
    #Give potential function or derivate in x or y of one polygon
    #Parameter
    #id: () id of polygons
    #case : (str) pot = potential; dx = derivate in x; dr = derivate in y
    #Return
    #(function)
    get_potential_ = function(id, case)
    {
      pot_poly = self$get_potentialpolygon(id)
      
      if (is.null(pot_poly))
      {return(NULL)}
      
      switch(case,
             pot = {return(pot_poly$get_potential())},
             dx = {return(pot_poly$get_dx())},
             dy = {return(pot_poly$get_dy())}
             )
      

    },
    #Give potential function or derivate in x or y of one or sereral polygons
    #Parameter
    #ids(optional): (list) list of ids of polygons
    #if ids is empty is egal ask all ids
    #case : (str) pot = potential; dx = derivate in x; dr = derivate in y
    #Return
    #(list of function)
    #if a polygon not exist, we add a NULL value in the list
    get_potentials_ = function(ids = list(), case)
    {
      if (length(ids) == 0)
      {
        #take all ids
        ids = self$get_ids()
      }

      pots = list()
      #add one by one potentials
      for (id  in ids)
      {
        pot = private$get_potential_(id, case)
        pots = append(pots, pot)
      }
      
      return(pots)
    },
    #Add or modify one PotentialPolygon
    #Parameter
    #pot_poly : PotentialPolygon
    set_potentialpolygon = function(pot_poly)
    {
      #pot_poly isnt a PotentialPolygon
      if (!is_PotentialPolygon(pot_poly))
      {
        return(1)
      }
      
      index = get_index(pot_poly$get_id(), private$ids)

      #it is a new PotentialPolygon in list => add in list
      if (index == 0)
      {
        private$pot_polys = append(private$pot_polys, pot_poly)
        private$ids = append( private$ids, pot_poly$get_id())
      }
      else
      {
        #Modify in list
        private$pot_polys[[index]] = pot_poly
      }
      return(0)
    }
  )
)

#Indicate if element is a PotentialPolygons
is_PotentialPolygons = function(x)
{
  return(class(x)[1] == "PotentialPolygons")
}

#----------PotentialLandscape class-------------#
#Define a landscape with potential function of each polygon
PotentialLandscape = R6Class("PotentialLandscape",
  inherit = PotentialPolygons,
  public = list(
    #initialize PotentialLandscape object
    #Parameter
    #landscape (SpatialPolygonDataFrame): Typing landscape
    #interaction_model (TypeInteractModel): interaction of each type with 
    #                                          others
    initialize = function(landscape, interaction_model)
    {
      #initialize  at empty PotentialPolygons
      super$initialize(list())
      if (is_TypeInteractModel(interaction_model))
      {
        private$interaction_model = interaction_model
        self$set_landscape(landscape)
      }
    },
    #Give the landscape
    #Return
    #(SpatialPolygonsDataFrame)
    get_landscape = function()
    {
      return(private$landscape) 
    },
    #Give the interaction_model
    #Return
    #(TypeInteractionModel)
    get_interaction_model = function()
    {
      return(private$interaction_model)
    },
    #Give neighboors of polygone
    #Parameter
    #id (str): identifiant of polygon
    #Return
    #(list) ids of polygon
    #empty list if id isn't exist
    get_neighbours_id = function(id)
    {
      index = get_index(id, attr(private$neighbours, "region.id"))
      
      if (index == 0)
      {
        return(list())
      }
      else
      {
        return(private$neighbours[[index]])
      }
    },
    #Give value of potential for a point
    #Parameters
    #x (float) : abscisse
    #y (float) : ordonate
    #Return
    #(float)
    #NA if coordinate (x,y) out of landscape
    get_potential_coord = function(x,y)
    {
      ids = private$which_polygon(x,y)
      
      if (is.null(ids))
      {
        return(NA)
      }
      
      potential = 0
      for (id in ids)
      {
        potential = potential + self$get_potentials(list(id))[[1]](x,y)
      }
      
      return(potential/length(ids))
    },
    #Give value of derivate in x for a point
    #Parameters
    #x (float) : abscisse
    #y (float) : ordonate
    #Return
    #(float)
    #NA if coordinate (x,y) out of landscape
    get_dx_coord = function(x,y)
    {
      ids = private$which_polygon(x,y)
      
      if (is.null(ids))
      {
        return(NA)
      }
      
      dx = 0
      for (id in ids)
      {
        dx = dx + self$get_dxs(list(id))[[1]](x,y)
      }
      
      return(dx/length(ids))
    },
    #Give value of derivate in y for a point
    #Parameters
    #x (float) : abscisse
    #y (float) : ordonate
    #Return
    #(float)
    #NA if coordinate (x,y) out of landscape
    get_dy_coord = function(x,y)
    {
      ids = private$which_polygon(x,y)
      
      if (is.null(ids))
      {
        return(NA)
      }
      
      dy = 0
      for (id in ids)
      {
        dy = dy + self$get_dys(list(id))[[1]](x,y)
      }
      
      return(dy/length(ids))
    },
    #Add or modify landscape
    #and
    #potential function of each polygon calculated
    #Parameter
    #landscape (SpatialPolygonsDataFrame): Typing landscape
    set_landscape = function(landscape)
    {
      private$landscape = landscape
      private$neighbours = get_neighbours(landscape)
      private$calculate_potential()
      
    },
    #Add or modify interaction_model
    set_interaction_model = function(interaction_model)
    {
      if (is_TypeInteractModel(interaction_model))
      {
        private$interaction_model = interaction_model
        private$calculate_potential()
        return(0)
      }
      else
      {
        return(1)
      }
    },
    #Plot the landscape with potential values
    #Parameter
    #precision (float): 
    #with_landscape (bool): superimpose borders of polygons of landscape
    plot_potential = function(precision = 1, with_lanscape = TRUE)
    {
      #Take extrem value of landscape
      min_x = attr(private$landscape,"bbox")[1,1]
      max_x = attr(private$landscape,"bbox")[1,2]
      min_y = attr(private$landscape,"bbox")[2,1]
      max_y = attr(private$landscape,"bbox")[2,2]
      pot = c()
      for (x in seq(min_x,max_x,precision))
      {
        row_pot = c()
        for (y in seq(min_y,max_y,precision))
        {
          row_pot = c(pp$get_potential_coord(x, y),row_pot)
        }
        #add new colunm
        pot = cbind(pot,row_pot)
      }
      #show matrix of potentials
      plot(raster(pot, xmn = min_x, xmx = max_x, ymn = min_y, ymx = max_y))
      
      #superimpose
      if (isTRUE(with_lanscape))
      {
        plot(private$landscape, add = TRUE)
      }
      return(pot)
    }
  ),
  private = list(
    landscape = NULL,
    neighbours = NULL,
    interaction_model = NULL,
    #Create potentiel function of each polycon
    calculate_potential = function()
    {
      for (id_poly in attr(private$neighbours, "region.id"))
      {
        landscape = private$landscape

        #create coordonate of line of polygon
        poly_coords = getCoordsSpatialPolygons(landscape, id_poly)
        poly_lines = cbind(poly_coords,c(poly_coords[,1][-1],0))
        poly_lines = cbind(poly_lines,c(poly_coords[,2][-1],0))
        
        index = 1
        potent = list()
        pos_poly = get_index(id_poly, getIdsSpatialPolygons(landscape))
        type_poly = landscape$id_type[pos_poly]
        
        for (nei in private$neighbours[[pos_poly]])
        {
          #verify if polygon have neighbour.
          if (nei > 0)
          {
            id_nei = attr(private$neighbours, "region.id")[nei]
            type_nei = landscape$id_type[nei]
            
            # Take function and parameters of function for interact 
            # between the two type
            func =  private$interaction_model$get_func_interact(type_poly, 
                                                                type_nei)
            param = private$interaction_model$get_params(type_poly, 
                                                            type_nei)
            
            # take coordonate of commun line between two polygone
            coords = commun_coords(landscape, c(id_poly,id_nei))
            
            for (ind_coord in 2:length(coords[,1]))
            {
              potent[[index]] = func(param[[1]],param[[2]],param[[3]],
                                     coords[ind_coord - 1,1],
                                     coords[ind_coord - 1,2], 
                                     coords[ind_coord,1],coords[ind_coord,2])
              index = index + 1
              
              num_line = 1
              repeat
              {
                #it is just a point
                a_point = unique(coords[1,] == coords[2,])
                if (length(a_point) == 1 & a_point)
                {
                  break
                }
                
                if (num_line > length(poly_lines[,1]))
                {
                  break
                }
                
                diff1 = unique(poly_lines[num_line,] == c(coords[ind_coord - 1,]
                                                          ,coords[2,])) 
                diff2 = unique(poly_lines[num_line,] == c(coords[2,],
                                                          coords[ind_coord - 1,]))
                if (length(diff1) == 1 & diff1
                    || length(diff2) == 1 & diff2 )
                {
                  poly_lines = poly_lines[-num_line,]
                  break
                }
                num_line = num_line + 1 
              }
            }
          }
          
          
        }
        
        if (length(poly_lines) > 4)
        {
          for (num_line in 1:(length(poly_lines[,1]) - 1))
          {
            # take function and parameters of function for border
            func =  private$interaction_model$get_func_interact(type_poly,0)
            param = private$interaction_model$get_params(type_poly,0)
            
            # take coordonate of commun line between two polygone
            potent[[index]] = func(param[[1]],param[[2]],param[[3]],
                                   poly_lines[num_line,1],poly_lines[num_line,2], 
                                   poly_lines[num_line,3],poly_lines[num_line,4])
            index = index + 1
          }
        }
        #take agglo function
        agglo = private$interaction_model$get_func_agglo(type_poly)
        self$set_potential(id_poly,agglo(c(potent)))
      }
    },
    #indicate in which polygon is a point
    #Parameters
    #x (float) : abscisse
    #y (float) : ordonate
    #Return
    #(list of str) id of polygons
    #Null if coordonate out of landscape
    which_polygon = function(x,y)
    {
      #verify if coordinate in landscape
      limit_land = bbox(private$landscape)
      if (findInterval(x, limit_land[1,], rightmost.closed = TRUE) != 1 &&
          findInterval(y, limit_land[2,], rightmost.closed = TRUE) != 1)
      {
        return(NULL)
      }
      
      ids = list()
      id_polys = attr(private$neighbours, "region.id")
      for (id_poly in id_polys)
      {
        poly_coords = getCoordsSpatialPolygons(private$landscape,
                                               id_poly)
        if (point.in.polygon(x,y,
                             poly_coords[,1],
                             poly_coords[,2]) > 0)
        {
          ids = append(ids, id_poly)
          #only  neighbours of polygon can contain too the point (x,y)
          index_poly = get_index(id_poly, id_polys)
          for (index_nei in private$neighbours[[index_poly]])
          {
            if (point.in.polygon(x,y,
                                 poly_coords[,1],
                                 poly_coords[,2]) > 0)
            {
              ids = append(ids, id_polys[index_nei])
            }
          }
          break
        }
      }
      # point (x,y) is
      if (length(ids) == 0)
      {
        ids = NULL
      }
      
      return(ids)
    }
  )
)