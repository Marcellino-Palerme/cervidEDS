# Visualisation et 


#----------PotentialPolygon class-------------#

#Define the potential function of polygon
PotentialPolygon <- R6::R6Class('PotentialPolygon',
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
PotentialPolygons = R6::R6Class("PotentialPolygons",
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
#' @title  PotentialLandscape
#' @description  Define a landscape with potential function of each polygon
#' @export
PotentialLandscape = R6::R6Class("PotentialLandscape",
  inherit = PotentialPolygons,
  public = list(
    #initialize PotentialLandscape object
    #Parameter
    #land_poly (SpatialPolygonDataFrame): Typing polygon landscape
    #land_lines (SpatialLinesDataFrame): Typing line landscape
    #interaction_model (TypeInteractModel): interaction of each type with 
    #                                          others
    #perception (string): kind to calculate the potential.
    #                   - n : see only neighbour
    #                   - s : all element of landscape spread
    initialize = function(land_poly, land_lines, interaction_model,
                          perception="n")
    {
      #initialize at empty PotentialPolygons
      super$initialize(list())
      if (is_TypeInteractModel(interaction_model))
      {
        private$interaction_model = interaction_model
        private$perception = perception
        self$set_landscape(land_poly, land_lines)
      }
    },
    #Give the landscape
    #Return
    #(SpatialPolygonsDataFrame)
    get_landscape = function()
    {
      return(private$land_poly) 
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
    #land_poly (SpatialPolygonDataFrame): Typing polygon landscape
    #land_lines (SpatialLinesDataFrame): Typing line landscape
    set_landscape = function(land_poly, land_lines)
    {
      private$land_poly = land_poly
      private$land_lines = land_lines
      # no neighbours when you are alone
      if (length(land_poly$id_type) > 1)
      {
        private$neighbours = get_neighbours(land_poly) 
      }
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
    plot_potential = function(precision = 1)
    {
      #Take extrem value of landscape
      min_x = attr(private$land_poly,"bbox")[1,1]
      max_x = attr(private$land_poly,"bbox")[1,2]
      min_y = attr(private$land_poly,"bbox")[2,1]
      max_y = attr(private$land_poly,"bbox")[2,2]
      
      #calculate potential of each point
      pot = c()
      for (x in seq(min_x,max_x,precision))
      {
        row_pot = c()
        for (y in seq(min_y,max_y,precision))
        {
          row_pot = c(self$get_potential_coord(x, y),row_pot)
        }
        #add new colunm
        pot = cbind(pot,row_pot)
      }

      #show matrix of potentials
      plot(raster(pot, xmn = min_x, xmx = max_x, ymn = min_y, ymx = max_y),
           xlim = c(min_x, max_x), ylim = c(min_y, max_y))

      # number of different type
      nb_type = max(c(private$land_poly$id_type,
                      private$land_lines$id_type)) + 2
      
      # def color for each type 
      type_color = rainbow(nb_type)

      # Add number of type of polygon with color of type
      centroid = getSpPPolygonsLabptSlots(private$land_poly)
      text(centroid, labels = private$land_poly$id_type,
           col = type_color[strtoi(private$land_poly$id_type) + 2])
      
      # Add lines in color of type
      coord_lines = coordinates(private$land_lines)
      for (index in seq_along(coord_lines))
      {
        lines(coord_lines[[index]][[1]],
              col = type_color[strtoi(private$land_lines$id_type[index]) + 2])
      }

      # add legend
      legend(max_x,max_y,
             legend = c("no type","border",
                        sapply(1:(nb_type - 2),
                               private$interaction_model$get_name)),
             col = type_color, lty = 1,xpd = T, text.col = type_color)

      return(pot)
    }
  ),
  private = list(
    land_poly = NULL,
    neighbours = NULL,
    land_lines = NULL,
    interaction_model = NULL,
    perception = "n",
    # Select method to calculate potential
    calculate_potential = function()
    {
      switch(
        private$perception,
        "n" = private$neighbours_potential(),
        "s" = private$spread_potential()
      )
    },
    #Create potentiel function of each polycon
    neighbours_potential = function()
    {
      ids_lines = getIdsSpatialLines(private$land_lines)
      ids_poly = getIdsSpatialPolygons(private$land_poly)
      
      for (id_poly in ids_poly)
      {
        index = 1
        potent = list()
        
        # take index of all line of polygon
        keep_ids = ids_lines[c(which(private$land_lines$id_poly1 %in% id_poly),
                               which(private$land_lines$id_poly2 %in% id_poly))]
        
        # Impossible polygon without lines
        if (is.na(keep_ids))
        {
          next
        }
        
        pos_poly = which(ids_poly %in% id_poly)
        type_poly = private$land_poly$id_type[pos_poly]
        
        for (id_line in keep_ids)
        {
          pos_line = which(ids_lines %in% id_line) 
          types = private$land_lines$id_type[pos_line]
          coords = getCoordsSpatialLines(private$land_lines, id_line)
          
          # if line is not a border 
          if (types != 0)
          {
            # take the neighbour polygon
            id_nei = sub(id_poly,"", paste(private$land_lines$id_poly1[pos_line],
                                           private$land_lines$id_poly2[pos_line],
                                           sep = ""))
            pos_nei = which(ids_poly %in% id_nei)
            types = list(types, private$land_poly$id_type[pos_nei])
          }
          
          for (type in types)
          {
            if (type == -1)
            {
              next()
            }
            # Take function and parameters of function for interact 
            # between the two type
            func =  private$interaction_model$get_func_interact(type_poly, 
                                                                type)
            param = private$interaction_model$get_params(type_poly, 
                                                         type)
            potent[[index]] = func(param[[1]],param[[2]],param[[3]],
                                   coords[1,1],coords[1,2], 
                                   coords[2,1],coords[2,2])
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
      limit_land = bbox(private$land_poly)
      if (findInterval(x, limit_land[1,], rightmost.closed = TRUE) != 1 &&
          findInterval(y, limit_land[2,], rightmost.closed = TRUE) != 1)
      {
        return(NULL)
      }
      
      ids = list()
      id_polys = attr(private$neighbours, "region.id")
      for (id_poly in id_polys)
      {
        poly_coords = getCoordsSpatialPolygons(private$land_poly,
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
            poly_coords = getCoordsSpatialPolygons(private$land_poly,
                                                   id_polys[index_nei])
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

      # point (x,y) is out of landscape
      if (length(ids) == 0)
      {
        ids = NULL
      }

      # The landscape composed of one polygone
      if (is.null(id_polys))
      {
        ids = getIdsSpatialPolygons(private$land_poly)
      }
      
      return(ids)
    }
  )
)
