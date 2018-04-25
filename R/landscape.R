# Visualisation et 


#----------PotentialPolygon class-------------#

#' @title PotentialPolygon class
#' @description Define the potential function of polygon
#' 
#' @export
PotentialPolygon <- R6::R6Class('PotentialPolygon',
  #' @section methods:
  public = list(
    #' \itemize{

    #' \item{PotentialPolygon$new(id, str_func)}{
    #' \describe{initialize PotentialPolygon object
    #' \itemize{
    #' \item{id}{(str) identifiant of polygon}
    #' \item{str_func}{(litteral potential function (eg:"5*exp(-x*2)")) }
    #' \item{(int)}{0 : sucess,
    #'              1 : impossible to transform str_func in function,
    #'              2 : impossible to create derivate in x,
    #'              3 : impossible to create derivate in y}
    #' }}}
    initialize = function(id = "0", str_func = "")
    {
      #initialize functions to modify
      private$potential = function(x,y) x + y
      private$deriv_x = function(x,y) x + y
      private$deriv_y = function(x,y) x + y
      
      #Add id
      if (self$set_id(id) > 0)
      {
        stop("id isn't characters or numeric")
      }
      #Add potential function
      if (self$set_potential(str_func) > 0)
      {
        stop(paste("Can't use",str_func))
      }
    },
    #' \item{$get_id()}{
    #' \describe{Give id of polygone
    #' \itemize{
    #' \item{(characters)}{identifiant of polygon}
    #' }}}
    get_id = function()
    {
      return(private$id)
    },
    #' \item{$get_potential()}{
    #' \describe{Give potential function
    #' \itemize{
    #' \item{(function)}{potential fucncion}
    #' }}}
    get_potential = function()
    {
      return(private$potential)
    },
    #' \item{$get_dx()}{
    #' \describe{Give derivate of potential function in x
    #' \itemize{
    #' \item{(function)}{derivate of potential function in x}
    #' }}}
    get_dx = function()
    {
      return(private$dx)
    },
    #' \item{$get_dy()}{
    #' \describe{Give derivate of potential function in y
    #' \itemize{
    #' \item{(function)}{derivate of potential function in y}
    #' }}}
    get_dy = function()
    {
      return(private$dy)
    },
    #' \item{$set_id(id)}{
    #' \describe{Modify id of polygon
    #' \itemize{
    #' \item{id}{(characters) identifiant of polygon}
    #' }}}
    set_id = function(in_id)
    {
      if (is.numeric(in_id))
      {
        private$id = as.character(in_id)
        return(0)
      }

      if (is.character(in_id))
      {
        private$id = in_id
        return(0)
      }
      return(1)
    },
    #' \item{$set_potential(str_func)}{
    #' \describe{Modify potential function and derivate in x and y
    #' \itemize{
    #' \item{str_func}{(str) litteral potential function (eg:"5*exp(-x*2)")}
    #' \item{(int)}{0 : sucess,
    #'              1 : impossible to transform str_func in function,
    #'              2 : impossible to create derivate in x,
    #'              3 : impossible to create derivate in y,
    #'              4 : str_func isn't characters}
    #' }}}
      set_potential = function(str_func)
    {
      # Verify if is character
      if (!is.character(str_func))
      {
        return(4)
      }
      # Create Null function with her gradient
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
    #' }
  ),
  private = list(
    id = "0",
    potential = NULL,
    deriv_x = NULL,
    #derivate in x return only the gradient
    dx = function(x,y) attr(private$deriv_x(x,y),"gradient"),
    deriv_y = NULL,
    #derivate in y return only the gradient
    dy = function(x,y) attr(private$deriv_y(x,y),"gradient")
  )
  
)

#' @title Is PotentialPolygon
#' @description Indicate if element is a PotentialPolygon
#' 
#' @param x element
#' @return boolean
#' @export
is_PotentialPolygon = function(x)
{
  return(class(x)[1] == "PotentialPolygon")
}

#----------PotentialPolygons class-------------#
#' @title  PotentialPolygons class
#' @description Define list of PotentialPolygon
#' 
#' @export
#
PotentialPolygons = R6::R6Class("PotentialPolygons",
  #' @section methods:
  public = list(
    #' \itemize{

    #' \item{PotentialPolygons$new(lt_pot_poly)}{
    #' \describe{initialize PotentialPolygons object
    #' \itemize{
    #' \item{lt_pot_poly }{(list) list of PotentialPolygon}
    #' }}}
    initialize = function(lt_pot_poly)
    {
      result = self$set_potentialpolygons(lt_pot_poly)
      # Verify process completely ok
      if ( result > 0)
      {
        stop("Element(s) in list not PotentialPolygon")
      }
      if ( result < 0)
      {
        stop("lt_pot_poly isn't a list")
      }
      
    },
    #' \item{$get_potentialpolygon(id)}{
    #' \describe{Give a PotentialPolygon from this id
    #' \itemize{
    #' \item{id }{(characters) id of PotentialPolygon}
    #' \item{(PotentialPolygon) }{Null : if polygon not find}
    #' }}}
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
    #' \item{$get_ids()}{
    #' \describe{Give list of ids
    #' \itemize{
    #' \item{(list)}{list of ids}
    #' }}}
    get_ids = function()
    {
      return(private$ids)
    },
    #' \item{$get_potentials(ids)}{
    #' \describe{Give potential function  one or sereral polygons
    #' \itemize{
    #' \item{ids(optional) }{(list) list of ids of polygons,
    #'                             if ids is empty is egal ask all ids}
    #' \item{(list of function) }{if a polygon not exist, we add a constant 
    #'                            function (f(x,y) = 0) in the list}
    #' }}}
    get_potentials = function(ids = list())
    {
      return(private$get_potentials_(ids, "pot"))
    },
    #' \item{$get_dxs(ids)}{
    #' \describe{Give derivate in x of potential function  one or sereral
    #'           polygons
    #' \itemize{
    #' \item{ids(optional) }{(list) list of ids of polygons
    #'                             if ids is empty is egal ask all ids}
    #' \item{}{() }
    #' \item{(list of function) }{if a polygon not exist, we add a NULL value in 
    #'                           the list}
    #' }}}
    get_dxs = function(ids = list())
    {
      return(private$get_potentials_(ids, "dx"))
    },
    #' \item{$get_dys(ids)}{
    #' \describe{Give derivate in y of potential function  one or sereral
    #'           polygons
    #' \itemize{
    #' \item{ids(optional) }{(list) list of ids of polygons
    #'                              if ids is empty is egal ask all ids}
    #' \item{(list of function) }{if a polygon not exist, we add a NULL value in 
    #'                            the list}
    #' }}}
    get_dys = function(ids = list())
    {
      return(private$get_potentials_(ids, "dy"))
    },
    #' \item{$set_potentialpolygons(lt_pot_poly)}{
    #' \describe{Add or modify one or several  PotentialPolygon
    #' \itemize{
    #' \item{lt_pot_poly }{(list) list of PotentialPolygon}
    #' \item{(int) }{number of element of list not added, -1 : input not a list}
    #' }}}
    set_potentialpolygons = function(lt_pot_poly)
    {
      # Verify is a list
      if (!is.list(lt_pot_poly))
      {
        return(-1)
      }
      nb_error = 0 
      for (pot_poly in lt_pot_poly)
      {
        nb_error = nb_error + private$set_potentialpolygon(pot_poly)
      }
      
      return(nb_error)
    },
    #' \item{$set_potential(id, str_func)}{
    #' \describe{Modify potential function of polygone. From this function
    #'           derivate in x and y are calculated. If id isn't in list, we 'll
    #'           create.
    #' \itemize{
    #' \item{id }{identifiant of polygon}
    #' \item{str_func }{(char) litteral potential function (eg:"5*exp(-x*2)")}
    #' \item{(int) }{0 : sucess,
    #'               1 : impossible to transform str_func in function,
    #'               2 : impossible to create derivate in x,
    #'               3 : impossible to create derivate in y,
    #'               4 : impossible to create PotentialPolygon}
    #' }}}
    set_potential = function(id, str_func)
    {
      index = get_index(id, private$ids)
      
      if (index == 0)
      {
        #create a new potentialpolygon
        tryCatch(
          {pot_poly = PotentialPolygon$new(id, str_func)},
          error = function(err) 
          {return(4)}
        )
        # add new potentialpolygon
        private$pot_polys = append(private$pot_polys, pot_poly)
        private$ids = append(private$ids, id)
      }
      else
      {
        #Modify the potential function
        return(private$pot_polys[[index]]$set_potential(str_func))
      }
      return(0)
    }
  #' }
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
    #if a polygon not exist, we function is constant function (f(x,y) = 0)
    get_potential_ = function(id, case)
    {
      pot_poly = self$get_potentialpolygon(id)
      
      if (is.null(pot_poly))
      {return(function(x,y) 0)}
      
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
    #if a polygon not exist, we add a constant function (f(x,y) = 0) in the list
    get_potentials_ = function(ids = list(), case)
    {
      # verify if it is a list
      if (!is.list(ids))
      {
        return(-1)
      }

      # no asked ids so take all ids 
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
  #' @section methods:
  public = list(
    #' \itemize{
    
    #' \item{PotentialLandscape$new()}{
    #' \describe{initialize PotentialLandscape object
    #' \itemize{
    #' \item{land_poly }{(SpatialPolygonDataFrame): Typing polygon landscape}
    #' \item{land_lines }{(SpatialLinesDataFrame): Typing line landscape}
    #' \item{interaction_model }{(TypeInteractModel): interaction of each type
    #'                                                with others}
    #' }}}
    initialize = function(land_poly, land_lines, interaction_model)
    {
      #initialize  at empty PotentialPolygons
      super$initialize(list())
      if (is_TypeInteractModel(interaction_model))
      {
        private$interaction_model = interaction_model
        self$set_landscape(land_poly, land_lines)
      }
      else
      {
        stop("parameter #3 isn't a TypeInteractModel")
      }
    },
    #' \item{$get_landscape()}{
    #' \describe{Give the landscape
    #' \itemize{
    #' \item{(SpatialPolygonsDataFrame) }{Landscape}
    #' }}}
    get_landscape = function()
    {
      return(private$land_poly) 
    },
    #' \item{$get_interaction_model()}{
    #' \describe{Give the interaction_model
    #' \itemize{
    #' \item{(TypeInteractionModel) }{interaction model}
    #' }}}
    get_interaction_model = function()
    {
      return(private$interaction_model)
    },
    #' \item{$get_neighbours_id(id)}{
    #' \describe{Give neighboors of polygon
    #' \itemize{
    #' \item{id }{(str): identifiant of polygon}
    #' \item{(list) }{ids of polygon,
    #'                empty list if id isn't exist}
    #' }}}
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
    #' \item{$get_potential_coord(x, y)}{
    #' \describe{Give value of potential for a point
    #' \itemize{
    #' \item{x }{(float) : abscisse}
    #' \item{y }{(float) : ordonate}
    #' \item{(float) }{NA if coordinate (x,y) out of landscape}
    #' }}}
    get_potential_coord = function(x,y)
    {
      ids = private$which_polygon(x,y)
      
      if (is.null(ids))
      {
        return(NA)
      }
      # Make average of potential if coordinate between two polygons
      potential = 0
      for (id in ids)
      {
        potential = potential + self$get_potentials(list(id))[[1]](x,y)
      }
      
      return(potential/length(ids))
    },
    #' \item{$get_dx_coord(x, y)}{
    #' \describe{Give value of derivate in x for a point
    #' \itemize{
    #' \item{x }{(float) : abscisse}
    #' \item{y }{(float) : ordonate}
    #' \item{(float) }{NA if coordinate (x,y) out of landscape}
    #' }}}
    get_dx_coord = function(x,y)
    {
      ids = private$which_polygon(x,y)
      
      if (is.null(ids))
      {
        return(NA)
      }
      # Make average of dx if coordinate between two polygons
      dx = 0
      for (id in ids)
      {
        dx = dx + self$get_dxs(list(id))[[1]](x,y)
      }
      
      return(dx/length(ids))
    },
    #' \item{$get_dy_coord(x, y)}{
    #' \describe{Give value of derivate in y for a point
    #' \itemize{
    #' \item{x }{(float) : abscisse}
    #' \item{y }{(float) : ordonate}
    #' \item{(float) }{NA if coordinate (x,y) out of landscape}
    #' }}}
    get_dy_coord = function(x,y)
    {
      ids = private$which_polygon(x,y)
      
      if (is.null(ids))
      {
        return(NA)
      }
      # Make average of dy if coordinate between two polygons
      dy = 0
      for (id in ids)
      {
        dy = dy + self$get_dys(list(id))[[1]](x,y)
      }
      
      return(dy/length(ids))
    },
    #' \item{$set_landscape(land_poly, land_lines)}{
    #' \describe{Add or modify landscape and potential function of each polygon
    #'           calculated
    #' \itemize{
    #' \item{land_poly }{(SpatialPolygonDataFrame): Typing polygon landscape}
    #' \item{land_lines }{(SpatialLinesDataFrame): Typing line landscape}
    #' }}}
    set_landscape = function(land_poly, land_lines)
    {
      if (class(land_poly)[1] != "SpatialPolygonsDataFrame" )
      {
        stop("parameter #1 isn't a SpatialPolygonsDataFrame")
      }
      if (class(land_lines)[1] != "SpatialLinesDataFrame")
      {
        stop("parameter #2 isn't a SpatialLinesDataFrame")
      }
      private$land_poly = land_poly
      private$land_lines = land_lines
      private$neighbours = NULL
      # no neighbours when you are alone
      if (length(land_poly$id_type) > 1)
      {
        private$neighbours = get_neighbours(land_poly) 
      }
      # Recalculate potential of polygones
      private$calculate_potential()
    },
    #' \item{$set_interaction_model(interaction_model)}{
    #' \describe{Add or modify interaction_model
    #' \itemize{
    #' \item{interaction_model }{(TypeInteractModel): interaction of each type
    #'                                                with others}
    #' \item{(integer)}{ 0: success, 1: fail}
    #' }}}
    set_interaction_model = function(interaction_model)
    {
      # Verify if it is interaction model
      if (is_TypeInteractModel(interaction_model))
      {
        private$interaction_model = interaction_model
        # Recalculate potential with new interation model
        private$calculate_potential()
        return(0)
      }
      else
      {
        return(1)
      }
    },
    #' \item{$plot_potential(precision)}{
    #' \describe{Plot the landscape with potential values
    #' \itemize{
    #' \item{precision }{(float)}
    #' }}}
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
      centroid = coordinates(private$land_poly)
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
      legend(max_x,min_y,
             legend = c("no type","border",
                        sapply(1:(nb_type - 2),
                               private$interaction_model$get_name)),
             col = type_color, lty = 1,xpd = T, text.col = type_color)

      return(pot)
    }
  #' }
  ),
  private = list(
    land_poly = NULL,
    neighbours = NULL,
    land_lines = NULL,
    interaction_model = NULL,
    #Create potentiel function of each polycon
    calculate_potential = function()
    {
      # Take ids of all lines
      ids_lines = getIdsSpatialLines(private$land_lines)
      # Take ids of all polygones
      ids_poly = getIdsSpatialPolygons(private$land_poly)
      
      for (id_poly in ids_poly)
      {
        index = 1
        potent = list()
        
        # take index of all line of polygon
        keep_ids = ids_lines[c(which(private$land_lines$id_poly1 %in% id_poly),
                               which(private$land_lines$id_poly2 %in% id_poly))]
        
        # Impossible polygon without lines
        if (length(keep_ids) == 1 && is.na(keep_ids))
        {
          next
        }
        # take position of polygon in the list of polygones
        pos_poly = which(ids_poly %in% id_poly)
        # Take type of polygon
        type_poly = private$land_poly$id_type[pos_poly]
        
        for (id_line in keep_ids)
        {
          # Get all information on line
          # position
          pos_line = which(ids_lines %in% id_line) 
          # Type
          types = private$land_lines$id_type[pos_line]
          # And coordonate
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
