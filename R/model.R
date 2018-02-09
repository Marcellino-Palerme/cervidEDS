#------- model.R----------#
#
# definir les fonction d'attarativite replusion des type

# require("R6")

#number of type
NB_TYPE = 5

AGGLO = "agglo_"
GEN_POT = "generic_potentiel"

#' @title Distance expression between two points
#' @description Give expresssion of calcul of distance between two points
#'
#' @param x0 (float or characters): abscisse of first point of segment
#' @param y0 (float or characters): ordonate of first point of segment
#' @param x1 (float or characters): abscisse of second point of segment
#' @param y1 (float or characters): ordonate of second point of segment
#'
#' @return string 
#' @export
dist_expr_two_points <- function(x0, y0, x1, y1)
{
  return(paste("(sqrt(((", x1, " - ",x0,")^2) + ((", y1, " - ",y0,")^2)))"))
}

#' @title Distance expression
#' @description Give expresssion of calcul of distance between segment and point
#'
#' @param x0 (float): abscisse of first point of segment
#' @param y0 (float): ordonate of first point of segment
#' @param x1 (float): abscisse of second point of segment
#' @param y1 (float): ordonate of second point of segment
#'
#' @return string 
#' @export
dist_expr <- function(x0, y0, x1, y1)
{
  #(x0,y0) and (x1, y1) is same points 
  # Calcute distance between two points
  if (x0 == x1 && y0 == y1)
  {
    return(dist_expr_two_points(x0, y0, "x", "y" ))
  }
  # segment is vertical
  if (x0 == x1)
  {
    # Define coordinate orthogonal projection of point on line segment
    Xh = as.character(x0)
    Yh = "y"
  }
  else
  {
    # segment is horizontal
    if (y0 == y1)
    {
      # Define coordinate orthogonal projection of point on line segment
      Xh = "x"
      Yh = as.character(y0)
    }
    else
    {
      # expression slot of segment
      alpha = paste("((",y1,"-",y0,") / (",x1,"-",x0,"))")
      
      # expression slot of line create by point and orthogonal projection of 
      # point on line segment
      beta = paste("(-1/", alpha,")")
      
      # expression vertical intercept of line segment
      bs = paste("(",y0,"-",alpha,"*",x0,")")
      
      # expression vertical intercept of lline create by point and orthogonal 
      # projection of point on line segment
      bp = paste("( y -",beta,"* x )")
      
      # expression coordinate of orthogonal projection of point on line segment
      Xh = paste("((",bp,"-",bs,")/(",alpha,"-",beta,"))")
      Yh = paste("((",alpha,"*",Xh,")+",bs,")")
    }
  }
  
  # expression distance between two point of segment
  seg = dist_expr_two_points(x0, y0, x1, y1)
  
  # expression of distance between first point ofsegment and orthogonal 
  # projection of point on line segment
  h2first = dist_expr_two_points(x0, y0, Xh, Yh)

  # expression of distance between second point ofsegment and orthogonal 
  # projection of point on line segment
  h2snd = dist_expr_two_points(x1, y1, Xh, Yh)  

  # expression of smaller distance between segment and orthogonal projection of 
  # point on line segment
  h2seg = paste("((",h2first,"+",h2snd,"-",seg,")/2)")
  
  # expression of distance between point and orthogonal projection of 
  # point on line segment
  h2p = dist_expr_two_points("x", "y", Xh, Yh) 
  
  # expression of square smaller distance between segment and point
  square_dist = paste("((",h2seg,"^2)+(",h2p,"^2))")

  return(paste("sqrt(",square_dist,")"))
}

#' @title potentiel function define by user
#' @description exponentiel decroissante
#'
#'
#' @param a (float): parameter of function
#' @param b (float): parameter of function
#' @param b (float): parameter of function
#' @param x0 (float): abscisse of first point of line
#' @param y0 (float): ordonate of first point of line
#' @param x1 (float): abscisse of second point of line
#' @param y1 (float): ordonate of second point of line
#'
#' @return (string): function expression
#' @export
potentiel_0 <- function(a, b, c, x0, y0, x1, y1)
{
  dist = dist_expr(x0, y0, x1, y1)
  return(paste("(",a,"*exp(-1*","(",dist,")^2","*",b,") + ",c,")"))
}

#' @title agglomeration function define by user
#' @description sum 
#' @param lt_expr(list of string)
#' @return string
#' @export
agglo_0 <- function(lt_expr)
{
  my_expr = ""
  for (expr in lt_expr)
  {
    my_expr = paste(my_expr, "+", expr)
  }
  my_expr = substr(my_expr,3, nchar(my_expr))
  return(my_expr)
}

#' @title Interact class
#' @description Define interaction of type on other
#' 
#' @export
Interact <- R6::R6Class("Interact",
   #' @section methods:
   public = list(
     #' \itemize{

     #' \item{Interact$new(id_interact, func_interact, params)}{
     #' \describe{Create Interact object
     #' \itemize{
     #' \item{id_interact}{(integer) id type interact with other}
     #' \item{func_interact}{(function) interaction function}
     #' \item{params}{(list) list of constant parameters of function}
     #' }}}
      initialize = function(id_interact, func_interact, params)
      {
        if (self$set_id(id_interact) > 0 )
        {
          stop("id_interact isn't a numeric")
        }
        if (self$set_func_interact(func_interact) > 0)
        {
          stop("func_interact isn't a function")
        }
        if (self$set_params(params) > 0)
        {
          stop("params isn't a list")
        }
      
      },
      
      #' \item{$get_id()}{
      #' \describe{give id type interact with other
      #' \itemize{
      #' \item{(integer)}{id type interact with other}
      #' }}}
      get_id = function()
      {
        return(private$id)
      },
      #' \item{$get_func_interact()}{
      #' \describe{give id type interact with other
      #' \itemize{
      #' \item{(function)}{interaction function}
      #' }}}
      get_func_interact = function()
      {
        return(private$func_interact)
      },
      #' \item{$get_params()}{
      #' \describe{give id type interact with other
      #' \itemize{
      #' \item{(list)}{list of constant parameters of function}
      #' }}}
      get_params = function()
      {
        return(private$params)
      },
      #' \item{$set_id(id)}{
      #' \describe{Affect id_interact
      #' \itemize{
      #' \item{id}{(integer) id type interact with other}
      #' \item{(integer)}{ 0: success, 1: fail}
      #' }}}
      set_id = function(id)
      {
         if (is.numeric(id))
         {
            private$id = id
            return(0)
         }
        return(1)

      },
      #' \item{$set_func_interact(func_interact)}{
      #' \describe{Define Interact function
      #' \itemize{
      #' \item{func_interact}{(function) interaction function}
      #' \item{(integer)}{ 0: success, 1: fail}
      #' }}}
      set_func_interact = function(func_interact)
      {
        if (is.function(func_interact))
        {
          private$func_interact = func_interact
          return(0)
        }
        return(1)
      },
      #' \item{$set_params(params)}{
      #' \describe{Define parameters for the Interact function
      #' \itemize{
      #' \item{params}{(list) list of constant parameters of function}
      #' }}}
      set_params = function(params)
      {
        if (is.list(params))
        {
          private$params = params
          return(0)
        }
        return(1)
      }
      #' }
 ),
 private = list(
   id = 0,
   func_interact = NULL,
   params = 0
 )
)

#Indicate if element is a Interact
is_Interact = function(x)
{
  return(class(x)[1] == "Interact")
}

#' @title TypeInteract class
#' @description  Define interaction of several neighbor type on host type
#' @export
TypeInteract = R6::R6Class("TypeInteract",
   #' @section methods:
   public = list(
     #' \itemize{
     
     #' \item{TypeInteract$new(id, name, func_agglo, interacts)}{
     #' \describe{Create TypeInteract object
     #' \itemize{
     #' \item{id}{(integer) id of host type}
     #' \item{name}{(character) name of type }
     #' \item{func_agglo}{(function) function define how aggregate interactions}
     #' \item{interacts}{(list of Interact) list of interaction of type with
     #'                  other}
     #' }}}
     initialize = function(id, name, func_agglo, interacts)
     {
       # Affect id
       if (self$set_id(id) > 0)
       {
         stop("id isn't a numeric")
       }
       # Affect name
       if (self$set_name(name) > 0)
       {
         stop("name isn't characters")
       }
       # Affect a agglomeration function
       if (self$set_func_agglo(func_agglo) > 0)
       {
         stop("func_agglo isn't a function")
       }
       # Affect interacts
       for (interact in interacts)
       {
         self$set_interact(interact)
       }
     },
     #' \item{$get_id()}{
     #' \describe{Give identifiant of host type 
     #' \itemize{
     #' \item{(integer)}{ id of host type }
     #' }}}
     get_id = function()
     {
       return(private$id)
     }, 
     #' \item{$get_name()}{
     #' \describe{Give name of host type
     #' \itemize{
     #' \item{(character)}{ name of host type }
     #' }}}
     get_name = function()
     {
       return(private$name)
     }, 
     #' \item{$get_func_agglo()}{
     #' \describe{Give function of aggregate
     #' \itemize{
     #' \item{(function)}{ function define how aggregate interactions}
     #' }}}
     get_func_agglo = function()
     {
       return(private$func_agglo)
     },
     #' \item{$get_interact(id)}{
     #' \describe{Give a interaction of neighbor type on host type
     #' \itemize{
     #' \item{id}{ (integer) id neighbor type}
     #' \item{(Interact)}{ interaction}
     #' }}}
     get_interact = function(id)
     {
       # verify if Interact with this type exist
       index = private$get_index_interact(id)
       if (index > 0)
       {
         return(private$interacts[[index]])
       }
       else
       {
         return(NULL)
       }
     },
     #' \item{$get_id_interacts()}{
     #' \describe{Give the list of id of neighbor type in interaction
     #' \itemize{
     #' \item{(list of integer)}{ list of id neighbor type}
     #' }}}
     get_id_interacts = function()
     {
       lt_id = list()
       
       for (interact in private$interacts)
       {
         lt_id = append(lt_id, interact$get_id())
       }
       
       return(lt_id)
     },
     #' \item{$get_func_interact(id)}{
     #' \describe{Give interaction function 
     #' \itemize{
     #' \item{id}{ (integer) id neighbor type}
     #' \item{(function)}{ interaction function}
     #' }}}
     get_func_interact = function(id)
     {
       # verify if Interact with this type exist
       index = private$get_index_interact(id)
       if (index > 0)
       {
         # Take interaction function
         return(private$interacts[[index]]$get_func_interact())
       }
       else
       {
         return(NULL)
       }
     },
     #' \item{$get_params(id)}{
     #' \describe{Give parameters of interaction function
     #' \itemize{
     #' \item{id}{ (integer) id neighbor type}
     #' \item{(list)}{ list of constant parameters of function}
     #' }}}
     get_params = function(id)
     {
       # verify if Interact with this type exist
       index = private$get_index_interact(id)
       if (index > 0)
       {
         # Take interaction function
         return(private$interacts[[index]]$get_params())
       }
       else
       {
         return(NULL)
       }
     },
     #' \item{$set_id(id)}{
     #' \describe{Define identifiant of type
     #' \itemize{
     #' \item{id}{(integer) id of host type }
     #' \item{(integer)}{ 0: success, 1: fail}
     #' }}}
     set_id = function(id)
     {
       if (is.numeric(id))
       {
         private$id = id
         return(0)
       }
       return(1)
     }, 
     #' \item{$set_name(name)}{
     #' \describe{Define name of host type
     #' \itemize{
     #' \item{name}{(character) name of host type }
     #' \item{(integer)}{ 0: success, 1: fail}
     #' }}}
     set_name = function(name)
     {
       if (is.character(name))
       {
         private$name = name
         return(0)
       }
       return(1)
     }, 
     #' \item{$set_func_agglo(func_agglo)}{
     #' \describe{Define function of aggregate
     #' \itemize{
     #' \item{func_agglo}{(function) function define how aggregate interactions}
     #' \item{(integer)}{ 0: success, 1: fail}
     #' }}}
     set_func_agglo = function(func_agglo)
     {
       if (is.function(func_agglo))
       {
         private$func_agglo = func_agglo
         return(0)
       }
       return(1)
     },
     #' \item{$set_interact(interact)}{
     #' \describe{Add or modify a interaction of neighbor type on host type
     #' \itemize{
     #' \item{interact}{(Interact) interaction of neighbor type}
     #' \item{(integer)}{ 0: success, 1: fail}
     #' }}}
     set_interact = function(interact)
     {
       if (is_Interact(interact))
       {
         #take id
         id_interact = interact$get_id()
         
         # verify if Interact with this type already exist
         index = private$get_index_interact(id_interact)
         if (index > 0)
         {
           # modify existing Interact
           private$interacts[[index]] = interact
         }
         else
         {
           # create the new Interact
           private$interacts = append(private$interacts, interact)
         }
         return(0)
       }
       return(1)
     },
     #' \item{$set_func_interact(id, func)}{
     #' \describe{Modify interaction function of one interaction
     #' \itemize{
     #' \item{id}{ (integer) id neighbor type}
     #' \item{func}{ (function) interaction function}
     #' \item{(integer)}{ 0: success, 1: fail}
     #' }}}
     set_func_interact = function(id, func)
     {
       # verify if Interact with this type exist
       index = private$get_index_interact(id)
       if (index > 0)
       {
         # Take interaction function
         return(private$interacts[[index]]$set_func_interact(func))
       }
       else
       {
         return(1)
       }
     },
     #' \item{$set_params(id, params)}{
     #' \describe{Modify parameters of interaction function of one interaction
     #' \itemize{
     #' \item{id}{ (integer) id neighbor type}
     #' \item{params}{ (list) list of constant parameters of function}
     #' \item{(integer)}{ 0: success, 1: fail}
     #' }}}
     set_params = function(id, params)
     {
       # verify if Interact with this type exist
       index = private$get_index_interact(id)
       if (index > 0)
       {
         # Take interaction function
         return(private$interacts[[index]]$set_params(params))
       }
       else
       {
         return(1)
       }
     },
     #' \item{$remove_interact(id)}{
     #' \describe{remove a interaction of neighbor typer
     #' \itemize{
     #' \item{id}{ (integer) id neighbor type}
     #' \item{(integer)}{ 0: success, 1: fail}
     #' }}}
     remove_interact = function(id)
     {
       # verify if Interact with this type exist
       index = private$get_index_interact(id)
       if (index > 0)
       {
         # remove the Interact
         private$interacts = private$interacts[-index]
         return(0)
       }
       return(1)
     }
     #' }
   ),
   private = list(
     # host type id
     id = "",
     # host name
     name = "",
     # agglomeration function
     func_agglo = "",
     # list of interactions on host
     interacts = NULL,

     # Get index of interact with the  id_interact equal to id in list 
     get_index_interact = function(id)
     {
       index = 0
       repeat
       {
         index = index + 1
         if (index > length(private$interacts))
         {
           index = 0
           break
         }
         if (private$interacts[[index]]$get_id() == id)
         {
           break
         }
       }
       
       return(index)
     }
   )
  
)

#Verify if is a TypeInteract
is_TypeInteract = function(x)
{
  return(class(x)[1] == "TypeInteract")
}

#----------TypeInteractModel class-------------#
#' @title TypeInteractModel class
#' @description Define interaction of all types with others
#' @export 
TypeInteractModel = R6::R6Class("TypeInteractModel",
  #' @section methods:
  public = list(
    #' \itemize{
    
    #' \item{TypeInteractModel$new(typeinteracts)}{
    #' \describe{Create typeinteracts object
    #' \itemize{
    #' \item{typeinteracts}{(list of TypeInteract) list of TypeInteract }
    #' }}}
    initialize = function(typeinteracts)
    {
      for (typeinterat in typeinteracts)
      {
        self$set_type_interact(typeinterat)
      }
    },
    #' \item{$get_model()}{
    #' \describe{Give model of interaction
    #' \itemize{
    #' \item{(list of TypeInteract)}{ list of TypeInteract }
    #' }}}
    get_model = function()
    {
      return(private$model)
    },
    #' \item{$get_id_typeinteract()}{
    #' \describe{Give id of all host type
    #' \itemize{
    #' \item{(list of integer)}{ list of all host type}
    #' }}}
    get_id_typeinteract = function()
    {
      lt_id = list()
      for (type_interact in private$model)
      {
        lt_id = append(lt_id, type_interact$get_id())
      }
    },
    #' \item{$get_typeinteract(id)}{
    #' \describe{Give TypeInteract for one host type
    #' \itemize{
    #' \item{id}{ (integer) id of host type}
    #' \item{(TypeInteract)}{ TypeInteract of host type}
    #' }}}
    get_typeinteract = function(id)
    {
      index = private$get_index_typeinteract(id)
      
      if (index > 0)
      {
        return(private$model[[index]])
      }
      else
      {
        return(NULL)
      }
    },
    #' \item{$get_id_interacts(id)}{
    #' \describe{Give the list of id of neighbor type of a host type
    #' \itemize{
    #' \item{id}{ (integer) id of host type}
    #' \item{(list of integer)}{ list of id of neighbor type of a host type}
    #' }}}
    get_id_interacts = function(id)
    {
      index = private$get_index_typeinteract(id)
      
      if (index > 0)
      {
        return(private$model[[index]]$get_id_interacts())
      }
      else
      {
        return(NULL)
      }
    },
    #' \item{$get_name(id)}{
    #' \describe{Give name of host type
    #' \itemize{
    #' \item{id}{ (integer) id of host type}
    #' \item{(character)}{ name of host type}
    #' }}}
    get_name = function(id)
    {
      type_interact = self$get_typeinteract(id)
      
      if (is.null(type_interact))
      {
        return(NULL)
      }
      else
      {
        return(type_interact$get_name())
      }
    },
    #' \item{$get_interact(id_host, id_neighbor)}{
    #' \describe{Give interact of type neighbor on type host
    #' \itemize{
    #' \item{id_host}{ (integer) id of type host}
    #' \item{id_neighbor}{ (integer) id of type neighbor}
    #' \item{(Interact)}{ Interaction B on A}
    #' }}}
    get_interact = function(id_host, id_neighbor)
    {
      type_interact = self$get_typeinteract(id_host)
      
      if (is.null(type_interact))
      {
        return(NULL)
      }
      else
      {
        return(type_interact$get_interact(id_neighbor))
      }
    },
    #' \item{$get_typeinteract(id)}{
    #' \describe{Give agglomeration function of host type 
    #' \itemize{
    #' \item{id}{ (integer) id of host type}
    #' \item{(function)}{ function of agglomeration}
    #' }}}
    get_func_agglo = function(id)
    {
      type_interact = self$get_typeinteract(id)
      
      if (is.null(type_interact))
      {
        return(NULL)
      }
      else
      {
        return(type_interact$get_func_agglo())
      }
    },
    #' \item{$get_func_interact(id_host, id_neighbor)}{
    #' \describe{Give interaction function of type neighbor on type host
    #' \itemize{
    #' \item{id_host}{ (integer) id of type host}
    #' \item{id_neighbor}{ (integer) id of type neighbor}
    #' \item{(function)}{ interaction function of type neighbor on type host}
    #' }}}
    get_func_interact = function(id_host, id_neighbor)
    {
      interact = self$get_interact(id_host, id_neighbor)
      
      if (is.null(interact))
      {
        return(NULL)
      }
      else
      {
        return(interact$get_func_interact())
      }
    },
    #' \item{$get_params(id_host, id_neighbor)}{
    #' \describe{Give parameters of interaction function of type neighbor on 
    #'           type host
    #' \itemize{
    #' \item{id_host}{ (integer) id of type host}
    #' \item{id_neighbor}{ (integer) id of type neighbor}
    #' \item{(list)}{ list of constant parameters of intraction function of 
    #'                type neighbor on type host}
    #' }}}
    get_params = function(id_host, id_neighbor)
    {
      interact = self$get_interact(id_host, id_neighbor)
      
      if (is.null(interact))
      {
        return(NULL)
      }
      else
      {
        return(interact$get_params())
      }
    },
    #' \item{$set_type_interact(typeinterat)}{
    #' \describe{Add or modify a TypeInteract to model
    #' \itemize{
    #' \item{typeinterat}{ (TypeInteract) Interact of other neighbor type on 
    #'                     one host}
    #' \item{(integer)}{ 0: success, 1: fail}
    #' }}}
    set_type_interact = function(typeinterat)
    {
      if (is_TypeInteract(typeinterat))
      {
        id = typeinterat$get_id()
        # verify if typeInteract with this id already exist 
        index = private$get_index_typeinteract(id)
        if (index > 0)
        {
          #modify exiting type
          private$model[[index]] = typeinterat
        }
        else
        {
          #add new type
          private$model = append(private$model, typeinterat)
        }
        return(0)
      }
      return(1)
    },
    #' \item{$set_func_agglo(id, func_agglo)}{
    #' \describe{modify agglomeration function for one host type
    #' \itemize{
    #' \item{id}{ (integer) identifiant of host type}
    #' \item{func_agglo}{ (function) function of agglomeration}
    #' }}}
    set_func_agglo = function(id, func_agglo)
    {
      # verify if typeInteract exist 
      index = private$get_index_typeinteract(id)
      if (index > 0)
      {
        #verify we have a fucntion
        if (is.function(func_agglo) )
        {
          private$model[[index]]$set_func_agglo(func_agglo)
        }
      }
    },
    #' \item{$set_interact(id, interact)}{
    #' \describe{Add or modify a Interact for one host type
    #' \itemize{
    #' \item{id}{(integer) identifiant of host type}
    #' \item{interact}{(Interact) interaction}
    #' }}}
    set_interact = function(id, interact)
    {
      # verify if typeInteract exist 
      index = private$get_index_typeinteract(id)
      if (index > 0)
      {
        #verify we have a Interact
        if (is_Interact(interact) )
        {
          private$model[[index]]$set_interact(interact)
        }
      }
    },
    #' \item{$set_func_interact(id_host, id_neighbor, func)}{
    #' \describe{Modify interaction function of type neighbor on type host
    #' \itemize{
    #' \item{id_host}{(integer) identifiant of type host}
    #' \item{id_neighbor}{(integer) identifiant of type neighbor}
    #' \item{func}{(function) interaction function}
    #' \item{(integer)}{ 0: success, 1: fail}
    #' }}}
    set_func_interact = function(id_host, id_neighbor, func)
    {
      # verify if typeInteract exist 
      index = private$get_index_typeinteract(id_host)
      if (index > 0)
      {
        return(private$model[[index]]$set_func_interact(id_neighbor, func))
      }
      return(1)
    },
    #' \item{$set_params(id_host, id_neighbor, params)}{
    #' \describe{Modify parameters of interaction function of type neighbor on
    #'           type host
    #' \itemize{
    #' \item{id_host}{(integer) identifiant of type host}
    #' \item{id_neighbor}{(integer) identifiant of type neighbor}
    #' \item{params}{(list) parameters of interaction function}
    #' \item{(integer)}{ 0: success, 1: fail}
    #' }}}
    set_params = function(id_host, id_neighbor, params)
    {
      # verify if typeInteract exist 
      index = private$get_index_typeinteract(id_host)
      if (index > 0)
      {
        return(private$model[[index]]$set_params(id_neighbor, params))
      }
      return(1)
    },
    #' \item{$remove_type_interact(id)}{
    #' \describe{remove all interction of neighbor type on host type
    #' \itemize{
    #' \item{id}{(integer) identifiant of host type}
    #' \item{(integer)}{ 0: success, 1: fail}
    #' }}}
    remove_type_interact = function(id)
    {
      # verify if Interact with this type exist
      index = private$get_index_typeinteract(id)
      if (index > 0)
      {
        # remove the Interact
        private$model = private$model[[-index]]
        return(0)
      }
      return(1)
    },
    #' \item{$remove_interact(id_host, id_neighbor)}{
    #' \describe{Remove a interaction of type neighbor on type host
    #' \itemize{
    #' \item{id_host}{(integer) identifiant of type host}
    #' \item{id_neighbor}{(integer) identifiant of type neighbor}
    #' \item{(integer)}{ 0: success, 1: fail}
    #' }}}
    remove_interact = function(id_host, id_neighbor)
    {
      # verify if typeInteract exist 
      index = private$get_index_typeinteract(id_host)
      if (index > 0)
      {
        return(private$model[[index]]$remove_interact(id_neighbor))
      }
      return(1)
    }
    #' }
  ),
  private = list(
    model = list(),
    
    # Get index of TypeInteract with the  id equal to id in list 
    get_index_typeinteract = function(id)
    {
      index = 0
      repeat
      {
        index = index + 1
        if (index > length(private$model))
        {
          index = 0
          break
        }
        if (private$model[[index]]$get_id() == id)
        {
          break
        }
      }
      
      return(index)
    }
  )
)

#Indicate if element is a TypeInteractModel
is_TypeInteractModel = function(x)
{
  return(class(x)[1] == "TypeInteractModel")
}