#------- model.R----------#
#
# definir les fonction d'attarativite replusion des type

# require("R6")

#number of type
NB_TYPE = 5

AGGLO = "agglo_"
GEN_POT = "generic_potentiel"



#' @title Distance expression
#' @description Give expresssion of calcul of distance between line and point
#'
#' @param x0 (float): abscisse of first point of line
#' @param y0 (float): ordonate of first point of line
#' @param x1 (float): abscisse of second point of line
#' @param y1 (float): ordonate of second point of line
#'
#' @return string 
#' @export
dist_expr <- function(x0, y0, x1, y1)
{
  #(x0,y0) and (x1, y1) is same points 
  # Calcute distance between two points
  if (x0 == x1 && y0 == y1)
  {
    return(paste("sqrt((", x0, " - x)^2 + (", y0, " - y)^2)"))
  }
  #decompose expression
  a <- paste("(",y1,"-",y0,")*x")
  b <- paste("(",x1, "-", x0,")*y")
  c <- paste("(",x1,"*",y0," - ",y1,"*",x0,")")
  d <- paste("sqrt((",y1,"-",y0,")^2 + (", x1, "-", x0, ")^2)")

  return(paste("(sqrt((",a, "-",b,"+",c,")^2) / ",d,")"))
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
#' @description Define Interact of type(undefined) with a other
#' (defined : id_interact)
#' 
#' @export
Interact <- R6::R6Class("Interact",
   public = list(
      initialize = function(id_interact, func_interact, params)
      {
        self$set_id(id_interact)
        self$set_func_interact(func_interact)
        self$set_params(params)
      
      },
      #give id type of with we interact
      #return
      #(integer)
      get_id = function()
      {
        return(private$id)
      },
      #Give the Interact function
      #return
      #(function)
      get_func_interact = function()
      {
        return(private$func_interact)
      },
      #Give the parameters of Interact function
      #return
      #(list)
      get_params = function(x)
      {
        return(private$params)
      },
      #Affect id_interact
      #Parameter
      #id_interact: (int) id type of with we interact
      set_id = function(id)
      {
         if (is.numeric(id))
         {
            private$id = id
         }
      },
      #Define Interact function
      #Parameter
      #func_interact: (function) Interact function (return equation)
      set_func_interact = function(func_interact)
      {
        if (is.function(func_interact))
        {
          private$func_interact = func_interact
          return(0)
        }
        return(1)
      },
      #Define parameters for the Interact function
      #parameter
      #params: (list) list of parameters
      set_params = function(params)
      {
        private$params = params
  
      }
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

#----------TypeInteract class-------------#
#Define Interact of type with others
TypeInteract = R6::R6Class("TypeInteract",
   public = list(
     initialize = function(id, func_agglo, interacts)
     {
       self$set_id(id)
       self$set_func_agglo(func_agglo)
       for (interact in interacts)
       {
         self$set_interact(interact)
       }
     },
     #Give identifiant of type 
     #return
     #id: (int) identifiant
     get_id = function()
     {
       return(private$id)
     }, 
     #Give function of agglomeration, how agglomerate interaction of
     #different types 'around' this type 
     #return
     #func_agglo: (function) function of agglomeration
     get_func_agglo = function()
     {
       return(private$func_agglo)
     },
     #Give a interaction of type with other
     #Parameter
     #id: (str) id type of with we interact
     #return 
     #interact: (Interact)
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
     #Give the list of id of type in interaction 
     #Return
     #list of str
     get_id_interacts = function()
     {
       lt_id = list()
       
       for (interact in private$interacts)
       {
         lt_id = append(lt_id, interact$get_id())
       }
       
       return(lt_id)
     },
     #Give interaction function 
     #Parameter
     # id : (str) identifiant of type
     #Return
     # Function
     get_function_interact = function(id)
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
     #Give parameters of interaction function 
     #Parameter
     # id : (str) identifiant of type
     #Return
     # list
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
     #Define identifiant of type 
     #Parameter
     #id: (int) identifiant
     set_id = function(id)
     {
       if (is.numeric(id))
       {
         private$id = id
       }
     }, 
     #Define function of agglomeration, how agglomerate interaction of
     #different types 'around' this type 
     #Parameter
     #func_agglo: (function) function of agglomeration
     set_func_agglo = function(func_agglo)
     {
       if (is.function(func_agglo))
       {
         private$func_agglo = func_agglo
       }
     },
     #Add or modify a interaction of type with other
     #Parameter
     #interact: (Interact)
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
     #Modify interaction function of one Interact
     #Parameters
     # id : (str) identifiant of type
     # func : (function) new interaction function
     #Return
     #(int) 0 : success, 1 : fail
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
     #remove a interaction of type with other
     #Parameter
     #id: (int) id type of with we interact
     remove_interact = function(id)
     {
       # verify if Interact with this type exist
       index = private$get_index_interact(id)
       if (index > 0)
       {
         # remove the Interact
         private$interacts = private$interacts[[-index]]
         return(0)
       }
       return(1)
     }
   ),
   private = list(
     id = "",
     func_agglo = "",
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
#Define interaction of all types with others
TypeInteractModel = R6::R6Class("TypeInteractModel",
  public = list(
    initialize = function(typeinteracts)
    {
      for (typeinterat in typeinteracts)
      {
        self$set_type_interact(typeinterat)
      }
    },
    #Give model
    #Return
    #list of TypeInteraction
    get_model = function()
    {
      return(private$model)
    },
    #Give interacted type
    #Return
    #list of str
    get_id_typeinteract = function()
    {
      lt_id = list()
      for (type_interact in private$model)
      {
        lt_id = append(lt_id, type_interact$get_id())
      }
    },
    #Give TypeInteract for one type
    #Parameter
    #id : (str) identifiant of type
    #Return
    #(TypeInteract)
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
    #Give the list of id of type in interaction 
    #Parameter
    #id : (str) identifiant of type
    #Return
    #list of str
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
    #Give interact of type B on type A
    #Parameter
    #id_a : (str) identifiant of type A
    #id_b : (str) identifiant of type B
    #Return
    #(Interact)
    get_interact = function(id_a, id_b)
    {
      type_interact = self$get_typeinteract(id_a)
      
      if (is.null(type_interact))
      {
        return(NULL)
      }
      else
      {
        return(type_interact$get_interact(id_b))
      }
    },
    #Give agglomeration function of type 
    #Parameter
    #id : (str) identifiant of type 
    #Return
    #(function)
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
    #Give interaction function of type B on type A
    #Parameter
    #id_a : (str) identifiant of type A
    #id_b : (str) identifiant of type B
    #Return
    #(function)
    get_func_interact = function(id_a, id_b)
    {
      interact = self$get_interact(id_a, id_b)
      
      if (is.null(interact))
      {
        return(NULL)
      }
      else
      {
        return(interact$get_func_interact())
      }
    },
    #Give parameters of interaction function of type B on type A
    #Parameter
    #id_a : (str) identifiant of type A
    #id_b : (str) identifiant of type B
    #Return
    #(list)
    get_params = function(id_a, id_b)
    {
      interact = self$get_interact(id_a, id_b)
      
      if (is.null(interact))
      {
        return(NULL)
      }
      else
      {
        return(interact$get_params())
      }
    },
    #Add or modify TypeInteract to model
    #Parameter
    #typeinteract (TypeInteract)
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
    #modify agglomeration function for one type
    #Parameters
    #id : (str) identifiant of type
    #func_agglo: (function) function of agglomeration
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
    #Add or modify a Interact for one type
    #Parameters
    #id : (str) identifiant of type
    #interact: (Interact) interaction 
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
    #Modify interaction function of type B on type A
    #Parameter
    #id_a : (str) identifiant of type A
    #id_b : (str) identifiant of type B
    #func : (function) interaction function
    set_func_interact = function(id_a, id_b, func)
    {
      # verify if typeInteract exist 
      index = private$get_index_typeinteract(id_a)
      if (index > 0)
      {
        return(private$model[[index]]$set_func_interact(id_b, func))
      }
      return(1)
    },
    #Modify parameters of interaction function of type B on type A
    #Parameter
    #id_a : (str) identifiant of type A
    #id_b : (str) identifiant of type B
    #params : (list) parameters of interaction function
    set_params = function(id_a, id_b, params)
    {
      # verify if typeInteract exist 
      index = private$get_index_typeinteract(id_a)
      if (index > 0)
      {
        return(private$model[[index]]$set_params(id_b, params))
      }
      return(1)
    },
    #remove a interction of type with other
    #Parameter
    #id: (int) id type of with we interact
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
    #Remove a interaction of type B on type A
    #Parameter
    #id_a : (str) identifiant of type A
    #id_b : (str) identifiant of type B
    remove_interact = function(id_a, id_b)
    {
      # verify if typeInteract exist 
      index = private$get_index_typeinteract(id_a)
      if (index > 0)
      {
        return(private$model[[index]]$remove_interact(id_b))
      }
      return(1)
    }
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