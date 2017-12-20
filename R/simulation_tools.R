#------- simulation_tools.R----------#
#
# Provide function to simulate animal moving in landscape.

#' @title Extract elements
#' @description Extract all element of typed landscape to create list of matrix
#'              containing coordonnate of element with parameter of potential
#'              function
#' 
#' @param land_poly (SpatialPolygonDataFrame): Typing polygon landscape
#' @param land_lines (SpatialLinesDataFrame): Typing line landscape
#' @param info_type (matrix): N*5 (id, repulsive=0 or attractive=1, alpha, beta,
#'                                 power) where N number of type.
#' 
#' @return (list of 2 matrix) 
#'            repulsive : M*7 (x1, y1, x2, y2, alpha, beta, power)
#'            attractive : M*7 (x1, y1, x2, y2, alpha, beta, power)
#' @export
extract_elements <- function(land_poly,
                             land_lines,
                             info_types)
{
  # Initialize 
  elements_land = list("repulsive" = c(),
                       "attractive" = c())

  # Extract elements from polygons
  element_poly = extract_element_(land_poly,
                                  info_types)
  # Extract elements from lines
  element_line = extract_element_(land_lines,
                                  info_types)

  # concatenate matrix
  elements_land$repulsive = rbind(element_poly$repulsive,
                                 element_line$repulsive)

  elements_land$attractive = rbind(element_poly$attractive,
                                   element_line$attractive)

  return(elements_land)
}

#' @title Extract elements (generic)
#' @description Extract all element of typed polygon or line to create list 
#'              of matrix containing coordonnate of element with parameter of 
#'              potential function
#' 
#' @param object (Spatial*DataFrame): Typing polygon or line landscape
#' @param info_type (matrix): N*5 (id, repulsive=0 or attractive=1, alpha, beta,
#'                                 power) where N number of type.
#' 
#' @return (matrix) M*7 (x1, y1, x2, y2, alpha, beta,
#'                       power)
extract_element_ <- function(object,
                             info_types)
{
  # Initialize list of matrix
  elements = list("repulsive" = c(),
                  "attractive" = c())

  # Deteminate if object is line or polygon
  if (class(object) == "SpatialPolygonsDataFrame")
  {
    info_object  = "Polygons"
    centroid = coordinates(object)
  }
  else
  {
    info_object  = "Lines"
  }
  
  # Take all ids
  ids = eval(parse(text = paste("getIdsSpatial",info_object,"(object)",
                                sep = "")))
  for (index_obj in 1:length(ids))
  {
    # Get id of type
    id_type = object@data$id_type[index_obj]

    # We don't process border (id_type = 0) or no type (id_type = -1)
    if (id_type == 0 || id_type == -1)
    {
      next()
    }
    
    if (info_object == "Lines")
    {
      # Get all coordonnate of polygon
      coords = getCoordsSpatialLines(object, ids[index_obj])
    }
    else
    {
      coords = rbind(centroid[index_obj,], centroid[index_obj,])
    }
    # Get all info on type
    info_type = info_types[which(info_types[,1] %in% id_type),] 
    # Indicate if type is repulsive or attractive effect
    effect = "repulsive"
    if (info_type[2] != 0)
    {
      effect = "attractive"
    }

    # Add each element: coordonate and parameter for potential function
    for (index_coord in 1:(length(coords[,1]) - 1))
    {
      elements[[effect]] = c(elements[[effect]], 
                             # x1 , y1
                             coords[index_coord,1], coords[index_coord,2],
                             # x2, x2
                             coords[index_coord + 1, 1], coords[index_coord + 1 ,2],
                             # alpha, beta, power
                             info_type[3], info_type[4], info_type[5])
    }
  }

  # transform in matrix
  # Impossible to tranform empty list to matrix N*7
  if (length(elements[["repulsive"]]) > 0)
  {
    elements[["repulsive"]] = matrix(elements[["repulsive"]],
                                     ncol = 7, byrow = TRUE)
  }    
  # Impossible to tranform empty list to matrix N*7
  if (length(elements[["attractive"]]) > 0)
  {
    elements[["attractive"]] = matrix(elements[["attractive"]],
                                      ncol = 7, byrow = TRUE)
  }    
  return(elements)
}

#' @title plot potential
#' @description plot potential on landscape
#' 
#' @param landscape (SpatialPolygon): landscape
#' @param land_component (list of 2 matrix) 
#'            repulsive : M*7 (x1, y1, x2, y2, alpha, beta, power)
#'            attractive : M*7 (x1, y1, x2, y2, alpha, beta, power)
#' @param precision (float) plot precision
#' @export
plot_potential <- function(landscape,
                           land_component,
                           precision = 1)
{
  #Take extrem value of landscape
  min_x = attr(landscape,"bbox")[1,1]
  max_x = attr(landscape,"bbox")[1,2]
  min_y = attr(landscape,"bbox")[2,1]
  max_y = attr(landscape,"bbox")[2,2]

  #calculate potential of each point
  pot = c()
  for (x in seq(min_x,max_x,precision))
  {
    row_pot = c()
    for (y in seq(min_y,max_y,precision))
    {
      row_pot = c(all_value(c(x, y),
                            land_component$attractive,
                            land_component$repulsive)
                  ,row_pot)
    }
    #add new colunm
    pot = cbind(pot,row_pot)
  }
  
  #show matrix of potentials
  plot(raster(pot, xmn = min_x, xmx = max_x, ymn = min_y, ymx = max_y),
       xlim = c(min_x, max_x), ylim = c(min_y, max_y))
  # Add number of type of polygon with color of type
  centroid = coordinates(landscape)
  text(centroid, labels = landscape$id_type)
}
