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
#' 
#' @return (matrix) M*5 (x1, y1, x2, y2, id_type)
#' @export
extract_elements <- function(land_poly,
                             land_lines)
{
  # Initialize 
  elements_land <- c()

  # Extract elements from polygons
  element_poly <- extract_element_(land_poly)
  # Extract elements from lines
  element_line <- extract_element_(land_lines)

  # concatenate matrix
  elements_land <- rbind(element_poly,
                         element_line)

  return(elements_land)
}

#' @title Extract elements (generic)
#' @description Extract all element of typed polygon or line to create list 
#'              of matrix containing coordonnate of element with parameter of 
#'              potential function
#' 
#' @param object (Spatial*DataFrame): Typing polygon or line landscape
#' 
#' @return (matrix) M*5 (x1, y1, x2, y2, id_type)
extract_element_ <- function(object)
{
  # Initialize list of matrix
  elements <- c()

  # Deteminate if object is line or polygon
  if (class(object) == "SpatialPolygonsDataFrame")
  {
    info_object <- "Polygons"
    centroid <- coordinates(object)
  }
  else
  {
    info_object <- "Lines"
  }
  
  # Take all ids
  ids <- eval(parse(text = paste("getIdsSpatial",info_object,"(object)",
                                 sep = "")))
  for (index_obj in 1:length(ids))
  {
    # Get id of type
    id_type <- object@data$id_type[index_obj]

    if (info_object == "Lines")
    {
      # Get all coordonnate of polygon
      coords <- getCoordsSpatialLines(object, ids[index_obj])
    }
    else
    {
      coords <- rbind(centroid[index_obj,], centroid[index_obj,])
    }

    # Add each element: coordonate and parameter for potential function
    for (index_coord in 1:(length(coords[,1]) - 1))
    {
      elements <- rbind(elements, 
                          # x1 , y1
                        c(coords[index_coord,1], coords[index_coord,2],
                          # x2, x2
                          coords[index_coord + 1, 1], coords[index_coord + 1 ,2],
                          # id_type
                          id_type))
    }
  }

  return(elements)
}

#' @title plot potential
#' @description plot potential on landscape

#' 
#' @param landscape (SpatialPolygon): landscape
#' @param land_component (matrix) all coordinates of segment 
#'                                Matrix N*5 (x1,y1,x2,y2,Id_type)
#' @param info_type (matrix): N*5 (id, repulsive=0 or attractive=1, alpha, beta,
#'                                 power) where N number of type.
#' @param precision (float) plot precision
#' @return (matrix) matrix of potential
#' @export
plot_potential <- function(landscape,
                           land_component,
                           info_type,
                           precision = 1)
{
  #Take extrem value of landscape
  min_x <- attr(landscape,"bbox")[1,1]
  max_x <- attr(landscape,"bbox")[1,2]
  min_y <- attr(landscape,"bbox")[2,1]
  max_y <- attr(landscape,"bbox")[2,2]

  #calculate potential of each point
  pot <- c()
  for (x in seq(min_x,max_x,precision))
  {
    row_pot <- c()
    for (y in seq(min_y,max_y,precision))
    {
      row_pot <- c(potential_value(c(x, y),
                                   land_component,
                                   info_type)
                   ,row_pot)
    }
    #add new colunm
    pot <- cbind(pot,row_pot)
  }

  #show matrix of potentials
  plot(raster(pot, xmn = min_x, xmx = max_x, ymn = min_y, ymx = max_y),
       xlim = c(min_x, max_x), ylim = c(min_y, max_y))
  

  # def color for each type 
  type_color = rainbow(length(unique(info_type[,1])) + 1)

  # put all lines
  coord_lines = coordinates(extract_lines(landscape))
  for (index_line in seq_along(coord_lines))
  {
    lines(coord_lines[[index_line]][[1]], col = type_color[1])
  }

  # put typed lines
  for (index_att in 1:length(land_component[,1]))
  {
    index_color = which(info_type[,1] %in% land_component[index_att, 5])
    lines(c(land_component[index_att, 1], land_component[index_att, 3]),
          c(land_component[index_att, 2], land_component[index_att, 4]),
          col = type_color[index_color + 1], lwd = 10)
  }

  # Add number of type of polygon with color of type
  centroid <- coordinates(landscape)
  text(centroid, labels = landscape$id_type)

  # add legend
  legend(max_x,min_y,
         legend = c("no type or border",
                    unique(info_type[,1])),
         col = type_color, lty = 1,xpd = T, text.col = type_color)
  
  return(pot)
}
