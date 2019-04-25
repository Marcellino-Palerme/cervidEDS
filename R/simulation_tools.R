#------- simulation_tools.R----------#
#
# Provide function to simulate animal moving in landscape.

#' @title distance point to polygon
#' @description Give distance a point to polygon and derivate in x and y
#' 
#' @param x (double) position in x
#' @param y (double) position in y
#' @param land_poly (SpatialPolygon): polygon landscape
#' @param Id : Id of polygon
#' @return (vector) distance to segment (dist),
#'                  derivate in x (dx),
#'                  derivate in y (dy)
#' @export
distance2poly <- function(x,
                          y,
                          land_poly,
                          id)
{
  result = c("dist" = 0,
             "dx" = 0,
             "dy" = 0)
  # get coordinate of polygon
  poly_coords = getCoordsSpatialPolygons(land_poly,
                                         id)

  # Verify if the points is in polygon
  if (sp::point.in.polygon(x,y,
                           poly_coords[,1],
                           poly_coords[,2]) > 0)
  {
    return(result)
  }
  else
  {
    # get first distance
    result = distance2segment(x,
                              y,
                              poly_coords[1, 1],
                              poly_coords[1, 2],
                              poly_coords[2, 1],
                              poly_coords[2, 2])
    # Calculate distance point and all segment of polygon
    for (index_coord in 1:(length(poly_coords[,1]) - 1)) 
    {
      tmp = distance2segment(x,
                             y,
                             poly_coords[index_coord, 1],
                             poly_coords[index_coord, 2],
                             poly_coords[index_coord + 1, 1],
                             poly_coords[index_coord + 1, 2])
      # Keep the shorter distance
      if (tmp["dist"] < result["dist"])
      {
        result = tmp
      }
    }
  }
  return(result)
}

#' @title Extract elements
#' @description Extract all element of typed landscape to create list of matrix
#'              containing coordonnate of element with parameter of potential
#'              function
#' 
#' @param x (double) position in x
#' @param y (double) position in y
#' @param land_poly (SpatialPolygonDataFrame): Typing polygon landscape
#' @param land_lines (SpatialLinesDataFrame): Typing line landscape
#' 
#' @return (matrix) M*4 (dist, dx, dy, id_type)
#' @export
extract_elements <- function(x,
                             y,
                             land_poly,
                             land_lines)
{
  # Initialize 
  elements_land <- c()

  # Extract elements from polygons
  element_poly <- extract_element_(x,
                                   y,
                                   land_poly)
  # Extract elements from lines
  element_line <- extract_element_(x,
                                   y,
                                   land_lines)

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
#' @param x (double) position in x
#' @param y (double) position in y
#' @param object (Spatial*DataFrame): Typing polygon or line landscape
#' 
#' @return (matrix) M*4 (dist, dx, dy, id_type)
extract_element_ <- function(x,
                             y,
                             object)
{
  # Initialize list of matrix
  elements <- c()

  # Deteminate if object is line or polygon
  if (class(object) == "SpatialPolygonsDataFrame")
  {
    info_object <- "Polygons"
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
      # Get distance and gradient between point and line
      dist <- distance2segment(x,
                               y,
                               coords[1, 1],
                               coords[1, 2],
                               coords[2, 1],
                               coords[2, 2])
    }
    else
    {
      # Get distance and gradient between point and segment
      dist <- distance2poly(x,
                            y,
                            object,
                            ids[index_obj])
    }

    # Add each element: distance, gradient and id of type
    elements <- rbind(elements, 
                      # distance and gradient
                      c(dist,
                        # id_type
                        id_type))

  }

  return(elements)
}

#' @title plot potential
#' @description plot potential on landscape
#' 
#' @param land_poly (SpatialPolygonDataFrame): Typing polygon landscape
#' @param land_lines (SpatialLinesDataFrame): Typing line landscape
#' @param info_type (matrix): N*5 (id, repulsive=0 or attractive=1, alpha, beta,
#'                                 power) where N number of type.
#' @param precision (float) plot precision
#' @return (matrix) matrix of potential
#' @export
plot_potential <- function(land_poly,
                           land_lines,
                           info_type,
                           precision = 1)
{
  #Take extrem value of landscape
  min_x <- attr(land_poly,"bbox")[1,1]
  max_x <- attr(land_poly,"bbox")[1,2]
  min_y <- attr(land_poly,"bbox")[2,1]
  max_y <- attr(land_poly,"bbox")[2,2]

  #calculate potential of each point
  pot <- c()
  for (x in seq(min_x,max_x,precision))
  {
    row_pot <- c()
    for (y in seq(min_y,max_y,precision))
    {
      row_pot <- c(potential_value(extract_elements(x,
                                                    y,
                                                    land_poly,
                                                    land_lines),
                                   info_type)
                   ,row_pot)
    }
    #add new colunm
    pot <- cbind(pot,row_pot)
  }

  #show matrix of potentials
  plot(raster::raster(pot, xmn = min_x, xmx = max_x, ymn = min_y, ymx = max_y),
                      xlim = c(min_x, max_x), ylim = c(min_y, max_y))
  

  # def color for each type 
  type_color = rainbow(length(unique(info_type[,1])) + 2)

  # put all lines
  coord_lines = sp::coordinates(land_lines)
  for (index_line in seq_along(coord_lines))
  {
    index_color = which(c(-1, 0, info_type[,1]) %in% land_lines$id_type[index_line])
    lines(coord_lines[[index_line]][[1]],
          col = type_color[index_color])
  }

  # put typed lines
  # for (index_att in 1:length(land_poly[,1]))
  # {
  #   index_color = which(info_type[,1] %in% land_poly[index_att, 5])
  #   lines(c(land_poly[index_att, 1], land_poly[index_att, 3]),
  #         c(land_poly[index_att, 2], land_poly[index_att, 4]),
  #         col = type_color[index_color + 1], lwd = 10)
  # }

  # Add number of type of polygon with color of type
  centroid <- sp::coordinates(land_poly)
  text(centroid, labels = land_poly$id_type)

  # add legend
  legend(max_x,min_y,
         legend = c("no type "," border",
                    unique(info_type[,1])),
         col = type_color, lty = 1,xpd = T, text.col = type_color)
  
  return(pot)
}

#' @title derivate coord
#' @description give values of derivate in x and y for a point 
#' 
#' @param x (double) position in x
#' @param y (double) position in y
#' @param land_poly (SpatialPolygonDataFrame): Typing polygon landscape
#' @param land_lines (SpatialLinesDataFrame): Typing line landscape
#' @param info_type (matrix): N*5 (id, repulsive=0 or attractive=1, alpha, beta,
#'                                 power) where N number of type.
#' @param sigma (double) repulsif effect adaptor
#' @param time_step (double) step of time
#' @return (NumericVector) derivate in x and y (['x'];['y'])
#' @export
d_coord <- function(x,
                    y,
                    land_poly,
                    land_lines,
                    info_type,
                    sigma,
                    time_step)
{
  #Take extrem value of landscape
  min_x <- attr(land_poly,"bbox")[1,1]
  max_x <- attr(land_poly,"bbox")[1,2]
  min_y <- attr(land_poly,"bbox")[2,1]
  max_y <- attr(land_poly,"bbox")[2,2]
  
  dist_element <- extract_elements(x,
                                   y,
                                   land_poly,
                                   land_lines)

  effect = all_effect(c(x,y),
                      max_x - min_x,
                      max_y - min_y,
                      sigma,
                      dist_element,
                      info_type,
                      time_step)

  return(c(effect["x"] , effect["y"]))
}

#' @title next coord
#' @description calculate next coordinates 
#' 
#' @param x (double) position in x
#' @param y (double) position in y
#' @param land_poly (SpatialPolygonDataFrame): Typing polygon landscape
#' @param land_lines (SpatialLinesDataFrame): Typing line landscape
#' @param info_type (matrix): N*5 (id, repulsive=0 or attractive=1, alpha, beta,
#'                                 power) where N number of type.
#' @param sigma (double) repulsif effect adaptor
#' @param time_step (double) step of time
#' @return (NumericVector) next coord in x and y (['x'];['y'])
#' @export
next_coord <- function(x,
                       y,
                       land_poly,
                       land_lines,
                       info_type,
                       sigma,
                       time_step)
{
  #Take derivate in x and y for a point 
  derivate <- d_coord(x,
                      y,
                      land_poly,
                      land_lines,
                      info_type,
                      sigma,
                      time_step)
  
  x = x + derivate[1] + diffusion(sigma,
                                  time_step)
  y = y + derivate[2] + diffusion(sigma,
                                  time_step)
  
  return(c(x , y))
}