#' @title Generate landscape
#' @description  Generer un paysage de polygone convexe
#' @param nb_poly : number of polygon
#' @param width : width of landscape
#' @param height : height of landscape
#'
#' @return SpatialPolygonDataFrame : landscape with convex ploygons
#' @export
gen_land <- function(nb_poly=10, width=60, height=60)
{
  # size of the landscape in meters (min is 60*60)
  xlim <- c(0,max(width,60))
  ylim <- c(0,max(height,60)) 
  # number of polygone (min is 10)
  n <- max(nb_poly,10)
  
  # regularity of the polygonal seeds pattern
  iterations <- 100
  
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

#' @title Get identifiant of Spatial*
#' @description Give Id of all elements
#' @param object (Spatial*): object Spatial*
#' @param name_element (str): 
#' @return list of id
getIdsSpatial_ <- function(object, name_element)
{
  return(sapply(slot(object, name_element),
                function(x) slot(x, "ID")))
}

#' @title Get identifiant of SpatialPolygons
#' @description Give Id of all polygons
#' @param land (SpatialPolygons*): the landscape
#' @return list of id
#' @export
getIdsSpatialPolygons <- function(land)
{
  return(getIdsSpatial_(land, "polygons"))
}

#' @title Get identifiant of SpatialLines
#' @description Give Id of all lines
#' @param sp_line (SpatialLines*): the landscape
#' @return list of id
#' @export
getIdsSpatialLines <- function(sp_line)
{
  return(getIdsSpatial_(sp_line, "lines"))
}

#' @title affect polygon type
#' @description Give a type at each polygon (ramdom)
#' @param landscape (SpatialPolygonsDataFrame): the landscape whose affect types
#' @param nb_type (int) : number types are affect
#' @return landscape (SpatialPolygoneDataFrame): the landscape with types
#' @export
affect_polygons_type <- function(landscape, nb_type)
{
  # min number type is 1
  nb_type = max(1,nb_type)
  # create random list of type
  lt_id_types = sample(1:nb_type,length(landscape),replace = T)
  # affect type 
  landscape$id_type = lt_id_types
  
  return(landscape)
}

#' @title affect line type
#' @description Give a type at each line (ramdom)
#' @param lt_lines (list to named row): the line landscape whose affect types
#' @param nb_type (int) : number types are affect
#' @return lt_lines (list to named row): the line landscape with types
#' @export
affect_lines_type <- function(lt_lines, nb_type)
{
  # min number type is 1
  nb_type = max(1,nb_type)
  # create random list of type
  lt_id_types = sample(0:nb_type,length(lt_lines$id_poly1),replace = T)
  # All 0 replace by -1 mean the line don't have type
  lt_id_types[lt_id_types == 0] = -1
  # affect type 
  lt_lines$id_type = lt_id_types
  # affect type 0 for border
  lt_lines$id_type[which(lt_lines$id_poly1 %in% 0)] = 0
  lt_lines$id_type[which(lt_lines$id_poly2 %in% 0)] = 0
  
  return(lt_lines)
}

#' @title get neighbours
#' @description give neighbours of each polygon
#' @param landscape (SpatialPolygons*): the landscape
#' @return list of list: one row by polygon. In each list of neighbours polygons
get_neighbours <- function(landscape)
{
  return(poly2nb(landscape, queen = TRUE,
                 row.names = getSpPPolygonsIDSlots(landscape)))
}

#' @title get coordonnate Spatial*
#' @description get coordonnate of element of Spatial*
#'
#' @param (Spatial*)
#' @param name_elements (vector str): two name to attain coords
#' @param id (str): id of element
#'
#' @return (matrix X*2): coordinates of element
getCoordsSpatial_ <- function(object, name_elements, id)
{
  # get position of polygon with this id
  pos_id = which(getIdsSpatial_(object, name_elements[1]) %in% id)
  # get list of element
  elements = attr(object,name_elements[1])
  # get the element 
  element = attr(elements[[pos_id]],name_elements[2])
  # a polygon is a list of polygons
  coords = NULL
  for (elemt in element)
  {
    # concatenate all coordinate of each element
    coords = rbind(coords,attr(elemt,"coords"))
  }
  #return coordinates
  return(coords)
}

#' @title get coordonnate SpatialPolygons
#' @description get coordonnate of polygon in SpatialPolygons
#'
#' @param landscape (SpatialPolygons)
#' @param id (int): id of polygon
#' @return (matrix X*2): coordinates of polygon
#' @export
getCoordsSpatialPolygons <- function(landscape, id)
{
  #return coordinates
  return(getCoordsSpatial_(landscape,c("polygons","Polygons"),id))
}

#' @title get coordonnate SpatialLines
#' @description get coordonnate of line in Spatiallines
#'
#' @param landscape (SpatialPolygons)
#' @param id (int): id of polygon
#' @return (matrix X*2): coordinates of polygon
#' @export
getCoordsSpatialLines <- function(my_lines, id)
{
  #return coordinates
  return(getCoordsSpatial_(my_lines,c("lines","Lines"),id))
}

#' @title commun coords
#' @description give commun coordonate between two polygons
#'
#' @param landscape (SpatialPolygonsDataFrame): SIG
#' @param ids (tupple of int): id of two polygons
#'
#' @return (matrix X*2): coordonate of commun points
#' @export
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

#' @title Extract lines of spatialpolygons
#' @description  Extract lines of spatialpolygons
#' @param landscape (SpatialPolygons*)
#' @return SpatialLinesDataFrame
#' @export
extract_lines <- function(landscape)
{
  # take all ids of landscape
  lt_ids = getIdsSpatialPolygons(landscape)
  
  dic_lines =  list(id_poly1 = c(), id_poly2 = c(),
                    x0 = c(), x1 = c(), y0 = c(), y1 = c() )
  for (id in lt_ids)
  {
    # take coordonnate each polygon
    lt_coords = getCoordsSpatialPolygons(landscape, id)
    for (i in seq(2,along.with = lt_coords[-1,1]))
    {
      x0 = lt_coords[i - 1,1]
      y0 = lt_coords[i - 1,2]
      x1 = lt_coords[i ,1]
      y1 = lt_coords[i ,2]
      index_01 = match(1,(x0 == dic_lines$x0) * (y0 == dic_lines$y0) *
                         (x1 == dic_lines$x1) * (y1 == dic_lines$y1))
      
      index_10 = match(1,(x0 == dic_lines$x1) * (y0 == dic_lines$y1) *
                         (x1 == dic_lines$x0) * (y1 == dic_lines$y0))

      
      if (!is.na(index_01))
      {
        dic_lines$id_poly2[index_01] = id
      }
      else if (!is.na(index_10))
      {
        dic_lines$id_poly2[index_10] = id
      }
      else
      {
        dic_lines$id_poly1 = c(dic_lines$id_poly1, id)
        dic_lines$id_poly2 = c(dic_lines$id_poly2, 0)
        dic_lines$x0 = c(dic_lines$x0, x0)
        dic_lines$x1 = c(dic_lines$x1, x1)
        dic_lines$y0 = c(dic_lines$y0, y0)
        dic_lines$y1 = c(dic_lines$y1, y1)
      }
    }
  }
  
  # create SpatialLinesDataFrame
  lt_line = list()
  for (i in 1:length(dic_lines$x0))
  {
    coords = cbind(c(dic_lines$x0[i],dic_lines$x1[i]),
                     c(dic_lines$y0[i],dic_lines$y1[i]))
    my_line = Lines(Line(coords), ID = toString(i))
    lt_line[i] = my_line
  }
  sl = SpatialLines(lt_line, slot(landscape,"proj4string"))
  sldf = SpatialLinesDataFrame(sl,data.frame(id_poly1 = dic_lines$id_poly1,
                                             id_poly2 = dic_lines$id_poly2))
  return(sldf)
}