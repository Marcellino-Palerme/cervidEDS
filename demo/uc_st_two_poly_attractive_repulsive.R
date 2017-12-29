#' two polygon simulation tool use case
#' The smaller polygon is attrative and the taller is repulsive


require("cervideDS")

# create the smaller
coords = matrix(c(0, 15, 15, 50, 0, 50 , 0, 15),
                ncol = 2,byrow = T)
small <- Polygon(coords)
smalls <- Polygons(list(small),ID = c(31))

# create the taller
coords = matrix(c(0, 15, 15, 50, 50, 50 , 0, 50, 0, 0, 0, 15),
                ncol = 2,byrow = T)
tall <- Polygon(coords)
talls <- Polygons(list(tall),ID = c(8))
land_two_poly = SpatialPolygons(list(smalls, talls))

# define type
type_two_poly = matrix(c(2, 1, 1, 0.001, 2,
                         1, -1, 1, 0.001, 2),
                       nrow = 2, byrow = TRUE)

# get lines of landscape
line_two_poly = extract_lines(land_two_poly)
# affect types
land_two_poly = affect_polygons_type(land_two_poly, 2)
line_two_poly = affect_lines_type(line_two_poly,1)

element_two_poly = extract_elements(land_two_poly,
                                    line_two_poly)

plot_potential(land_two_poly,
               element_two_poly,
               type_two_poly,
               0.05)