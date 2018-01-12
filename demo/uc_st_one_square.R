#' One square simulation tool use case
#' The square have a attractive center


require("cervideDS")

# create a square
coords = matrix(c(0,0,50,0,50,50,0,50,0,0),5,2,byrow = T)
p = Polygon(coords)
ps = Polygons(list(p),ID=c(1))
land_square = SpatialPolygons(list(ps))

# get lines of square
line_square = extract_lines(land_square)
# affect types
land_square = affect_polygons_type(land_square, 1)
line_square = affect_lines_type(line_square,1)

type_square = matrix(c(1, 1, 1, 0.001, 2),nrow = 1)

# element_square = extract_elements(land_square,
#                                   line_square)

plot_potential(land_square,
               line_square,
               type_square,
               1)

# nx = c(20,40)
# 
# for (i in 1:600)
# {
#   nx = next_coord(nx,50,50,0.01,
#                  element_square,
#                  type_square,
#                  2)
#   points(nx[1],nx[2])
# }