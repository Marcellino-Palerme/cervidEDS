require("cervideDS")

#Interact border
border = Interact$new(0,potentiel_0,list(runif(1,2,2),
                                         runif(1,4,4),#bordure
                                         runif(1,0,0)))
#Interact hote type 1 neighbour type 1
int1_1 = Interact$new(1,potentiel_0,list(0,0,0))

#def type 1
type_1 = TypeInteract$new(1, agglo_0,list(border, int1_1))

#def model
model_square = TypeInteractModel$new(list(type_1))

# create a square
coords = matrix(c(0,0,5,0,5,5,0,5,0,0),5,2,byrow = T)
p = Polygon(coords)
ps = Polygons(list(p),ID=c(1))
land_square = SpatialPolygons(list(ps))

# get lines of square
line_square = extract_lines(land_square)
# affect types
land_square = affect_polygons_type(land_square, 1)
line_square = affect_lines_type(line_square,1)

# create potential landscape
pl_square = PotentialLandscape$new(land_square, line_square, model_square)
pl_square$plot_potential(0.05)
