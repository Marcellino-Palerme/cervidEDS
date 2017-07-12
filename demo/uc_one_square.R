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
my_model = TypeInteractModel$new(list(type_1))

# create a square
coords=matrix(c(0,0,5,0,5,5,0,5,0,0),5,2,byrow=T)
p = Polygon(coords)
ps = Polygons(list(p),ID=c(1))
my_land = SpatialPolygons(list(ps))

# get lines of square
my_line = extract_lines(my_land)
# affect types
my_land = affect_polygons_type(my_land, 1)
my_line = affect_lines_type(my_line,1)

# create potential landscape
pl = PotentialLandscape$new(my_land, my_line, my_model)
pl$plot_potential(0.05)