#Interact dans type 0 pres 0
int0_0 = Interact$new(0,potentiel_0,list(0,0,0))
#Interact dans type 0 pres 0
int0_1 = Interact$new(1,potentiel_0,list(0,0,0))

#Interact dans type 1 pres 0 bordure
int1_0 = Interact$new(0,potentiel_0,list(runif(1,2,2),
                                         runif(1,4,4),#bordure
                                         runif(1,0,0)))
#Interact dans type 1 pres 1
int1_1 = Interact$new(1,potentiel_0,list(0,0,0))

#def type 1
type_0 = TypeInteract$new(0, agglo_0,list(int0_0, int0_1))
#def type 1
type_1 = TypeInteract$new(1, agglo_0,list(int1_0, int1_1))

#def model
my_model = TypeInteractModel$new(list(type_0,type_1))

coords=matrix(c(0,0,5,0,5,5,0,5,0,0),5,2,byrow=T)
p=Polygon(coords)
ps=Polygons(list(p),ID=c(1))
my_land=SpatialPolygons(list(ps))

my_line = extract_lines(my_land)
my_land = affect_polygons_type(my_land, 1)
my_line = affect_lines_type(my_line,1)

pl = PotentialLandscape$new(my_land, my_line, my_model)
pl$plot_potential(0.05)