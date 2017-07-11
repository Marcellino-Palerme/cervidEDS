#Interact dans type 0 pres 0
int0_0 = Interact$new(0,potentiel_0,list(0,0,0))
#Interact dans type 0 pres 0
int0_1 = Interact$new(1,potentiel_0,list(0,0,0))
#Interact dans type 0 pres 0
int0_2 = Interact$new(2,potentiel_0,list(0,0,0))
#Interact dans type 0 pres 0
int0_3 = Interact$new(3,potentiel_0,list(0,0,0))

#Interact dans type 1 pres 0 bordure
int1_0 = Interact$new(0,potentiel_0,list(runif(1,2,2),
                                         runif(1,4,4),#bordure
                                         runif(1,0,0)))
#Interact dans type 1 pres 1
int1_1 = Interact$new(1,potentiel_0,list(0,0,0))
#Interact dans type 1 pres 2
int1_2 = Interact$new(2,potentiel_0,list(runif(1,1,1),
                                         runif(1,0.01,0.01),#2 attractif
                                         runif(1,0,0)))
#Interact dans type 1 pres 3
int1_3 = Interact$new(3,potentiel_0,list(runif(1,1,1),
                                         runif(1,0.07,0.07),#3réplusif
                                         runif(1,0,0)))
#Interact dans type 2 pres 0 bordure
int2_0 = Interact$new(0,potentiel_0,list(runif(1,2,2),
                                         runif(1,4,4),#bordure
                                         runif(1,0,0)))
#Interact dans type 2 pres 1
int2_1 = Interact$new(1,potentiel_0,list(runif(1,1,1),
                                         runif(1,0.04,0.04),#1 moyen
                                         runif(1,0,0)))
#Interact dans type 2 pres 2
int2_2 = Interact$new(2,potentiel_0,list(0,0,0))
#Interact dans type 2 pres 3
int2_3 = Interact$new(3,potentiel_0,list(runif(1,1,1),
                                         runif(1,0.07,0.07),
                                         runif(1,0,0)))
#Interact dans type 3 pres 0 bordure
int3_0 = Interact$new(0,potentiel_0,list(runif(1,2,2),
                                         runif(1,4,4),#bordure
                                         runif(1,0,0)))
#Interact dans type 3 pres 1
int3_1 = Interact$new(1,potentiel_0,list(runif(1,1,1),
                                         runif(1,0.04,0.04),
                                         runif(1,0,0)))
#Interact dans type 3 pres 2
int3_2 = Interact$new(2,potentiel_0,list(runif(1,1,1),
                                         runif(1,0.01,0.01),
                                         runif(1,0,0)))
#Interact dans type 3 pres 3
int3_3 = Interact$new(3,potentiel_0,list(0,0,0))

#def type 1
type_0 = TypeInteract$new(0, agglo_0,list(int0_0, int0_1, int0_2,int0_3))
#def type 1
type_1 = TypeInteract$new(1, agglo_0,list(int1_0, int1_1, int1_2,int1_3))
#def type 2
type_2 = TypeInteract$new(2, agglo_0,list(int2_0, int2_1,int2_2,int2_3))
#def type 3
type_3 = TypeInteract$new(3, agglo_0,list(int3_0, int3_1,int3_2,int3_3))

#def model
my_model = TypeInteractModel$new(list(type_0,type_1,type_2, type_3))

coords=matrix(c(0,0,6,0,6,3,0,3,0,0),5,2,byrow=T)
p=Polygon(coords)
coords=matrix(c(0,3,6,3,6,6,0,6,0,3),5,2,byrow=T)
p1=Polygon(coords)
ps=Polygons(list(p),ID=c(1))
ps1=Polygons(list(p1),ID=c(2))
my_land=SpatialPolygons(list(ps,ps1))

my_line = extract_lines(my_land)
my_land = affect_polygons_type(my_land, 3)
my_line = affect_lines_type(my_line,3)
#my_land$id_type = c(1,1)
#my_line$id_type = c(0,0,0,0,0,0,0)

pl = PotentialLandscape$new(my_land, my_line, my_model)
pl$plot_potential(0.05)