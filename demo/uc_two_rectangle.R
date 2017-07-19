require("cervideDS")

#Interact of border
border = Interact$new(0,potentiel_0,list(runif(1,2,2),
                                         runif(1,4,4),
                                         runif(1,0,0)))
#Interact hote type 1 neighbour type 1
int1_1 = Interact$new(1,potentiel_0,list(0,0,0))
#Interact hote type 1 neighbour type 2
int1_2 = Interact$new(2,potentiel_0,list(runif(1,-1,-1),
                                         runif(1,0.1,0.1),#2 attractif
                                         runif(1,2,2)))

#Interact hote type 1 neighbour type 3
int1_3 = Interact$new(3,potentiel_0,list(runif(1,-1,-1),
                                         runif(1,0.01,0.01),
                                         runif(1,1,1)))

#Interact dans type 2 pres 1
int2_1 = Interact$new(1,potentiel_0,list(runif(1,1,1),
                                         runif(1,0.1,0.1),#1 moyen
                                         runif(1,0,0)))
#Interact dans type 2 pres 2
int2_2 = Interact$new(2,potentiel_0,list(0,0,0))

#Interact hote type 2 neighbour type 3
int2_3 = Interact$new(3,potentiel_0,list(0,0,0))


#def type 1
type_1 = TypeInteract$new(1, "type_1", agglo_0,list(border, int1_1, int1_2,
                                                    int1_3))
#def type 2
type_2 = TypeInteract$new(2, "type_2", agglo_0,list(border, int2_1, int2_2,
                                                    int2_3))
#def type 3
type_3 = TypeInteract$new(3, "line", NULL, list())

#def model
model_two_rec = TypeInteractModel$new(list(type_1, type_2, type_3))

coords = matrix(c(0,0,6,0,6,3,0,3,0,0),5,2,byrow = T)
p = Polygon(coords)
coords = matrix(c(0,3,6,3,6,6,0,6,0,3),5,2,byrow = T)
p1 = Polygon(coords)
ps = Polygons(list(p),ID=c(1))
ps1 = Polygons(list(p1),ID=c(2))
land_two_rec = SpatialPolygons(list(ps,ps1))

line_two_rec = extract_lines(land_two_rec)

land_two_rec$id_type = c(1,2)
line_two_rec$id_type = c(0,3,0,0,0,0,0)

pl_two_rec = PotentialLandscape$new(land_two_rec, line_two_rec, model_two_rec)
valu = pl_two_rec$plot_potential(0.1)
