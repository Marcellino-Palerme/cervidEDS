require("raster")
#source("landscape.R",local = TRUE)

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

#def paysage
my_land = gen_land()

# coords=matrix(c(0,0,60,0,60,30,0,30,0,0),5,2,byrow=T)
# p=Polygon(coords)
# coords=matrix(c(0,30,60,30,60,60,0,60,0,30),5,2,byrow=T)
# p1=Polygon(coords)
# ps=Polygons(list(p),ID=c(1))
# ps1=Polygons(list(p1),ID=c(2))
# my_land=SpatialPolygons(list(ps,ps1))


# coords=matrix(c(0,0,30,0,30,30,0,30,0,0),5,2,byrow=T)
# p=Polygon(coords)
# coords=matrix(c(0,30,30,30,30,60,0,60,0,30),5,2,byrow=T)
# p1=Polygon(coords)
# coords=matrix(c(30,0,60,0,60,30,30,30,30,0),5,2,byrow=T)
# p2=Polygon(coords)
# coords=matrix(c(30,30,60,30,60,60,30,60,30,30),5,2,byrow=T)
# p3=Polygon(coords)
# 
# coords=matrix(c(0,0,30,0,0,0),3,2,byrow=T)
# border1=Polygon(coords)
# coords=matrix(c(30,0,60,0,30,0),3,2,byrow=T)
# border12=Polygon(coords)
# coords=matrix(c(60,0,60,30,60,0),3,2,byrow=T)
# border2=Polygon(coords)
# coords=matrix(c(60,30,60,60,60,30),3,2,byrow=T)
# border22=Polygon(coords)
# coords=matrix(c(60,60,30,60,60,60),3,2,byrow=T)
# border3=Polygon(coords)
# coords=matrix(c(30,60,0,60,30,60),3,2,byrow=T)
# border32=Polygon(coords)
# coords=matrix(c(0,60,0,30,0,60),3,2,byrow=T)
# border4=Polygon(coords)
# coords=matrix(c(0,30,0,0,0,30),3,2,byrow=T)
# border42=Polygon(coords)

# 
# coords=matrix(c(0,0,30,0,
#                 60,0,60,30,
#                 60,60,30,60,
#                 0,60,0,30,
#                 0,0),9,2,byrow=T)
# border1=Polygon(coords)

# ps=Polygons(list(p),ID=c(1))
# ps1=Polygons(list(p1),ID=c(2))
# ps2=Polygons(list(p2),ID=c(3))
# ps3=Polygons(list(p3),ID=c(4))
# border1=Polygons(list(border1),ID=c(5))
# border2=Polygons(list(border2),ID=c(6))
# border3=Polygons(list(border3),ID=c(7))
# border4=Polygons(list(border4),ID=c(8))
# border12=Polygons(list(border12),ID=c(9))
# border22=Polygons(list(border22),ID=c(10))
# border32=Polygons(list(border32),ID=c(11))
# border42=Polygons(list(border42),ID=c(12))
# my_land=SpatialPolygons(list(ps,ps1,ps2,ps3))#,
                             #border1,border2,border3,border4,
                             #border12,border22,border32,border42))

my_line = extract_lines(my_land)
my_land = affect_polygons_type(my_land, 3)
my_line = affect_lines_type(my_line,3)
# my_land$id_type = c(1,2)#,3,1)#,0,0,0,0,0,0,0,0)
# my_line$id_type = c(0,3,0,0,0,0,0)

pp = PotentialLandscape$new(my_land, my_line, my_model)

#pose une bete

x_animal = runif(1,0,60)
y_animal = runif(1,0,60)
dt = 0.1
sig = 0.2
x = c(x_animal)
y = c(y_animal)
for(i in 1:(600/dt))
{

  pot = pp$get_potential_coord(x_animal, y_animal)

    tmpx = pp$get_dx_coord(x_animal, y_animal)
    tmpy = pp$get_dy_coord(x_animal, y_animal)
    
    print("pot")
    print(x_animal)
    print(y_animal)
    print(tmpx)
    print(tmpy)

  x_animal = x_animal + tmpx * dt + runif(1, -abs(tmpx), abs(tmpx)) * sig
  y_animal = y_animal + tmpy * dt + runif(1, -abs(tmpy), abs(tmpy)) * sig
  x = c(x,x_animal)
  y = c(y,y_animal)
}

pp$plot_potential(0.5)
points(x[1],y[1], col="green", lwd=5)
points(x[length(x)],y[length(y)], col="red", lwd=5)
lines(x,y, col="blue")
points(x,y, col="blue", pch=3)
# plot(my_raste, add=TRUE, useRaster=TRUE,
#      interpolate = FALSE,alpha=0.5,col=topo.colors(15))

