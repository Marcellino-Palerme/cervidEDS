require("cervideDS")

#param attractive
attractive = c(2, 0.01, 2)

#param  repulsive
repulsive = c(2, 0.01, 2)

type_ochrst = matrix(c(1, 1, attractive,
                       2, -1, repulsive,
                       3, 1, attractive,
                       4, -1, repulsive),nrow = 4, byrow = TRUE)



# Create landscapes
land_ochrst = gen_land(10,80,80)
lines_ochrst = extract_lines(land_ochrst)
land_ochrst = affect_polygons_type(land_ochrst, 2)
lines_ochrst = affect_lines_type(lines_ochrst,2)
lines_ochrst$id_type[lines_ochrst$id_type == 1] = 3
lines_ochrst$id_type[lines_ochrst$id_type == 2] = 4

plot_potential(land_ochrst,
               lines_ochrst,
               type_ochrst,
               1)

x_animal = runif(1,0,80)
y_animal = runif(1,0,80)
dt = 0.1
sig = 0.2
x = c(x_animal)
y = c(y_animal)
for (i in 1:(600/dt))
{
  
  result = next_coord(x_animal,
                      y_animal,
                      land_ochrst,
                      lines_ochrst,
                      type_ochrst,
                      sig,
                      dt)
  x_animal = result["x"]
  y_animal = result["y"]
  x = c(x,x_animal)
  y = c(y,y_animal)
}

points(x[1],y[1], col = "green", lwd = 5)
points(x[length(x)],y[length(y)], col = "red", lwd = 5)
lines(x,y, col = "blue")

