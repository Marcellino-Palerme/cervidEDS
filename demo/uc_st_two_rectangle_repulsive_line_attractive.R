#' two rectangle repulsive and between two rectangle a line attractive

require("cervideDS")

# define type
type_two_rec_st = matrix(c(2, -1, 1, 0.001, 2,
                           4, 1, 1, 0.001, 2),
                       nrow = 2, byrow = TRUE)

coords = matrix(c(0,0,60,0,60,30,0,30,0,0),5,2,byrow = T)
p = sp::Polygon(coords)
coords = matrix(c(0,30,60,30,60,60,0,60,0,30),5,2,byrow = T)
p1 = sp::Polygon(coords)
ps = sp::Polygons(list(p),ID = c(1))
ps1 = sp::Polygons(list(p1),ID = c(2))
land_two_rec_st = sp::SpatialPolygons(list(ps,ps1))

line_two_rec_st = extract_lines(land_two_rec_st)

land_two_rec_st$id_type = c(4, 4)
line_two_rec_st$id_type = c(0,2,0,0,0,0,0)

# element_two_rec_st = extract_elements(land_two_rec_st,
#                                       line_two_rec_st)

plot_potential(land_two_rec_st,
               line_two_rec_st,
               type_two_rec_st,
               1)