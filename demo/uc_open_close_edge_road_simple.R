require("cervideDS")

#Interact border
border = Interact$new(0,potentiel_0,list(runif(1,2,2),
                                         runif(1,4,4),
                                         runif(1,0,0)))
#param potentiel_0 to attractive
attractive = list(-1, 0.1, 2)

#param potentiel_0 to repulsive
repulsive = list(1, 0.1, 1)

#param potentiel_0 to neutre
neutre = list(0, 0, 0)


#Interact hote open neightbour close
h_open_n_close = Interact$new(1, potentiel_0, attractive)

#Interact hote open neightbour open
h_open_n_open = Interact$new(2, potentiel_0, neutre)

#Interact hote open neightbour hedge
h_open_n_hedge = Interact$new(3, potentiel_0, attractive)

#Interact hote open neightbour road
h_open_n_road = Interact$new(4, potentiel_0, repulsive)


#Interact hote close neightbour close
h_close_n_close = Interact$new(1, potentiel_0, neutre)

#Interact hote close neightbour open
h_close_n_open = Interact$new(2, potentiel_0, repulsive)

#Interact hote close neightbour hedge
h_close_n_hedge = Interact$new(3, potentiel_0, neutre)

#Interact hote close neightbour road
h_close_n_road = Interact$new(4, potentiel_0, repulsive)


#def type_close
type_close = TypeInteract$new(1, "close", agglo_0,list(border, h_close_n_close,
                                                       h_close_n_open,
                                                       h_close_n_hedge,
                                                       h_close_n_road))
#def type_open
type_open = TypeInteract$new(2, "open", agglo_0,list(border, h_open_n_close,
                                                     h_open_n_open,
                                                     h_open_n_hedge,
                                                     h_open_n_road))
#def type_hedge
type_hedge = TypeInteract$new(3, "hedge", NULL, list())

#def type_road
type_road = TypeInteract$new(4, "road", NULL, list())

#def model
model_ochrs = TypeInteractModel$new(list(type_close, type_open,
                                         type_hedge, type_road))


# Create landscapes
land_ochrs = gen_land()
lines_ochrs = extract_lines(land_ochrs)
land_ochrs = affect_polygons_type(land_ochrs, 2)
lines_ochrs = affect_lines_type(lines_ochrs,2)
lines_ochrs$id_type[lines_ochrs$id_type == 1] = 3
lines_ochrs$id_type[lines_ochrs$id_type == 2] = 4


pl__ochrs = PotentialLandscape$new(land_ochrs, lines_ochrs, model_ochrs)
mat = pl__ochrs$plot_potential(1)




