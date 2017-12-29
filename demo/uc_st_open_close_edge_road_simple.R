require("cervideDS")


#param attractive
attractive = c(3, 0.01, 2)

#param  repulsive
repulsive = c(2, 0.1, 2)

#param  neutre
neutre = c(0, 0, 0)

type_ochrst = matrix(c(1, 1, attractive,
                       2, -1, repulsive,
                       3, 1, attractive,
                       4, -1, repulsive),nrow = 4, byrow = TRUE)



# Create landscapes
land_ochrst = gen_land()
lines_ochrst = extract_lines(land_ochrst)
land_ochrst = affect_polygons_type(land_ochrst, 2)
lines_ochrst = affect_lines_type(lines_ochrst,2)
lines_ochrst$id_type[lines_ochrst$id_type == 1] = 3
lines_ochrst$id_type[lines_ochrst$id_type == 2] = 4

element_ochrst = extract_elements(land_ochrst,
                                  lines_ochrst)

plot_potential(land_ochrst,
               element_ochrst,
               type_ochrst)




