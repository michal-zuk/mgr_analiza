#saves unfiltered data_frame
data_unflitered = data %>%  mutate(neuro = if_else(str_detect(data$profil, "neurologiczni"),1,0),
                                   reuma = if_else(str_detect(data$profil, "reumatologiczni"),1,0),
                                   ozn_ruch = if_else(str_detect(data$profil, "osoby z niepełnosprawnością ruchową"),1,0),
                                   ozn_int = if_else(str_detect(data$profil, "osoby z niepełnosprawnością intelektualną"),1,0),
                                   pedia = if_else(str_detect(data$profil, "pediatryczni"),1,0),
                                   geria = if_else(str_detect(data$profil, "geriatryczni"),1,0),
                                   dz_wady_p = if_else(str_detect(data$profil, "dzieci z wadami postawy"),1,0),
                                   kardio = if_else(str_detect(data$profil, "kardiologiczni"),1,0),
                                   orto = if_else(str_detect(data$profil, "ortopedyczni"),1,0),
                                   stoma = if_else(str_detect(data$profil, "stomatologiczni"),1,0),
                                   uro = if_else(str_detect(data$profil, "urologiczni"),1,0),
                                   dzieci = if_else(str_detect(data$profil, "dzieci"),1,0),
                                   onko = if_else(str_detect(data$profil, "onkologiczni"),1,0),
                                   OIT = if_else(str_detect(data$profil, "oddział intensywnej terapii"),1,0),
                                   ginek = if_else(str_detect(data$profil, "ginekologiczne"),1,0),
                                   osteo = if_else(str_detect(data$profil, "osteopatia"),1,0),
                                   pedia_2 = if_else(str_detect(data$profil, "pacjenci pediatryczni"),1,0),
                                   nie_prac = if_else(str_detect(data$profil, "nie pracuję"),1,0)) %>% select(!profil)


#melts and nests data, changes some variables to factors
data_longer = data %>% pivot_longer(Q1_1:Q3_10, names_to = "question", values_to = "answer") #%>% mutate(plec = factor(plec),
#stopien = factor(stopien),
#zajecia = factor(zajecia),
#zajecia_dodatkowe = factor(zajecia_dodatkowe),
#neuro = factor(neuro),
#reuma = factor(reuma),
#ozn_ruch = factor(ozn_ruch),
#ozn_int = factor(ozn_int),
#pedia = factor(pedia),
#geria = factor(geria),
#dz_wady_p = factor(dz_wady_p),
#kardio = factor(kardio),
#orto = factor(orto),
#stoma = factor(stoma),
#uro = factor(uro),
#dzieci = factor(dzieci),
#onko = factor(onko),
#OIT = factor(OIT),
#ginek = factor(ginek),
#osteo = factor(osteo),
#pedia_2 = factor(pedia_2),
#nie_prac = factor(nie_prac),
#answer = factor(answer))