library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(purrr)
library(infer)
library(broom)
library(gtsummary)

rm(data)
data = read_csv("data.csv") %>% select(-2)
data$profil = str_to_lower(data$profil)


#filter out null profiles, 
data = data %>% filter(!is.na(profil)) 

# mutate bugs out when piping from filter, doing this as separate operations helps
data = data %>%  mutate(neuro = if_else(str_detect(data$profil, "neurologiczni"),1,0),
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

data2 = data %>% select(
  Płeć  = plec,
  "Stopień naukowy" = stopien,
  Zajęcia = zajecia,
  "Zajęcia dodatkowe" = zajecia_dodatkowe,
  Neurologiczni = neuro,
  Reumatologiczni = reuma,
  "OzN ruchową" = ozn_ruch,
  "OzN intelektualną" = ozn_int,
  Geriatryczni = geria,
  Kardiologiczni = kardio,
  Ortopedyczni = orto,
  Urologiczni = uro,
  Ginekologiczne = ginek,
  Onkologiczni = onko
)

summary = data2 %>% tbl_summary(by = Zajęcia) %>% modify_header(label ~ "**Zmienna**")
summary
