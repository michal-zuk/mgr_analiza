library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(purrr)
library(infer)
library(broom)

rm(data)
data = read_csv("data.csv") %>% select(-2)
data$profil = str_to_lower(data$profil)

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
#filter out null profiles, creates column for each profile with logical coding of their scope of work, zamiana plci, stopnia itp na binarne
data = data %>% filter(!is.na(profil)) %>% mutate(kobieta = if_else(plec == "Kobieta", 1, 0),
                                                  magister = if_else(stopien == "Magister", 1, 0),
                                                  zajecia = if_else(zajecia == "Tak", 1, 0),
                                                  zajecia_dodatkowe = if_else(zajecia_dodatkowe == "Tak", 1, 0),
                                                  wiedza_mean = ((Q1_1 + Q1_2 + Q1_3 + Q1_4 + Q1_5 + Q1_6 + Q1_7 + Q1_8 + Q1_9 + Q1_10) / 10),
                                                  wiedza = case_when(
                                                    wiedza_mean >= 3.5 ~ 1,
                                                    wiedza_mean <= 2.5 ~ 0,
                                                    between(wiedza_mean, 2.5, 3.5) ~ 99)) %>% select(id, kobieta, magister, wiedza, zajecia, zajecia_dodatkowe, profil:Q3_10)
  
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
                  

profile_summary = data %>% summarise(neuro = sum(neuro),
                                     reuma = sum(reuma),
                                     ozn_ruch = sum(ozn_ruch),
                                     ozn_int = sum(ozn_int),
                                     pedia = sum(pedia),
                                     geria = sum(geria),
                                     dz_wady_p = sum(dz_wady_p),
                                     kardio = sum(kardio),
                                     orto = sum(orto),
                                     stoma = sum(stoma),
                                     uro = sum(uro),
                                     dzieci = sum(dzieci),
                                     onko = sum(onko),
                                     OIT = sum(OIT),
                                     ginek = sum(ginek),
                                     osteo = sum(osteo),
                                     pedia_2 = sum(pedia_2),
                                     nie_prac = sum(nie_prac))
#rearranges columns
data = data %>% select(id:zajecia_dodatkowe, neuro:nie_prac, Q1_1:Q3_10)

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
                                                                                                    
#data_longer_numeric = data %>% pivot_longer(Q1_1:Q3_10, names_to = "question", values_to = "answer")
data_longer_wiedza = data_longer %>% filter(wiedza != 99, question != c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q1_7", "Q1_8", "Q1_9", "Q1_10"))

data_nested = data_longer %>% group_by(question) %>% nest()
data_nested_numeric = data_longer_numeric %>% group_by(question) %>% nest()

data_nested_sample = data_nested[[2]][[12]]




test = data_longer %>% group_by(question) %>% nest() %>% mutate(
  model_kobieta = map(data, ~ wilcox.test(.$answer ~ .$kobieta)),
  model_kobieta = map(model_kobieta, tidy),
  model_magister = map(data, ~ wilcox.test(.$answer ~ .$magister)),
  model_magister = map(model_magister, tidy),
  model_zajecia = map(data, ~ wilcox.test(.$answer ~ .$zajecia)),
  model_zajecia = map(model_zajecia, tidy),
  model_zajecia_dodatkowe = map(data, ~ wilcox.test(.$answer ~ .$zajecia_dodatkowe)),
  model_zajecia_dodatkowe = map(model_zajecia_dodatkowe, tidy),
  model_neuro = map(data, ~ wilcox.test(.$answer ~ .$neuro)),
  model_neuro = map(model_neuro, tidy),
  model_reuma = map(data, ~ wilcox.test(.$answer ~ .$reuma)),
  model_reuma = map(model_reuma, tidy),
  model_ozn_ruch = map(data, ~ wilcox.test(.$answer ~ .$ozn_ruch)),
  model_ozn_ruch = map(model_ozn_ruch, tidy),
  model_ozn_int = map(data, ~ wilcox.test(.$answer ~ .$ozn_int)),
  model_ozn_int = map(model_ozn_int, tidy),
  model_geria = map(data, ~ wilcox.test(.$answer ~ .$geria)),
  model_geria = map(model_geria, tidy),
  model_kardio = map(data, ~ wilcox.test(.$answer ~ .$kardio)),
  model_kardio = map(model_kardio, tidy),
  model_orto = map(data, ~ wilcox.test(.$answer ~ .$orto)),
  model_orto = map(model_orto, tidy),
  model_uro = map(data, ~ wilcox.test(.$answer ~ .$uro)),
  model_uro = map(model_uro, tidy),
  model_ginek = map(data, ~ wilcox.test(.$answer ~ .$ginek)),
  model_ginek = map(model_ginek, tidy),
  model_onko = map(data, ~ wilcox.test(.$answer ~ .$onko)),
  model_onko = map(model_onko, tidy)
) %>% unnest(
  model_kobieta,
  model_magister,
  model_zajecia,
  model_zajecia_dodatkowe,
  model_neuro,
  model_reuma,
  model_ozn_ruch,
  model_ozn_int,
  model_geria,
  model_kardio,
  model_orto,
  model_uro,
  model_ginek,
  model_onko,
  names_sep = "_"
)

test_output = test %>% select(-data)


#test_wiedza



test_wiedza = data_longer_wiedza %>% group_by(question) %>% nest() %>% 
  mutate(model_wiedza = map(data, ~ wilcox.test(.$answer ~ .$wiedza)),
         model_wiedza = map(model_wiedza, tidy)) %>% unnest(model_wiedza)

test_wiedza_output = test_wiedza %>% select(-data)

#zapisywanie
write_csv(test_output, "results/test_output")
write_csv(test_wiedza_output, "results/test__wiedza_output")



#dodac kolumne z tytulem
#dodac labelki do oy
#profesje do sprawdzenia neuro + reuma + ozn_ruch + ozn_int + geria + kardio + orto  +uro + onko + ginek
profesje = c("neuro",  "reuma", "ozn_ruch", "ozn_int", "geria", "kardio", "orto", "uro", "onko", "ginek")


#dzialajacy wykres
data_nested[[2]][[3]] %>% ggplot(aes(factor(kobieta), answer, fill = factor(kobieta))) + 
  geom_violin(adjust = 1.5) + 
  geom_boxplot(fill = "white",width = 0.1) +
  theme(legend.position = "none") + 
  scale_y_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
  xlab("Kobieta")

#robienie wykresow

boxplot_1 = function(x, y) {ggplot(aes(x = .data[x], y = .data[y], fill = .data[y])) + 
    geom_violin(adjust = 1.5) + coord_flip() +
    geom_boxplot(fill = "white",width = 0.1) +
    theme(legend.position = "none") + 
    scale_x_discrete(limit = 1:5, labels = c("Brak", "Słaba", "Średnia", "Dobra", "Znakomita")) +
    xlab("Odpowiedź")} 

boxplot_2 = function(x, y) {ggplot(aes(x = .data[x], y = .data[y], fill = .data[y])) + 
  geom_violin(adjust = 1.5) + coord_flip() +
  geom_boxplot(fill = "white",width = 0.1) +
  theme(legend.position = "none") + 
  scale_x_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
  xlab("Odpowiedź") +
    ylab()}


data_nested_p1 = data_nested %>% filter(question %in% c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q1_7", "Q1_8", "Q1_9", "Q1_10"))
data_nested_p2 = data_nested %>% tail(20)

plots1 = data_nested_p1 %>% 
  mutate(plots_zajecia = map(data, ~ggplot(., aes(factor(.$zajecia), .$answer, fill = factor(.$zajecia))) + 
                               geom_violin(adjust = 1.5) + 
                               geom_boxplot(fill = "white",width = 0.1) +
                               theme(legend.position = "none") + 
                               scale_y_discrete(limit = 1:5, labels = c("Brak", "Słaba", "Średnia", "Dobra", "Znakomita")) +
                               xlab("zajecia") +
                               ylab("Odpowiedź")))

plots2 = data_nested_p2 %>% 
  mutate(plots_zajecia = map(data, ~ggplot(., aes(factor(.$zajecia), .$answer, fill = factor(.$zajecia))) + 
                               geom_violin(adjust = 1.5) + 
                               geom_boxplot(fill = "white",width = 0.1) +
                               theme(legend.position = "none") + 
                               scale_y_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
                               xlab("zajecia") +
                               ylab("Odpowiedź")))

myplots = plots1$plots_zajecia + plots2$plots_zajecia

pdf("results/plots_zajecia.pdf", encoding = "WinAnsi.enc")
plots1$plots_zajecia
plots2$plots_zajecia
dev.off()



#plots wiedza vs reszta pytan
data_nested_wiedza = data_longer_wiedza %>% group_by(question) %>% nest()

plots_wiedza = data_nested_wiedza %>% 
  mutate(plots_wiedza = map(data, ~ggplot(., aes(factor(.$wiedza), .$answer, fill = factor(.$wiedza))) + 
                               geom_violin(adjust = 1.5) + 
                               geom_boxplot(fill = "white",width = 0.1) +
                               theme(legend.position = "none") + 
                               scale_y_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
                               xlab("wiedza") +
                               ylab("Odpowiedź")))


pdf("results/plots_wiedza.pdf", encoding = "WinAnsi.enc")
plots_wiedza$plots_wiedza
dev.off()



 
 
 