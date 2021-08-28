data_nested = data_longer %>% group_by(question) %>% nest()

pytania = read_csv("pytania.csv")
data_nested = data_nested %>% left_join(pytania, by = c("question" = "pytanie"))

data_nested_p1 = data_nested %>% filter(question %in% c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q1_7", "Q1_8", "Q1_9", "Q1_10"))
data_nested_p2 = data_nested %>% tail(20)



plots1 = data_nested_p1 %>% 
  mutate(plots_zajecia_dodatkowe = map2(data, tresc, ~ggplot(.x, aes(factor(.$zajecia_dodatkowe), .$answer, fill = factor(.$zajecia_dodatkowe))) + 
                               geom_violin(adjust = 1.5) + 
                               geom_boxplot(fill = "white",width = 0.1) +
                               theme_bw() +
                               theme(legend.position = "none") + 
                               scale_y_discrete(limit = 1:5, labels = c("Brak", "Słaba", "Średnia", "Dobra", "Znakomita")) +
                               scale_x_discrete(labels = c("0" ="Nie", "1" = "Tak")) +
                               xlab("Udział w szkoleniach dotyczących rehabilitacji seksualnej") +
                               ylab("Odpowiedź") +
                               ggtitle(.y)))

#plots1_title = imap(plots1$plots_zajecia_dodatkowe, ~ .x + ggtitle(.$tresc))

plots2 = data_nested_p2 %>% 
  mutate(plots_zajecia_dodatkowe = map2(data, tresc, ~ggplot(.x, aes(factor(.$zajecia_dodatkowe), .$answer, fill = factor(.$zajecia_dodatkowe))) + 
                               geom_violin(adjust = 1.5) + 
                               geom_boxplot(fill = "white",width = 0.1) +
                               theme_bw() +
                               theme(legend.position = "none") + 
                               scale_y_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
                               scale_x_discrete(labels = c("0" ="Nie", "1" = "Tak")) +
                               xlab("Udział w szkoleniach dotyczących rehabilitacji seksualnej") +
                               ylab("Odpowiedź") +
                               ggtitle(.y)))

plots1$plots_zajecia_dodatkowe[10]
pdf("results/plots_zajecia_dodatkowe.pdf", encoding = "ISOLatin2.enc", width = 12.1)
plots1$plots_zajecia_dodatkowe
plots2$plots_zajecia_dodatkowe
dev.off()


data_longer_wiedza = data_longer %>% filter(wiedza != 99, question != c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q1_7", "Q1_8", "Q1_9", "Q1_10"))
data_nested_wiedza = data_longer_wiedza %>% group_by(question) %>% nest() %>% left_join(pytania, by = c("question" = "pytanie"))

plots_wiedza = data_nested_wiedza %>% 
  mutate(plots_wiedza = map2(data, tresc, ~ggplot(., aes(factor(.$wiedza), .$answer, fill = factor(.$wiedza))) + 
                              geom_violin(adjust = 1.5) + 
                              geom_boxplot(fill = "white",width = 0.1) +
                              theme_bw() +
                              theme(legend.position = "none") + 
                              scale_y_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
                              scale_x_discrete(labels = c("0" ="Niska", "1" = "Wysoka")) +
                              xlab("Ocena własnej wiedzy") +
                              ylab("Odpowiedź") +
                              ggtitle(.y)))


pdf("results/plots_wiedza.pdf", encoding = "ISOLatin2.enc", width = 12.1)
plots_wiedza$plots_wiedza
dev.off()

#działający
data_nested[[2]][[3]] %>% ggplot(aes(factor(zajecia_dodatkowe), answer, fill = factor(zajecia_dodatkowe))) + 
  geom_violin(adjust = 1.5) + 
  geom_boxplot(fill = "white",width = 0.1) +
  theme(legend.position = "none") + 
  scale_y_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
  scale_x_discrete(labels = c("0" ="Mężczyzna", "1" = "zajecia_dodatkowe")) +
  xlab("zajecia_dodatkowe") +
  ylab("Odpowiedź")



