data_nested = data_longer %>% group_by(question) %>% nest()

data_nested = data_nested %>% left_join(pytania, by = c("question" = "pytanie"))

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

pdf("results/test/plots_zajecia.pdf", encoding = "ISOLatin2.enc")
plots1$plots_zajecia
plots2$plots_zajecia
dev.off()