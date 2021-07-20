plots1 = data_nested_p1 %>% 
  mutate(plots_onko = map(data, ~ggplot(., aes(factor(.$onko), .$answer, fill = factor(.$onko))) + 
                               geom_violin(adjust = 1.5) + 
                               geom_boxplot(fill = "white",width = 0.1) +
                               theme(legend.position = "none") + 
                               scale_y_discrete(limit = 1:5, labels = c("Brak", "Słaba", "Średnia", "Dobra", "Znakomita")) +
                               xlab("onko") +
                               ylab("Odpowiedź")))

plots2 = data_nested_p2 %>% 
  mutate(plots_onko = map(data, ~ggplot(., aes(factor(.$onko), .$answer, fill = factor(.$onko))) + 
                               geom_violin(adjust = 1.5) + 
                               geom_boxplot(fill = "white",width = 0.1) +
                               theme(legend.position = "none") + 
                               scale_y_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
                               xlab("onko") +
                               ylab("Odpowiedź")))

myplots = plots1$plots_onko + plots2$plots_onko

pdf("results/plots_onko.pdf", encoding = "WinAnsi.enc")
plots1$plots_onko
plots2$plots_onko
dev.off()