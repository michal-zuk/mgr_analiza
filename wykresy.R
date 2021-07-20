#dzialajacy ostatnio
data_nested[[2]][[3]] %>% ggplot(aes(factor(kobieta), answer, fill = factor(kobieta))) + 
  geom_violin(adjust = 1.5) + 
  geom_boxplot(fill = "white",width = 0.1) +
  theme(legend.position = "none") + 
  scale_y_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
  xlab("Kobieta")


boxplot_1 = function(x, y) {ggplot(aes(x = factor(.data[x]), y = .data[y], fill = factor(.data[y]))) + 
    geom_violin(adjust = 1.5) + 
    coord_flip() +
    geom_boxplot(fill = "white",width = 0.1) +
    theme(legend.position = "none") + 
    scale_x_discrete(limit = 1:5, labels = c("Brak", "Słaba", "Średnia", "Dobra", "Znakomita")) +
    xlab("Odpowiedź")} 

boxplot_2 = function(x, y) {ggplot(aes(x = factor(.data[x]), y = .data[y], fill = factor(.data[y])))  + 
    geom_violin(adjust = 1.5) + 
    coord_flip() +
    geom_boxplot(fill = "white",width = 0.1) +
    theme(legend.position = "none") + 
    scale_x_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
    xlab("Odpowiedź")}
