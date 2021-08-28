data_test = data_longer %>% filter(question == "Q1_1") %>% group_by(zajecia, answer) %>% mutate(answer_count = n(), answer_percent = answer_count/n()) #%>% group_by(answer_percent) %>% summarise(answer_count = count(answer_percent))
data_longer %>% filter(question == "Q1_1") %>% group_by(zajecia, answer) %>% mutate(answer_count = n(), answer_percent = answer_count/n())


data_longer %>% filter(question == "Q1_1") %>% mutate(zajecia = as.factor(zajecia)) %>% group_by(zajecia, answer) %>% count(answer) %>% ggplot(aes(x = answer, y = n, colour = zajecia, fill = zajecia)) + geom_col(position = "dodge")
data_longer %>% filter(question == "Q1_1") %>% 
  mutate(zajecia = as.factor(zajecia)) %>% 
  group_by(zajecia, answer) %>% 
  count(answer) %>% 
  group_by(zajecia) %>% 
  mutate(percent_answer = (n/sum(n))*100) %>% 
  ggplot(aes(x = answer, y = percent_answer, colour = zajecia, fill = zajecia)) + 
  geom_col(position = "dodge")




data_longer %>% filter(question == "Q1_1") %>% ggplot(aes(x = answer, colour = zajecia)) + geom_histogram(position = "dodge", binwidth = 1)


plot_1 = data_longer %>% filter(question == "Q1_1") %>% 
  mutate(zajecia = as.factor(zajecia)) %>% 
  group_by(zajecia, answer) %>% 
  count(answer) %>% 
  group_by(zajecia) %>% 
  mutate(percent_answer = (n/sum(n))*100) %>% 
  ggplot(aes(x = answer, y = percent_answer, fill = zajecia)) + 
  geom_col(position = "dodge", width = .6) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_discrete(limit = 1:5, labels = c("Brak", "Słaba", "Średnia", "Dobra", "Znakomita")) +
  scale_fill_discrete(labels = c("0" ="Nie", "1" = "Tak"), name = "Udział w zajęciach z rehabilitacji seksualnej") +
  xlab("Odpowiedź") +
  ylab("Procent wszystkich odpowiedzi w grupie") +
  ggtitle(pytania$tresc[1])

plot_2 =data_longer %>% filter(question == "Q2_3") %>% 
  mutate(zajecia = as.factor(zajecia)) %>% 
  group_by(zajecia, answer) %>% 
  count(answer) %>% 
  group_by(zajecia) %>% 
  mutate(percent_answer = (n/sum(n))*100) %>% 
  ggplot(aes(x = answer, y = percent_answer, fill = zajecia)) + 
  geom_col(position = "dodge", width = .6) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
  scale_fill_discrete(labels = c("0" ="Nie", "1" = "Tak"), name = "Udział w zajęciach z rehabilitacji seksualnej") +
  xlab("Odpowiedź") +
  ylab("Procent wszystkich odpowiedzi w grupie") +
  ggtitle(pytania$tresc[13])

plot_3 = data_longer %>% filter(question == "Q3_2") %>% 
  mutate(zajecia = as.factor(zajecia)) %>% 
  group_by(zajecia, answer) %>% 
  count(answer) %>% 
  group_by(zajecia) %>% 
  mutate(percent_answer = (n/sum(n))*100) %>% 
  ggplot(aes(x = answer, y = percent_answer, fill = zajecia)) + 
  geom_col(position = "dodge", width = .6) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_discrete(limit = 1:5, labels = c("Zdecydowanie nie zgadzam się", "Raczej nie zgadzam się", "Nie mam zdania", "Raczej zgadzam się", "Zdecydowanie zgadzam się")) +
  scale_fill_discrete(labels = c("0" ="Nie", "1" = "Tak"), name = "Udział w zajęciach z rehabilitacji seksualnej") +
  xlab("Odpowiedź") +
  ylab("Procent wszystkich odpowiedzi w grupie") +
  ggtitle(pytania$tresc[22])

pdf("results/wykresy_slupkowe.pdf", encoding = "ISOLatin2.enc", width = 12.1)
plot_1
plot_2
plot_3
dev.off()

