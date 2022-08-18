library(readxl)
Df <- read_excel("pilot.xlsx", sheet = "Data")
View(Df)
attach(Df)

library(dplyr)
library(ggplot2)
library(ggprism)
library(ggpubr)

#reorder levels
Df$Time <- factor(Time, levels=c('Pre', 'Post'))

Df %>% group_by(Time) %>%
  summarise(
    count = n(),
    mean = mean(RM, na.rm = TRUE),
    sd = sd(RM, na.rm = TRUE))

T_test <- t.test(RM ~ Time, paired = TRUE, data=Df,  alternative = c("two.sided"))
T_test

Df %>% ggplot(aes(Time,RM, fill=Time)) + geom_boxplot() + theme_bw() +
  theme_prism() + stat_compare_means(method = "t.test", label.x=1.4)
ggsave("boxplot_pilot.png")


Df %>% ggplot(aes(Time,RM, fill=Time)) + geom_boxplot() +
  labs(y="Estimated Repetition Maximum Back Squat (Kg)") + theme_bw() +
  theme_prism() + geom_point(aes(group = Athlete),
                              alpha = 1, colour = "black") +
  geom_line(aes(group = Athlete),
   alpha = 1, colour = "black") + stat_compare_means(method = "t.test", label.x=1.4)
ggsave("boxplot_connecting_pilot.png")
