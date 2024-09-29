library(rio)
data_Bericht <-import("/Users/larablumrich/Desktop/Datensatz Forschungsbericht 26.09/Blatt 2-Tabelle 1.csv")

data_Bericht$Person <- factor(data_Bericht$Person)
data_Bericht$Zeitpunkt <- factor(data_Bericht$Zeitpunkt)
data_Bericht$Therapie <- factor(data_Bericht$Therapie)

data_Bericht$Werte <- as.numeric(gsub(",", ".", data_Bericht$Werte))
print(data_Bericht)

#Varianzhomogenität prüfen
install.packages("dplyr")
library(dplyr)
data_Bericht %>%
  group_by(Zeitpunkt, Therapie) %>%
  summarise(var(Werte)) %>%
  as.data.frame()

library(car)
leveneTest(Werte ~ Therapie * Zeitpunkt, data = data_Bericht)

#Ausreißer Identifizieren 
iqr_value <- IQR(data_Bericht$Werte)
q1 <- quantile(data_Bericht$Werte, 0.25)
q3 <- quantile(data_Bericht$Werte, 0.75)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
outliers <- data_Bericht$Werte[data_Bericht$Werte < lower_bound | data_Bericht$Werte > upper_bound]
outliers

# deskriptive Statistik 
library(psych)
tapply(data_Bericht$Werte, list(data_Bericht$Therapie, data_Bericht$Zeitpunkt), mean)
tapply(data_Bericht$Werte, list(data_Bericht$Therapie, data_Bericht$Zeitpunkt), sd)

#Interaktionsplot
install.packages("ggplot2")
library(ggplot2)
ggplot(data_Bericht, aes(x = Zeitpunkt, y = Werte, group = Therapie, shape = Therapie, linetype = Therapie)) +
  stat_summary(fun = mean, geom = "line", aes(color = Therapie)) +  
  stat_summary(fun = mean, geom = "point", aes(color = Therapie)) +  
  labs(x = "Zeitpunkt", y = "Mittelwert Neurotizismus", color = "Gruppe") +  
  scale_x_discrete(labels = c("Zeitpunkt 1", "Zeitpunkt 2")) +
  ylim(1.5, 4.5) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = 10), legend.text = element_text(size = 8), legend.box = "horizontal") +
  guides(color = guide_legend(title = "Therapie"),
         shape = guide_legend(title = "Therapie"), 
         linetype = guide_legend(title = "Therapie"))

#ANOVA durchführen 
install.packages("afex")
library(afex)

results_ANOVA <- aov_car(Werte ~ Therapie * Zeitpunkt + Error(Person/Zeitpunkt), data = data_Bericht)
summary(results_ANOVA)

#Effektstärke
install.packages("effectsize")
library(effectsize)
eta_squared_results <- eta_squared(results_ANOVA)
eta_squared_results

#Post-hoc-Test
install.packages("emmeans")
library(emmeans)

post_hoc_results <- emmeans(results_ANOVA, ~ Therapie * Zeitpunkt)
summary(post_hoc_results)
pairwise_comparisons <- pairs(post_hoc_results)
summary(pairwise_comparisons)
