load("Data_Projet34.RData")

str(Data)

mali = Data[Data$Pays == 'Mali',]
tonga = Data[Data$Pays == 'Tonga', ]

#install.packages('Hmisc')
#install.packages("gridExtra")
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(esquisse)
library(dplyr)
esquisser()

describe(Data$Age)
describe(Data$Age[Data$Sexe == "Girls"])
sd(Data$Age[Data$Sexe == "Girls"])
describe(Data$Age[Data$Sexe == "Boys"])
sd(Data$Age[Data$Sexe == "Boys"])

describe(Data$Taille)
describe(Data$Taille[Data$Sexe == "Girls"])
sd(Data$Taille[Data$Sexe == "Girls"])
describe(Data$Taille[Data$Sexe == "Boys"])
sd(Data$Taille[Data$Sexe == "Boys"])

ggplot(Data) +
  aes(x = " ", y = Age, fill = Sexe) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(Boys = "#B2FFD5",
               Girls = "#D5BDFF")) +
  labs(title = "Répartition globale de l'âge", x = " ", y = "Age (en années décimales)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  annotate("text", x = 0.81, y = 12.4, label = "11.912") +
  annotate("text", x = 1.18, y = 12.4, label = "11.989")

ggplot(Data) +
  aes(x = "", y = Taille, fill = Sexe) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(Boys = "#B2FFD5",
               Girls = "#D5BDFF")) +
  labs(title = "Répartition globale de la taille", x = " ", y = "Taille (en cm)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  annotate("text", x = 0.81, y = 152.5, label = "149.9") +
  annotate("text", x = 1.18, y = 154.5, label = "151.9")



describe(mali$Age[mali$Sexe == 'Boys'])
describe(mali$Age[mali$Sexe == 'Girls'])
describe(tonga$Age[tonga$Sexe == 'Boys'])
describe(tonga$Age[tonga$Sexe == 'Girls'])

ggplot(Data) +
  aes(x = Pays, y = Age, fill = Sexe) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(Boys = "#B2FFD5",
               Girls = "#D5BDFF")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Répartition de l'âge par pays et par sexe", y = "Age (en années décimales)") +
  annotate("text", x = 0.81, y = 12.5, label = "11.932") +
  annotate("text", x = 1.18, y = 12.75, label = "12.196") +
  annotate("text", x = 1.82, y = 12.4, label = "11.890") +
  annotate("text", x = 2.19, y = 12.3, label = "11.774")


describe(mali$Taille[mali$Sexe == 'Boys'])
describe(mali$Taille[mali$Sexe == 'Girls'])
describe(tonga$Taille[tonga$Sexe == 'Boys'])
describe(tonga$Taille[tonga$Sexe == 'Girls'])

ggplot(Data) +
  aes(x = Pays, y = Taille, fill = Sexe) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(Boys = "#B2FFD5",
               Girls = "#D5BDFF")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Répartition de la taille par pays et par sexe", y = "Taille (en cm)") +
  annotate("text", x = 0.81, y = 148.5, label = "146.2") +
  annotate("text", x = 1.18, y = 152.5, label = "150.1") +
  annotate("text", x = 1.82, y = 155.5, label = "152.9") +
  annotate("text", x = 2.19, y = 156.2, label = "153.5")



# ajustement polynomial #

for (degree in 1:4) {
  # Ajustement polynomial
  model <- lm(Taille ~ poly(Age, degree = degree), data = subset(mali, Sexe == "Boys"))
  
  # Calcul du coefficient de détermination (R²)
  r_squared <- summary(model)$r.squared
  
  # Calcul de l'erreur quadratique moyenne (RMSE)
  predicted <- predict(model)
  rmse <- sqrt(mean((mali$Taille - predicted)^2))
  
  # Affichage des résultats
  cat("Degré du polynôme:", degree, "\n")
  cat("Coefficient de détermination (R²):", r_squared, "\n")
  cat("Erreur quadratique moyenne (RMSE):", rmse, "\n\n")
}


# MALI #

boys_mali_model = lm(Taille ~ poly(Age, 2), data = subset(mali, Sexe == "Boys"))
girls_mali_model = lm(Taille ~ poly(Age, 2), data = subset(mali, Sexe == "Girls"))


age_values_mali = seq(min(mali$Age), max(mali$Age), by = 0.1)

preticted_height_boys_mali = predict(boys_mali_model, newdata = data.frame(Age = age_values_mali))
preticted_height_girls_mali = predict(girls_mali_model, newdata = data.frame(Age = age_values_mali))


ggplot(mali) +
  aes(x = Age, y = Taille, colour = Sexe) +
  geom_point(shape = 'bullet', size = 1.5) +
  scale_color_manual(
    values = c(Boys = "#B2FFD5",
               Girls = "#D5BDFF")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = data.frame(age_values_mali, preticted_height_boys_mali), aes(x = age_values_mali, y = preticted_height_boys_mali), col = 'darkgreen', lwd = 1) +
  geom_line(data = data.frame(age_values_mali, preticted_height_girls_mali), aes(x = age_values_mali, y = preticted_height_girls_mali), col = 'purple', lwd = 1) +
  labs(title = "Répartition de la taille en fonction de l'âge et par sexe au Mali", x = "Age (en années décimales)", y = "Taille (en cm)")



# TONGA #

boys_tonga_model = lm(Taille ~ poly(Age, 2), data = subset(tonga, Sexe == "Boys"))
girls_tonga_model = lm(Taille ~ poly(Age, 2), data = subset(tonga, Sexe == "Girls"))

age_values_tonga = seq(min(tonga$Age), max(tonga$Age), by = 0.1)

preticted_height_boys_tonga = predict(boys_tonga_model, newdata = data.frame(Age = age_values_tonga))
preticted_height_girls_tonga = predict(girls_tonga_model, newdata = data.frame(Age = age_values_tonga))


ggplot(tonga) +
  aes(x = Age, y = Taille, colour = Sexe) +
  geom_point(shape = 'bullet', size = 1.5) +
  scale_color_manual(
    values = c(Boys = "#B2FFD5",
               Girls = "#D5BDFF")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = data.frame(age_values_tonga, preticted_height_boys_tonga), aes(x = age_values_tonga, y = preticted_height_boys_tonga), col = "darkgreen", lwd = 1) +
  geom_line(data = data.frame(age_values_tonga, preticted_height_girls_tonga), aes(x = age_values_tonga, y = preticted_height_girls_tonga), col = "purple", lwd = 1) +
  labs(title = "Répartition de la taille en fonction de l'âge et par sexe au Tonga", x = "Age (en années décimales)", y = "Taille (en cm)")


str(Data)

# DECOUPE PAR AGE #


Data$groupe_age_interval = cut(Data$Age, breaks = c(4, 7, 10, 13, 16, 19), include.lowest = TRUE)

mediane_groupe = aggregate(Data$Taille, by = list(Data$groupe_age_interval), FUN = median)
mediane_groupe$Age = c(5.5, 8.5, 11.5, 14.5, 17.5)

premier_quartile_groupe <- aggregate(Data$Taille, by = list(Data$groupe_age_interval), FUN = function(x) {
  quantile(x, probs = 0.25)
})
premier_quartile_groupe$Age = c(5.5, 8.5, 11.5, 14.5, 17.5)
colnames(premier_quartile_groupe) <- c("groupe_age_interval", "premier_quartile", "Age")

troisieme_quartile_groupe <- aggregate(Data$Taille, by = list(Data$groupe_age_interval), FUN = function(x) {
  quantile(x, probs = 0.75)
})
troisieme_quartile_groupe$Age = c(5.5, 8.5, 11.5, 14.5, 17.5)
colnames(troisieme_quartile_groupe) <- c("groupe_age_interval", "troisieme_quartile", "Age")

ggplot() +
  geom_line(data = mediane_groupe, aes(x = Age, y = x, colour = "Médiane"), linewidth = 1.5) +
  geom_line(data = premier_quartile_groupe, aes(x = Age, y = premier_quartile, colour = "Premier Quartile"), linewidth = 1.5) +
  geom_line(data = troisieme_quartile_groupe, aes(x = Age, y = troisieme_quartile, colour = "Troisième Quartile"), linewidth = 1.5) +
#  geom_point(data = mediane_groupe, aes(x = Age, y = x), shape = "circle", size = 1.5, colour = "#112446") +
#  geom_point(data = premier_quartile_groupe, aes(x = Age, y = premier_quartile), shape = "circle", size = 1.5, colour = "#112446") +
#  geom_point(data = troisieme_quartile_groupe, aes(x = Age, y = troisieme_quartile), shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Comparaison des quartiles de taille de toute la population", x = "Âge (en années décimales)", y = "Taille (en cm)", color = "Quartile") +
  scale_colour_manual(values = c("#bc5090", "#003f5c", "#ffa600"), labels = c("Médiane", "Premier Quartile", "Troisième Quartile"))









############


Data$groupe_age_interval = cut(Data$Age, breaks = c(4, 7, 10, 13, 16, 19), include.lowest = TRUE)

mediane_groupe_girls = aggregate(Data$Taille[Data$Sexe == "Girls"], by = list(Data$groupe_age_interval[Data$Sexe == "Girls"]), FUN = median)
mediane_groupe_girls$Age = c(5.5, 8.5, 11.5, 14.5, 17.5)
mediane_groupe_boys = aggregate(Data$Taille[Data$Sexe == "Boys"], by = list(Data$groupe_age_interval[Data$Sexe == "Boys"]), FUN = median)
mediane_groupe_boys$Age = c(5.5, 8.5, 11.5, 14.5, 17.5)

ggplot() + 
  geom_line(data = mediane_groupe_boys, aes(x = Age, y = x, colour = "Médiane Garçons"), linewidth = 1.5) +
  geom_line(data = mediane_groupe_girls, aes(x = Age, y = x, colour = "Médiane Filles"), linewidth = 1.5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Comparaison des Quartiles de Taille", x = "Âge", y = "Taille (en cm)", color = "Quartile") +
  scale_colour_manual(values = c("#08bdba", "#4589ff", "#d4bbff"), labels = c("Médiane Garçons", "Médiane Filles"))







ggplot(mediane_groupe, aes(x = Age, y = x)) +
  geom_line(linewidth = 1, colour = "red", group = 1) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Médiane Taille", x = "Âge", y = "Taille (en cm)")
#  annotate("text", x = "[4,7]", y = 122, label = "118.88") +
#  annotate("text", x = "(7,10]", y = 135, label = "131.56") +
#  annotate("text", x = "(10,13]", y = 152, label = "148.48") +
#  annotate("text", x = "(13,16]", y = 164, label = "161.59") +
#  annotate("text", x = "(16,19]", y = 164, label = "167.61")


ggplot(premier_quartile_groupe, aes(x = Age, y = premier_quartile)) +
  geom_line(linewidth = 1, colour = "red", group = 1) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Premier Quartile Taille", x = "Âge", y = "Taille (en cm)")



ggplot(troisieme_quartile_groupe, aes(x = Age, y = troisieme_quartile)) +
  geom_line(linewidth = 2, colour = "red", group = 1) +
  geom_point(shape = "circle", size = 2.5, colour = "darkred") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Troisième Quartile Taille", x = "Âge", y = "Taille (en cm)")


# Taille en fonction de l'age
ggplot(Data) +
  aes(x = Age, y = Taille) +
  geom_point(shape = "circle", size = 1.5, colour = "#2B436D") +
  theme_minimal() +
  labs(title = "Taille en fonction de l'age")

# Taille en fonction de l'age par pays
ggplot(Data) +
  aes(x = Age, y = Taille, colour = Pays) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()

# Taille en fonction de l'age par sexe au Mali
ggplot(mali) +
  aes(x = Age, y = Taille, colour = Sexe) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  labs(title = "Taille en fonction de l'age par sexe au Mali")




boys_mali_model = lm(Taille ~ poly(Age, 2), data = subset(mali, Sexe == "Boys"))
girls_mali_model = lm(Taille ~ poly(Age, 2), data = subset(mali, Sexe == "Girls"))

age_values_mali = seq(min(mali$Age), max(mali$Age), by = 0.1)

preticted_height_boys_mali = predict(boys_mali_model, newdata = data.frame(Age = age_values_mali))
preticted_height_girls_mali = predict(girls_mali_model, newdata = data.frame(Age = age_values_mali))


ggplot(mali) +
  aes(x = Age, y = Taille, colour = Sexe) +
  geom_point(shape = 'bullet', size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  geom_line(data = data.frame(age_values_mali, preticted_height_boys_mali), aes(x = age_values_mali, y = preticted_height_boys_mali), col = 'red', lwd = 1) +
  geom_line(data = data.frame(age_values_mali, preticted_height_girls_mali), aes(x = age_values_mali, y = preticted_height_girls_mali), col = 'blue', lwd = 1) +
  labs(title = "Repartition de la taille en fonction de l'age et par sexe au Mali")





boys_tonga_model = lm(Taille ~ poly(Age, 2), data = subset(tonga, Sexe == "Boys"))
girls_tonga_model = lm(Taille ~ poly(Age, 2), data = subset(tonga, Sexe == "Girls"))

age_values_tonga = seq(min(tonga$Age), max(tonga$Age), by = 0.1)

preticted_height_boys_tonga = predict(boys_tonga_model, newdata = data.frame(Age = age_values_tonga))
preticted_height_girls_tonga = predict(girls_tonga_model, newdata = data.frame(Age = age_values_tonga))


ggplot(tonga) +
  aes(x = Age, y = Taille, colour = Sexe) +
  geom_point(shape = "bullet", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  geom_line(data = data.frame(age_values_tonga, preticted_height_boys_tonga), aes(x = age_values_tonga, y = preticted_height_boys_tonga), col = "red", lwd = 1) +
  geom_line(data = data.frame(age_values_tonga, preticted_height_girls_tonga), aes(x = age_values_tonga, y = preticted_height_girls_tonga), col = "blue", lwd = 1) +
  labs(title = "Repartition de la taille en fonction de l'age et par sexe au Tonga")
