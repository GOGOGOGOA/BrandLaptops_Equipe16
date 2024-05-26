library(ggplot2)
ordinateur <- read.csv("H:/RSTUDIO/Laptop2024.csv") 

hist(ordinateur$Price, breacks = 40, col = "steelblue", frame = FALSE) #diagrame du prix


ggplot(ordinateur, aes(x = as.factor(ram_memory))) +
  geom_bar(fill = "red", width = 0.7) +
  labs(title = "Diagramme à barres de la mémoire RAM",
       x = "Mémoire RAM",
       y = "Nombre d'odinateur") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))#diagramme mémoire RAM
summary(ordinateur)
table(ordinateur$ram_memory)#tableau synthetique RAM

ggplot(ordinateur, aes(x = "", fill = processor_brand)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  theme_void() +
  geom_text(stat = 'count', aes(label = paste0(round((..count..)/sum(..count..)*100), "%")), 
            position = position_stack(vjust = 0.5)) #graphique du processeur
summary(ordinateur)
table(ordinateur$processor_brand)#tab synthétique proc


ordinateur$is_touch_screen <- factor(ordinateur$is_touch_screen, levels = c(FALSE, TRUE), labels = c("Non", "Oui"))

# Créer un diagramme à barres
barplot(table(ordinateur$is_touch_screen), col = "steelblue", main = "Répartition de is_touch_screen", xlab = "is_touch_screen", ylab = "Nombre d'ordi")
summary(ordinateur)
table(ordinateur$is_touch_screen)

ggplot(ordinateur, aes(x = resolution_height)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "white") +
  labs(title = "Répartition des résolutions en hauteur",
       x = "Résolution ",
       y = "Nombre d'ordinateur")
summary(ordinateur)
table(ordinateur$resolution_height)# Créer un histogramme de la répartition des résolutions en hauteur


# Créer un histogramme de la répartition des résolutions en hauteur
ggplot(ordinateur, aes(x = resolution_width)) +
  geom_histogram(binwidth = 100, fill = "orange", color = "white") +
  labs(title = "Répartition des résolutions width",
       x = "Résolution ",
       y = "Nombre d'ordinateur")
summary(ordinateur)
table(ordinateur$resolution_width)

# Créer un graphique linéaire pour représenter display_size en fonction du nombre de voitures
ggplot(ordinateur, aes(x = display_size)) +
  geom_bar() +
  labs(title = "Nombre d'ordinateur par taille d'écran",
       x = "Taille de l'écran",
       y = "Nombre d'ordinateur")
summary(ordinateur)
table(ordinateur$display_size)

ggplot(ordinateur, aes(x = primary_storage_capacity )) +
  geom_bar() +
  labs(title = "capacité de stockage des ordinateurs",
       x = "capacité de stockage",
       y = "Nombre d'ordinateur")
summary(ordinateur)
table(ordinateur$primary_storage_capacity)

ggplot(ordinateur, aes(x = "", fill = brand)) +
  geom_bar(width = 1) +
  geom_text(stat = "count",
            aes(label = ifelse(after_stat(count) / sum(after_stat(count)) * 100 > 10, 
                               paste0(round(after_stat(count) / sum(after_stat(count)) * 100), "%"), "")),
            position = position_stack(vjust = 0.5)) +
  geom_hline(yintercept = seq(0, 0, by = 20), color = "black", linetype = "solid") +
  labs(title = "Répartition des marques",
       fill = "Marques") +
  coord_polar(theta = "y") +
  theme_void() #repartition des marques
table(ordinateur$brand)

ggplot(ordinateur, aes(x = "", fill = OS)) +
  geom_bar(width = 1) +
  geom_text(stat = "count",
            aes(label = ifelse(after_stat(count) / sum(after_stat(count)) * 100 > 10, 
                               paste0(round(after_stat(count) / sum(after_stat(count)) * 100), "%"), "")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des systèmes d'exploitation") +
  theme_void()
summary(ordinateur)
table(ordinateur$OS)

ggplot(ordinateur, aes(x = "", fill = primary_storage_type)) +
  geom_bar(width = 1) +
  geom_text(stat = "count",
            aes(label = ifelse(after_stat(count) / sum(after_stat(count)) * 100 > 10, 
                               paste0(round(after_stat(count) / sum(after_stat(count)) * 100), "%"), "")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "répartition du type de mémoire primaire") +
  theme_void()
summary(ordinateur)
table(ordinateur$primary_storage_type)




