# Spécifiez le chemin complet du fichier CSV
chemin <- "C:/Users/adaml/Desktop/Laptop2024.csv"

# Lisez le fichier CSV en utilisant la fonction read.csv()
donnees <- read.csv(chemin)

# Affichez les premières lignes des données pour vérifier
head(donnees)

# 2. Explorer les données
summary(donnees)

# Modèle de régression simple pour la note
modele_note <- lm(donnees$Price ~ donnees$Rating , data = donnees)
summary(modele_note)

# Modèle de régression simple pour la mémoire RAM
modele_ram <- lm(donnees$Price ~ donnees$ram_memory , data = donnees)
summary(modele_ram)

# Modèle de régression simple pour le stockage
modele_stockage <- lm(donnees$Price ~ donnees$primary_storage_capacity , data = donnees)
summary(modele_stockage)

# Modèle de régression simple pour la taille de l'écran
modele_taille <- lm(donnees$Price ~ donnees$display_size , data = donnees)
summary(modele_taille)




# 4. Créer un modèle de régression multiple avec les variables sélectionnées
# Par exemple, si la RAM, le stockage et la taille de l'écran sont sélectionnés comme variables importantes
modele_multiple <- lm(donnees$Price ~ donnees$ram_memory + donnees$Rating + donnees$display_size, data = donnees)

# 5. Évaluer et interpréter le modèle
summary(modele_multiple)


# Charger les bibliothèques nécessaires
install.packages("ggplot2")
library(ggplot2)

donnees <- read.csv(chemin)



# Créer un graphique de dispersion entre le prix et la note
ggplot(donnees, aes(x = donnees$Rating, y = donnees$Price)) +
  geom_point(color = "blue", alpha = 0.5) + # Points du graphique
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Ligne de régression linéaire
  labs(title = "Relation entre le Prix et la Note",
       x = "Note",
       y = "Prix") +
  theme_minimal() # Thème du graphique
