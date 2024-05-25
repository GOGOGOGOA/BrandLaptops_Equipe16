# Installer les packages nécessaires (pas besoin de le faire si ils sont déja installés)
install.packages("dplyr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("magrittr")

# Charger les packages nécessaires
library(dplyr)
library(magrittr)
library(rpart)
library(rpart.plot)
library(caret)


# On commence par charger le jeu de donnée
df <- read.csv("Laptop2024.csv")

# on demande un aperçu des données pour vérifier que le bon tableau a bien été chargé
head(df)

# Ici on demande de ne sélectionner que certaines colonnes (ici cela permet de supprimer les colonnes model brand et proccesor tier qui ne nous interesse pas)

df <- df %>%
  select(-Model,-brand, -processor_tier)

# Cette ligne nous permet de poser une graine qui nous permettra de bien avoir les mêmes apprentissage et donc de pouvoir bien faire les tests
set.seed(40)
train_index <- sample(1:nrow(df),0.80* nrow(df)) # les 3 lignes suivantes servent à ne réaliser l'apprentissage que sur 80% des variables afin de garder les 20% restants et les stocker dans test_data afin de réaliser les tests dessus, cela permet de faire les tests sur des variables qui n'ont pas fait partit de l'apprentissage et donc d'avoir des résultats fiables
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# On crée l'arbre de décision
tree_model <- rpart(is_touch_screen ~ ., data = train_data, method = "class")
                

# On affiche l'arbre de décision avec le format qu'on a vu en cours et un titre
rpart.plot(tree_model, type = 0, extra = 1, fallen.leaves = TRUE, main = "Arbre de décision pour prédire les écrans tactiles")


# On utilise les 20% de variables gardés pour les tests et on leur applique l'arbre de décision en stockant la valeur de is_touch_screen que l'arbre leur prédit
predictions <- predict(tree_model, test_data, type = "class")

# Matrice de confusion en comparant les valeurs que l'arbre prédit et les valeurs réels
confusion_matrix <- table(predictions, test_data$is_touch_screen)
print(confusion_matrix)

# Calcul et affichage de la précision
precision <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(precision)

# Calcul et affichage du rappel
recall <- confusion_matrix[2, 2] / (confusion_matrix[2, 2] + confusion_matrix[2, 1])
print(recall)

# Calcul et affichage de la F-mesure
f_measure <- 2 * (precision * recall) / (precision + recall)
print(f_measure)

