# Charger les bibliothèques nécessaires
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)

# Charger le fichier CSV
ordi <- read_csv("H:/RSTUDIO/Laptop2024.csv")

# Afficher les noms des colonnes pour vérifier
names(ordi)

ordi_selected <- ordi %>% select(Price, ram_memory, display_size, resolution_width, OS, resolution_height, primary_storage_capacity)

# Supprimer les lignes avec des valeurs manquantes (si nécessaire)
ordi_selected <- na.omit(ordi_selected)

# Vérifier les types de données et convertir les colonnes catégorielles en facteurs si nécessaire
#ordi_selected$brand <- as.factor(ordi_selected$brand)
ordi_selected$OS <- as.factor(ordi_selected$OS)

# Sélectionner uniquement les colonnes numériques pour le clustering
ordi_selected_numeric <- ordi_selected %>% select(Price, ram_memory, display_size, resolution_width, resolution_height, primary_storage_capacity)

# Normaliser les données numériques
ordi_scaled <- scale(ordi_selected_numeric)

# Clustering avant ACP
set.seed(123)
kmeans_before <- kmeans(ordi_scaled, centers = 3)
ordi$Cluster_before_PCA <- kmeans_before$cluster

# Visualisation des clusters avant ACP
fviz_cluster(kmeans_before, data = ordi_scaled)


