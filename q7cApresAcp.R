###########################################################################
####################### Mise en place des données #########################

# Importer les données
data <- read_csv("H:/RSTUDIO/Laptop2024.csv")

# Sélectionner les variables quantitatives
selected_columns <- data[, c("Price", "num_cores", "ram_memory", "primary_storage_capacity", "display_size", "resolution_width", "resolution_height")]

# Remplacer les valeurs "No information" par la valeur moyenne de la colonne "year_of_warranty" et tout convertir en numérique
selected_columns[selected_columns == "No information"] <- NA  
selected_columns <- apply(selected_columns, 2, function(x) as.numeric(as.character(x)))  
selected_columns <- apply(selected_columns, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))  

# Standardisation des données
scaled_data <- scale(selected_columns)

# Conversion en data.frame
scaled_data <- as.data.frame(scaled_data)

# Calcul des composantes principales
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

###########################################################################
######################### Premier plan factoriel ##########################

### Eboulis des valeurs propres
prop_var <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
plot(prop_var, type = "b", main = "Éboulis des valeurs propres", xlab = "Composante principale", ylab = "Proportion de variance expliquée")
print(prop_var)

# Fonction 
correlation_circle <- function(pca_result, variables) {
  eigenvectors <- pca_result$rotation[, 1:2]
  names(eigenvectors) <- c("PC1", "PC2")
  
  variables_coords <- variables %*% eigenvectors
  
  plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), xlab = "PC1", ylab = "PC2")
  abline(h = 0, v = 0, col = "gray", lty = 2)
  arrows(0, 0, eigenvectors[,1], eigenvectors[,2], length = 0.1)
  text(eigenvectors, labels = rownames(eigenvectors), pos = 3)text(variables_coords, labels = colnames(variables), col = "red")
}

# Cercle de corrélation (PC1, PC2)
correlation_circle(pca_result, selected_columns)
# Obtenir les chargements factoriels des variables sur PC1 et PC2
loadings <- pca_result$rotation[, 1:2]
print(loadings)

# Tracer le nuage des individus 
individuals_coords <- pca_result$x[, 1:2]
plot(individuals_coords, pch = 19, col = "blue", xlab = "PC1", ylab = "PC2", main = "Nuage des individus sur PC1 et PC2")

###########################################################################
######################## Troisième plan factoriel #########################

# Fonction pour le cercle de corrélation pour PC3 et PC4
correlation_circle_pc3_pc4 <- function(pca_result, variables) {
  eigenvectors <- pca_result$rotation[, c(3, 4)]
  names(eigenvectors) <- c("PC3", "PC4")
  
  variables_coords <- variables %*% eigenvectors
  
  plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), xlab = "PC3", ylab = "PC4")
  abline(h = 0, v = 0, col = "gray", lty = 2)
  arrows(0, 0, eigenvectors[,1], eigenvectors[,2], length = 0.1)
  text(eigenvectors, labels = rownames(eigenvectors), pos = 3)
  text(variables_coords, labels = colnames(variables), col = "red")
}

# Appel de la fonction pour tracer le cercle de corrélation pour PC3 et PC4
correlation_circle_pc3_pc4(pca_result, selected_columns)

# Obtenir les chargements factoriels des variables sur PC3 et PC4
loadings_pc3_pc4 <- pca_result$rotation[, c(3, 4)]
print(loadings_pc3_pc4)

# Tracer le nuage des individus 
individuals_coords_pc3_pc4 <- pca_result$x[, c(3, 4)]
plot(individuals_coords_pc3_pc4, pch = 19, col = "blue", xlab = "PC3", ylab = "PC4", main = "Nuage des individus sur PC3 et PC4")

# Compter le nombre d'individus représentés
nb_individuals <- nrow(individuals_coords_pc3_pc4)
print(nb_individuals)
# Charger les bibliothèques nécessaires
library(cluster)
library(factoextra)

# Sélectionner les composantes principales pour le clustering
pca_components <- pca_result$x[, 1:2]  # Nous utilisons les deux premières composantes principales

# Déterminer le nombre de clusters (à ajuster selon les besoins)
k <- 3

# Effectuer le clustering avec K-means sur les composantes principales
set.seed(123)  # Pour la reproductibilité des résultats
kmeans_after_pca <- kmeans(pca_components, centers = k)

# Ajouter les clusters obtenus au dataframe original
data$Cluster_after_PCA <- kmeans_after_pca$cluster

# Afficher les résultats du clustering
print(kmeans_after_pca)

# Visualisation des clusters après ACP
fviz_cluster(kmeans_after_pca, data = pca_components, geom = "point",
             ellipse.type = "convex",  # Type d'ellipse
             ellipse.level = 0.68,     # Niveau de confiance
             ggtheme = theme_minimal(),
             main = "Clustering après ACP") +
  labs(x = "Composante principale 1", y = "Composante principale 2")
###############################dendogramme##################################
# Calculer la matrice de distance entre les individus
distance_matrix <- dist(pca_components)

# Appliquer le clustering hiérarchique avec la méthode de Ward pour minimiser la variance intra-cluster
hc <- hclust(distance_matrix, method = "ward.D2")

# Tracer le dendrogramme
plot(hc, main = "Dendrogramme du clustering hiérarchique", xlab = "", sub = "", cex = 0.9)



