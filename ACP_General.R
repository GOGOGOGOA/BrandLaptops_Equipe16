###########################################################################
####################### Mise en place des données #########################
# Importer les données
data <- read.csv("Laptop2024.csv")

# Sélectionner les variables quantitatives
selected_columns <- data[, c("Price", "Rating", "num_cores", "num_threads", "ram_memory", "primary_storage_capacity", "secondary_storage_capacity", "display_size", "resolution_width", "resolution_height", "year_of_warranty")]

# Remplacer les valeurs "No information" par la valeur moyenne de la colonne "year_of_warranty" et tout convertir en numérique
selected_columns[selected_columns == "No information"] <- NA  
selected_columns <- apply(selected_columns, 2, function(x) as.numeric(as.character(x)))  
selected_columns <- apply(selected_columns, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))  

# Standardisation des données
scaled_data <- scale(selected_columns)

# Calcul des composantes principales
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

###########################################################################
######################### Premier plan factoriel ##########################

### Eboulis des valeurs propres
prop_var <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
plot(prop_var, type = "b", main = "Éboulis des valeurs propres", xlab = "Composante principale", ylab = "Proportion de variance expliquée")
print(prop_var)

# Fonction 
correlation_circle <- function(pca_result, variables, pc1, pc2) {
  eigenvectors <- pca_result$rotation[, 1:2]
  names(eigenvectors) <- c("PC1", "PC2")
  
  variables_coords <- variables %*% eigenvectors
  
  plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), xlab = "PC1", ylab = "PC2", main = "Cercle de corrélation (PC1, PC2)")
  abline(h = 0, v = 0, col = "gray", lty = 2)
  arrows(0, 0, eigenvectors[,1], eigenvectors[,2], length = 0.1)
  text(eigenvectors, labels = rownames(eigenvectors), pos = 3)
  text(variables_coords, labels = colnames(variables), col = "red")
}

# Cercle de corrélation (PC1, PC2)
correlation_circle(pca_result, selected_columns, 1, 2)
# Obtenir les chargements factoriels des variables sur PC1 et PC2
loadings <- pca_result$rotation[, 1:2]
print(loadings)

# Tracer le nuage des individus 
individuals_coords <- pca_result$x[, 1:2]
plot(individuals_coords, pch = 19, col = "blue", xlab = "PC1", ylab = "PC2", main = "Nuage des individus sur PC1 et PC2")

###########################################################################
######################## Troisième plan factoriel #########################

# Fonction pour le cercle de corrélation pour PC3 et PC4
correlation_circle_pc3_pc4 <- function(pca_result, variables, pc3, pc4) {
  eigenvectors <- pca_result$rotation[, c(pc3, pc4)]
  names(eigenvectors) <- c("PC3", "PC4")
  
  variables_coords <- variables %*% eigenvectors
  
  plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), xlab = "PC3", ylab = "PC4", main = "Cercle de corrélation (PC3, PC4)")
  abline(h = 0, v = 0, col = "gray", lty = 2)
  arrows(0, 0, eigenvectors[,1], eigenvectors[,2], length = 0.1)
  text(eigenvectors, labels = rownames(eigenvectors), pos = 3)
  text(variables_coords, labels = colnames(variables), col = "red")
}

# Appel de la fonction pour tracer le cercle de corrélation pour PC3 et PC4
correlation_circle_pc3_pc4(pca_result, selected_columns, 3, 4)

# Obtenir les chargements factoriels des variables sur PC3 et PC4
loadings_pc3_pc4 <- pca_result$rotation[, c(3, 4)]
print(loadings_pc3_pc4)

# Tracer le nuage des individus 
individuals_coords_pc3_pc4 <- pca_result$x[, c(3, 4)]
plot(individuals_coords_pc3_pc4, pch = 19, col = "blue", xlab = "PC3", ylab = "PC4", main = "Nuage des individus sur PC3 et PC4")

# Compter le nombre d'individus représentés
nb_individuals <- nrow(individuals_coords_pc3_pc4)
print(nb_individuals)
