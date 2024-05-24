###########################################################################
####################### Mise en place des données #########################

# Importer les données
data <- read.csv("Laptop2024.csv")

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
  text(eigenvectors, labels = rownames(eigenvectors), pos = 3)
  text(variables_coords, labels = colnames(variables), col = "red")
}

# Cercle de corrélation (PC1, PC2)
correlation_circle(pca_result, selected_columns)
# Obtenir les chargements factoriels des variables sur PC1 et PC2
loadings <- pca_result$rotation[, 1:2]
print(loadings)

# Tracer le nuage des individus 
individuals_coords <- pca_result$x[, 1:2]
plot(individuals_coords, pch = 19, col = "blue", xlab = "PC1", ylab = "PC2", main = "Nuage des individus sur PC1 et PC2")
