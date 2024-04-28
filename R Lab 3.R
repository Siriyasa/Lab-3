# Load required libraries
library(stats)
library(cluster)

# Function to simulate student features
simulate_student_features <- function(n = 100) {
  # Set the random seed
  set.seed(260923)
  
  # Generate unique student IDs
  student_ids <- seq(1, n)
  
  # Simulate student engagement
  student_engagement <- rnorm(n, mean = 50, sd = 10)
  
  # Simulate student performance
  student_performance <- rnorm(n, mean = 60, sd = 15)
  
  # Combine the data into a data frame
  student_features <- data.frame(
    student_id = student_ids,
    student_engagement = student_engagement,
    student_performance = student_performance
  )
  
  # Return the data frame
  return(student_features)
}

# Simulate student features
student_data <- simulate_student_features(n = 100)

# Perform PCA for dimensionality reduction
pca_result <- prcomp(student_data[, -1], scale. = TRUE)

# Get PCA results
pca_data <- as.data.frame(pca_result$x)

# Cluster using KMeans
kmeans_clusters <- kmeans(pca_data, centers = 3)

# Cluster using Hierarchical clustering
hierarchical_clusters <- hclust(dist(pca_data))

# Plot the PCA results with KMeans clusters
plot(pca_data[, 1:2], col = kmeans_clusters$cluster, pch = 19, 
     main = "PCA with KMeans Clustering", xlab = "PC1", ylab = "PC2")

# Plot the dendrogram from Hierarchical clustering
plot(hierarchical_clusters, main = "Dendrogram from Hierarchical Clustering")
