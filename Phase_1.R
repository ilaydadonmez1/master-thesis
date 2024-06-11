# Install necessary packages
install.packages("motifr")
install.packages("igraph")
install.packages("statnet")
install.packages("ggplot2")
install.packages("corrplot")

# Load necessary libraries
library(rlang)
library(fs)
library(igraph)
library(statnet)
library(motifr)
library(ggplot2)
library(corrplot)

# Read the table and sort by DSM
table <- read.csv('/Users/ilaydadonmez/Desktop/R/R table.csv', sep=';')
table <- table[order(table$DSM),]

# Get file paths
file_paths <- sort(fs::dir_ls("/Users/ilaydadonmez/Desktop/R/DSM - CSV Files"))

# Initialize lists to store results
file_contents <- list()
modularity_list <- list()
edges_list <- list()
mutual_list <- list()
cycle3_list <- list()
cycle4_list <- list()
cycle5_list <- list()
istar4_list <- list()
istar5_list <- list()
twopath_list <- list()
ttriple_list <- list()
ctriple_list <- list()
triangle_list <- list()
transitive_list <- list()
transitiveties_list <- list()

# Loop through file paths
for (i in seq_along(file_paths)) {
  # Read the adjacency matrix
  ig_matrix <- as.matrix(read.table(file_paths[[i]], sep=";", header=FALSE))
  
  # Ensure the matrix is numeric
  ig_matrix <- apply(ig_matrix, 2, as.numeric)
  
  # Check if ig_matrix is a valid matrix
  if (!is.matrix(ig_matrix)) {
    stop("The input data is not a valid matrix.")
  }
  
  # Print the matrix to check its contents
  print(paste("Processing file:", file_paths[[i]]))
  print("Adjacency matrix:")
  print(ig_matrix)
  
  # Create graph from adjacency matrix
  ig <- graph_from_adjacency_matrix(ig_matrix, mode="directed", weighted=TRUE)
  
  # Compute components and modularity
  community <- components(ig)
  membership <- community$membership
  modularity_score <- modularity(ig, membership = membership)
  modularity_score <- round(modularity_score, 2)
  
  # Store results
  file_contents[[i]] <- ig
  modularity_list[[i]] <- modularity_score
  
  # Convert to network object
  g2CommunicationNetwork <- as.network(ig_matrix, matrix.type='adjacency', directed=TRUE, ignore.eval=FALSE, names.eval='weight')
  
  # Print the network object to check its contents
  print("Network object:")
  print(g2CommunicationNetwork)
  
  # Get network summary statistics
  g2CommunicationNetwork.obs <- summary(g2CommunicationNetwork ~ edges + mutual + cycle(3:5) + istar(4:5) + twopath + ctriple + ttriple + triangle + transitive + transitiveties)
  
  # Store network statistics
  edges_list <- c(edges_list, g2CommunicationNetwork.obs["edges"])
  mutual_list <- c(mutual_list, g2CommunicationNetwork.obs["mutual"])
  cycle3_list <- c(cycle3_list, g2CommunicationNetwork.obs["cycle3"])
  cycle4_list <- c(cycle4_list, g2CommunicationNetwork.obs["cycle4"])
  cycle5_list <- c(cycle5_list, g2CommunicationNetwork.obs["cycle5"])
  istar4_list <- c(istar4_list, round(g2CommunicationNetwork.obs["istar4"], 2))
  istar5_list <- c(istar5_list, round(g2CommunicationNetwork.obs["istar5"], 2))
  twopath_list <- c(twopath_list, g2CommunicationNetwork.obs["twopath"])
  ttriple_list <- c(ttriple_list, g2CommunicationNetwork.obs["ttriple"])
  ctriple_list <- c(ctriple_list, g2CommunicationNetwork.obs["ctriple"])
  triangle_list <- c(triangle_list, g2CommunicationNetwork.obs["triangle"])
  transitive_list <- c(transitive_list, g2CommunicationNetwork.obs["transitive"])
  transitiveties_list <- c(transitiveties_list, g2CommunicationNetwork.obs["transitiveties"])
}

# Add results to table
table$Mod_2 <- modularity_list
table$Mod <- ifelse(table$Mod_2 < 0.5, "Low", "High")
table$Edges <- edges_list
table$Mutual <- mutual_list
table$Cycle3 <- cycle3_list
table$Cycle4 <- cycle4_list
table$Cycle5 <- cycle5_list
table$Istar4 <- istar4_list
table$Istar5 <- istar5_list
table$Twopath <- twopath_list
table$Ctriple <- ctriple_list
table$Ttriple <- ttriple_list 
table$Triangle <- triangle_list 
table$Transitive <- transitive_list
table$Transitiveties <- transitiveties_list

# Convert table to data frame
table.df <- as.data.frame.matrix(table)

# Ensure numeric columns are correctly formatted
numeric_columns <- c("Mod_2", "Edges", "Mutual", "Cycle3", "Cycle4", "Cycle5", "Istar4", "Istar5", "Twopath", "Ctriple", "Ttriple", "Triangle", "Transitive", "Transitiveties")
table.df[numeric_columns] <- lapply(table.df[numeric_columns], function(x) as.numeric(as.character(x)))

# Ensure factor columns are correctly formatted
table.df$Ind_2 <- factor(table.df$Ind_2)
table.df$Gran_2 <- factor(table.df$Gran_2)
table.df$Mod_2 <- factor(table.df$Mod_2)

# Plotting
ggplot(table.df, aes(x = Ind)) +
  geom_bar() +
  labs(x = "Industry", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Industry Distribution by Count")

ggplot(table.df, aes(x = Ind, fill = Mod)) +
  geom_bar(position = "dodge") +
  labs(x = "Industry", y = "Count") +
  scale_fill_manual(values = c("Low" = "#9CC28C", "High" = "#8CB0C2")) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Industry Distribution by Modularity")

ggplot(table.df, aes(x = Ind, fill = Gran)) +
  geom_bar(position = "dodge") +
  labs(x = "Industry", y = "Count") +
  scale_fill_manual(values = c("Low" = "#FCC489", "High" = "#FC9089")) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Industry Distribution by Granularity")

ggplot(table.df, aes(x = Size, y = Mod_2, color = Mod)) +
  geom_point() +
  labs(x = "Size of the DSM", y = "Modularity") +
  ggtitle("Color-Coded Scatter Plot: Size vs. Modularity") +
  scale_color_manual(values = c("Low" = "purple", "High" = "blue"))

ggplot(table.df, aes(x = Size, y = Gran_2, color = Gran)) +
  geom_point() +
  labs(x = "Size of the DSM", y = "Granularity") +
  ggtitle("Color-Coded Scatter Plot: Size vs. Granularity") +
  scale_color_manual(values = c("Low" = "purple", "High" = "blue"))

# Correlation plot
numeric_vars <- table.df[, sapply(table.df, is.numeric)]
correlation_matrix <- cor(numeric_vars)
my_color_palette <- colorRampPalette(c("#FC9098", "#FCC489", "#9CC28C"))(100)
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, col = my_color_palette, main = "Correlation Matrix", mar = c(0, 0, 2, 0))

# Additional scatter plots
ggplot(table.df, aes(x = Size, y = Cycle3)) +
  geom_point() +
  labs(x = "Size of the DSM", y = "Cycle3") +
  ggtitle("Size vs. Cycle3 Scatter Plot")

ggplot(table.df, aes(x = Size, y = Transitiveties)) +
  geom_point() +
  labs(x = "Size of the DSM", y = "Transitiveties") +
  ggtitle("Size vs. Transitiveties Scatter Plot")
