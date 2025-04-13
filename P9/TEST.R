# download data
dir.create('data')
file_list <- c('gene_counts', 'cell_coords', 'neuron_cells', 'other_cells',
               'positive_genes', 'negative_genes', 'other_genes')
for(filename in file_list) {
  download.file(paste0('https://github.com/dmcable/BIOSTAT620/raw/main/data/p9/',filename,'.rds'), 
                destfile = paste0('data/',filename,'.rds'))
}

# required packages
library(ggplot2)
library(Matrix)
library(tidyverse)

# load data
counts <- readRDS('data/gene_counts.rds') # gene counts matrix
coords <- readRDS('data/cell_coords.rds') # cell coords matrix

neuron_cells <- readRDS('data/neuron_cells.rds') # list of neuron cell barcodes.
other_cells <- readRDS('data/other_cells.rds') # list of non-neuron cell barcodes.

positive_genes <- readRDS('data/positive_genes.rds') # list of genes specific for neurons
negative_genes <- readRDS('data/negative_genes.rds') # list of genes specific for not neurons
other_genes <- readRDS('data/other_genes.rds') # selected list of other genes

all_genes <- c(positive_genes, negative_genes, other_genes) # all genes
small_counts <- as.matrix(counts[all_genes, c(neuron_cells, other_cells)]) # subset of the counts

coords <- readRDS('data/cell_coords.rds') # cell coords matrix



set.seed(2025)

coords_df <- as.data.frame(coords)
colnames(coords_df) <- c("x", "y")

ggplot(coords_df, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Cell coordinates in 2D space")



library(ggplot2)


hpca_counts <- counts["Hpca", ]


hpca_df <- data.frame(count = hpca_counts)


ggplot(hpca_df, aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") + 
  theme_minimal() +
  labs(
    title = "Expression of Hpca gene",
    x = "Hpca counts",
    y = "Number of cells"
  )

# 3
coords_subset <- coords[match(colnames(small_counts), rownames(coords)), ]
hpca_counts <- small_counts["Hpca", ]


hpca_df <- data.frame(
  dim_1 = coords_subset[, 1],
  dim_2 = coords_subset[, 2],
  count = hpca_counts
)

max_count <- 20
library(ggplot2)

ggplot(hpca_df, aes(x = dim_1, y = dim_2, color = pmin(count, max_count))) +
  geom_point(size = 1) +
  scale_color_gradient(low = "gray90", high = "red", limits = c(0, max_count)) +
  theme_minimal() +
  labs(
    title = "Spatial plot of Hpca gene expression",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Hpca counts"
  )

# 4
coords_subset <- coords[match(colnames(small_counts), rownames(coords)), ]

cell_type <- ifelse(colnames(small_counts) %in% neuron_cells, "Neuron", "Non-neuron")

plot_df <- data.frame(
  dim_1 = coords_subset[, 1],
  dim_2 = coords_subset[, 2],
  cell_type = cell_type
)

library(ggplot2)

ggplot(plot_df, aes(x = dim_1, y = dim_2, color = cell_type)) +
  geom_point(size = 1) +
  theme_minimal() +
  labs(
    title = "Spatial plot of neuron and non-neuron cells",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Cell Type"
  ) +
  scale_color_manual(values = c("Neuron" = "red", "Non-neuron" = "blue"))


# 5
library(fields)
library(FNN)

dist_matrix <- rdist(plot_df[, c("dim_1", "dim_2")])

k <- 25

knn_result <- get.knn(plot_df[, c("dim_1", "dim_2")], k = k)

first_cell_neighbors <- knn_result$nn.index[1, ]

library(ggplot2)

ggplot(plot_df, aes(x = dim_1, y = dim_2)) +
  geom_point(color = "grey80", size = 1) +
  geom_point(data = plot_df[1, , drop = FALSE], color = "blue", size = 3) + 
  geom_point(data = plot_df[first_cell_neighbors, , drop = FALSE], color = "red", size = 2) +  
  theme_minimal() +
  labs(
    title = "First cell and its 25 nearest neighbors",
    x = "Dimension 1",
    y = "Dimension 2"
  )

# 6
library(FNN)
nn_res <- get.knn(plot_df[, c("dim_1", "dim_2")], k = 25)

neighbor_indices <- nn_res$nn.index
neuron_prop <- apply(neighbor_indices, 1, function(neighbors) {
  mean(plot_df$cell_type[neighbors] == "neuron")
})


plot_df$neuron_prop <- neuron_prop

library(ggplot2)

ggplot(plot_df, aes(x = dim_1, y = dim_2, color = neuron_prop)) +
  geom_point(size = 1) +
  scale_color_gradient(low = "gray90", high = "blue") +
  theme_minimal() +
  labs(
    title = "Proportion of Neuron Neighbors",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Neuron\nProportion"
  )

# 7
loess_fit <- loess(neuron_prop ~ dim_1 + dim_2, data = plot_df, degree = 1, span = 0.05)

plot_df$neuron_prop_smooth <- predict(loess_fit)

library(ggplot2)
ggplot(plot_df, aes(x = dim_1, y = dim_2, color = neuron_prop_smooth)) +
  geom_point(size = 1) +
  scale_color_gradient(low = "gray90", high = "blue") +
  theme_minimal() +
  labs(
    title = "Smoothed Proportion of Neuron Neighbors",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Smoothed\nNeuron Proportion"
  )

library(ggplot2)

# 8
plot_data <- plot_df %>%
  filter(dim_2 > 50, dim_2 < 3000)

ggplot(plot_data, aes(x = dim_1)) +
  geom_line(aes(y = neuron_prop, color = "Neighbor Proportion")) +
  geom_line(aes(y = neuron_prop_smooth, color = "Smooth Fit")) +
  labs(
    title = "1D Slice of Smoothed Neuron Proportion",
    x = "Dimension 1",
    y = "Proportion"
  ) +
  scale_color_manual(values = c("Neighbor Proportion" = "blue", "Smooth Fit" = "red")) +
  theme_minimal()

# 9
pca_res <- prcomp(t(as.matrix(small_counts)), scale. = TRUE)

total_counts <- colSums(small_counts)

pca_df <- data.frame(
  PC1 = pca_res$x[, 1],
  Total_Count = total_counts
)


ggplot(pca_df, aes(x = Total_Count, y = PC1)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "PCA: PC1 vs Total Cellular Count",
    x = "Total Cellular Count",
    y = "First Principal Component (PC1)"
  )

# 10
cell_sums <- colSums(small_counts)

normalized_counts <- sweep(small_counts, 2, cell_sums, "/")

normalized_pca <- prcomp(t(as.matrix(normalized_counts)), scale. = TRUE)

pve <- normalized_pca$sdev^2 / sum(normalized_pca$sdev^2)

pve_df <- data.frame(
  PC = 1:length(pve),
  PVE = pve
) %>% filter(PC <= 500)

library(ggplot2)

ggplot(pve_df, aes(x = PC, y = PVE)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") + # 5% threshold
  theme_minimal() +
  labs(
    title = "Percent Variance Explained (PVE) After Normalization",
    x = "Principal Component",
    y = "Percent Variance Explained"
  )



# 11
coords_subset <- coords[colnames(small_counts), ]
colnames(coords_subset) <- c("dim1", "dim2")

pca_scores <- as.data.frame(normalized_pca$x)
plot_data <- coords_subset %>%
  mutate(
    PC1 = pca_scores$PC1,
    PC2 = pca_scores$PC2,
    PC3 = pca_scores$PC3,
    PC4 = pca_scores$PC4
  )

plot_pc <- function(pc_number) {
  ggplot(plot_data, aes(x = dim1, y = dim2, color = !!sym(paste0("PC", pc_number)))) +
    geom_point(size = 1) +
    scale_color_gradient(low = "gray90", high = "blue") +
    theme_minimal() +
    labs(
      title = paste("Spatial Plot of PC", pc_number),
      x = "Dimension 1",
      y = "Dimension 2",
      color = paste("PC", pc_number)
    )
}
plot_pc(1)
plot_pc(2)
plot_pc(3)
plot_pc(4)

# 12
loadings <- normalized_pca$rotation
loadings_20 <- loadings[, 1:20]

positive_weights <- colMeans(loadings_20[positive_genes, ])
negative_weights <- colMeans(loadings_20[negative_genes, ])
other_weights <- colMeans(loadings_20[other_genes, ])

weight_df <- data.frame(
  PC = rep(1:20, times = 3),
  AverageWeight = c(positive_weights, negative_weights, other_weights),
  GeneSet = rep(c("Positive", "Negative", "Other"), each = 20)
)

ggplot(weight_df, aes(x = PC, y = AverageWeight, color = GeneSet)) +
  geom_point() +
  geom_line() +  
  labs(
    title = "Average Gene Weight per PC",
    x = "Principal Component (PC)",
    y = "Average Weight",
    color = "Gene Set"
  ) +
  theme_minimal()

# 13
loadings_5 <- loadings[, 1:5]

compute_summary <- function(genes, loadings_matrix) {
  means <- colMeans(loadings_matrix[genes, ])
  sds <- apply(loadings_matrix[genes, ], 2, sd)
  n <- length(genes)
  se <- sds / sqrt(n)
  
  data.frame(
    PC = 1:5,
    AverageWeight = means,
    SE = se
  )
}

positive_summary <- compute_summary(positive_genes, loadings_5)
positive_summary$GeneSet <- "Positive"

negative_summary <- compute_summary(negative_genes, loadings_5)
negative_summary$GeneSet <- "Negative"

other_summary <- compute_summary(other_genes, loadings_5)
other_summary$GeneSet <- "Other"

summary_df <- rbind(positive_summary, negative_summary, other_summary)

ggplot(summary_df, aes(x = PC, y = AverageWeight, color = GeneSet)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = AverageWeight - SE, ymax = AverageWeight + SE), width = 0.2) +
  labs(
    title = "Average Gene Weight with SE (PC1 to PC5)",
    x = "Principal Component (PC)",
    y = "Average Weight",
    color = "Gene Set"
  ) +
  theme_minimal()


