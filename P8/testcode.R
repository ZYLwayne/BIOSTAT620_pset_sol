set.seed(2025); x <- matrix(rnorm(1000), 100, 10); x[1:5, 1:4]
cat("dim:", dim(x), "\nnrow:", nrow(x), "\nncol:", ncol(x), "\n")
y <- x + 1:100; y[1:5, 1:4]
z <- sweep(y, 2, 2*(1:10), FUN = "+"); z[1:5, 1:4]
(rowMeans(z))[1:10]
(matrix(rep(1/nrow(z), nrow(z)), nrow = 1) %*% z)[1, 1:10]

#7.1
n <- nrow(z)
m <- ncol(z)
one_n <- matrix(1, n, 1)
one_m <- matrix(1, 1, m)

mu <- (t(one_n) / n) %*% z


mu_mat <- one_n %*% mu


centered <- z - mu_mat
squared <- centered * centered


sums <- t(one_n) %*% squared  # 1 x m

sds <- as.vector((sums / (n - 1)) ^ (1/2))

# Print
sds


#7.2
column_means <- matrix(rep(1/nrow(z), nrow(z)), nrow = 1) %*% z

mean_matrix <- matrix(rep(column_means, each = nrow(z)), nrow = nrow(z))


squared_diff <- (z - mean_matrix)^2
squared_sums <- colSums(squared_diff)

column_sds <- sqrt(squared_sums / (nrow(z) - 1))

column_sds
#7.3
as.vector(sqrt(colSums((z - matrix(rep(1, nrow(z)), ncol = 1) %*% (matrix(rep(1/nrow(z), nrow(z)), nrow = 1) %*% z))^2) / (nrow(z) - 1)))

# 8
library(dslabs)
mnist <- read_mnist()

is_gray <- mnist$train$images >= 50 & mnist$train$images <= 205

gray_prop_overall <- sapply(0:9, function(d) {
  images_d <- mnist$train$images[mnist$train$labels == d, ]
  total_pixels <- length(images_d)
  gray_pixels <- sum(images_d >= 50 & images_d <= 205)
  gray_pixels / total_pixels
})

gray_prop_overall


# 9

is_gray <- mnist$train$images >= 50 & mnist$train$images <= 205
gray_per_image <- rowMeans(is_gray)

gray_avg_by_digit <- sapply(0:9, function(d) {
  mean(gray_per_image[mnist$train$labels == d])
})

gray_avg_by_digit






# 10
gray_prop_per_image <- rowMeans(mnist$train$images >= 50 & mnist$train$images <= 205)

labels <- mnist$train$labels

boxplot(gray_prop_per_image ~ labels,
        main = "Grey Pixel Proportion per Image by Digit Class",
        xlab = "Digit",
        ylab = "Grey Pixel Proportion (50–205)",
        col = "lightgray")



A <- matrix(c(1, 2, -2,
              2, 1, -5,
              1, -4, 1), nrow = 3, byrow = TRUE)

b <- c(-15, -21, 18)


solution <- solve(A, b)
cat("x =", solution[1], "\ny =", solution[2], "\nz =", solution[3])

