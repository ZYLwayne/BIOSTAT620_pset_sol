
library(ranger)
library(tidyverse)


dat <- readRDS("pset-10-mnist.rds")


X_train <- dat$train$images
y_train <- as.factor(dat$train$labels)
X_test <- dat$test$images


train_df <- as.data.frame(X_train)
train_df$label <- y_train


set.seed(123)
rf_model <- ranger(
  formula = label ~ .,
  data = train_df,
  num.trees = 100,          
  importance = "impurity", 
  probability = FALSE,     
  num.threads = parallel::detectCores()  
)


test_df <- as.data.frame(X_test)
digit_predictions <- predict(rf_model, data = test_df)$predictions
digit_predictions <- as.integer(as.character(digit_predictions))

saveRDS(digit_predictions, file = "digit_predictions.rds")



preds <- readRDS("digit_predictions.rds")
head(preds)
str(preds)

imp_df <- data.frame(
  Pixel = names(rf_model$variable.importance),
  Importance = rf_model$variable.importance
) %>%
  arrange(desc(Importance)) %>%
  slice(1:20)

imp_df %>%
  ggplot(aes(x = reorder(Pixel, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Important Pixels", x = "Pixel Index", y = "Importance")
