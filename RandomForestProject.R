#install.packages("devtools")
#devtools::install_github("r-lib/conflicted")

# Load required libraries
library(tidyverse)
library(scales)
library(randomForest)
library(caret)
library(heatmaply)

# Read data
dataset <- read_csv("/Users/JumpMan/Downloads/UH Spring 2023/Math 4322/Cardio Vascular project/heart_data 2.csv")

# Clean data
dataset <- dataset %>% 
  select(-index, -id)

# Check for null values
colSums(is.na(dataset))

# Count plot for cardio
cols <- c("#ffa600", "#bc5090")
ggplot(dataset, aes(x = factor(cardio), fill = factor(cardio))) +
  geom_bar() +
  scale_fill_manual(values = cols) +
  labs(x = "Cardio", fill = "Cardio")


# Split the data
set.seed(100)
trainIndex <- createDataPartition(dataset$cardio, p = 0.8, list = FALSE)
x_train <- dataset[trainIndex, -ncol(dataset)]
y_train <- dataset[trainIndex, ncol(dataset)] %>% pull()
x_test <- dataset[-trainIndex, -ncol(dataset)]
y_test <- dataset[-trainIndex, ncol(dataset)] %>% pull()


# Normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

x_train <- as.data.frame(lapply(x_train, normalize))
x_test <- as.data.frame(lapply(x_test, normalize))

# Convert the response variable to a factor
y_train <- as.factor(y_train)
y_test <- as.factor(y_test)

# Train the random forest classifier
R <- randomForest(x_train, y_train)

# Make predictions
y_pred <- predict(R, x_test)

# Calculate accuracy
ac1 <- mean(y_pred == y_test) * 100
print(ac1)

# Calculate precision, recall, F1-score
cm <- confusionMatrix(y_pred, y_test)
precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste0("Precision: ", precision *100))
print(paste0("Recall: ", recall * 100))
print(paste0("F1 Score: ", f1_score *100))

# Rename factor levels in the confusion matrix
cm$table <- as.table(apply(cm$table, c(1, 2), function(x) {
  cell_value <- ifelse(x == 0, "No", "Yes")
  return(cell_value)
}))
rownames(cm$table) <- c("Pred No", "Pred Yes")
colnames(cm$table) <- c("True No", "True Yes")

# Display the heatmap
heatmaply(cm$table / sum(cm$table),
          colors = colorRampPalette(c("#bc5090", "#f7f7f7", "#ffa600"))(25),
          showticklabels = c(T, T, F, F),
          legend_title = "Proportion",
          Rowv = NA, Colv = NA)

