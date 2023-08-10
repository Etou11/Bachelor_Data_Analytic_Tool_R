library(xgboost)
library(readr)
library(dplyr)
library(Metrics)
library(MLmetrics)
library(foreach)
library(doParallel)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(pandoc)
library(caret)
library(pROC)

# Settings
count_iterations <- 2
selected_scenario <- 1

settings_default_nthread <- 6

default_max_depth <- 6
default_eta <- 0.3
default_nrounds <- 10


mae_list <- list()
rmse_list <- list()
mape_list <- list()

# Path html files
html_file_path_statistic <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/statistic.html"
html_file_path_statistic_table <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/statistic_table.html"
html_file_path_total <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/total.html"

## Functions

# Delete existing html
delete_existing_html <- function(html_file_path) {
  if (file.exists(html_file_path)) {
    file.remove(html_file_path)
  }
}

graphical_output <- function() {
  # Delete existing files
  delete_existing_html(html_file_path_statistic_table)

  # Total transactions by period
  transaction_count <- data %>%
    group_by(Year, Month) %>%
    summarize(Total_Transactions = n())

  # Total revenue by period
  revenue_by_period <- data %>%
    group_by(Year, Month) %>%
    summarize(Total_Revenue = sum(grandTotal))

  # Create interactive table with plotly
  table_plot <- plot_ly(
    data = results_df,
    type = "table",
    header = list(
      values = colnames(results_df),
      align = c("left", "center"),
      fill = list(color = "#119DFF"),
      font = list(color = "white", family = "Arial", size = 12)
    ),
    cells = list(
      values = list(
        results_df$Iteration,
        results_df$RunTime,
        results_df$Depth,
        results_df$NRounds,
        results_df$LearningRate,
        results_df$Gesamtumsatz2016,
        results_df$Gesamtumsatz2017,
        results_df$ProgRevTotal2018,
        results_df$ActualRevTotal2018,
        results_df$RMSE,
        results_df$MAE,
        results_df$MAPE
      ),
      align = c("left", "center"),
      fill = list(color = c("#F0F0F0", "#FFFFFF")),
      font = list(color = c("#000000"))
    )
  )

  saveWidget(as_widget(table_plot), html_file_path_statistic_table)
}

# Create empty results data frame
results_df <- data.frame(
  Iteration = integer(),
  RunTime = numeric(),
  Depth = integer(),
  LearningRate = numeric(),
  Gesamtumsatz2016 = numeric(),
  Gesamtumsatz2017 = numeric(),
  ProgRevTotal2018 = numeric(),
  ActualRevTotal2018 = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

file_path <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Data Source/Pakistan-Largest-Ecommerce-Dataset-Cleansed.csv"
data <- read_csv(file_path, skip = 1, col_names = c("itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "incrementId", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId"))

# Prepare data
data <- data %>%
  mutate(
    incrementId = as.numeric(incrementId),
    itemId = as.numeric(itemId),
    createdAt = as.numeric(createdAt),
    price = as.numeric(price),
    qtyOrdered = as.numeric(qtyOrdered),
    grandTotal = as.numeric(grandTotal),
    discountAmount = as.numeric(discountAmount),
    workingDate = as.numeric(workingDate),
    MV = as.numeric(gsub(",", "", MV)),
    Year = as.numeric(Year),
    Month = as.numeric(Month),
    customerId = as.numeric(customerId),
    customerSince = parse_date(customerSince, format = "%Y-%m"),
    MY = parse_date(MY, format = "%b-%y")
  )


# Select relevant columns
columns_to_consider <- c("incrementId", "itemId", "createdAt", "price", "qtyOrdered", "grandTotal", "discountAmount", "customerId", "Year", "Month", "workingDate")
data_selected <- data %>% select(all_of(columns_to_consider))

# Convert date into numeric (unix)
data_selected <- data_selected %>%
  mutate(createdAt = as.numeric(as_datetime(createdAt)))
data_selected <- data_selected %>%
  mutate(workingDate = as.numeric(as_datetime(workingDate)))

# Split data by year
data_2016 <- data_selected %>% filter(Year == 2016 & (Month == 7 | Month == 8))
data_2017 <- data_selected %>% filter(Year == 2017 & (Month == 7 | Month == 8))
data_2018 <- data_selected %>% filter(Year == 2018 & (Month == 7 | Month == 8))

# Check columns for non-numeric columns
non_numeric_columns <- sapply(data_selected, function(x) !is.numeric(x))

# Print names of non-numeric columns
print(names(data_selected)[non_numeric_columns])


# Greate gradient boosting model
gbm <- xgboost(
  data = as.matrix(rbind(data_2016[, -6], data_2017[, -6])),
  label = c(data_2016$grandTotal, data_2017$grandTotal),
  nrounds = 100
)


epsilon <- 1e-10 # Small epsilon value

rmse <- c()
mse <- c()
mae <- c()
mape <- c()

predictions_value <- c()
accuracy <- c()

# Execute cross validation
folds <- createFolds(data_2018$grandTotal, k = 3)

for (fold in folds) {
  train_data <- rbind(data_2016, data_2017)[-fold, ]
  test_data <- rbind(data_2016, data_2017)[fold, ]


  # Partition training data
  positive_examples <- train_data[train_data$grandTotal > 0, ]
  negative_examples <- train_data[train_data$grandTotal == 0, ]

  # Check positive & negative examples
  if (nrow(negative_examples) > nrow(positive_examples)) {
    # Execute data balance
    balanced_negative_examples <- sample_n(negative_examples, size = nrow(positive_examples))
    balanced_train_data <- rbind(positive_examples, balanced_negative_examples)
  } else {
    balanced_train_data <- train_data
  }

  # Train the model
  gbm <- xgboost(data = as.matrix(train_data[, -6]), label = train_data$grandTotal, nrounds = 100)

  # Make predictions for the test set
  predictions <- c(predict(gbm, as.matrix(test_data[, -6])))
  predictions_value <- c(predictions_value, sum(predictions))

  # Calculate accuracy and handle cases with true grand total of zero
  accuracy <- c(accuracy, mean(abs(predictions - test_data$grandTotal) / pmax(test_data$grandTotal, epsilon)))

  # Calculate RMSE
  rmse_fold <- sqrt(mean((predictions - test_data$grandTotal)^2))
  rmse <- c(rmse, rmse_fold)

  # Calculate MSE
  mse_fold <- mean((predictions - test_data$grandTotal)^2)
  mse <- c(mse, mse_fold)

  # Calculate MAE
  mae_fold <- mean(abs(predictions - test_data$grandTotal))
  mae <- c(mae, mae_fold)

  # Calculate MAPE
  mape_fold <- MAPE(test_data$grandTotal, predictions)
  mape <- c(mape, mape_fold)
}

actual_total_revenue_2018 <- sum(data_2018$grandTotal)
prediction_total_revenue_2018 <- sum(predictions_value)
mean_accuracy <- mean(accuracy)

cat("Actual total revenue for 2018: ", actual_total_revenue_2018, " predicted total revenue for 2018: ", prediction_total_revenue_2018, "\n", "Mean Accuracy is: ", mean_accuracy, " mean RMSE: ", mean(rmse), " mean MSE: ", mean(mse), " mean MAE: ", mean(mae), "mean MAPE: ", mean(mape), "\n")

for (i in 1:length(accuracy)) {
  cat("Accuracy for Fold", i, ":", accuracy[i], " with RMSE: ", rmse[i], " MSE: ", mse[i], " MAE: ", mae[i], "MAPE: ", mape[i], "predicted revenue: ", predictions_value[i], "\n")
}
