library(xgboost)
library(readr)
library(dplyr)
library(Metrics)
library(caret)
library(MLmetrics)
library(foreach)
library(doParallel)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(pandoc)
library(data.table)
library(forecast)

# Mandatory to avoid random convertion in exponential format, which screws with MAPE calculation
options(scipen = 20)

# One-Hot-Encoding
one_hot_encoding <- function(data, category) {
  data <- data %>%
    mutate(across({{ category }}, ~ ifelse(. == unique(data %>% pull({{ category }})), 1, 0)))

  return(data)
}


delete_existing_html <- function(html_file_path) {
  if (file.exists(html_file_path)) {
    file.remove(html_file_path)
  }
}

mape_score <- function(actual_values, predicted_values) {
  ele_count <- length(actual_values)
  mape <- (1 / ele_count) * sum(abs((actual_values - predicted_values) / actual_values)) * 100
  # View(actual_values)
  # View(predicted_values)
  # while (1 == 1) {
  #  Sys.sleep(1)
  # }
  # cat("Actual values count: ", length(actual_values), "predicted values count: ", length(predicted_values), "actual values sum: ", sum(abs(actual_values)), "predicted values sum: ", sum(abs(predicted_values)), " MAPE: ", mape)
  # cat("Actual values = ", actual_values, "predicted values: ", predicted_values)
  return(mape)
}

# Settings
count_iterations <- 2
selected_scenario <- 1

default_max_depth <- 6
default_eta <- 0.3
default_nrounds <- 10
total_iterations <- 1


mae_list <- list()
rmse_list <- list()
mape_list <- list()

# Path html files
html_file_path_statistic_table <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/statistic_table.html"

# Create empty data frame
results_df <- data.table(
  Iteration = integer(),
  Scenario = integer(),
  Featureset = character(),
  RunTime = numeric(),
  Depth = integer(),
  NRounds = integer(),
  LearningRate = numeric(),
  ProgRevTotal2018 = numeric(),
  ActualRevTotal2018 = numeric(),
  PercentageError = numeric(),
  RMSE = numeric(),
  # MSE = numeric(),
  MAE = numeric(),
  SMAPE = numeric()
)

results_list <- list()

file_path <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Data Source/Pakistan-Largest-Ecommerce-Dataset-Cleansed.csv"
data <- read_csv(file_path, skip = 1, col_names = c("incrementId", "itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId"))

# Prepare data
data <- data %>%
  mutate(
    itemId = as.numeric(itemId),
    createdAt = createdAt,
    price = as.numeric(price),
    qtyOrdered = as.numeric(qtyOrdered),
    grandTotal = as.numeric(grandTotal),
    incrementId = as.numeric(incrementId),
    discountAmount = as.numeric(discountAmount),
    workingDate = workingDate,
    MV = as.numeric(gsub(",", "", MV)),
    Year = as.numeric(Year),
    Month = as.numeric(Month),
    customerId = as.numeric(customerId),
    customerSince = parse_date(customerSince, format = "%Y-%m"),
    MY = parse_date(MY, format = "%b-%y")
  )

data <- data %>%
  one_hot_encoding(categoryName) %>%
  one_hot_encoding(BIStatus) %>%
  one_hot_encoding(status) %>%
  one_hot_encoding(paymentMethod) %>%
  one_hot_encoding(FY)

scenario_list <- c(1, 2, 3)
featureset <- c("ip", "short")
max_depth_list <- c(3, 5, 8)
learn_rate_list <- c(0.1, 0.2, 0.3)
nrounds_list <- c(10, 25, 50, 100)

training_data_scenario_one <- data %>%
  filter((format(createdAt, "%Y") %in% c("2016", "2017")) &
    (format(createdAt, "%m") %in% c("07", "08", "06", "05", "04", "03", "02", "01", "09", "10", "11", "12")))
prediction_data_scenario_one <- data %>%
  filter((format(createdAt, "%Y") == "2018") &
    (format(createdAt, "%m") %in% c("07", "08", "06", "05", "04", "03", "02", "01", "09", "10", "11", "12")))

training_data_scenario_two <- data %>%
  filter((format(createdAt, "%Y") %in% c("2016", "2017")) &
    (format(createdAt, "%m") %in% c("07", "08", "06", "05", "04", "03", "02", "01")))
prediction_data_scenario_two <- data %>%
  filter((format(createdAt, "%Y") == "2018") &
    (format(createdAt, "%m") %in% c("07", "08", "06", "05", "04", "03", "02", "01")))

training_data_scenario_three <- data %>%
  filter((format(createdAt, "%Y") %in% c("2016", "2017")) &
    (format(createdAt, "%m") %in% c("07", "08")))
prediction_data_scenario_three <- data %>%
  filter((format(createdAt, "%Y") == "2018") &
    (format(createdAt, "%m") %in% c("07", "08")))


execute_scenarios <- function(selected_scenario, selected_featureset, max_depth, learn_rate, nrounds, total_iterations_arg) {
  cat("Iteration:", total_iterations_arg, " Scenario: ", selected_scenario, " FeatureSet: ", selected_featureset, "Max Depth: ", max_depth, "Learn Rate: ", learn_rate, "NRounds", nrounds, "\n")

  # Select scenario
  switch(selected_scenario,
    {
      # Szenario 1: Training with data for entire 2016 and 2017, prediction for entire 2018
      if (selected_scenario == 1) {
        training_data <- training_data_scenario_one

        prediction_data <- prediction_data_scenario_one
      }
    },
    {
      # Szenario 2: Training with data with partial 2016 and 2017 (until 31.08. each), prediction until 31.08.2018
      if (selected_scenario == 2) {
        training_data <- training_data_scenario_two

        prediction_data <- prediction_data_scenario_two
      }
    },
    {
      # Szenario 3: Training with data 2016 and 2017 (july and august only), prediction 2018 only july and august
      if (selected_scenario == 3) {
        training_data <- training_data_scenario_three

        prediction_data <- prediction_data_scenario_three
      }
    }
  )

  # Convert createdAt, workingDate and customerSince into numerical value
  tryCatch(
    {
      training_data$createdAt <- as.numeric(training_data$createdAt)
      prediction_data$createdAt <- as.numeric(prediction_data$createdAt)
      training_data$workingDate <- as.numeric(training_data$workingDate)
      prediction_data$workingDate <- as.numeric(prediction_data$workingDate)
      training_data$customerSince <- as.numeric(training_data$customerSince)
      prediction_data$customerSince <- as.numeric(prediction_data$customerSince)
      training_data$MY <- as.numeric(training_data$MY)
      prediction_data$MY <- as.numeric(prediction_data$MY)
    },
    error = function(e) {
      print("Error: Convert createdAt, workingDate and customerSince into numerical value")
      print(e)
    }
  )

  features <- c("incrementId", "itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId")
  response <- "grandTotal"

  # Select featureset
  switch(selected_featureset,
    "short" = {
      # Featureset short
      features <- c("incrementId", "itemId", "createdAt", "price", "qtyOrdered", "grandTotal", "MV", "customerId")
    },
    "ip" = {
      # Featureset ip
      features <- c("incrementId", "itemId", "status", "createdAt", "price", "qtyOrdered", "grandTotal", "categoryName", "discountAmount", "paymentMethod", "BIStatus", "MV", "Month", "customerSince", "MY", "FY", "customerId")
    }
  )

  starting_time <- Sys.time()

  tryCatch(
    {
      # Create DMatrix objects for training and prediction
      dtrain <- xgb.DMatrix(data = as.matrix(training_data[, features]), label = training_data[[response]])
      dpred <- xgb.DMatrix(data = as.matrix(prediction_data[, features]))
    },
    error = function(e) {
      print("Error: Create DMatrix objects for training and prediction")
      print(e)
    }
  )


  tryCatch(
    {
      # Define XGBoost parameters
      params <- list(
        objective = "reg:squarederror",
        max_depth = max_depth,
        eta = learn_rate,
        nthread = 12
      )

      # Train XGB model
      xgboost_model <- xgb.train(params, dtrain, nrounds = nrounds)

      # Use model to predict for 2018
      predictions <- predict(xgboost_model, dpred)

      # Extract actual values for 2018
      actual_values <- prediction_data$grandTotal

      # Replace null values with small values
      actual_values <- replace(actual_values, actual_values == 0, 1)

      # Limit decimal place to 3, to prevent weird behaviour and values during MAPE calculation
      actual_values <- round(actual_values, 3)

      # Calculate total and predicted revenue
      predicted_total_revenue_2018 <- sum(predictions)
      actual_total_revenue_2018 <- sum(actual_values)

      # Calculate PercentageError PE
      pe_value <- ((actual_total_revenue_2018 - predicted_total_revenue_2018) / actual_total_revenue_2018) * 100

      # Calculate RMSE
      rmse_value <- rmse(actual_values, predictions)

      # Calculate MSE
      # mse_value <- mse(actual_values, predictions)

      # Calculate MAE
      mae_value <- mae(actual_values, predictions)

      # Calculate SMAPE
      # mape_value <- mape_score(actual_values, predictions)
      # mape_value <- MAPE(predictions, actual_values)
      smape_value <- smape(actual_values, predictions) * 100


      end_time <- Sys.time()
      run_time <- end_time - starting_time

      results_df <<- rbind(results_df, list(
        Iteration = total_iterations_arg,
        Scenario = selected_scenario,
        Featureset = selected_featureset,
        RunTime = round(as.numeric(run_time, units = "secs"), 3),
        Depth = max_depth,
        NRounds = nrounds,
        LearningRate = learn_rate,
        ProgRevTotal2018 = round(predicted_total_revenue_2018, 2),
        ActualRevTotal2018 = round(actual_total_revenue_2018, 2),
        PercentageError = round(pe_value, 2),
        RMSE = round(rmse_value, 2),
        # MSE = round(mse_value, 2),
        MAE = round(mae_value, 2),
        SMAPE = round(smape_value, 2)
      ))
    },
    error = function(e) {
      print("Error: Define XGBoost parameters")
      print(e)
    }
  )

  return(total_iterations_arg + 1)
}

# Execute functions
foreach(selected_scenario = scenario_list) %do% {
  foreach(selected_featureset = featureset) %do% {
    foreach(selected_learn_rate = learn_rate_list) %do% {
      foreach(selected_nrounds = nrounds_list) %do% {
        foreach(selected_max_depth = max_depth_list) %do% {
          total_iterations <- execute_scenarios(selected_scenario, selected_featureset, selected_max_depth, selected_learn_rate, selected_nrounds, total_iterations)
        }
      }
    }
  }
}


# Delete existing files
delete_existing_html(html_file_path_statistic_table)

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
      results_df$Scenario,
      results_df$Featureset,
      results_df$RunTime,
      results_df$Depth,
      results_df$NRounds,
      results_df$LearningRate,
      results_df$ProgRevTotal2018,
      results_df$ActualRevTotal2018,
      results_df$PercentageError,
      results_df$RMSE,
      results_df$MAE,
      results_df$SMAPE
    ),
    align = c("left", "center"),
    fill = list(color = c("#F0F0F0", "#FFFFFF")),
    font = list(color = c("#000000"))
  )
)

# Add sort button
sort_buttons <- list()
for (col_index in 1:ncol(results_df)) {
  col_name <- colnames(results_df)[col_index]
  sort_buttons <- c(
    sort_buttons,
    list(
      args = list(list(order = list(list(column = col_index, ascending = TRUE)))),
      label = paste(col_name, "aufsteigend"),
      method = "sort"
    ),
    list(
      args = list(list(order = list(list(column = col_index, ascending = FALSE)))),
      label = paste(col_name, "absteigend"),
      method = "sort"
    )
  )
}

# Add interactive sort
table_plot <- table_plot %>%
  layout(
    updatemenus = list(
      list(
        buttons = sort_buttons,
        direction = "down",
        showactive = FALSE,
        type = "buttons",
        x = 0.05,
        xanchor = "left",
        y = 1.1,
        yanchor = "top"
      )
    )
  )


saveWidget(as_widget(table_plot), file = html_file_path_statistic_table)
