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

# One-Hot-Encoding for payment method
payment_mapping <- function(data) {
  data <- data %>%
    mutate(across(paymentMethod, ~ ifelse(. == unique(data$paymentMethod), 1, 0)))

  return(data)
}

# One-Hot-Encoding for status
status_mapping <- function(data) {
  data <- data %>%
    mutate(across(status, ~ ifelse(. == unique(data$status), 1, 0)))

  return(data)
}

# One-Hot-Encoding for categoryName
categoryName_mapping <- function(data) {
  data <- data %>%
    mutate(across(categoryName, ~ ifelse(. == unique(data$categoryName), 1, 0)))

  return(data)
}


delete_existing_html <- function(html_file_path) {
  if (file.exists(html_file_path)) {
    file.remove(html_file_path)
  }
}

# Settings
count_iterations <- 2
selected_scenario <- 1

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

# Create empty data frame
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

file_path <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Data Source/Pakistan Largest Ecommerce Dataset.csv"
data <- read_csv(file_path, skip = 1, col_names = c("itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "incrementId", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId"))

# Prepare data
data <- data %>%
  mutate(
    itemId = as.numeric(itemId),
    createdAt = parse_date(createdAt, format = "%m/%d/%Y"),
    price = as.numeric(price),
    qtyOrdered = as.numeric(qtyOrdered),
    grandTotal = as.numeric(grandTotal),
    incrementId = as.numeric(incrementId),
    discountAmount = as.numeric(discountAmount),
    workingDate = parse_date(workingDate, format = "%m/%d/%Y"),
    MV = as.numeric(gsub(",", "", MV)),
    Year = as.numeric(Year),
    Month = as.numeric(Month),
    customerId = as.numeric(customerId),
    customerSince = parse_date(customerSince, format = "%Y-%m"),
    MY = parse_date(MY, format = "%b-%y")
  )

data <- payment_mapping(data)
data <- status_mapping(data)
data <- categoryName_mapping(data)

results <- data.frame(
  Iteration = numeric(),
  Depth = numeric(),
  NRounds = numeric(),
  Learning_Rate = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  MAPE = numeric()
)

total_iterations <- 1

for (k in 1:count_iterations) {
  for (j in 1:count_iterations) {
    for (i in 1:count_iterations) {
      print(total_iterations)
      starting_time <- Sys.time()

      # Select scenario
      switch(selected_scenario,
        {
          # Szenario 1: Training with data for entire 2016 and 2017, prediction for entire 2018
          if (selected_scenario == 1) {
            training_data <- data %>%
              filter((createdAt >= as.Date("2016-01-01") & createdAt <= as.Date("2017-12-31")))

            prediction_data <- data %>%
              filter(Year == 2018)
          }
        },
        {
          # Szenario 2: Training with data with partial 2016 and 2017 (until 31.08. each), prediction until 31.08.2018
          if (selected_scenario == 2) {
            training_data <- data %>%
              filter((createdAt >= as.Date("2016-01-01") & createdAt <= as.Date("2016-08-31")) |
                (createdAt >= as.Date("2017-01-01") & createdAt <= as.Date("2017-08-31")))

            prediction_data <- data %>%
              filter(Year == 2018 & createdAt <= as.Date("2018-08-31"))
          }
        },
        {
          # Szenario 3: Training with data 2016 and 2017 (july and august only), prediction 2018 only july and august
          if (selected_scenario == 3) {
            training_data <- data %>%
              filter((format(createdAt, "%Y") %in% c("2016", "2017")) &
                (format(createdAt, "%m") %in% c("07", "08")))

            prediction_data <- data %>%
              filter((format(createdAt, "%Y") == "2018") &
                (format(createdAt, "%m") %in% c("07", "08")))
          }
        }
      )

      # Prepare data for gradient boost algorithm
      features <- c("itemId", "status_numeric", "sku", "price", "qtyOrdered", "grandTotal", "incrementId", "categoryName_numeric", "salesCommisionCode", "discountAmount", "paymentMethod_numeric", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId", "createdAt")
      response <- "grandTotal"

      columns_to_consider <- c("itemId", "createdAt", "price", "qtyOrdered", "grandTotal", "discountAmount", "customerId", "workingDate", "paymentMethod_numeric", "status_numeric", "categoryName_numeric")
      ignore_columns <- setdiff(features, columns_to_consider)

      # Remove columns from features
      features <- setdiff(features, ignore_columns)

      # Convert createdAt and workingDate into numerical value
      training_data$createdAt <- as.numeric(training_data$createdAt)
      prediction_data$createdAt <- as.numeric(prediction_data$createdAt)
      training_data$workingDate <- as.numeric(training_data$workingDate)
      prediction_data$workingDate <- as.numeric(prediction_data$workingDate)

      # Create DMatrix objects for training and prediction
      dtrain <- xgb.DMatrix(data = as.matrix(training_data[, features]), label = training_data[[response]])
      dpred <- xgb.DMatrix(data = as.matrix(prediction_data[, features]))

      max_depth_param <- default_max_depth + ((i - 1) * 4)
      eta_param <- default_eta + ((k - 1) * 0.1)
      nrounds_param <- default_nrounds + ((j - 1) * 5)

      # Define XGBoost parameters
      params <- list(
        objective = "reg:squarederror",
        max_depth = max_depth_param,
        eta = eta_param,
        nthread = 2
      )

      # Train XGB model
      xgboost_model <- xgb.train(params, dtrain, nrounds = nrounds_param)

      # Use model to predict for 2018
      predictions <- predict(xgboost_model, dpred)

      # Extract actual values for 2018
      actual_values <- prediction_data$grandTotal

      # Calculate total revenue for 2016, 2017 and 2018
      total_revenue_2016 <- sum(data$grandTotal[data$Year == 2016 & data$createdAt <= as.Date("2016-08-31")])
      total_revenue_2017 <- sum(data$grandTotal[data$Year == 2017 & data$createdAt <= as.Date("2017-08-31")])
      predicted_total_revenue_2018 <- sum(predictions)
      actual_total_revenue_2018 <- sum(actual_values)

      # Calculate RMSE
      rmse_value <- rmse(actual_values, predictions)

      # Calculate MAE
      mae_value <- mae(actual_values, predictions)

      # Calculate MAPE
      mape_value <- MAPE(actual_values, predictions) * 100

      end_time <- Sys.time()

      run_time <- end_time - starting_time


      results_df <- rbind(results_df, list(
        Iteration = total_iterations,
        RunTime = as.numeric(run_time, units = "secs"),
        Depth = max_depth_param,
        NRounds = nrounds_param,
        LearningRate = eta_param,
        Gesamtumsatz2016 = total_revenue_2016,
        Gesamtumsatz2017 = total_revenue_2017,
        ProgRevTotal2018 = predicted_total_revenue_2018,
        ActualRevTotal2018 = actual_total_revenue_2018,
        RMSE = rmse_value,
        MAE = mae_value,
        MAPE = mape_value
      ))

      # Print results
      iteration_results <- data.frame(
        Iteration = i,
        Depth = max_depth_param,
        NRounds = nrounds_param,
        Learning_Rate = eta_param,
        RMSE = rmse_value,
        MAE = mae_value,
        MAPE = mape_value
      )

      results <- rbind(results, iteration_results)

      total_iterations <- total_iterations + 1
    }
  }
}

# Create plot
plot <- plot_ly(results, x = ~Iteration) %>%
  add_trace(y = ~RMSE, name = "RMSE", type = "scatter", mode = "lines") %>%
  add_trace(y = ~MAE, name = "MAE", type = "scatter", mode = "lines") %>%
  add_trace(y = ~MAPE, name = "MAPE", type = "scatter", mode = "lines") %>%
  layout(
    xaxis = list(title = "Iteration"),
    yaxis = list(title = "Error"),
    legend = list(title = "Error Metric")
  )

# Delete existing files
delete_existing_html(html_file_path_statistic)
delete_existing_html(html_file_path_total)
delete_existing_html(html_file_path_statistic_table)


# Total transactions by period
transaction_count <- data %>%
  group_by(Year, Month) %>%
  summarize(Total_Transactions = n())

# Total revenue by period
revenue_by_period <- data %>%
  group_by(Year, Month) %>%
  summarize(Total_Revenue = sum(grandTotal))

# Create plot
plot_transactions <- plot_ly(transaction_count, x = ~Month, y = ~Total_Transactions, color = ~Year, type = "bar", name = "Transactions") %>%
  layout(
    xaxis = list(title = "Month"),
    yaxis = list(title = "Transaction Count"),
    legend = list(title = "Year")
  )

plot_revenue <- plot_ly(revenue_by_period, x = ~Month, y = ~Total_Revenue, color = ~Year, type = "bar", name = "Revenue") %>%
  layout(
    xaxis = list(title = "Month"),
    yaxis = list(title = "Total Revenue"),
    legend = list(title = "Year")
  )

# Show plots together
subplot_plot <- subplot(plot_transactions, plot_revenue, nrows = 2, shareX = TRUE)

# Show plot
saveWidget(as_widget(subplot_plot), html_file_path_total)

# Show plot
saveWidget(as_widget(plot), html_file_path_statistic)


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
