library(xgboost)
library(readr)
library(dplyr)
library(Metrics)
library(foreach)
library(doParallel)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(pandoc)

# Settings
count_iterations <- 3

default_max_depth <- 6
default_eta <- 0.3
default_nrounds <- 10


mae_list <- list()
rmse_list <- list()
mape_list <- list()

file_path <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Data Source/Pakistan Largest Ecommerce Dataset.csv"
data <- read_csv(file_path, skip = 1, col_names = c("itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "incrementId", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId"))

# Daten vorbereiten
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

results <- data.frame(
  Iteration = numeric(),
  Depth = numeric(),
  Learning_Rate = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  MAPE = numeric()
)

for (i in 1:count_iterations) {
  starting_time <- Sys.time()

  # Daten filtern für das Training und die Prognose
  training_data <- data %>% filter(createdAt <= as.Date("2017-08-31"))
  prediction_data <- data %>% filter(Year == 2018)

  # Vorbereiten der Daten für den Gradient-Boosting-Algorithmus
  features <- c("itemId", "status", "sku", "price", "qtyOrdered", "grandTotal", "incrementId", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId", "createdAt")
  response <- "grandTotal"

  columns_to_consider <- c("itemId", "createdAt", "price", "qtyOrdered", "grandTotal", "incrementId", "discountAmount", "customerId")
  ignore_columns <- setdiff(features, columns_to_consider)

  # Entferne die Spalten aus der Feature-Liste
  features <- setdiff(features, ignore_columns)

  # Umwandeln von createdAt in numerischen Wert für Training und Prognose
  training_data$createdAt <- as.numeric(training_data$createdAt)
  prediction_data$createdAt <- as.numeric(prediction_data$createdAt)

  # Erstellen der DMatrix-Objekte für das Training und die Prognose
  dtrain <- xgb.DMatrix(data = as.matrix(training_data[, features]), label = training_data[[response]])
  dpred <- xgb.DMatrix(data = as.matrix(prediction_data[, features]))

  max_depth_param <- default_max_depth * i
  eta_param <- default_eta * i
  nrounds_param <- default_nrounds * i

  # Definiere die XGBoost-Parameter
  params <- list(
    objective = "reg:squarederror", # Zielfunktion für die Regression
    max_depth = max_depth_param, # Maximale Tiefe der Bäume
    eta = eta_param, # Lernrate
    nthread = 2 # Anzahl der Threads für die Parallelverarbeitung
  )

  # Trainiere das XGBoost-Modell mit dem gesamten Trainingsdatensatz
  xgboost_model <- xgb.train(params, dtrain, nrounds = nrounds_param)

  # Verwende das Modell zur Vorhersage für das Jahr 2018
  predictions <- predict(xgboost_model, dpred)

  # Extrahiere die tatsächlichen Werte für das Jahr 2018
  actual_values <- prediction_data$grandTotal

  # Berechne den Gesamtumsatz für die Jahre 2016, 2017 und 2018
  total_revenue_2016 <- sum(data$grandTotal[data$Year == 2016 & data$createdAt <= as.Date("2016-08-31")])
  total_revenue_2017 <- sum(data$grandTotal[data$Year == 2017 & data$createdAt <= as.Date("2017-08-31")])
  predicted_total_revenue_2018 <- sum(predictions)
  actual_total_revenue_2018 <- sum(actual_values)

  # Calculate RMSE
  rmse_value <- rmse(actual_values, predictions)

  # Calculate MAE
  mae_value <- mae(actual_values, predictions)

  # Calculate MAPE
  mape_value <- mean(abs((actual_total_revenue_2018 - predicted_total_revenue_2018) / actual_total_revenue_2018) * 100)

  end_time <- Sys.time()

  run_time <- end_time - starting_time

  # Ausgabe der Ergebnisse
  cat("Iteration: ", i, "\n")
  cat("Run time was: ", as.numeric(run_time, units = "secs"), "\n")
  cat("Depth: ", max_depth_param, "\n")
  cat("Learning Rate: ", eta_param, "\n")
  cat("Gesamtumsatz 2016:", total_revenue_2016, "\n")
  cat("Gesamtumsatz 2017:", total_revenue_2017, "\n")
  cat("Prognostizierter Gesamtumsatz 2018:", predicted_total_revenue_2018, "\n")
  cat("Tatsächlicher Gesamtumsatz 2018:", actual_total_revenue_2018, "\n")
  cat("RMSE Value: ", rmse_value, "\n")
  cat("MAE Value: ", mae_value, "\n")
  cat("MAPE Value: ", mape_value, "\n")

  # Ausgabe der Ergebnisse
  iteration_results <- data.frame(
    Iteration = i,
    Depth = max_depth_param,
    Learning_Rate = eta_param,
    RMSE = rmse_value,
    MAE = mae_value,
    MAPE = mape_value
  )

  results <- rbind(results, iteration_results)
}

# Plot erstellen
plot <- plot_ly(results, x = ~Iteration) %>%
  add_trace(y = ~RMSE, name = "RMSE", type = "scatter", mode = "lines") %>%
  add_trace(y = ~MAE, name = "MAE", type = "scatter", mode = "lines") %>%
  add_trace(y = ~MAPE, name = "MAPE", type = "scatter", mode = "lines") %>%
  layout(
    xaxis = list(title = "Iteration"),
    yaxis = list(title = "Error"),
    legend = list(title = "Error Metric")
  )





# Dateipfad für die HTML-Datei
html_file_path_statistic <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/statistic.html"
html_file_path_total <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/total.html"

# Überprüfe, ob die HTML-Datei bereits existiert, und lösche sie gegebenenfalls
if (file.exists(html_file_path_statistic)) {
  file.remove(html_file_path_statistic)
}

if (file.exists(html_file_path_total)) {
  file.remove(html_file_path_total)
}


# Gesamtanzahl der Transaktionen nach Periode berechnen
transaction_count <- data %>%
  group_by(Year, Month) %>%
  summarize(Total_Transactions = n())

# Gesamtumsatz nach Periode berechnen
revenue_by_period <- data %>%
  group_by(Year, Month) %>%
  summarize(Total_Revenue = sum(grandTotal))

# Plot erstellen
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

# Plots gemeinsam anzeigen
subplot_plot <- subplot(plot_transactions, plot_revenue, nrows = 2, shareX = TRUE)

# Plot anzeigen
saveWidget(as_widget(subplot_plot), html_file_path_total)

# Plot anzeigen
saveWidget(as_widget(plot), html_file_path_statistic)
