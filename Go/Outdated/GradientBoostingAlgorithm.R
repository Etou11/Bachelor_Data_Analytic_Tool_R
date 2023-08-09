library(xgboost)
library(readr)
library(dplyr)
library(Metrics)
library(foreach)
library(doParallel)

mae_list <- list()
rmse_list <- list()
mape_list <- list()

file_path <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Data Source/Pakistan Largest Ecommerce Dataset.csv"
data <- read_csv(file_path, skip = 1, col_names = c("itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "incrementId", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId"))

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

for (i in 1:1) {

  set.seed(i)

  data <- data[sample(nrow(data)), ]

  # Aufteilen der Daten in Trainings- und Testdatensätze
  train_ratio <- 0.8 # Prozentsatz für den Trainingsdatensatz (z.B. 80%)
  train_size <- floor(train_ratio * nrow(data))
  train_data <- data[1:train_size, ]
  test_data <- data[(train_size + 1):nrow(data), ]


  # Vorbereiten der Daten für den Gradient-Boosting-Algorithmus
  features <- c("itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "incrementId", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId")
  response <- "grandTotal"

  columns_to_ignore <- c("itemId", "price", "qtOrdered", "grandTotal", "incrementId", "discountAmount", "customerId")
  ignore_columns <- setdiff(features, columns_to_ignore)

  # Entferne die Spalten aus der Feature-Liste
  features <- setdiff(features, ignore_columns)

  # Erstellen der DMatrix-Objekte für den Gradient-Boosting-Algorithmus
  dtrain <- xgb.DMatrix(data = as.matrix(train_data[, features]), label = train_data[[response]])
  dtest <- xgb.DMatrix(data = as.matrix(test_data[, features]), label = test_data[[response]])

  # Definiere die XGBoost-Parameter
  params <- list(
    objective = "reg:squarederror", # Zielfunktion für die Regression
    max_depth = 6, # Maximale Tiefe der Bäume
    eta = 0.3, # Lernrate
    nthread = 2 # Anzahl der Threads für die Parallelverarbeitung
  )

  # Trainiere das XGBoost-Modell mit Kreuzvalidierung
  xgboost_cv <- xgb.cv(params, dtrain, nrounds = 10, nfold = 5, metrics = "rmse", verbose = TRUE)

  # Bestimme die optimale Anzahl der Runden (Bäume)
  best_nrounds <- which.min(xgboost_cv$evaluation_log$test_rmse_mean)

  # Trainiere das finale XGBoost-Modell mit der optimalen Anzahl der Runden
  final_model <- xgb.train(params, dtrain, nrounds = best_nrounds)

  # Verwende das finale Modell zur Vorhersage
  predictions <- predict(final_model, dtest)

  # Extrahiere die tatsächlichen Werte
  actual_values <- test_data[[response]]



  # Vergleiche die Vorhersagen mit den tatsächlichen Werten
  comparison <- data.frame(Actual = actual_values, Predicted = predictions)

  rmse_value <- rmse(actual_values, predictions)
  mae_value <- mae(actual_values, predictions)

  rmse_value
  mae_value

  mae_list[[paste0("value", i)]] <- mae_value
  rmse_list[[paste0("value", i)]] <- rmse_value

  # Berechnung des MAPE
  any(actual_values == 0)
  actual_values[actual_values == 0] <- 1e-10

  mape <- ifelse(actual_values != 0, mean(abs((actual_values - predictions) / actual_values)) * 100, 0)
  mape_list[[paste0("value", i)]] <- mape
}

#foreach(item = mae_list) %do% {
 # item
#}

#mape_list

mae_list

rmse_list
