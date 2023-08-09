library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(openxlsx)
library(xgboost)

# One-Hot-Encoding
one_hot_encoding <- function(data, category) {
    data <- data %>%
        mutate(across({{ category }}, ~ ifelse(. == unique(data %>% pull({{ category }})), 1, 0)))

    return(data)
}

# Load data file
file_path <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Data Source/Pakistan-Largest-Ecommerce-Dataset-Cleansed.csv"
data <- read_csv(file_path, skip = 1, col_names = c("incrementId", "itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId"))

# Remove excluded columns
data <- data %>%
    select(-sku, -salesCommisionCode)

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

data$customerSince <- as.numeric(data$customerSince)
data$MY <- as.numeric(data$MY)

data <- data %>%
    one_hot_encoding(categoryName) %>%
    one_hot_encoding(BIStatus) %>%
    one_hot_encoding(status) %>%
    one_hot_encoding(paymentMethod) %>%
    one_hot_encoding(FY)

# Split data into training and prediction data
train_data <- data %>%
    filter((createdAt >= as.Date("2016-01-01") & createdAt <= as.Date("2017-12-31")))

prediction_data <- data %>%
    filter(Year == 2018)


# Remove rows with NA values
data <- na.omit(data)

params <- list(
    objective = "reg:squarederror",
    booster = "gbtree",
    eta = 0.1,
    max_depth = 6,
    nrounds = 100
)

model <- xgboost(
    data = as.matrix(train_data[, !(names(train_data) %in% "grandTotal")]),
    label = train_data$grandTotal,
    params = params,
    nrounds = params$nrounds
)

# Generate the importance matrix
importance_matrix <- xgb.importance(model = model)

# Save the importance matrix as a CSV file
write.csv(importance_matrix, file = "importance_matrix.csv", row.names = TRUE)
