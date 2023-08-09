library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(openxlsx)

# One-Hot-Encoding
one_hot_encoding <- function(data, category) {
    # Create one-hot encoded columns for category parameter
    data <- data %>%
        mutate(across({{ category }}, ~ ifelse(. == unique(data %>% pull({{ category }})), 1, 0)))

    return(data)
}

# Perform correlation analysis
correlation_analysis <- function(data_corr) {
    # Create correlation matrix
    cor_matrix <- cor(data_corr)

    cor_matrix_melted <- melt(cor_matrix)
    cor_matrix_melted <- cor_matrix_melted[cor_matrix_melted$Var1 != cor_matrix_melted$Var2, ]

    heatmap_plot <- ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "blue", high = "red") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Save heatmap as png file
    ggsave("heatmap.png", heatmap_plot, width = 10, height = 8, units = "in", dpi = 300)

    # Convert melted dataframe back to matrix
    cor_matrix <- acast(cor_matrix_melted, Var1 ~ Var2, value.var = "value")

    # Convert correlation matrix to a dataframe
    cor_matrix_df <- as.data.frame(cor_matrix)

    # Save correlation matrix as xlsx file
    write.xlsx(cor_matrix_df, "korrelationsmatrix.xlsx", rowNames = TRUE)
}

# Load data file
file_path <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Data Source/Pakistan-Largest-Ecommerce-Dataset-Cleansed.csv"
data <- read_csv(file_path, skip = 1, col_names = c("incrementId", "itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId"))

# Remove excluded columns
data <- data %>%
    select(-sku, -salesCommisionCode)

# Filter data to only apply correlation analysis to later training data
data <- data %>%
    filter((createdAt >= as.Date("2016-01-01") & createdAt <= as.Date("2017-12-31")))

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


# Remove rows with NA values
data <- na.omit(data)

correlation_analysis(data)
