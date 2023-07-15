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


file_path <- "D:/Data Source/Pakistan Largest Ecommerce Dataset.csv"
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
