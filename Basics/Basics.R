main <- function() {
  a <- 5
  b <- 7
  c <- a + b
  print(c)

  name <- "Peter Mayer"
  print(name)

  x <- 3.14
  y <- 42L
  print(paste("X = ", x, " Y = ", y))
}

generate_data <- function(count) {
    # Generiere N Transaktionen
    n_transactions <- count

    # Erstelle einen leeren Datensatz
    transactions <- data.frame(
        customer_id = integer(),
        age = integer(),
        gender = character(),
        product_id = integer(),
        product_category = character(),
        price = numeric(),
        purchase_time = character(),
        promotion = character(),
        rating = numeric(),
        stringsAsFactors = FALSE
    )

    # Fülle den Datensatz mit zufälligen Daten
    for (i in 1:n_transactions) {
        # Kunden-ID
        customer_id <- sample(1:1000, 1)

        # Alter
        age <- sample(18:70, 1)

        # Geschlecht
        gender <- sample(c("M", "F"), 1)

        # Produkt-ID
        product_id <- sample(1:50, 1)

        # Produktkategorie
        product_category <- sample(c("Electronics", "Clothing", "Books", "Beauty", "Food"), 1) # nolint

        # Preis des Produkts
        price <- round(runif(1, 10, 500), 2)

        # Zeitpunkt des Kaufs
        purchase_time <- as.character(Sys.time() - sample(1:365, 1) * 3600 * 24)

        # Werbeaktion oder Rabatt
        promotion <- sample(c("None", "10% off", "20% off", "Free shipping"), 1)

        # Kundenbewertung
        rating <- round(runif(1, 1, 5), 1)

        # Füge eine neue Zeile zum Datensatz hinzu
        transactions <- rbind(transactions, data.frame(
            customer_id = customer_id,
            age = age,
            gender = gender,
            product_id = product_id,
            product_category = product_category,
            price = price,
            purchase_time = purchase_time,
            promotion = promotion,
            rating = rating
        ))
    }
    View(transactions)
    write.csv(transactions, file = "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Basics/transactions.csv", row.names = FALSE) # nolint
}

main()