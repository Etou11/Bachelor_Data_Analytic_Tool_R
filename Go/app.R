# Laden der erforderlichen Pakete
library(shiny)
library(DT)
library(readr)

file_path <- "D:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Data Source/Pakistan Largest Ecommerce Dataset.csv"
data <- read_csv(file_path, skip = 1, col_names = c("itemId", "status", "createdAt", "sku", "price", "qtyOrdered", "grandTotal", "incrementId", "categoryName", "salesCommisionCode", "discountAmount", "paymentMethod", "workingDate", "BIStatus", "MV", "Year", "Month", "customerSince", "MY", "FY", "customerId"))

# Definieren der UI
ui <- fluidPage(
  dataTableOutput("mytable")
)

# Definieren des Servers
server <- function(input, output) {
  output$mytable <- renderDataTable({
    # Laden der CSV-Datei
    data <- read.csv(file_path)

    # Erstellen der DataTable mit serverseitiger Verarbeitung
    datatable(data, options = list(
      server = TRUE,
      scrollX = TRUE,
      scrollY = "500px",
      pageLength = 1000
    ))
  })
}

# Starten der Shiny-Anwendung
shinyApp(ui, server, options = list(port = 4098))
