library(ggplot2)
library(shiny)

port <- 6263

generate_display_library <- function() {
  print("Display diagramm")

  # Daten generieren
  data <- data.frame(
    Kategorie = c("A", "B", "C", "D"),
    Wert = c(10, 20, 15, 12)
  )

  # UI definieren
  ui <- fluidPage(
    titlePanel("Balkendiagramm"),
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        plotOutput("barplot")
      )
    )
  )

  # Server definieren
  server <- function(input, output) {
    output$barplot <- renderPlot({
      ggplot(data, aes(x = Kategorie, y = Wert)) + # nolint
        geom_bar(stat = "identity")
    })
  }

  # R-Shiny-App starten
  shinyApp(ui = ui, server = server, options = list(port = port))
}



  generate_display_library()
  shiny::runApp("d:/Users/Chris/Documents/Studium Extended/Semester 7/Bachelorarbeit/R/Basics") # nolint: line_length_linter.


