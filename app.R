library(shiny)
library(visNetwork)
library(readxl)
library(dplyr)
library(stringr)

source("interactive.undir.visNetwork.R", encoding = "UTF-8")

ui <- fluidPage(
  titlePanel("BUA Citizen Science Network"),
  tags$h3(style = "text-align:left; margin-top:-10px; margin-bottom:20px;",
          "Sustainability, current runtime and cooperation with other universities"),
  tags$head(
    tags$style(
      HTML("
      * {
      word-break: break-word !important;
      }")
    )
  ),
  fluidRow(
    column(width = 10,
      selectInput("selected_nodes", "Select Sustainable Development Goal(s) from the list:",
                  choices = c("All", unique(vis.nodes$name.sdg)),
                  selected = "All")
    )
  ),

  mainPanel(
    visNetworkOutput("visnet", height = "400px", width = "150%")
  )
)

server <- function(input, output) {
  observe({
    selected_nodes <- input$selected_nodes
    if (is.null(selected_nodes) || selected_nodes == "All") {
      vis.nodes$color.background <- ifelse(vis.nodes$running, "green", "lightblue")
      #vis.nodes$color.highlight.background <- "orange"    
      } else {
      vis.nodes$color.background <- ifelse(nodes$name.sdg %in% selected_nodes, "orange", "lightgray")
      #vis.nodes$color.highlight.background <- "orange"
      }
    visNetworkProxy("visnet") |>
      visUpdateNodes(nodes = vis.nodes)
  
  # output$visnet <- renderVisNetwork({
  #   visnet %>%
  #     visOptions(highlightNearest = TRUE,  
  #                selectedBy = list(variable = "name.sdg", highlight = T))
  })
  
  
  output$visnet <- renderVisNetwork(visnet)
  # output$networkGraph <- renderVisNetwork({
  #   output$visnet <- renderVisNetwork(
  #     visnet <- input$visnet
  #   )
  # })
}

# Run the application
shinyApp(ui, server)