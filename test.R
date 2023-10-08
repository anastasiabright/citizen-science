#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load packages ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(shiny)
library(visNetwork)
library(readxl)
library(dplyr)
library(stringr)
library(shinyjs)
library(shinythemes)
library(shinyBS)
library(DT)
#library(tidyr)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load functions and modules ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("functions.R", encoding = "UTF-8")
source("modules.R", encoding = "UTF-8")
source("datasets_panel.R", encoding = "UTF-8")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UI ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
ui <- fluidPage(
  # tags$style("
  #   html, body, .container {
  #     height: 100%;
  #     width: 100%;
  #   }
  # "),
  tagList(
        navbarPage(
          "",
          theme = shinytheme("readable"),
          id = "navbarTabs",
          tabPanel(
            "Start page",
            value = "tabStart",
            sidebarPanel(
              style = "width: 80%",
              background="#ffffff",
              titlePanel(
                h4(
                  style = "margin-left:0cm",
                  "BUA Citizen Science Network"
                  #align = "left"
                )
              ),
              fluidRow(
#                h4(
                  p(style = "margin-left:0.5cm",
                  'Connecting scientist as well as citizens by finding new projects,
                  informing about possible collaboraitions and projects of their interests.'
                 # )
                  )
                ),
            ),
            mainPanel(
              style = "width: 60%",
              moduleUI("network-call")
            )
          ),
                 # )
               # ),
          tabPanel(
            "Methods",
            value = "tabMethods",
            h4("Methods"),
            p("Citizen Science projects selection"),
            bsCollapse(id = "methodsPanels_projectsSelection",
                       bsCollapsePanel("Citizen Science projects selection",
                       'Collection of the manually extracted projects from the following
                       sources: ....'
                             )),
            p("Other section"),
            bsCollapse(id = "methodsPanels_Section",
                       bsCollapsePanel("Other section",
                       #methods_panel("Other section",
                                     'Description of other section'
                                           #'The Earth Sciences Library first created a list of journal article publications by FU Earth Science researchers, then queried the Unpaywall database via its API to obtain information on the Open Access (OA) status of those publications. Unpaywall is today the most comprehensive database of open access information on research articles. It has been queried using Digital Object Identifiers (DOIs) for each of the publications. There are different OA statuses a publication can have, which are color-coded. Gold OA denotes publication in a pure OA journal. Green OA denotes a freely available repository version. Hybrid OA denotes an OA publication in a paywalled journal where the author(s) have opted to pay for their article to be open access. Bronze OA denotes a publication which is freely available on the publisher website, but without a clear open license enabling re-use: this includes articles in a paywalled journal which have been made free to read but access might be withdrawn at any time. Thus we only consider the categories gold, green and hybrid to be true open access here. As one publication can have several OA versions (e.g. a gold version in an OA journal as well as a green version in a repository), a hierarchy is used so that each publication is assigned the OA category with the highest level of openness. The standard hierarchy used here is gold - hybrid - green (journal version before repository version, excepting bronze). We group the results from Unpaywall by OA status and publication year. One important point for OA status is that it may change over time: the OA percentage is not a fixed number. Repository versions (green OA) are often made available after a delay, such that the OA percentage for a given year typically rises retrospectively. Thus the point in time of data retrieval is important for understanding the OA percentage. The current OA status data were retrieved in November 2022.',
                                           #'Unpaywall only stores information for publications that have a DOI assigned by Crossref. Articles without a Crossref DOI have to be excluded from the OA analysis. However, these publications will be evaluated manually.'
                             )
                       )
            ),
          tabPanel(
            "Dataset",
            value = "tabDatasets",
            h4("Dataset"),
            p("The following table contain the datasets underlying the network graph
               included in this Shiny app."),
            br(),
            #p(id = "datasetPanels_datasetNG",
              #p("Network Graph dataset",
                DT::dataTableOutput("datatable"),
                style = "default"#)),
            # bsCollapse(id = "datasetPanels_datasetNG",
            #            bsCollapsePanel("Network Graph dataset",
            #                 DT::dataTableOutput("vis.nodes"),
            #                 style = "default")),
            ),
            tabPanel("About", value = "tabAbout",
                     h1("About"))
          )


),

    data_table_ng <- DT::renderDataTable({
      make_datatable_cs(vis.nodes)
    })

)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Server ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++


server <- function(input, output) {
  moduleServer_network("network-call") #, session, output
  output$datatable = DT::renderDataTable({
    vis.nodes
  })
}

# 
# #  URI routing
#   observeEvent(session$clientData$url_hash, {
#     currentHash <- sub("#", "", session$clientData$url_hash)
#     if(is.null(input$navbarTabs) || !is.null(currentHash) && currentHash != input$navbarTabs){
#       freezeReactiveValue(input, "navbarTabs")
#       updateTabsetPanel(session, "navbarTabs", selected = currentHash)
#     }
#   }, priority = 1)
#   
#   observeEvent(input$navbarTabs, {
#     currentHash <- sub("#", "", session$clientData$url_hash)
#     pushQueryString <- paste0("#", input$navbarTabs)
#     if(is.null(currentHash) || currentHash != input$navbarTabs){
#       freezeReactiveValue(input, "navbarTabs")
#       updateQueryString(pushQueryString, mode = "push", session)
#     }
#   }, priority = 0)
#   


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Run Shiny ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyApp(ui = ui, server = server)