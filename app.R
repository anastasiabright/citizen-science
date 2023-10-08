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
library(RColorBrewer)
library(tidyr)

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

  tagList(
        navbarPage(
          "BUA Citizen Science Network",
          theme = shinytheme("readable"),
          id = "navbarTabs",
          tabPanel(
            "Network Graph",
            value = "tabStart",
              #background="#ffffff",
            wellPanel(
              fluidRow(
                # h4(
                #   style = "margin-left:0.5cm","BUA Citizen Science Network" #align = "left"
                #   ),
              #  ),
              #sidebarPanel(width = 4,
                         # HTML(
                          column(4,
                          h4('Our Challenge'),
                          p('BUA does a lot of research with society – and thus contributes to opening up science. 
                               Participatory and transformative approaches are a significant part of BUA’s research. 
                               Unfortunately, they are hardly visible.'),
                          ),
              
                          column(4,
                          h4('Our Mission'),
                          p('We want to create visibility for researchers and their participatory and transformative
                               research and to expand the network on research with society towards an integrated 
                               research environment in Berlin. Our tool supports this mission.'),
                               ),
                         column(4,
                         h4('Our Vision'),
                         p('The Berlin University Alliance is perceived as a driver of research with society and 
                               supports it with appropriate future-oriented research structures')
                     
                )
              )
                ),
              # actionButton(inputId = 'buttonMethodsBSS',
              #              label = 'See methods',
              #              style = "color: white; background-color: #d44b57;"),
              # mainPanel(
              moduleUI("network-call")
          
          
        ),

          tabPanel(
            "Methods",
            value = "tabMethods",
            h4("Methods"),
            #wellPanel(
            # fluidRow(
            #   h4(
            #     style = "margin-left:0.5cm",
            #     "BUA Citizen Science Network"
            #     #align = "left"
            #   )
            # ),
           # sidebarPanel(
             # width = 4,
                HTML('<p>The Berlin University Alliance (BUA) fosters connections among 
                researchers and research groups within the same research field
                as well as across interdisciplinary domains.</p>
                <p>The BUA Citizen Science Network Graph offers an overview 
                of ongoing and completed Citizen Science projects within the BUA 
                Community. Citizen Science involves participation of the general 
                public in scientific research. There are different definitions but 
                in this context, the term "Citizen Science" encompasses concepts such 
                as "Transdisciplinary Research," "Reallabs," and "Community-driven Research".</p>
                <p>The main aims of the Network Graph are to:</p>'),
                         tags$ul(
                           tags$li('increase visibility of and to promote BUA Citizen 
                            Science projects amongst different stakeholder groups;'),
                           tags$li('monitor the development of BUA Citizen Science projects;'),
                           tags$li('showcase the inter- and transdisciplinary collaboration 
                           networks within the BUA Community;'),
                           tags$li('facilitate knowledge exchange amongst BUA partners 
                            to develop new scientific perspectives;'),
                           tags$li('build new collaboration networks and facilitate 
                            the engagement of citizens and/or scientists.')
                       #  )
           ),
            #p("Citizen Science projects selection"),
            bsCollapse(id = "methodsPanels_projectsSelection",
                       bsCollapsePanel("Selection of the projects",
                       'Collection of the manually extracted projects from the following
                       sources: ....'
                             )),
            #p("Other section"),
            bsCollapse(id = "methodsPanels_Section",
                       bsCollapsePanel("Description of the indicators",
                       #methods_panel("Other section",
                                     'Description...'
                                          )
                       )
            ),
          tabPanel(
            "Dataset",
            value = "tabDatasets",
            h4("Dataset"),
            p("The following table contains the datasets underlying the Network Graph
               included in this Shiny app. Data collection was carried out manually from the following sources: ..."),
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
      make_datatable_cs(data_table)
    })

)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Server ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++


server <- function(input, output, session) {

  moduleServer_network("network-call") #, session, output
  output$datatable = DT::renderDataTable({
    data_table
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