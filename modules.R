#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modules for dashboard ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UI modules ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

moduleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      #h4(style = "color: #f00000","Size of the node depends on the number of scientists involved in the project"),
      # column(4,
      #        selectInput(ns("select_uni"),
      #                    label = "Select University:",
      #                    choices = c("Humboldt-Universität zu Berlin",
      #                                "Freie Universität Berlin",
      #                                "Charité – Universitätsmedizin Berlin",
      #                                "Technische Universität Berlin"
      #                                ),#"All Universities",
      #                                #unique(vis.nodes$project_short)
      #                                
      #                    selected = c("Humboldt-Universität zu Berlin",
      #                                 "Freie Universität Berlin",
      #                                 "Charité – Universitätsmedizin Berlin",
      #                                 "Technische Universität Berlin"),
      #                    multiple = TRUE,
      #                    selectize = TRUE,
      #                    width = NULL,
      #                    size = NULL)
      # ),
      # column(4,
      #        selectInput(ns("select_eng"),
      #                    label = "Filter by Engagement Level:",
      #                    choices = c("All",unique(vis.nodes$name.of.engagement)),
      #                    selected = "All",
      #                    multiple = FALSE,
      #                    selectize = TRUE,
      #                    width = NULL,
      #                    size = NULL)
      #       ),
      # column(4,
      #        selectInput(ns("select_sdg"),
      #                    label = "Filter by Sustainable Development Goal:",
      #                    choices = c("All", unique(vis.nodes$name.sdg)),
      #                    selected = "All",
      #                    multiple = FALSE,
      #                    selectize = TRUE,
      #                    width = NULL,
      #                    size = NULL)
      # ),
    #   column(8,
    #          selectInput(ns("select_sdg"),
    #                      "Select Sustainable Development Goal(s) from the list:",
    #                      choices = c("All", unique(vis.nodes$name.sdg)),
    #                      selected = "All",
    #                      multiple = F,
    #                      selectize = F,
    #                      width = 6,
    #                      size = NULL
    #                      )
    #   )
    #   ),

    #fluidRow(
      # column(3,
      #        checkboxGroupInput(
      #          ns("select_uni"),
      #          label = NULL,
      #          choices = list(
      #            "Humboldt Universität", 
      #            "Freie Universität", 
      #            "Technische Universität",
      #            "Charite"),
      #          selected = c("Humboldt Universität", 
      #                       "Freie Universität", 
      #                       "Technische Universität",
      #                       "Charite")),
             ),

 visNetworkOutput(NS(id, "network"), height = "100vh") # "1000px"
  )
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Server module ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
#sdg <- unique(vis.nodes$name.sdg)



moduleServer_network <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {


  #     filtered_nodes_and_links <- reactive({
  # 
  #       #selected_universities <- input$select_uni
  # 
  #       #print(selected_universities)
  # 
  #       #filtered_nodes <- vis.nodes[vis.nodes$project_short %in% selected_universities, ]
  #       #filtered_links <- vis.links[vis.links$project_short %in% selected_universities, ]
  #         #vis.links$from %in% filtered_nodes$id | vis.links$to %in% filtered_nodes$id, ]
  # 
  #       print(filtered_nodes)
  #       #print(filtered_links)
  # 
  #       list(nodes = filtered_nodes, edges = vis.links)
  # })

    output$network <- renderVisNetwork({
      #selected_universities <- input$select_uni
      #filtered_nodes <- vis.nodes[vis.nodes$project_short %in% selected_universities, ]
      visnet()
        #vis.nodes = filtered_nodes_and_links()$nodes,
        #vis.links = filtered_nodes_and_links()$edges,
        #project = input$select_uni)
    })
    
    # observe({
    #   visNetworkProxy("network") %>%
    #     visFocus(id = input$selected_uni)
    # })
  
    observe({
      visNetworkProxy("network")# %>% visNodes(uni = input$select_uni)
    })
        }
    )}
        
        # project_selection()
        # 
        # if (input$checkbox == FALSE) {
        #   
        #   #data <- data_prep_prio(x, y, z)
        #   # Draw chart
        #   #plot_prio(data, status = input$module_status, research = input$module_research, field = input$module_field)
        # 
        #    if (is.null(input$select_sdg) || ("All" %in% input$select_sdg)) {
        #      filtered_sdg <- vis.nodes
        #    } else {
        #      filtered_sdg <- vis.nodes[vis.nodes$name.sdg %in% input$select_sdg, ]
        #      filtered_sdg$color.highlight.background <- "orange"
        #    }
        # 
        #    visnet$nodes <- filtered_sdg
        #    visnet()
       # } else {
          
          #data <- data_prep_prio_likert(x, y, z)
          #plot_prio_likert(data, status = input$module_status, research = input$module_research, field = input$module_field)
          
         # visnet(vis.nodes, vis.links,
        #         project = input$select_project)
       # }
        
     # })
       # 
       #  if (is.null(input$select_sdg) || ("All" %in% input$select_sdg)) {
       #    filtered_sdg <- vis.nodes
       #  } else {
       #    filtered_sdg <- vis.nodes[vis.nodes$name.sdg %in% input$select_sdg, ]
       #    filtered_sdg$color.highlight.background <- "orange"
       #  }
       # 
       #  visnet$nodes <- filtered_sdg
       #  visnet
       # }
      
      # output$network <- renderVisNetwork({
      #   visnet_filtered <- visnet
      #   
      #   if (!is.null(input$select_sdg) && !("All" %in% input$select_sdg)) {
      #     selected_nodes <- visnet_filtered$nodes[visnet_filtered$nodes$name.sdg %in% input$select_sdg, ]
      #     unselected_nodes <- visnet_filtered$nodes[!(visnet_filtered$nodes$name.sdg %in% input$select_sdg), ]
      #     
      #     
      #     
      #     visnet_filtered$nodes <- selected_nodes
      #     visnet_filtered$edges <- visnet_filtered$edges[visnet_filtered$edges$from %in% selected_nodes$id, ]
      #     
      #     visnet_filtered$nodes$color.highlight.background <- "orange"
      #     unselected_nodes$color.highlight.background <- "grey"
      #     
      #     visnet_filtered$nodes <- rbind(selected_nodes, unselected_nodes)
      #   }
      #   
      #   visnet_filtered
       #})
    #})


