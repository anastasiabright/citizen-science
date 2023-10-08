
  # Read in the data
  vis.nodes <- read_excel("data/cs_nodes_v3.xlsx")
  vis.links <- read_excel("data/cs_edges.xlsx")
  data_table <- read_excel("data/data_dashboard.xlsx")
  
  data_table <- data_table[-c(1:4), ]
  
  vis.nodes <- vis.nodes |> filter(runtime_in_graph == "TRUE")
  #vis.links <- vis.links[vis.links$project_short %in% vis.nodes$project_short, ]
  
  colors <- brewer.pal(7,"Set2")
  color_palette <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF")

  simulate_scientists_n <- sample(3:15, nrow(vis.nodes), replace = TRUE)
  vis.nodes$simulate_scientists_n <- simulate_scientists_n
 
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Wrap long strings in project names ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++

  wrap_strings <- function(vector_of_strings,width){
    as.character(sapply(vector_of_strings, FUN=function(x){
      paste(strwrap(x, width=width), collapse="\n")
    }))
  }
  
  vis.nodes$project_short = wrap_strings(vis.nodes$project_short, 20)
  
  
  # vis.nodes <- vis.nodes |>
  #   filter(name.sdg %in% name.sdg)
    
    
    
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Features for nodes ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ### light shadowed nodes
  vis.nodes$shadow <- TRUE 
  
  ### description of the projects when pointing on the node
  vis.nodes$title <- paste("<b>Goal</b>: ", vis.nodes$goal,"<br>",
                           "<br><b>Description</b>: ", vis.nodes$description, "<br>",
                           "<br><b>Website(s)</b>: ", vis.nodes$project.website)
  ### label text on the node
  vis.nodes$label  <- vis.nodes$project_short
  
  ### color of the label text
  vis.nodes$color.label  <- "black"
  #specific_ids <- c("id1", "id2", "id3", "id4")
  nodes_to_bold <- c("Humboldt-Universität zu Berlin",
            "Freie Universität Berlin",
            "Charité – Universitätsmedizin Berlin",
            "Technische Universität Berlin")
  
  vis.nodes$label_with_formatting <- ifelse(vis.nodes$project_short %in% nodes_to_bold,
                                            paste("<b>", vis.nodes$label, "</b>"),
                                            vis.nodes$label)
  
  
    
  ######################################################################
  
  # Define size of nodes by number of scientists ???
  vis.nodes$size <- simulate_scientists_n * 2
  
  ### thickness of the border of the node if applicable
  vis.nodes$borderWidth <- 1
  vis.nodes$color.border <- "black"
    
  ### backnd of the node if no filter or similar applied
  vis.nodes$color.background <- ifelse(vis.nodes$running, "lightgreen", "lightblue")
  
  
  ### highlighting of the colours depending on the sdg level
  #vis.nodes$color.background <- colorRampPalette(c("yellow", "blue"))(length(unique(nodes$sdg.level)))
  
  ### colors when highlighting (filter or clicking) the node(s)
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
 
  
  ###########################################################################
  
  ### control shape of nodes
  #shapes = c("square", "ellipse", "triangle", "diamond", "circle", "star", "dot") 
  ### Create a new vector that repeats the 'shapes' vector to match the number of nodes
  #num_nodes <- nrow(vis.nodes)
  #shapes_rep <- rep(shapes, length.out = num_nodes)
  ### Add a new column to 'vis.nodes' with the corresponding shape for each node
  #vis.nodes$shape <- shapes_rep[1:num_nodes] 
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Features for links ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
  ### line width depending on the number of institutions involved - thicker for many
  vis.links$width <- 1 + vis.links$type*3 
  
  ### normal color if not highlighted
  vis.links$color <- "darkgray"
    
  ### where the arrows showing
  vis.links$arrows <- "from" # arrows: 'from', 'to', or 'middle'
  
  ### curved arrows
  vis.links$smooth <- TRUE
  
  ### shadows for the arrows
  vis.links$shadow <- FALSE    # edge shadow
        
        
        
  ### Add nodes for Legend
  addNodes <- data.frame(label = c("Running", "Expired", "University"),
                         shape = "circle",
                         icon.color = c("lightgreen", "lightblue", "#97C2FC"))
  

  
  #vis.nodes$group <- sample(vis.nodes$name.of.engagement,
   #                         nrow(vis.nodes), replace = T)

  
  vis.nodes$Sustainability <- sample(vis.nodes$name.sdg, 
                                     nrow(vis.nodes), replace = T)
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Visualize network graph ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
  visnet <- function(selected_universities) { 
    visNetwork(vis.nodes, vis.links,
               #width = "100%",
               #background="#ffffff",
               clickEvent = TRUE) |>
    # visGroups(groupname = "Sonstiges", #shape = shapes[1],
    #           color = list(background = colors[1], border="black"))|>
    # visGroups(groupname = "Transdisziplinäre Forschung", #shape = shapes[2],
    #           color = list(background = colors[2], border="black"))|>
    # visGroups(groupname = "Citizen Science", #shape = shapes[3],
    #           color = list(background = colors[3], border="black"))|>
    # visGroups(groupname = "Reallabor", #shape = shapes[4],
    #           color = list(background = colors[4], border="black"))|>
    # visGroups(groupname = "Patientenbeteiligung", #shape = shapes[5],
    #           color = list(background = colors[5], border="black"))|>
    # visGroups(groupname = "Lässt sich nicht kategorisieren", #shape = shapes[6],
    #           color = list(background = colors[6], border="black"))|>
    #visNodes(label = "label", group = "group") |>
    visOptions(highlightNearest = list(enabled = TRUE,
                                       hover = TRUE,
                                       hideColor = 'rgba(200,100,100,0)'),
               manipulation = TRUE,
               nodesIdSelection = list(enabled = TRUE,
                                       values = list("i01",
                                                  "i02",
                                                  "i03",
                                                  "i04"),
                                       main = "Select University",
                                       style = 'width: 300px; height: 28px;background: #ffffff;outline:none;'),
               selectedBy = list(variable = "Sustainability", multiple = T)) |>
    
   # visLayout("layout_sugiyama") |> #, randomSeed = 123
    visLegend(main="Legend",
              addNodes = addNodes,
              position="right",
              ncol=1,
              useGroups = F,
              stepX = 500,
              stepY = 100,
              zoom = F,
              enabled = T,
              ) |>
      addFontAwesome() |>
    visPhysics(stabilization = F,
               #solver = "forceAtlas2Based", 
               solver = "barnesHut",
               barnesHut = list(
                 gravitationalConstant = -500,
                 centralGravity = 0.1,
                 springConstant = 0.08,
                 springLength = 200,
                 repulsion = list(
                   nodeDistance = 250
                 )
               )
    )|>
      visInteraction(dragNodes = T, 
                     dragView = FALSE, 
                     zoomView = T,
                     hideEdgesOnDrag = F,
                     hideNodesOnDrag = F,
                     navigationButtons = TRUE,
                     keyboard = TRUE,
                     tooltipDelay = 1,
                     #hover = T,
                     hoverConnectedEdges = T,
                     selectable = T,
                     selectConnectedEdges = T) #|> visConfigure(enabled = TRUE)
  }
  