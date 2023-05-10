#library(igraph)
#library(visNetwork)
#library(readxl)


# Read in the data
nodes <- read_excel("data/cs_nodes.xlsx")
links <- read_excel("data/cs_edges.xlsx")

########## Wrap long strings in project names ####################
wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

nodes$project = wrap_strings(nodes$project, 20)
vis.nodes <- nodes
vis.links <- links

# # Define different node shapes for universities and projects
# selected_nodes <- c("i01", "i02", "i03", "i04")
# vis.nodes$shape <- ifelse(nodes$id %in% selected_nodes, "box", "circle")

# Nodes will drop shadow
vis.nodes$shadow <- TRUE

vis.nodes$title <- paste("<b>Goal</b>: ", vis.nodes$goal,"<br>",
                         "<br><b>Description</b>: ", vis.nodes$description, "<br>", 
                         "<br><b>Website(s)</b>: ", vis.nodes$project.website)
vis.nodes$label  <- vis.nodes$project
vis.nodes$color.label  <- "black"

######################################################################
# Define size of nodes by numbr of scientists ???
#vis.nodes$size  <- as.numeric(vis.nodes$scientists_n)*20
vis.nodes$borderWidth <- 1 # Node border width

### Coloring by SDG level
#############################################################
# selected_sdg <- "SDG 13 Climate Action"
# filtered_nodes <- vis.nodes[vis.nodes$name.sdg == selected_sdg,]$id
# vis.nodes$color.background <- ifelse(nodes$id %in% filtered_nodes, "yellow", 
#                                      ifelse(vis.nodes$running, "green", "lightblue"))
# vis.nodes$color.highlight.background <- ifelse(vis.nodes$id %in% filtered_nodes, "orange", "yellow")
# vis.nodes$color.highlight.border <- ifelse(vis.nodes$id %in% filtered_nodes, "darkred", "yellow")
#############################################################

vis.nodes$color.background <- ifelse(vis.nodes$running, "green", "lightblue")
#vis.nodes$color.background <- colorRampPalette(c("yellow", "blue"))(length(unique(nodes$sdg.level)))
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"


vis.links$width <- 1 + vis.links$type*2 # line width
vis.links$color <- "darkgray"    # line color  
vis.links$arrows <- "from" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow


###########################################################################
# shapes <- c("square", "ellipse", "triangle", "diamond", "circle", "star")#, "box")
# shapes <- rep(shapes, length.out = nrow(vis.nodes))
# vis.nodes$shape <- shapes[vis.nodes$level.of.engagement]
shapes = c("square", "ellipse", "triangle", "diamond", "circle", "star", "dot")    
#color = c("darkred", "grey", "orange", "darkblue", "purple") # control shape of nodes
#vis.nodes$shape <- vis.nodes[vis.nodes$level.of.engagement == shapes,]$level.of.engagement

# Create a new vector that repeats the 'shapes' vector to match the number of nodes
num_nodes <- nrow(vis.nodes)
shapes_rep <- rep(shapes, length.out = num_nodes)

# Add a new column to 'vis.nodes' with the corresponding shape for each node
vis.nodes$shape <- shapes_rep[1:num_nodes]

### Add nodes for Legend
addNodes <- data.frame(label = c("Running project", "Expired project"), shape = "circle",
                      icon.color = c("green", "lightblue"), color.border = "black") #icon.code = c("f0c0", "f007"),

visnet <- visNetwork(vis.nodes, vis.links, background="#ffffff",
                     #main="BUA Citizen Science Network", 
                     #submain="Click on a node to see more information",
                     footer= "Click on a node to see more information",
                     clickEvent = TRUE) |> 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) |>
  #visGroups(groupname = "Citizen Science", shape = "square") |>
                       #color = list(background = "gray", border="black")) |>
  # visGroups(groupname = "TV", shape = "dot",       
  #                    color = list(background = "tomato", border="black"))
  # visGroups(groupname = "Online", shape = "diamond",   
  #                    color = list(background = "orange", border="black")) |>
  visLegend(useGroups = FALSE, addNodes = addNodes) |>
            # addNodes = data.frame(label = "Expired project", shape = "circle",
            #                       color = "lightblue")) |>
  visPhysics(solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -180))
  # visPhysics(solver = "barnesHut",
  #            barnesHut = list(gravity = -200, centralGravity = 0.05,
  #                             springLength = 100, springConstant = 0.05,
  #                             damping = 0.1, avoidOverlap = 2,
  #                             maxVelocity = 0.05))
  # # visFit(
  #   graph,
  #   #vis.nodes = NULL,
  #   animation = list(duration = 1500, easingFunction = "easeInOutQuad")
  # )
# visnet <- visEdges(visnet, color=list(color="black", highlight = "orange"),
#                     smooth = FALSE, width=2, dashes= TRUE, arrows = 'middle' )

# A shiny example
#shiny::runApp("./shiny/app.R")


# ######### With legend and different shapes #####################
# 
# nodes$group <- nodes$name.of.engagement 
# visnet3 <- visNetwork(nodes, links)
# visnet3 <- visGroups(visnet3, groupname = "Citizen Science", shape = "square",
#                      color = list(background = "gray", border="black"))
# visnet3 <- visGroups(visnet3, groupname = "Reallabor", shape = "dot",       
#                      color = list(background = "tomato", border="black"))
# visnet3 <- visGroups(visnet3, groupname = "Transdisciplinary", shape = "diamond",   
#                      color = list(background = "orange", border="black"))
# visLegend(visnet3, main="Legend", position="right", ncol=1)
# 
#   
# 
# ################# Third attempt (squered shapes) #################
# visnet2 <- visNetwork(nodes, links)
# visnet2 <- visNodes(visnet2, shape = "square", shadow = TRUE, 
#                     color=list(background="gray", highlight="orange", border="black"))
# visnet2 <- visEdges(visnet2, color=list(color="black", highlight = "orange"),
#                     smooth = FALSE, width=2, dashes= TRUE, arrows = 'middle' ) 
# visnet2
