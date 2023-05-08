library(igraph)
library(visNetwork)
library(readxl)


# Read in the data:
#nodes <- read.csv("data/cs_nodes_v3.csv", header=T, sep = ";", fileEncoding = "UTF-16LE")
nodes <- read_excel("data/cs_nodes.xlsx")
#links <- read.csv("data/cs_edges_v3.csv", header=T, sep = ";", encoding = "UTF-8")
links <- read_excel("data/cs_edges.xlsx")
#net_undir<-  graph_from_data_frame(d=links, vertices=nodes, directed=F) 


########## Wrap long strings in project names ####################
wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}


# Apply the function to wrap the node labels (to 20 characters)
#V(net.js)$project = wrap_strings(V(net.js)$project, 20)

########## First attempt ########
# visNetwork(nodes, links, width="100%", height="400px", background="#eeefff",
#            main="BUA Citizen Science Network", submain="Sustainability and Co.",
#            footer= "Click on any node to see more information")

########## Second attempt ##########

# We'll start by adding new node and edge attributes to our dataframes. 
nodes$project = wrap_strings(nodes$project, 20)
vis.nodes <- nodes
vis.links <- links

selected_nodes <- c("i01", "i02", "i03", "i04")

vis.nodes$shape <- ifelse(nodes$id %in% selected_nodes, "box", "circle")
vis.nodes$shadow <- TRUE # Nodes will drop shadow

# vis.nodes <- data.frame(title = paste("Goal pf the project: ", vis.nodes$goal, 
#                                       "<br> Description: ", vis.nodes$description,
#                                       "<br> Website: ", vis.nodes$project.website))


vis.nodes$title <- paste("Sustainable Development Goals: ", vis.nodes$goal, 
                         "<br>Description: ", vis.nodes$description, 
                         "<br>Website: ", vis.nodes$project.website)
vis.nodes$label  <- vis.nodes$project # Node label
vis.nodes$color.label  <- "black"
vis.nodes$size   <- as.numeric(vis.nodes$quantity_bua_uni_involved)*20 # Node size
vis.nodes$borderWidth <- 1 # Node border width

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


#vis.links$width <- 1+vis.links$type/8 # line width
vis.links$color <- "gray"    # line color  
vis.links$arrows <- "from" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow

visnet <- visNetwork(vis.nodes, vis.links, background="#ffffff",
                     #main="BUA Citizen Science Network", 
                     #submain="Sustainability, current runtime and cooperation with other universities",
                     footer= "Click on a node to see more information",
                     clickEvent = TRUE) |> #highlightEdges = TRUE, , height="400px",layout = "layout_with_fr"
  #visNodes(vis.nodes, color = ifelse(vis.nodes$running, "green", "blue")) |>
  # visOptions(highlightNearest = list(enabled = TRUE, degree = 1),
  #           selectedBy = list(variable = "name.sdg", highlight = T)) |>
  # #, multiple = T
  visPhysics(solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -150))
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
