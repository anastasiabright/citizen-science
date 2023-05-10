library(igraph)
library(visNetwork)
library(readxl)
# Read in the data:
#nodes <- read.csv("data/cs_nodes_v3.csv", header=T, sep = ";", fileEncoding = "UTF-16LE")
nodes <- read_excel("data/cs_nodes_Kopie.xlsx")
#links <- read.csv("data/cs_edges_v3.csv", header=T, sep = ";", encoding = "UTF-8")
links <- read_excel("data/cs_edges_Kopie.xlsx")
#net_undir<-  graph_from_data_frame(d=links, vertices=nodes, directed=F) 


visnet <- visNetwork(vis.nodes, vis.links, width = "100%", height = "100%",
                     main = "BUA Citizen Science Network", submain = "Sustainability and Co.",
                     footer = "Click on a node to see more information",
                     highlightEdges = TRUE, clickEvent = TRUE,
                     layout = list(randomSeed = 123))

visnet$x$options$physics <- list(hierarchical = list(enabled = TRUE, levelSeparation = 100))
visnet$x$options$interaction <- list(hover = TRUE, zoomView = FALSE)
visnet$x$options$manipulation <- list(enabled = TRUE)
visnet$x$options$nodes <- list(shape = "dot", shadow = TRUE, borderWidth = 2,
                               font = list(face = "Arial"), size = 20,
                               scaling = list(label = list(enabled = TRUE)),
                               color = list(background = "white"))
visnet$x$options$edges <- list(arrows = list(to = list(enabled = TRUE, scaleFactor = 1)),
                               smooth = list(enabled = TRUE, type = "cubicBezier"),
                               width = 1, color = list(color = "gray"))

# Highlight hub nodes
nodes$is_hub <- as.logical(nodes$is_hub)


# Create vis nodes including hub nodes
vis.nodes <- visNodes(nodes, id = "id", label = "label", shape = "circle", 
                      size = "size", color = ifelse(nodes$is_hub, "red", "blue")) %>%
  visNodes(hub_nodes, color = "green")

# Create vis edges
vis.links <- visEdges(links, id = "id", from = "from", to = "to", 
                      arrows = list(to = list(enabled = TRUE, scaleFactor = 1)),
                      smooth = TRUE)

# Add edges to hub nodes
vis.links <- visEdges(links, id = "id", from = "from", to = "to", 
                      arrows = list(to = list(enabled = TRUE, scaleFactor = 1)),
                      smooth = TRUE) %>%
  visEdges(from = links$from, to = hub_nodes$id, 
           arrows = list(to = list(enabled = TRUE, scaleFactor = 1)),
           smooth = TRUE)

