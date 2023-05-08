#install.packages("igraph") 
#install.packages("network") 
#install.packages("sna")
#install.packages("ggraph")
#install.packages("visNetwork")
#install.packages("threejs")
#install.packages("networkD3")
#install.packages("ndtv")

# Load the 'igraph' library:
library("igraph")


# -------~~ Test 1  --------

# Read in the data:
nodes <- read.csv("data/cs_nodes.csv", header=T, sep = ";")#, as.is=T)
links <- read.csv("data/cs_edges.csv", header=T, sep = ";") #, as.is=T)

nodes_2 <- read.csv("data/cs_nodes_v2.csv", header=T, sep = ";")#, as.is=T)
links_2 <- read.csv("data/cs_edges_v2.csv", header=T, sep = ";") #, as.is=T)

# Examine the data:
head(nodes)
head(links)

# Converting the data to an igraph object:
# The graph_from_data_frame() function takes two data frames: 'd' and 'vertices'.
# 'd' describes the edges of the network - it should start with two columns 
# containing the source and target node IDs for each network tie.
# 'vertices' should start with a column of node IDs.
# Any additional columns in either data frame are interpreted as attributes.

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

net_2 <- graph_from_data_frame(d=links_2, vertices=nodes_2, directed=T) 


# Examine the resulting object:
class(net)
net 

# # We can access the nodes, edges, and their attributes:
# E(net)
# V(net)
# E(net)$type
# V(net)$project

# Or find specific nodes and edges by attribute:
# (that returns objects of type vertex sequence / edge sequence)
#V(net)[media=="BBC"]
#E(net)[type=="mention"]


# If you need them, you can extract an edge list 
# or a matrix back from the igraph networks.
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

# Or data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")


# You can also look at the network matrix directly:
net[1,]
net[5,7]

# First attempt to plot the graph:
plot(net) # not pretty!

# Removing loops from the graph:
#net <- simplify(net, remove.multiple = F, remove.loops = T) 

## Function to wrap long strings
# Source: http://stackoverflow.com/a/7367534/496488
wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

# Apply the function to wrap the node labels
V(net)$project = wrap_strings(V(net)$project, 12)

## Shrink font
V(net)$project.cex = 0.8

# Function to increase node separation (for explanatory details, see the link below)
# Source: http://stackoverflow.com/a/28722680/496488
layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  net <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(net)$type <- 1
  
  attr <- cbind(id=1:vcount(net), val=wc)
  net <- net + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(net, weights=E(net)$type)[1:vcount(graph),]
  return(l)
}

## Make layout reproducible. Different values will produce different layouts,
##  but setting a seed will allow you to reproduce a layout if you like it.
set.seed(3)

# Let's and reduce the arrow size and remove the labels:
plot(igraph::simplify(net, edge.attr.comb="min"), edge.arrow.size=.4, vertex.color="orange", 
     vertex.frame.color="#ffffff", vertex.label=V(net)$project, 
     vertex.label.color="black") #vertex.shape="none", vertex.size=1,

#################################################################

plot(net, edge.arrow.size=.4, edge.curved=.1)

plot(net, edge.arrow.size=.1, edge.curved=.1,
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$project, vertex.label.color="black") #edge.color="orange"

##################
# Color and Size #
##################
# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$project]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$sdg.level <- deg*3
# We could also use the audience size value:
#V(net)$sdg.level <- V(net)$sdg.level*0.6

# The labels are currently node IDs.
# ???? Setting them to NA will render no labels:
#V(net)$label <- NA

# ??? Set edge width based on weight:
E(net)$width <- E(net)$weight

# change arrow size and edge color:
#E(net)$arrow.size <- .2
#E(net)$edge.color <- "gray80"
  
# We can even set the network layout:
graph_attr(net, "layout") <- layout_with_lgl





plot(net, edge.arrow.size=.1, edge.curved=.1, edge.color="gray85",
     vertex.shape="none", vertex.label=V(net)$project, 
     vertex.label.font=2, vertex.label.cex=.7, vertex.color="orange", 
     vertex.frame.color="#ffffff",
     vertex.label.color="black") #vertex.label.color="gray40",

# 
# net.bg <- sample_pa(100) 
# V(net.bg)$size <- 8
# V(net.bg)$frame.color <- "white"
#   V(net.bg)$color <- "orange"
#     V(net.bg)$label <- "" 
#     E(net.bg)$arrow.mode <- 0
#     plot(net.bg)
#     
# plot(net.bg, layout=layout_randomly)


############################################ Colouring

par(mfrow=c(1,2))

# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(net)
class(clp)

# Community detection returns an object of class "communities" 
# which igraph knows how to plot: 
plot(clp, net)

# We can also plot the communities without relying on their built-in plot:
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])



################## Highlight specific nodes and links ########################

inc.edges <- incident(net, c('i01', 'i02', 'i03', 'i04'), mode = "all")
                           #to=V(net), weights=NA) # | bua_institut.type=="2"

# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(inc.edges)+1)
col <- col[inc.edges+1]

plot(net, vertex.color=col, vertex.label=inc.edges, edge.arrow.size=.6, 
     vertex.label.color="white")
