library("igraph")
library(threejs)
library(readxl)
library(htmlwidgets)

# Read in the data:
#nodes <- read.csv("data/cs_nodes_v3.csv", header=T, sep = ";", fileEncoding = "UTF-16LE")
nodes <- read_excel("data/cs_nodes.xlsx")
#links <- read.csv("data/cs_edges_v3.csv", header=T, sep = ";", encoding = "UTF-8")
links <- read_excel("data/cs_edges.xlsx")
net_undir<-  graph_from_data_frame(d=links, vertices=nodes, directed=F) 

net.js <- net_undir
graph_attr(net.js, "layout") <- NULL 

########## Wrap long strings in project names ####################
wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}


# Apply the function to wrap the node labels (to 20 characters)
V(net.js)$project = wrap_strings(V(net.js)$project, 20)

# Plot the graph using threejs
graphjs(net.js)

##### Plot interactive by eigen centrality ############
# Create numerical vector of vertex eigenvector centralities 
cent <- eigen_centrality(net.js)
ec_undir <- as.numeric(cent$vector)

# Create new vector 'v' that is equal to the square-root of 'ec' multiplied by 5
v <- 5*sqrt(ec_undir)

# Plot threejs plot of graph setting vertex size to v
#graphjs(net.js, vertex.size = v)
greed = fastgreedy.community(net.js)
i <- membership(greed)
net.js <- set_vertex_attr(net.js,
                             "color",
                             value = c("yellow",
                                               "blue",
                                               "red")[i])


gjs <- graphjs(net.js, main="BUA Citizen Science Network", bg="gray10", showLabels=T, 
               vertex.size = v, stroke=F, vertex.label = V(net.js)$project,
               curvature=0.1, attraction=0.9, repulsion=0.8, opacity=0.9)
print(gjs)
saveWidget(gjs, file="Network-gjs.html")
browseURL("Network-gjs.html")


# A shiny example
shiny::runApp(system.file("examples/graph", package="threejs"))

################# Interactive ###############################
graphjs(net_undir)

net_undir <- set_vertex_attr(net_undir,
                             "label",
                             value = V(net_undir)$project)

int_net <- set_vertex_attr(net_undir,
                           "color",
                           value = "mistyrose")
graphjs(int_net, vertex.size = 1)


                                               
graphjs(net_undir)  

##################### Interactive based on fast greed ####################
# Create an object 'i' containin the memberships of the fast-greedy community detection
i <-  membership(x)

# Check the number of different communities
#sizes(x)

# Add a color attribute to each vertex, setting the vertex color based on community membership
net_undir <- set_vertex_attr(net_undir, "color", value = c("yellow", "blue", "red")[i])
