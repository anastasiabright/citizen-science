library("igraph")

#######
# Convert friends dataframe to a matrix
friends.mat <- as.matrix(friends)
# Convert friends matrix to an igraph object
g <- graph.edgelist(friends.mat, directed = FALSE)
# Make a very basic plot of the network
plot(g)


# Read in the data:
#nodes <- read.csv("data/cs_nodes_v3.csv", header=T, sep = ";", fileEncoding = "UTF-16LE")
nodes <- read_excel("data/cs_nodes.xlsx")
#links <- read.csv("data/cs_edges_v3.csv", header=T, sep = ";", encoding = "UTF-8")
links <- read_excel("data/cs_edges.xlsx")
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 


vertex_attr(net)
# name, project, bua_institut.type, type.label, sdg.level, name.sdg, level.of.engagement, 
# name.of.engagement, weight, color

V(net)[1:5] #[1] i01 i02 i03 i04 s01
V(net)[[1:5]]
# + 5/22 vertices, named, from d50a618:
#   name                     project bua_institut.type                   type.label sdg.level
# 1  i01                   HU Berlin                 1                    HU Berlin        NA
# 2  i02      Free University Berlin                 2       Free University Berlin        NA
# 3  i03                  Charit\x82                 3                   Charit\x82        NA
# 4  i04 Technical University Berlin                 4  Technical University Berlin        NA
# 5  s01             Mind the Fungi!                NA Technical University Berlin         12
# name.sdg level.of.engagement name.of.engagement weight
# 1                                                                NA                         1
# 2                                                                NA                         2
# 3                                                                NA                         3
# 4                                                                NA                         2
# 5 SDG 12 Responsible consumption and production                   0                         1
# color
# 1 white
# 2   red
# 3   red
# 4   red
# 5 white

E(net)[[.inc('i01')]] # for Humboldt
E(net)[[weight>=3]]

######################## Color the edge by weight, but we actually need the vertix color...
E(net)$color <- ifelse(
  E(net)$to == "i01", "red", "white"
)

#################### Good ####################################
plot(net, vertex.label.color = "black",
     edge.arrow.size=.1, edge.curved=.1)#, edge.color="gray85")
     # vertex.label.font=2, vertex.label.cex=.7,
     # vertex.frame.color="#ffffff") #vertex.label=V(net)$project,

############### Different layout forms possible!
plot(net, layout = layout.circle(net)) # also layout_in_circle(net)
#plot(net, layout = layout.fruchterman.reingold(net)) # also layout_with_fr(net)

### TREE??? - rather no?
#plot(net, layout = layout_as_tree(net))

plot(net, layout = layout_nicely(net),
     edge.arrow.size=.2, edge.curved=.1, 
     vertex.color="orange", 
     vertex.frame.color="#ffffff", 
     vertex.label=V(net)$project, 
     vertex.label.color="black") # more like a fr?

#### Highlight edges for Institutes?
inc.edges <- incident(net, c('i01', 'i02', 'i03', 'i04'), mode = "all")
plot(net, vertex.label=inc.edges, edge.arrow.size=.2, edge.curved=.1, 
     vertex.color="orange", 
     vertex.frame.color="#ffffff", 
     vertex.label=V(net)$project, 
     vertex.label.color="black")
################################################################################
mean_distance(net, directed = F) #3.212308
edge_density(net) # 0.04153846 - 5% potential connections
cent <- eigen_centrality(net)
V(net)$project <- enc2utf8(V(net)$project)
highlight_vertices <- c("Humboldt University", "HU Berlin", "Free University Berlin", 
                        "Charité",
                        "Technical University Berlin")#'i01', 'i02', 'i03', 'i04'
highlight_indices <- which(V(net)$type.label %in% highlight_vertices)
V(net)$color <- ifelse(V(net)$type.label %in% highlight_vertices, "red", "orange")
#library(igraphextras)
#mark.vertex(net, mark = highlight_indices, col = "red", pch = 19)

# Wrap long strings
wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}


# Apply the function to wrap the node labels
V(net)$project = wrap_strings(V(net)$project, 20)

df <- data.frame(project=V(net)$project, additional_param=V(net)$sdg.level)
#V(net)$color <- ifelse(df$additional_param > 3, "red", "orange")
#V(net)$size <- 10 + df$additional_param
# Different colors for different levels
# degree <- degree(net, mode=c("in")) # i01 i02 i03 i04 11   5   1   6
# ego_net <- make_ego_graph(net, order = 2,mode=c("all"))
# colors <- c("black", "red", "orange", "blue", "dodgerblue", "cyan")

#V(net)$axes_param <- V(net)$sdg.level

V(net)$label <- V(net)$name
set.seed(42)   ## to make this reproducable
co <- layout.auto(net)

plot(0, type="n", ann=FALSE, axes=FALSE, xlim=extendrange(co[,1]), 
     ylim=extendrange(co[,2]))

plot(net, layout = co, #layout_nicely(net), 
     vertex.label=V(net)$project, 
     vertex.size = (strwidth(V(net)$label) + strwidth("00"))*20,
     vertex.size2 = strheight("I") * 2 * 15,
     #vertex.size = 15*cent$vector, 
     #vertex.size = degree+10,
     vertex.shape = "rectangle",
     vertex.label.cex = 0.6,
     vertex.frame.width = 1.0, 
     vertex.label.family = "sans",
     vertex.label.dist = 0.5,
     vertex.label.degree = 1.0,
     edge.arrow.size=0.05,
     edge.color = "gray",
     edge.curved=.1,
     axes=F)
     #xlim=c(1.0,16.0),
     #margin=c(5,15,0,5),
     #xaxs="i", yaxs="i",
     #xlab="Sustainability",
     #ylab="Level of engagement")

betweenness(net, directed = T) # all 0!

# "circle"
# "square"
# "csquare" (a square with rounded corners)
# "rectangle"
# "crectangle" (a rectangle with rounded corners)
# "vrectangle" (a rectangle standing on one of its sides)
# "pie" (a pie shape with a number of slices determined by the vertex degree)
# "rtriangle" (a right triangle pointing to the right)
# "ltriangle" (a right triangle pointing to the left)
# "triangle" (an equilateral triangle)
# "diamond"
# "cdiamond" (a diamond with rounded corners)
# "star"
# "cstar" (a star with rounded corners)
########################## Thicken edges

'# Create a vector of weights based on the number of hours each pair spend together
w1 <- E(g1)$hours

# Plot the network varying edges by weights
m1 <- layout_nicely(g1)
plot(g1, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = w1,
     layout = m1)

# Create a new igraph object by deleting edges that are less than 2 hours long 
g2 <- delete_edges(g1, E(g1)[hours < 2])'

### My graph#################################################
w1 <- E(net)$weight
m1 <- layout_nicely(net)
plot(net, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = w1,
     layout = m1)


# Plot the new graph 
w2 <- E(g2)$hours
m2 <- layout_nicely(g2)

plot(g2, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = w2,
     layout = m2)