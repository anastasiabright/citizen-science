library("igraph")
library(readxl)

# Read in the data:
#nodes <- read.csv("data/cs_nodes_v3.csv", header=T, sep = ";", fileEncoding = "UTF-16LE")
nodes <- read_excel("data/cs_nodes.xlsx")
#links <- read.csv("data/cs_edges_v3.csv", header=T, sep = ";", encoding = "UTF-8")
links <- read_excel("data/cs_edges.xlsx")
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

#### Available attributes in the built net
vertex_attr(net)
# name, project, bua_institut.type, type.label, sdg.level, name.sdg, level.of.engagement, 
# name.of.engagement, weight, color

########## Wrap long strings in project names ####################
wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}


# Apply the function to wrap the node labels (to 20 characters)
V(net)$project = wrap_strings(V(net)$project, 20)

######################## Highlight BUA insitutions by color ######################
#highlight_vertices <- c('i01', 'i02', 'i03', 'i04')
highlight_vertices <- c("Humboldt University", "Free University\nBerlin", "Charité", "Technical\nUniversity Berlin") #'i01', 'i02', 'i03', 'i04'
highlight_indices <- which(V(net)$project %in% highlight_vertices)
V(net)$color <- ifelse(V(net)$project %in% highlight_vertices, "red", "green")

################### Measure vertix importance ########################
# degree
# betweenness - how frequently vertex lies on the shortest pth between any 2 vertices (the higher the more important)
# eigenvector centrality
# closeness centrality
# pagerank centrality

################ Eigenvector centrality ########################
# High eigenvector - connected to many others and especially through
# themselves highly connected to others.
cent <- eigen_centrality(net)
 
  #Rplot_Cent
plot(net,
     vertex.label.color = "black", 
     vertex.label.cex = 0.6,
     edge.arrow.size = .2,
     vertex.label=V(net)$project,
     vertex.size = 25*(cent$vector),
     edge.color = 'gray88',
     vertex.frame.color="#ffffff"
     #main = "Forrest Gump Network"
)

############### Adjust on the length of the text of the names ##############
V(net)$label <- V(net)$project
set.seed(42)   ## to make this reproducable
co <- layout_nicely(net)

plot(0, type="n", ann=FALSE, axes=FALSE, xlim=extendrange(co[,1]), 
     ylim=extendrange(co[,2]))


######### Colro by sdg #################
# Generate a vector of rainbow colors with length equal to the number of unique values of the "sdg" attribute
sdg_colors <- terrain.colors(length(unique(V(net)$sdg.level))) # 5, alpha=1,

# Assign a color from the vector to each vertex based on its "sdg" attribute
# Specify the SDG levels to highlight with a specific color


highlight_levels <- c("Humboldt University", "Free University\nBerlin", "Charité", "Technical\nUniversity Berlin")

# Create a vector of colors with specific colors for the highlighted SDG levels
highlight_color <- "#FFA500" # Orange and Cyan

vertex_colors <- sdg_colors[as.factor(V(net)$sdg.level)]

# Highlight specific SDG levels with a specific color
for (level in highlight_levels) {
  vertex_colors[V(net)$project == level] <- highlight_color
}

# Generate a vector of sdg names based on their index in the graph
sdg_names <- V(net)$name.sdg

#Rplot2

plot(net, layout = co, #layout_nicely(net), 
     vertex.label=V(net)$project, 
     vertex.size = 25*(cent$vector),
     vertex.color = vertex_colors,
     #vertex.size = (strwidth(V(net)$label) + strwidth("00"))*20,
     #vertex.size2 = strheight("I") * 2 * 15,
     #vertex.size = degree+10,
     vertex.shape = "circle",
     vertex.label.cex = 1.0,
     vertex.label.family = "sans",
     #vertex.label.dist = 0.5,
     vertex.label.degree = 1.0,
     vertex.label.color = "black",
     vertex.frame.color="#ffffff",
     edge.arrow.size=0.3,
     edge.color = "gray50",
     edge.curved=0.1)

# Generate a vector of sdg names based on their index in the graph
#sdg_names <- V(net)$name.sdg[!(V(net)$name.sdg %in% exclude_names)]

#legend_colors <- sdg_colors[!(sdg_names %in% highlight_color)]
#legend_names <- sdg_names[!(sdg_names %in% highlight_color)]

# Create a legend that maps the colors of the "sdg" attribute to the corresponding project names
legend(x=1.5, y=1.1, legend = sdg_names, pch=21,
       col="black", pt.bg=sdg_colors, pt.cex=2, cex=.8, bty="n", ncol=1) #legend =sdg_names[!(V(net)$name.sdg %in% exclude_names)]


##############Final plot on the color of BUA inst, size of centrity ...#######################

# Define the highlight color
highlight_color <- "red"
  
# Generate a vector of colors based on the "sdg" attribute using a color ramp
sdg_colors <- colorRampPalette(c("yellow", "blue", "green"))(length(unique(V(net)$sdg.level)))

# Assign a color from the vector to each vertex based on its "sdg" attribute
vertex_colors <- sdg_colors[as.factor(V(net)$sdg.level)]
names(vertex_colors) <- V(net)$sdg.level

# Identify the names of the highlighted vertices
highlight_names <- V(net)$project[V(net)$project %in% highlight_vertices]

# Highlight the vertices by assigning the highlight color to their "sdg" attribute
vertex_colors[highlight_names] <- highlight_color

##############Final plot on the color of BUA inst, size of centrity ...#######################

# Plot the graph with vertex colors based on the "sdg" attribute
plot(net, layout = co,
     vertex.label = V(net)$project,
     vertex.size = 25 * (cent$vector),
     vertex.color = vertex_colors,
     vertex.shape = "circle",
     vertex.label.cex = 0.8,
     vertex.label.family = "sans",
     vertex.label.degree = 1.0,
     vertex.label.color = "black",
     vertex.frame.color = "#ffffff",
     edge.arrow.size = 0.2,
     edge.color = "gray88",
     edge.curved = 0.1)

# Generate a vector of project names based on their index in the graph
sdg_names <- V(net)$name.sdg

# Create a legend that maps the colors of the "sdg" attribute to the corresponding project names
legend_colors <- sdg_colors[!(sdg_names %in% highlight_color)]
legend_names <- sdg_names[!(sdg_names %in% highlight_color)]
legend(x = 1.5, y = 1.1, legend_names, pch = 21,
       col = "black", pt.bg = legend_colors, pt.cex = 2, cex = .8, bty = "n", ncol = 1)

 

# transparent colouring
#palf <- colorRampPalette(c(rgb(1,1,1, .2),rgb(.8,0,0, .7)), alpha=TRUE)
# vertex.color=palf(10)


# Get a vector of geodesic distances of all vertices
mean_dists <- mean_distance(net, directed = F) #3.212308


################ Density #########################
# Overall pattern/structure of network
# e.g. density 0.19 - 19% of potential edges being present
# How interconnected is the network

dens <- edge_density(net) # 0.04153846 - 5% potential connections

net_r <- erdos.renyi.game(n = gorder(net), p.or.m = edge_density(net), type = "gnp")
plot(net_r)
mean_distance(net_r, directed = F) # 3 ->  highly interconnected than original

gl <- vector('list', 1000)

for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(
    n = gorder(net),
    p.or.m = edge_density(net),
    type = "gnp"
  )
}

gl.apls <- unlist(
  lapply(gl, mean_distance, directed = F)
)

hist(gl.apls, breaks = 20)

abline(v = mean_distance(net, directed = F),
       col = "red", # average of original network
       lty = 3,
       lwd = 2)

############ Transitivity ########################
triangles(net)
########### how connected they are
transitivity(net) # 0

largest_cliques(net) # all paired
max_cliques(net)

###### Assortativity ##############
#This is a measure of how preferentially attached vertices are to other vertices 
# with identical attributes.
assortativity(net, V(net)$sdg.level)

################# Communities detection #######################
net <-  graph_from_data_frame(d=links, vertices=nodes, directed=F) 
x_dir <- fastgreedy.community(net)

y <- edge.betweenness.community(net)
# IGRAPH clustering edge betweenness, groups: 6, mod: 0.46
# + groups:
#   $`1`
# [1] "i01" "s08" "s09" "s10" "s11" "s12" "s13" "s14" "s15" "s16" "s17"
# 
# $`2`
# [1] "i02" "i04" "s04" "s05" "s06" "s07" "s18" "s19" "s20"
# 
# $`3`
# [1] "i03" "s21" "s22"
# 
# $`4`
# + ... omitted several groups/vertices

length(y)
# 6
sizes(y)
# Community sizes
# 1  2  3  4  5  6 
# 11  9  3  1  1  1 
membership(y)
# i01 i02 i03 i04 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 
# 1   2   3   2   4   5   6   2   2   2   2   1   1   1   1   1   1   1   1   1 
# s17 s18 s19 s20 s21 s22 
# 1   2   2   2   3   3  

plot(y, net)

################# Interactive ###############################
graphjs(net)

net <- set_vertex_attr(net_undir,
                             "label",
                             value = V(net)$project)

#int_net <- set_vertex_attr(net,"color",value = "mistyrose")
#graphjs(int_net, vertex.size = 1)

greed = fastgreedy.community(net)
i <- membership(greed)

net <- set_vertex_attr(net,
                             "color",
                             value = c("yellow", "blue","red")[i])
                                               
graphjs(net)  

##################### Interactive based on fast greed ####################
# Create an object 'i' containin the memberships of the fast-greedy community detection
i <-membership(x_dir)

# Check the number of different communities
#sizes(x)

# Add a color attribute to each vertex, setting the vertex color based on community membership
net <- set_vertex_attr(net, "color", value = c("yellow", "blue", "red")[i])

# Plot the graph using threejs
graphjs(net)

##### Plot interactive by eigen centrality ############
# Create numerical vector of vertex eigenvector centralities 
ec_dir <- as.numeric(eigen_centrality(net)$vector)

# Create new vector 'v' that is equal to the square-root of 'ec' multiplied by 5
v_dir <- 5*sqrt(ec_dir)

# Plot threejs plot of graph setting vertex size to v
graphjs(net, vertex.size = v_dir)


#### In case I want to color the edges ####

# e.g. highlight one most collaborative BUA institution
#E(net)[[.inc('i01')]] # for Humboldt
#E(net)[[weight>=3]] # weight not found?

E(net)$color <- ifelse(
  E(net)$to == "i01", "red", "white") # error





















