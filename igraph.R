library(igraph)
# interactive packages:
# statnet, ggnet, ggnetwork, ggraph, visNetwork, networkD3, sigma, rgex (to Gephi), threejs
library(threejs)

# Inspect the first few rows of the dataframe 'friends'
head(friends)

# Convert friends dataframe to a matrix
friends.mat <- as.matrix(friends)

# Convert friends matrix to an igraph object
g <- graph.edgelist(friends.mat, directed = FALSE)


# Make a very basic plot of the network
plot(g)

# Subset vertices and edges
V(g)
E(g)

# Count number of edges
gsize(g)

# Count number of vertices
gorder(g)

# Inspect the objects 'genders' and 'ages'
genders
ages

# Create new vertex attribute called 'gender'
g <- set_vertex_attr(g, "gender", value = genders)

# Create new vertex attribute called 'age'
g <- set_vertex_attr(g, "age", value = ages)

# View all vertex attributes in a list
vertex_attr(g)

# View attributes of first five vertices in a dataframe
V(g)[[1:5]] 

# View hours
hours

# Create new edge attribute called 'hours'
g <- set_edge_attr(g, "hours", value = hours)

# View edge attributes of graph object
edge_attr(g)

# Find all edges that include "Britt"
E(g)[[.inc('Britt')]]  

# Find all pairs that spend 4 or more hours together per week
E(g)[[hours>=4]]  

# Create an igraph object with attributes directly from dataframes
g1 <- graph_from_data_frame(d = friends1_edges, vertices = friends1_nodes, directed = FALSE)


# Subset edges greater than or equal to 5 hours
E(g1)[[hours>=5]]  

# Set vertex color by gender
V(g1)$color <- ifelse(V(g1)$gender == "F", "orange", "dodgerblue")

# Plot the graph
plot(g1, vertex.label.color = "black")

##### LAYOUTS ##########
# Plot the graph object g1 in a circle layout
plot(g1, vertex.label.color = "black", layout = layout_in_circle(g1))

# Plot the graph object g1 in a Fruchterman-Reingold layout 
plot(g1, vertex.label.color = "black", layout = layout_with_fr(g1))

# Plot the graph object g1 in a Tree layout 
m <- layout_as_tree(g1)
plot(g1, vertex.label.color = "black", layout = m)

# Plot the graph object g1 using igraph's chosen layout 
igraph <- layout_nicely(g1)
plot(g1, vertex.label.color = "black", layout = igraph)

###### Highlight edges ##########
# Create a vector of weights based on the number of hours each pair spend together
w1 <- E(g1)$hours

# Plot the network varying edges by weights
m1 <- layout_nicely(g1)
plot(g1, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = w1,
     layout = m1)


# Create a new igraph object by deleting edges that are less than 2 hours long 
g2 <- delete_edges(g1, E(g1)[hours < 2])


# Plot the new graph 
w2 <- E(g2)$hours
m2 <- layout_nicely(g2)

plot(g2, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = w2,
     layout = m2)

###### Directed network ##########
is.directed(g) # TRUE/FALSE
is.weighted(g)

# Degree - how many edges the vertice has - out and in going
# Check if any edge goes from A to E
g['A','E']
incident(f, 'A, mode=c("all')

# Is there an edge going from vertex 184 to vertex 178?
g['184', '178']

# Is there an edge going from vertex 178 to vertex 184?
g['178', '184']

# Show all edges going to or from vertex 184
incident(g, '184', mode = c("all"))

# Show all edges going out from vertex 184
incident(g, '184', mode = c("out"))

######### Identify netighbors

neighbors(g, "F", mode=c("all"))
# Neighbors in common
x <- neighbors(g, "F", mode=c("all"))
y <- neighbors(g, "D", mode=c("all"))
intersection(x,y)

############ Lengths of edges ####################

fatherst_vertices(g) # distance is how many vertices go thorough to the other vertice
get_diameter(g) # longest possible path   

ego(g, 2, 'F', mode=c('out')) # Reachable from F in 2 steps




################### Measure vertix importance ########################
# degree
# betweenness - how frequently vertex lies on the shortest pth between any 2 vertices (the higher the more important)
# eigenvector centrality
# closeness centrality
# pagerank centrality

degree(g, mode="out")

betweenness(g, directed = T)

# Calculate betweenness of each vertex
g.b <- betweenness(g, directed = T)

# Show histogram of vertex betweenness
hist(g.b, breaks = 80)

# Create plot with vertex size determined by betweenness score
plot(g, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     edge.arrow.size = 0.05,
     layout = layout_nicely(g))

###### Important nodes and edges ###############
# Make an ego graph
g184 <- make_ego_graph(g, diameter(g), nodes = '184', mode = c("all"))[[1]]

# Get a vector of geodesic distances of all vertices from vertex 184 
dists <- distances(g184, "184")

# Create a color palette of length equal to the maximal geodesic distance plus one.
colors <- c("black", "red", "orange", "blue", "dodgerblue", "cyan")

# Set color attribute to vertices of network g184.
V(g184)$color <- colors[dists+1]

# Visualize the network based on geodesic distance from vertex 184 (patient zero).
plot(g184, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05,
     main = "Geodesic Distances from Patient Zero"
)

################ Eigenvector centrality ########################
# High eigenvector - connected to many others and especially through
# themselves highly connected to others.
eigen_centrality(g)$vector

# Inspect Forrest Gump Movie dataset
head(gump)

# Make an undirected network
g <- graph_from_data_frame(gump, directed = FALSE)

# Identify key nodes using eigenvector centrality
g.ec <- eigen_centrality(g)
which.max(g.ec$vector)

# Plot Forrest Gump Network
plot(g,
     vertex.label.color = "black", 
     vertex.label.cex = 0.6,
     vertex.size = 25*(g.ec$vector),
     edge.color = 'gray88',
     main = "Forrest Gump Network"
)


################ Density #########################
# Overall pattern/structure of network
# e.g. density 0.19 - 19% of potential edges being present
# How interconnected is the network
edge_density(g)

################ Average path length ####################
# Average shortest pathseigen
mean_distance(g, directed = F)

# Get density of a graph
gd <- edge_density(g)

# Get the diameter of the graph g
diameter(g, directed = FALSE)

# Get the average path length of the graph g
g.apl <- mean_distance(g, directed = FALSE)
g.apl

############# Random graph (simulation) ###########################
# Randomization tests enable you to identify:
# Whether features of your original network are particularly unusual.
erdos.renyi.game(n = gorder(g), p.or.m = edge_density(g), type = "gnp")

### 1000 random networks to calculate their average density
#Rather than re-running this code many times, you can more formally address 
# this by creating 1000 random graphs based on the number of vertices and 
# density of the original Forrest Gump graph. Then, you can see how many 
# times the average path length of the random graphs is less than the 
# original Forrest Gump network. This is called a randomization test.
gl <- vector('list', 1000)

for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(
    n = gorder(g),
    p.or.m = edge_density(g),
    type = "gnp"
  )
}

gl.apls <- unlist(
  lapply(gl, mean_distance, directed = F)
)

hist(gl.apls, breaks = 20)

abline(v = mean_distance(g, directed = F),
       col = "red", # average of original network
       lty = 3,
       lwd = 2)

# Calculate the proportion of graphs with an average path length lower 
# than our observed
mean(gl.apls < g.apl)

############ Transitivity ########################
# measure of local connectivity
# find all triangles in the network 
# closed jave all connected edges
# open have some or none

 triangles(g)
count_triangles(net, vids = "A")

### connected - transitivity -messeares the probability that
# adjacent verteces of a given vertex are connected
transitivity(g)
transitivity(g, vids = "A", type = "local") # the higher the more closed triangels 
#connected to this vertex

# Calculate average transitivity of 1000 random graphs
gl.tr <- lapply(gl, transitivity)
gl.trs <- unlist(gl.tr)

# Get summary statistics of transitivity scores
summary(gl.trs)

# Calculate the proportion of graphs with a transitivity score higher than Forrest Gump's network
mean(gl.trs > g.tr)

#################### Cliques ##############################
#Identifying cliques is a common practice in undirected networks. 
# all verteces are connected to each other = all triengles they make together are closed
largest_cliques(g)
# list of cliques of the largest
max_cliques(g)

#### # Assign largest cliques output to object 'lc'
lc <- largest_cliques(g)

# Create two new undirected subgraphs, each containing only the vertices of each largest clique.
gs1 <- as.undirected(subgraph(g, lc[[1]]))
gs2 <- as.undirected(subgraph(g, lc[[2]]))


# Plot the two largest cliques side-by-side

par(mfrow=c(1,2)) # To plot two plots side-by-side

plot(gs1,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 1",
     layout.circle(gs1)
)

plot(gs2,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 2",
     layout.circle(gs2)
)

################# Assortativity ##########################
#This is a measure of how preferentially attached vertices are to other vertices 
# with identical attributes.
# Do vertices with high degree preferencially connect to pther vertices with high dgree?
# from - 1 to +1 - the lower, the more to the lower value

# Convert the gender attribute into a numeric value
values <- as.numeric(factor(V(g1)$gender))

assortativity(g, values)

# Calculate the assortativity degree of the network
assortativity.degree(g1, directed = FALSE)

############ Check on randomization #############
# Calculate the observed assortativity
observed.assortativity <- assortativity(g1, values)

# Calculate the assortativity of the network randomizing the gender attribute 1000 times
results <- vector('list', 1000)
for(i in 1:1000){
  results[[i]] <- assortativity(g1, sample(values))
}

# Plot the distribution of assortativity values and add a red vertical line at the original observed value
hist(unlist(results))
abline(v = observed.assortativity, col = "red", lty = 3, lwd=2)
############### Reciprocity ###################
# The reciprocity of a directed network reflects the proportion of edges 
# that are symmetrical. That is, the proportion of outgoing edges that also 
# have an incoming edge. It is commonly used to determine how inter-connected 
# directed networks are. 
reciprocity(g)

################# Communities detection #######################
# fast-greed - modular method, trying to build larger and larger communities
# by adding vertices to each community one by one and assessing a modularity
# score at each step.
# The modularity score is an idnex of how inter-connected edges are within
# versus between communities
x <- fastgreedy.community(g)
length(x)
sizes(x)
membership(x)

######################## Edge betweenness ################################
### Edge betweenness is divisive method -
# dividing a network into smaller and smaller pieces until it finds edges
# that it perceives to be 'bridges' between communities
edge.betweenness.community(net_undir)

# Plot community networks determined by fast-greedy and edge-betweenness methods side-by-side
par(mfrow = c(1, 2)) 
plot(kc, g)
plot(gc, g)


################# Interactive ###############################
graphjs(g)

g <- set_vertex_attr(g,
                     "label",
                     value = V(g)$name)

g <- set_vertex_attr(g,
                     "color",
                     value = "mistyrose")
graphjs(g, vertex.size = 1)

x = edge.betweenness.community(g)
i <- membership(g)

g <- set_vertex_attr(g,
                     "color",
                     value = c("yellow",
                                       "blue",
                                       "red")[i])
graphjs(g)               


##### Plot interactive by eigen centrality ############
# Create numerical vector of vertex eigenvector centralities 
ec <- as.numeric(eigen_centrality(g)$vector)

# Create new vector 'v' that is equal to the square-root of 'ec' multiplied by 5
v <- 5*sqrt(ec)

# Plot threejs plot of graph setting vertex size to v
graphjs(g, vertex.size = v)

#############  Fast greed community interactive by color #############
# Create an object 'i' containin the memberships of the fast-greedy community detection
i <-  membership(kc)

# Check the number of different communities
sizes(kc)

# Add a color attribute to each vertex, setting the vertex color based on community membership
g <- set_vertex_attr(g, "color", value = c("yellow", "blue", "red")[i])

# Plot the graph using threejs
graphjs(g)
