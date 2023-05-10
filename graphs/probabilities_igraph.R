library(igraph)

# Inspect the customers dataframe
head(customers)

# Count the number of churners and non-churners
table(customers$churn)

# Add a node attribute called churn
V(network)$churn <- customers$churn

# Visualize the network
plot(network, vertex.label = NA, edge.label = NA,
     edge.color = 'black', vertex.size = 2)

# Add a node attribute called color
V(network)$color <- V(network)$churn

# Change the color of churners to red and non-churners to white
V(network)$color <- gsub("1", "red", V(network)$color) 
V(network)$color <- gsub("0", "white", V(network)$color)

# Plot the network
plot(network, vertex.label = NA, edge.label = NA,
     edge.color = "black", vertex.size = 2)

# Create a subgraph with only churners
churnerNetwork <- induced_subgraph(network, 
                                   v = V(network)[which(V(network)$churn == 1)])

# Plot the churner network 
plot(churnerNetwork, vertex.label = NA, vertex.size = 2)

# Compute the churn probabilities
churnProb <- ChurnNeighbors / (ChurnNeighbors + NonChurnNeighbors)

# Find who is most likely to churn
mostLikelyChurners <- which(churnProb == max(churnProb))

# Extract the IDs of the most likely churners
customers$id[mostLikelyChurners]


#################################### Identify probabilities #####################
# Find churn probability of the 44th customer
churnProb[44]

# Update the churn probabilties and the non-churn probabilities
churnProb_updated <- as.vector((AdjacencyMatrix %*% churnProb) / neighbors)

# Find updated churn probability of the 44th customer
churnProb_updated[44]

##################################### Collective inferencing ###########################
# perform collective inferencing and see the effect it has on the churn prediction
# using the AUC performance measure. AUC, or area under the ROC curve, is commonly 
# used to assess the performance of classification techniques.
# AUC = probability that a randomly chosen churner is ranked higher by the model 
# than a randomly chosen non-churner
# AUC = number between 0.5 and 1, where a higher number means a better model

# Load the pROC package and data
library(pROC)
load("Nex132.RData")

# Compute the AUC
auc(customers$churn, as.vector(churnProb))

# Write a for loop to update the probabilities
for(i in 1:10){
  churnProb <- as.vector((AdjacencyMatrix %*% churnProb) / neighbors)
}

# Compute the AUC again
auc(customers$churn, as.vector(churnProb))


################################# Homophily ############################
# Homophily depends on the labels of nodes that are connected, so it helps
# to define different types pf edges.

names <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J')
tech <- c(rep('R', 6), rep('P', 4))
DataScientists <- data.frame(name = names, technology = tech)
DataScienceNetwork <- data.frame(
  from=c('A', 'A', 'A', 'A', 'B', 'B','C','C','D','D',
         'D', 'E','F','F','G','G','H','H','I'),
  to=c('B','C','D','E','C','D','D','G','E','F',
       'G','F','G','I','I','H','I','J','J'),
  label=C(rep('rr',7),'rp','rr','rr','rp','rr','rp','rp',rep('pp',5))
)
g<- graph_from_data_frame(DataScienceNetwork, directed = F)

## technology as a node
V(g)$label <- as.character(DataScientists$technology)
V(g)$color <- V(g)$label
V(g)$color <- gsub('R','blue3', V(g)$color)
V(g)$color <- gsub('P','green4',V(g)$color)

### color the edges
E(g)$color <- E(g)$label
E(g)$color = gsub('rp', 'red', E(g)$color)
E(g)$color = gsub('rr', 'blue3', E(g)$color)
E(g)$color = gsub('pp', 'green4', E(g)$color)

### Visualize
pos <- cbind(c(2,1,1,1.5,2.5,4,4.5,3,3.5,5.6),
             c(10.5,9.5,8,8.5,9,7.5,6,4.5,5.5,4))

plot(g, edge.label =NA, vertex.label.color ="white",
     layout = pos, vertex.size=25)

###########Count edges by color ##########
edge_rr <- sum(E(g)$label =='rr')

################################ Match / extract type of adges ##################
#match the customer IDs in the customer dataframe with the customer edgelist to 
#find out whether each edge is a churn, non-churn or a mixed edge. Using the function 
# match(), you will add two columns to the edgelist.

#   fromLabel with the churn status of the from column
#   toLabel with the churn status of the to column

#The command match(x, y) returns a vector with the location of x in y. In the figure 
# above match(edgeList$from, customers$id) is 1,1,1,2,2. For example, the fourth line in 
# edgeList$from, which is customer with id 393, is the second element in customers$id. 
# The churn label of this customer is, therefore, customers[2,2] or 0. Similarly, 
# the churn label of everyone in edgeList$from is customers[match(edgeList$from, customers$id),2].

# Add the column edgeList$FromLabel
edgeList$FromLabel <- customers[match(edgeList$from, customers$id), 2]

# Add the column edgeList$ToLabel
edgeList$ToLabel <- customers[match(edgeList$to, customers$id), 2]

# Add the column edgeList$edgeType
edgeList$edgeType <- edgeList$FromLabel + edgeList$ToLabel

# Count the number of each type of edge
table(edgeList$edgeType)

### count the number of each edge type using the edgeType column in the edgeList 
# dataframe. In the edgeList$edgeType column, there are three different values:

#   0 for edges that connect two non-churn nodes.
#   1 for edges that connect a non-churn and a churn node. These are called mixed or cross-label edges.
#   2 for edges that connect two churn nodes.

# Count churn edges
ChurnEdges <- sum(edgeList$edgeType == '2')

# Count non-churn edges
NonChurnEdges <- sum(edgeList$edgeType == '0')

# Count mixed edges
MixedEdges <- sum(edgeList$edgeType == '1')

# Count all edges
edges <- ChurnEdges + NonChurnEdges + MixedEdges

#Print the number of edges
edges

### count the number of each edge type using the edgeType column in the edgeList dataframe. 
# In the edgeList$edgeType column, there are three different values:

#   0 for edges that connect two non-churn nodes.
#   1 for edges that connect a non-churn and a churn node. These are called mixed or cross-label edges.
#2 for edges that connect two churn nodes.

# Count the number of churn nodes
ChurnNodes <- sum(customers$churn == '1')

# Count the number of non-churn nodes
NonChurnNodes <- sum(customers$churn == '0')

# Count the total number of nodes
nodes <- ChurnNodes + NonChurnNodes

# Compute the network connectance
connectance <- 2 * edges / nodes / (nodes - 1)

# Print the value
connectance
#This network is rather sparse!