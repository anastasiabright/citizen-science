library("igraph")
library(readxl)

# Read in the data:
#nodes <- read.csv("data/cs_nodes_v3.csv", header=T, sep = ";", fileEncoding = "UTF-16LE")
nodes <- read_excel("data/cs_nodes.xlsx")
#links <- read.csv("data/cs_edges_v3.csv", header=T, sep = ";", encoding = "UTF-8")
links <- read_excel("data/cs_edges.xlsx")
net_undir_undir <-  graph_from_data_frame(d=links, vertices=nodes, directed=F) 

########## Wrap long strings in project names ####################
wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}


# Apply the function to wrap the node labels (to 20 characters)
V(net_undir)$project = wrap_strings(V(net_undir)$project, 20)

######################## Highlight BUA insitutions by color ######################
#highlight_vertices <- c('i01', 'i02', 'i03', 'i04')
highlight_vertices <- c("Humboldt University", "Free University\nBerlin", "Charité", "Technical\nUniversity Berlin") #'i01', 'i02', 'i03', 'i04'
highlight_indices <- which(V(net_undir)$project %in% highlight_vertices)
V(net_undir)$color <- ifelse(V(net_undir)$project %in% highlight_vertices, "red", "green")

highlight_levels <- c("Humboldt University", "Free University\nBerlin", "Charité", "Technical\nUniversity Berlin")

# Create a vector of colors with specific colors for the highlighted SDG levels
highlight_color <- "#FFA500" # Orange and Cyan
  
vertex_colors <- sdg_colors[as.factor(V(net_undir)$sdg.level)]

# Highlight specific SDG levels with a specific color
for (level in highlight_levels) {
  vertex_colors[V(net_undir)$project == level] <- highlight_color
}

# Generate a vector of sdg names based on their index in the graph
sdg_names <- V(net_undir)$name.sdg

#Rplot2

plot(x, net_undir, layout = co, #layout_nicely(net_undir), 
     vertex.label=V(net_undir)$project, 
     vertex.size = 25*(cent$vector),
     vertex.color = vertex_colors,
     #vertex.size = (strwidth(V(net_undir)$label) + strwidth("00"))*20,
     #vertex.size2 = strheight("I") * 2 * 15,
     #vertex.size = degree+10,
     vertex.shape = "circle",
     vertex.label.cex = .5,
     vertex.label.family = "sans",
     #vertex.label.dist = 0.5,
     vertex.label.degree = 1.0,
     vertex.label.color = "black",
     vertex.frame.color="#ffffff",
     edge.arrow.size=0.3,
     edge.color = "gray50",
     edge.curved=0.1)
################# Communities detection #######################
# fast-greed - Only undirected!
# fast-greed - modular method, trying to build larger and larger communities
# by adding vertices to each community one by one and assessing a modularity
# score at each step.
# The modularity score is an idnex of how inter-connected edges are within
# versus between communities
x <- fastgreedy.community(net_undir)
# 
# IGRAPH clustering fast greedy, groups: 3, mod: 0.54
# + groups:
#   $`1`
# [1] "i02" "i04" "s01" "s02" "s04" "s05" "s06" "s07" "s18" "s19" "s20"
# 
# $`2`
# [1] "i01" "s08" "s09" "s10" "s11" "s12" "s13" "s14" "s15" "s16" "s17"
# 
# $`3`
# [1] "i03" "s03" "s21" "s22"
length(x) 
# 3
sizes(x)
# Community sizes
# 1  2  3 
# 11 11  4
membership(x)
#i01 i02 i03 i04 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 
# 2   1   3   1   1   1   3   1   1   1   1   2   2   2   2   2   2   2   2   2 
# s17 s18 s19 s20 s21 s22 
# 2   1   1   1   3   3

plot(x, net_undir)

### Edge betweenness is divisive method -
# dividing a network into smaller and smaller pieces until it finds edges
# that it perceives to be 'bridges' between communities
b <- edge.betweenness.community(net_undir) #  same as fast greed!

# Plot community networks determined by fast-greedy and edge-betweenness methods side-by-side
par(mfrow = c(1, 2)) 
plot(x, net_undir)
plot(b, net_undir)

