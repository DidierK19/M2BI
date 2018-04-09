##======================================================================##
##                                                                      ##
##				Social networks analysis using iGraph & R				##
##                                                                      ##
##              Ahmed Mohameden, amed.mohameden@gmail.com               ##
##                                                                      ##
##======================================================================##


#  ---------------------- CONTENTS
# 1. Creating graphs
# 2. Generating graphs
	# 2.1 Deterministic graphs
	# 2.2 Random graphs
	# 2.3 Get information about graph
# 3. Modifying graphs
	# 3.1 Structural modifications
	# 3.2 Visual modifications
# 4. Plotting networks with iGraph
	# 4.1 Non interactive visualization
	# 4.2 Interactive visualization
	# 4.3 3D visualization
# 5. Reading network data from files
# 6. Exporting graphs
# 7. Files formats
# 8. Adjacency matrix
# 9. Local and global measures


# ================ Begin ================


# Install the iGraph package if it's not already installed
install.packages("igraph") 
library(igraph) # Import it

# ================ 1. Creating graphs ================

# Example 1

g1 <- make_empty_graph()

# Add nodes
g1 <- add_vertices(g1, 1, id = 1, name = "Ahmed", color = "green")
plot(g1)
g1 <- add_vertices(g1, 1, id = 2,  name = "Idy", color = "orange")
plot(g1)
g1 <- add_vertices(g1, 4, id = c(3, 4, 5, 6), name = c("Sene", "Khaled", "Danish", "Cazabet"), color = c("red", "blue", "yellow", "gray"))
plot(g1)

# Add edges
g1 <- add_edges(g1, c(1, 2))
plot(g1)
g1 <- add_edges(g1, c(3, 1))
plot(g1)
g1 <- add_edges(g1, c(5, 1, 6, 1, 4, 5, 2, 4, 3, 6, 2, 3, 6, 5))
plot(g1)

# Example 2
g2 <- make_graph(edges = c(1, 2, 2, 3, 3, 1), dir = FALSE) 
plot(g2)

g2 <- make_graph(edges = c(1, 2, 2, 3, 3, 1), n = 10) # By default, the graph is directed
plot(g2)

# Example 3

# Build a graph from user input. The input will be read as edge list. Thus, all attributes will be associated to edges
# To illustrate that, create two attributes: color (character) and weight (numeric)
# Note that the created window is dynamic, you can resize it as you want to create more edges attributes
g3 <- graph_from_data_frame(edit(data.frame())) # g3 : 1 -> 2, 2->3, 3->1
plot(g3, edge.label = E(g3)$weight)

# ================ 2. Generating graphs ================

#  ------->> 2.1 Deterministic graphs --------

st <- make_star(40, mode = "undirected")
plot(st, layout = layout_as_star)

rn <- make_ring(15)
plot(rn, layout = layout_in_circle)

fg <- make_full_graph(25)
plot(fg)

bipart <- make_full_bipartite_graph(12, 4)
plot(bipart, layout = layout_as_bipartite)

tr <- make_tree(n = 40, children = 3)
plot(tr, layout = layout_as_tree)
plot(tr, layout = layout_as_tree(tr, circular = TRUE))

#  ------->> 2.2 Random graphs --------

# Erdos-Renyi random graph model
er <- sample_gnp(n = 40, 0.3, loops = TRUE)
plot(er)

plot(sample_gnp(n = 10, 0))

plot(sample_gnp(n = 10, 1))

# Watts-Strogatz small-world model
# The parameters of this model are: dim, size, nei, p, loops and multiple
# You find below a description of the arguments:
# dim : the dimensions number of the starting lattice (1D, 2D, 3D, 4D, ....).
# size : the vertices number of the lattice along each dimension.
# nei : neighborhood step, the neighborhood within which the vertices of the lattice will be connected.
# p real constant between zero and one, the rewiring probability.
# loops logical scalar, whether loops edges are allowed in the generated graph.
# multiple logical scalar, whether multiple edges are allowed int the generated graph.

sw <- sample_smallworld(dim = 2, size = 10, nei = 1, p = 0.1)
plot(sw)

# Barabasi-Albert preferential attachment model for scale-free graphs
ba <- sample_pa(n = 100, power = 1, m = 1, directed = F)
plot(ba)

#  ------->> 2.3 Get information about graph --------

summary(bipart)
summary(g3)

graph_attr_names(er)
graph_attr(er)

vertex_attr_names(bipart)
vertex_attr(bipart)

vertex_attr_names(g1)
vertex_attr(g1)
V(g1)[[]]
# Make conditions
V(g1)[[id > 3]]

edge_attr_names(g3)
edge_attr(g3)

V(g1)
vcount(g1)

E(g1)
E(g1)[]
E(g1)[[]]
ecount(g1)

as_edgelist(g1) # Edge list

is_directed(g1)
is_directed(ba)

is_simple(g1)
is_simple(er)

is.weighted(g1)
is.weighted(g3)
E(g3)$weight

is.named(rn)
is.named(g1)
V(g1)$name

# What is the difference between these matrices ?
g1[]
g3[] # For weighted graphs, the [] operators returns the weights matrix

g4 <- delete_edge_attr(g3, "weight")
g4[]

# Build the adjacency matrix
as_adjacency_matrix(g3, attr = NULL)
# Generate the weights matrix
as_adjacency_matrix(g3, attr="weight")

make_empty_graph(10)[]
make_full_graph(10)[] # Why there are no values on diagonal ?
make_full_graph(10, loops = TRUE)[]
# What should we do to get 1 on all diagonal ?
make_full_graph(10, loops = TRUE)[] - make_full_graph(10)[]
# The corresponding graph ?
plot(make_full_graph(10, loops = TRUE) - make_full_graph(10))
 
# ================ 3. Modifying graphs ================

#  ------->> 3.1 Structural modifications --------

# Remove vertices
vertex_attr(g2)
V(g2)$id <- c(1:vcount(g2))
vertex_attr(g2)

plot(g2)
g2 <- g2 - V(g2)[id == 4]
plot(g2)
V(g2)$id
plot(g2, vertex.label = V(g2)$id)

g2 <- delete_vertices(g2, V(g2)[id > 4])
plot(g2)

# Create a new edge attribute 
E(g1)$weight <- c(1:ecount(g1)) 
E(g1)$weight
E(g1)[weight < 6]
E(g1)[[weight < 6]]

g5 <- g1

# Remove edges
plot(g1)
g1 <- delete_edges(g1, E(g1)["Danish|Ahmed"])
plot(g1)
g1 <- delete_edges(g1, E(g1)[[1:ecount(g1)]])
plot(g1)

plot(g3)
g3 <- g3 - E(g3)[1%->%2]

# Change the link direction
g3 <- g3 - edge(2|3)
g3 <- g3 + edge(3,2)

# Change the link weight
E(g3)[3%->%2]$weight <- 10
plot(g3, edge.label = E(g3)$weight)
E(g3)[weight == 1]$weight <- 7

# Remove vertex attribute
plot(g1)
vertex_attr_names(g1)
g1 <- delete_vertex_attr(g1, "color")
vertex_attr_names(g1)
plot(g1)

#  ------->> 3.2 Visual modifications --------

V(g1)[name == "Idy"]$name <- "Idrissa Sarr"
plot(g1)

V(g1)[name == "Cazabet"]$color <- "green"
plot(g1)

V(g1)$color
V(g1)[name != "Cazabet"]$color <- "yellow"
V(g1)$color
plot(g1)

E(g3)$width <- c(9, 2, 4)
plot(g3)
plot(g3, edge.arrow.mode = 0)

g3 <- delete_edge_attr(g3, "width")
V(g3)$name <- c("Ahmed", "Idy", "Boly")
E(g3)$color <- c("green", "orange", "blue")
plot(g3)

# ================ 4. Plotting networks with iGraph ================

#  ------->> 4.1 Non interactive visualization --------

E(g5)$weight <- sample(c(1, 3, 7), ecount(g5), rep = TRUE)

plot(g5, main = "Example of plotting network", vertex.color = "magenta", vertex.label.color = "black",
vertex.size = 20, vertex.label.dist = -1.5, edge.label = E(g5)$weight, edge.color = "black")

dev.off()

#  ------->> 4.2 Interactive visualization --------

tkplot(st)
tk_off()

#  ------->> 4.3 3D visualization --------

# Using the rgl package, rglplot plots a graph in 3D.
install.packages("rgl") 
library(rgl)

# The plot can be zoomed, rotated, shifted, etc.
rglplot(fg, layout = layout_on_grid(fg, dim = 2))

rgl.close()

# A simple graph in the form of a 3-dimensional grid, for each dimension, we define the vertices number
lc <- make_lattice(length = 4, dim = 3)
rglplot(lc, layout = layout_on_grid(lc, dim = 2))

rglplot(lc, layout = layout_on_grid(lc, dim = 3))

rglplot(fg, layout = layout_nicely(fg, dim = 3))

# ================ 5. Reading network data from files ================

lesmis <- read_graph("lesmis.gml", format = "gml")
plot(lesmis)

karate <- read_graph("http://cneurocvs.rmki.kfki.hu/igraph/karate.net", format = "pajek")
plot(karate)

nodes <- read.csv("MediaNodes.csv", sep = ";", header = T)
nrow(nodes)
head(nodes)

links <- read.csv("MediaEdges.csv", sep = ";", header = T)
head(links)
nrow(links)

net <- graph_from_data_frame(links, vertices = nodes, directed = TRUE)
plot(net)

vertex_attr_names(net)
V(net)[[]]

edge_attr_names(net)
E(net)[[]]

# ================ 6. Exporting graphs ================

write_graph(fg, file = "FullGraph.gml", format = "gml")

# Export the graph as an image
jpeg("myGraph.jpg")
plot(lesmis)
dev.off()

# Export the graph as a transparent image
plot(lesmis)
dev.copy(png,"myGraph.png")
dev.off()

# Exporting as PDF - high quality resolution
pdf(file="myGraph.pdf")
plot(lesmis)
dev.off()

# ================ 7. Files formats ================

# Writing graphs

write_graph(g1, file = "edgelistFile.txt", format = "edgelist")
write_graph(g1, file = "pajekFile.net", format = "pajek")
write_graph(g1, file = "graphmlFile.graphml", format = "graphml")

write.csv2(as_data_frame(lesmis, what = "vertices"), file = "Vertices.csv", row.names = FALSE)
write.csv2(as_data_frame(lesmis, what = "edges"), file = "Edges.csv", row.names = FALSE)

# Reading graphs

edgelistFile <- read_graph("edgelistFile.txt", format = "edgelist")
plot(edgelistFile)

pajekFile <- read_graph("pajekFile.net", format = "pajek")
plot(pajekFile)

graphmlFile <- read_graph("graphmlFile.graphml", format = "graphml")
plot(graphmlFile)

# ================ 8. Adjacency matrix ================

# Build the adjacency matrix
mat <- as_adjacency_matrix(lesmis)
# Generate the weights matrix
matWeight <- as_adjacency_matrix(lesmis, attr="value")

# Install the MASS package to be able to save matrices in text File
install.packages("MASS")
# Load MASS
library(MASS)
# Export the matrices as text files
write.matrix(mat, file = "adjacencyMatrix.txt", sep = " ")
write.matrix(matWeight, file = "weightedMatrix.txt", sep = " ")

# Build graph from adjacency matrix
adjacencyMatrix <- as.matrix(read.table(file = "adjacencyMatrix.txt", sep = " "))
colnames(adjacencyMatrix) <- V(lesmis)$label
graph <- graph_from_adjacency_matrix(adjacencyMatrix, mode = "directed")
plot(graph)

# ================ 9. Local and global measures ================

# Discover the network properties

# Density
edge_density(net, loops=F)
ecount(net)/(vcount(net)*(vcount(net)-1)) # For a directed network

# Transitivity
transitivity(net, type="global")
transitivity(net, type="local")

# Diameter
diameter(net, directed=F, weights=NA)
diameter(net, directed=F)
get_diameter(net, directed=T)

# Degree
deg <- degree(net, mode="all")
plot(net, vertex.size=deg*3)

# Distances and paths
mean_distance(net, directed=F)
mean_distance(net, directed=T)

distances(net) # With edge weights
distances(net, weights=NA) # Ignore weights

dist.from.NYT <- distances(net, v=V(net)[name=="NY Times"], to=V(net), weights=NA)

# Centrality measures

# Media neighbors
degree(net)

# Media that mentioned more than 8 media
degree(net, V(net)[degree(net) > 8])

# Assign to media a proportional size to number of mentions they received
V(net)$size <- degree(net, mode = "in")
plot(net)

# Sort the media according ti the number of mentions they made
unlist(lapply(V(net), function(x) sum(incident_edges(net, x, mode = "out")[[1]]$weight)))

# What are the best 5 media from the point of view "crossing bridges" ?
head(sort(betweenness(net), decreasing = TRUE), 5)

# What are the top 3 media that can most diffuse information ?
head(sort(closeness(net), decreasing = TRUE), 5)

# What is the media that has been most mentioned by the most famous media ?
V(net)[eigen_centrality(net)$vector == max(eigen_centrality(net)$vector)]$name
# Explanation ?
head(sort(degree(net), decreasing = TRUE), 10)
tkplot(make_ego_graph(net, 1, "Wall Street Journal")[[1]])

# Display the measures as a datatable
# To this end, you have to install DT package
library(DT)

# Take a look
datatable(arrange(data_frame(sparrow=V(lesmis)$label, Degree=degree(lesmis), Betweenness_centrality=betweenness(lesmis),
Closeness_centrality=closeness(lesmis), Spectral_centrality=eigen_centrality(lesmis)$vector), desc(Degree)))

# Interpretation of degree distribution

# Degree histogram
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")

# Degree distribution
plot(degree_distribution(net))
lines(degree_distribution(net))

# Degree distribution based on frequency instead of density
data <- as.data.frame(table(degree(net)))
data[2] <- data[2]/vcount(net)
colnames(data) <- c("Degree", "Degree distribution")
x <- as.vector(unlist(data[1]))
y <- as.vector(unlist(data[2]))
plot(x, y, xlab = "Degree", ylab = "Fraction", main = "Degree distribution", pch = 16)
lines(x, y)

# ================ End ================