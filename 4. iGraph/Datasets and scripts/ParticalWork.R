# ================ PART II ================

library(xlsx)

nodes <- read.xlsx("Media.xlsx", sheetIndex = 1)
links <- read.xlsx("Media.xlsx", sheetIndex = 2)

net <- graph_from_data_frame(links, vertices = nodes, directed = TRUE)

net <- simplify(net, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight="sum", "ignore"))

# The grep function takes a regex as the first argument, and the input vector as the second argument
# ^layout_ menas that we would select the layout begining with layout_
# If we use only "layout_", that means the we would select all element containing "layout_"
# ls("package:igraph") returns all igraph packages
# If you pass value = FALSE or omit the value parameter, grep returns a new vector with the indexes of the elements in the input vector
# If you pass value = TRUE, grep returns a vector with copies of the actual elements in the input vector that could be matched
# [-1] removes the first element of the returned vector
# This command menas : search in the igraph packages and select the layouts whose the names begin with layout_ execpt the first one
layouts <- grep("^layout_", ls("package:igraph"), value = TRUE)[-1]

# The grepl function returns a boolean vector showing if the values indicated in the first parameter appear or not in the vector's values
# Remove the layouts that we don't need
layouts <- layouts[grepl("star|components|circle|nicely|drl|randomly|grid|graphopt|mds", layouts)]

# plot function allow to combine multiple plots into one overall graph
# mfrow = c(nrows, ncols) option is used to create a matrix of nrows x ncols plots that are filled in by row
# mar is a numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right
par(mfrow = c(3, 3), mar = c(1 , 1, 1, 1))

for (layout in layouts) {
  l <- do.call(layout, list(net)) 
  plot(net, vertex.label = NA, vertex.color = "magenta", edge.color = "black", edge.arrow.mode = 0, layout = l, main = layout)
}

V(net)[followers > 35]$type

namesList <- V(net)[degree(net, mode = "in") == degree(net, mode = "out")]$name
length(namesList)
V(net)[name %in% namesList]$color <- "green"
V(net)[!name %in% namesList]$color <- "magenta"

hist(betweenness(net), main="Histogram of betweenness centrality")

plot(degree_distribution(net))
lines(degree_distribution(net))

shortestPath <- shortest_paths(net, from = V(net)[name == "FOX News"],
								to = V(net)[name == "WashingtonPost.com"], output = "epath")
ecol <- rep("gray", ecount(net))
ecol[unlist(shortestPath$epath)] <- "black"
tkplot(net, vertex.color = "orange", edge.color = ecol, edge.curved = 1, layout = layout_in_circle(net))

write_graph(net, file = "GraphMLVersion.graphml", format = "graphml")

graphMLNetwork <- read_graph("GraphMLVersion.graphml", format = "graphml")

netMatrix <- as_adjacency_matrix(net)
graphMLNetworkMatrix <- as_adjacency_matrix(graphMLNetwork)
netMatrix - graphMLNetworkMatrix

hyperlink <- net - E(net)[type=="hyperlink"]
mention <- net - E(net)[type=="mention"]

tkplot(hyperlink, vertex.color = "orange", edge.arrow.size = 1, edge.curved = .5, edge.color = "black")
