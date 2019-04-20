## The following "if" commands check your local repositories for the presence of 
## certain packages. If the package is not found, then R installs it.
if(!requireNamespace("magrittr")) install.packages("magrittr")
if(!requireNamespace("tidyr")) install.packages("tidyr")
if(!requireNamespace("dplyr")) install.packages("dplyr")
if(!requireNamespace("ggplot2")) install.packages("ggplot2")
if(!requireNamespace("igraph")) install.packages("igraph")
if(!requireNamespace("ggraph")) install.packages("ggraph")

#library(magrittr)  ## Pipe operator. Helps make long code easier to read.
#library(tidyr)     ## Data wrangler: from long to wide formats and vice-versa
#library(dplyr)     ## Data wrangler: filters, selections, pivots, and summaries  Cheat sheet at https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
#library(ggplot2)   ## "Grammar of Graphics" declarative data visualization package. Cheat sheet at https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
library(tidyverse) ## Loads all the packages above this one. 
library(readxl)
library(igraph)    ## Network/graph package.
library(ggraph)    ## "Grammar of Graphics" extension for igraph objects



CC_Nodes <- read_excel("Data/CLIMATECHANGE_NODELIST.xlsx")
CC_Edges <- read.csv(file="Data/CLIMATECHANGE_EDGELIST_reliability.csv", header=TRUE, sep=",")  %>% select(Source:reliable)

CC_Nodes <- 
  CC_Edges %>% 
  as_tibble() %>% select(Source:Target) %>% ## Selecting the Source and targt Nodes
  gather(key = Node, value = id) %>%     ## Pivotting into a singke list, step 1  
  unique() %>% select(2,1) %>%            ## Pivoting, step 2
  mutate(label = as.character(id),
         node_type = gsub("^Source$", "Twitter", x = Node) %>% ## Renaming for clarity
                gsub("^Target$", "Web_Domain", x = . ))

#VIEW 
CC_Edges %>% View()
CC_Nodes %>% View()
# %>% is a pipe operator

write.csv(CC_Edges, file = "CC_EDGES.csv")
write.csv(CC_Nodes, file = "CC_NODES.csv")


CC_Graph01 <- igraph::graph_from_data_frame(d = CC_Edges, 
                                            directed = TRUE, 
                                            vertices = CC_Nodes)
CC_Graph01

## Example by Nicolas
ggraph(CC_Graph01, layout = "kk") + 
  geom_node_point(mapping = aes(color = node_type)) + 
  geom_edge_arc(alpha=0.2) + 
  theme_graph()
#+ geom_node_text(mapping = aes(label = label)) 
#+ coord_cartesian(xlim = c(2,8), ylim = c(-25, -15))
ggraph(CC_Graph01, layout = "kk") + 
  geom_node_point(mapping = aes(color = node_type)) + 
  geom_edge_arc(alpha=0.2) +
  coord_cartesian(xlim = c(-5,5), ylim = c(-2, 3))
#+
  #theme_graph()


#plot
plot(CC_Graph01)

ggraph(CC_Graph01, layout = "kk") + geom_node_point(mapping = aes(color = node_type)) + geom_edge_arc()

## Description begins with four characters. In this case DN-B.
## 1st. Directed or undirected network: D for directed, U for undirected
## 2nd. Named or unnamed graph. N for named (nodes have a $name attribute), - for unnamed
## 3rd. Weighted or unweighted graph. W for weighted (edges have a $weight attribute), - for unweighted.
## 4th. Bipartite/Two-mode status. B for Bipartite (nodes have a $type attribute)

## A set of two numbers describe the number of nodes and edges. In this case, 25 Nodes and 66 Edges.

## Explore the Edges/Links
# E is a function of igraph

E(GCC_Graph01)
E(CC_Graph01)[[3]]

## Explore the Nodes/Vertexes
V(CC_Graph01)
V(CC_Graph01)[[5]]

## Plot the Graph with different layout algorithms

plot(CC_Graph01)
plot(CC_Graph01, layout = layout_as_star)
plot(CC_Graph01, layout = layout_on_grid)
plot(CC_Graph01, layout = layout_with_fr) # Fruchterman-Reingold, distributes nodes evenly.
plot(CC_Graph01, layout = layout_with_kk) # Kamada-Kawai, simulates a physical model of springs
## There are many more layout styles.

################################################
## Some key network statistics and properties ##
################################################

## 1. Density: The number of actual edges divided by the total number of possible edges
ecount(CC_Graph01)/(vcount(CC_Graph01)*(vcount(CC_Graph01)-1))

#look at mean distance


## 2. Counts of mutual, asymmetric, and null edges
dyad_census(CC_Graph01)

## 3. Degrees: The sum of edges to/from each node
degree(CC_Graph01, mode = "all")
degree(CC_Graph01, mode = "all") %>% as_tibble() %>% mutate(node = row.names.data.frame(.)) %>% arrange(desc(value))

## Example: Obtain in-degrees, out-degrees, and plot the graph as function of the in-degree value
in_degree <- degree(CC_Graph01, mode = "in" )
out_degree <- degree(CC_Graph01, mode = "out" )

#par sets up a layout to display plots side by side
#par allows set up multiple plots side by side or in a grid c(1,2) is number of vectors and rows this is a 1 by 2 matrix
par(mfrow = c(1,2))

plot(CC_Graph01, vertex.size = in_degree^2, layout = layout_on_grid, main = "in-degrees")
plot(CC_Graph01, vertex.size = out_degree^2, layout = layout_on_grid, main = "out-degrees")

#if you want to return to just one image at a time reset the parameter
par(mfrow = c(1,1))

## 4. Some Centrality measures

## 4.A Closeness: The reciprocal of the sum of the length of the shortest paths between a node and all the
## the other nodes in the network.
closeness(CC_Graph01, mode = "all", normalized = TRUE) #  %>% as_tibble() %>% mutate(node = row.names.data.frame(.)) %>% arrange(desc(value))

## 4.B Betweeness: The sum of shortest paths among all nodes crossing through this node.
betweenness(CC_Graph01, directed = TRUE, weights=NA) %>% as_tibble() %>% mutate(node = row.names.data.frame(.)) %>% arrange(desc(value))
#tibble is a nice looking table
#mutate lets you add new or replace existing column

#closeness is a property of hte noodes-how close is anode to other nodes (how many paths cross node) 1/distance from other nodes
#betweenes is the property of the edge-how edge is close to other edges (how many paths cross edge)

## Visualizing Betweeness
plot(CC_Graph01, 
     vertex.size = betweenness(CC_Graph01, directed = TRUE, weights = NA),
     layout = layout_as_star)

## Distances: An Origin/Destination Matrix among all nodes
distances(CC_Graph01, weights = NA) %>% 
  as.matrix() %>% 
  View()

View(as.matrix(distances(CC_Graph01, weights = NA)))

mean_distance(CC_Graph01, directed = TRUE)

## Diameter: The longest shortest path between any two nodes in the network. 
## (AKA the longest geodesic distance)
diameter(CC_Graph01, directed = TRUE, weights = NA)

## Obtain the list of nodes in the diameter's path
CC_diameter_path <- get_diameter(CC_Graph01, directed = TRUE)
CC_diameter_path

## Example: Colour the diameter's nodes and edges in red to highlight its path
CC_node_color <- rep("blue", vcount(CC_Graph01))
CC_node_color
CC_node_color[CC_diameter_path] <- "red"
CC_node_color

CC_edge_color <- rep("darkblue", ecount(CC_Graph01))
CC_edge_color[E(graph = CC_Graph01, path = CC_diameter_path, directed = TRUE )] <- "red2"
CC_edge_width <- rep(0.5, ecount(CC_Graph01))
CC_edge_width[E(graph = CC_Graph01, path = CC_diameter_path, directed = TRUE )] <- 2

plot(C_Graph01, 
     layout = layout_with_fr,
     vertex.color = CC_node_color,
     vertex.label.color = "black",
     edge.color = CC_edge_color, 
     edge.arrow.mode = 0,
     edge.width = CC_edge_width)





