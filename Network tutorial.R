# Data source
# Node: 242 students of the elementary school
# Edge: 8317 times of temporal interaction measured by their physical distance

install.packages("readr")
install.packages ("RColorBrewer")

library(igraph)
library(readr)
library(RColorBrewer)

# Read files and convert CSV file into an igraph object

urlfile1 <- "https://raw.githubusercontent.com/Arizonagong/vCIES2020_Network-Analysis/master/igraph/primaryschool.csv"
urlfile2 <- "https://raw.githubusercontent.com/Arizonagong/vCIES2020_Network-Analysis/master/igraph/metadata_primaryschool.csv"

D <- read_csv(url(urlfile1))
D_meta <- read_csv(url(urlfile2))

## Mobile devices records contact when they are close to each other; creates duplicates
## We need a edge weight (number of contacts for each of the students)

#2 Manage dataset
B <- as.data.frame(table(D)) # Create an edge weight column named "Freq"
B1 <- subset(B, Freq > 0) # Delete all the edges having weight equal to 0
  
#3 Create an igraph object from the dataframe ------
Stucont <- graph_from_data_frame(B1, directed = FALSE, vertices = D_meta) #Node list (vertices) from D = meta
E(Stucont)$weight <- E(Stucont)$Freq #Assigning edge attribute to each edge (representing connections)

Stucont

# #EXPLORE YOUR igraph DATA -----------

#1 igraph summary
Stucont
gsize(Stucont) # Size of network object; number of edges
gorder(Stucont) # Number of vertex


#2 Nodelist
V(Stucont)

#3 Edge list
E(Stucont) # List of edges

#4 Attributes
V(Stucont)$Gender
V(Stucont)$Gender[V(Stucont)$Gender == "Unknown"] <- NA # Change missing data "unknown" as NA 
V(Stucont)$Class

#5 Adjacent matrix
Stucont[c(1:10), c(1:10)]

# Measuring Centrality --------

# Degree centrality <-  la cantidad de conexiones quw tiene un nodo en un grafo
## importancia del nodo dentro de la red basado en el no. de conexiones directas.
Stucont_deg <- degree(Stucont, mode = c("All")) # All beacuse there's no direction
V(Stucont)$degree <- Stucont_deg
V(Stucont)$degree
which.max(Stucont_deg)


#2 Eigenvector centrality: indica la importancia de un nodo en una red, 
# no solo según sus conexiones directas, 
# sino también según las conexiones de sus vecinos.
## "un nodo es importante no solo porque tiene muchas conexiones, sino porque está conectado a nodos importantes"

Stucont_eig <- eigen_centrality(Stucont)$vector
V(Stucont)$Eigen <- Stucont_eig
V(Stucont)$Eigen
which.max(Stucont_eig)

# Betweeness centrality
## Que tanto contibuye un nodo a conectar a otro (que tanto es un puente)
Stucont_bw <- betweenness(Stucont, directed = FALSE) # Calculamos la centralidad de autovalores para cada nodo del grafo.
V(Stucont)$betweenness <- Stucont_bw # Asignar medida como atributo
V(Stucont)$betweenness # Mostramos la importancia de cada nodo.
which.max(Stucont_bw) # Encontramos el nodo más importante según esta medida.

DF <- as_long_data_frame(Stucont) # Convert igraph object to data frame (we can look inside)
Stucont

# Meassuring network structure ---------

#1 Network Density: measure of how connected a network is. 
## It tells you how many connections (edges) exist in a network compared to the total possible number of connections.
edge_density(Stucont) # Global density
A1 <- induced_subgraph(Stucont, V(Stucont)[Class == "1A"], imp = c("auto")) # Subgraphing (class level density)
edge_density(A1) # Class level density

#2. Assortativity: tendency of nodes in a network 
# to connect to other nodes that are similar to themselves in some way
# It tells you whether similar nodes (in terms of some attribute or characteristic) 
# are more likely to be connected than dissimilar ones.

values <- as.numeric(factor(V(Stucont)$Class))
assortativity_nominal(Stucont, types=values)

#2.1. Calculate the observed assortativity
observed.assortativity <- assortativity_nominal(Stucont, types = values)
results <- vector("list", 1000)
for(i in 1:1000){results[[i]] <- assortativity_nominal(Stucont, sample(values))}

#2.2.  Plot the distribution of assortativity values and add a red vertical line at the original observed value
hist(unlist(results), xlim = c(0,0.4))
abline(v = observed.assortativity,col = "red", lty = 3, lwd=2)


# ----- NETWORK VISUALIZATION -----------

# Plotting a network with the degree centrality
library(RColorBrewer)
set.seed(1001)# Create the same shape of network for multiple trials
pal <- brewer.pal(length(unique(V(Stucont)$Class)), "Set3") # Vertex color assigned per each class number

plot(Stucont,edge.color = "black", vertex.label.cex = 0.5,
     vertex.color = pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_deg)/3, edge.width = sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold) # Set shape and distance between vertices

# Interpretation:
# Each of the size of vertex means the degrree centrality score
# Color indicates de class
# Unique colors inside the others are the teachers


# Plotting a network with the eigenvector centrality
set.seed(1001)
plot(Stucont,edge.color = 'black', vertex.label.cex  = 0.5,
     vertex.color = pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_eig)*10, edge.width = sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold)

# Size of vertices in the middle are much bigger in comparison with the previous ones
# Plotting a network with the betweenness centrality

set.seed(1001)
plot(Stucont, edge.color = 'black', vertex.lablel.cex = 0.5,
     vertex.color = pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_bw)/3, edge.width = sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold)

#3.1 Plotting a scatter plot to see the correlation

#3.1 between degree and betweenness centrality

plot(V(Stucont)$degree, V(Stucont)$betweenness)
cor(V(Stucont)$degree, V(Stucont)$betweenness) # High correlation between degree and betweenness centrality


#3.2 between degree and eigenvector centrality
plot(V(Stucont)$degree, V(Stucont)$Eigen)
cor(V(Stucont)$degree, V(Stucont)$Eigen) # Less correlation 


# Community detection -------

#1 Louvain clustering
lc <- cluster_louvain(Stucont) # Create a cluster based on the Louvain method
communities(lc) # You can check which vertices belongs to which clusters.

set.seed(1001) # To duplicate the computer process and create exactly the same network repetitively you should set the seed.
plot(lc, Stucont, edge.color = 'black',vertex.label.cex = 0.5,
     vertex.color = pal[as.numeric(as.factor(vertex_attr(Stucont, "Class")))],
     vertex.size = sqrt(Stucont_bw)/3, edge.width = sqrt(E(Stucont)$weight/800),
     layout = layout.fruchterman.reingold)

# Colors according to interaction and reduce cluster into 6 rather than 10

# set the data set as lc


