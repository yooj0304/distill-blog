#This script imports the Florentine Families dataset from the network package.

#Let's load the libraries you need (install them first if you need to)
  if("statnet" %in% rownames(installed.packages()) == FALSE) {install.packages("statnet")}
  if("igraph"   %in% rownames(installed.packages()) == FALSE) {install.packages("igraph")}
  if("igraphdata"   %in% rownames(installed.packages()) == FALSE) {install.packages("igraphdata")}

  library(statnet)
  library(igraph)
  library(igraphdata)

#Lets read the data into the enviroment. This will import it as an adjacency matric
  data("USairports", package = "igraphdata")
  network_igraph <- USairports
  rm(USairports)

#Now let's make an edgelist versian
  network_edgelist <- as.data.frame(as_edgelist(network_igraph), stringsAsFactors = FALSE)
  
  #Lets add edge attributes
    network_edgelist$Carrier    <- as.character(E(network_igraph)$Carrier)
    network_edgelist$Departures <- as.character(E(network_igraph)$Departures)
    network_edgelist$Seats      <- as.character(E(network_igraph)$Seats)
    network_edgelist$Passengers <- as.character(E(network_igraph)$Passengers)
    network_edgelist$Aircraft   <- as.character(E(network_igraph)$Aircraft)
    network_edgelist$Distance   <- as.character(E(network_igraph)$Distance)
    network_edgelist$weight <- as.character(E(network_igraph)$Passengers)
    
    
#Now let's create a node attribute dataframe
    network_nodes <- data.frame(name = as.character(V(network_igraph)$name), 
                                City = as.character(V(network_igraph)$City),
                                Position = as.character(V(network_igraph)$Position),
                                stringsAsFactors = FALSE)

#Lets create an statnet network object from our edgelist
  network_statnet <- network(as.matrix(network_edgelist[1:2]), matrix.type = 'edgelist', directed = TRUE)
  
  network_statnet%e%'Carrier'    <- network_edgelist$Carrier    
  network_statnet%e%'Departures' <- network_edgelist$Departures 
  network_statnet%e%'Seats'      <- network_edgelist$Seats      
  network_statnet%e%'Passangers' <- network_edgelist$Passengers 
  network_statnet%e%'Aircraft'   <- network_edgelist$Aircraft   
  network_statnet%e%'Distance'   <- network_edgelist$Distance
  network_statnet%e%'weight' <- network_edgelist$Passengers 
  
  
  network_statnet%v%'City'       <- as.character(network_nodes$City)
  network_statnet%v%'Distance'   <- as.character(network_nodes$Distance)
  

  
#Let's add a helpful message that will tell  users what this script has done.
  cat(
'#################################################################################################
"US Airport.R" has imported a a network of flights between US airports from the igraphdata
package.

This is a large, weighted, directed network. Nodes are airports and edges are flights. Edges are
weighted by total number of passangers. For more information use "?USairports".

The import script has created four objects that represent the network:
     -network_edgelist      (a dataframe of an edgelist and edge attributes)
     -network_nodes         (a dataframe of node attributes)
     -network_igraph        (an igraph object)
     -network_statnet       (a network object compatable with statnet packages like sna & ergm)


Each object name starts, quite generically, with "network_" and ends with the type of object it 
is. Note that the names are generic so that they are  compatable with other scripts you will use
in this course. Feel free to rename the objects for your purposes. 
################################################################################################')

  
  
  
save(network_edgelist, network_igraph, network_nodes, network_statnet, file="USAirports.rda")
  