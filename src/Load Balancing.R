# Load a test network -----------------------------------------------------
# in this section we load a test network in order to test the heuristic 
# load balancing 

# reading the 'toy2' network and attributes
el <- read.csv("./data/networks/toy2/edgelist.csv", header=TRUE)
nodes <- read.csv("./data/networks/toy2/node.attr.csv")


# Load Balancing Process --------------------------------------------------
## The load should be balanced to meet the nodes with highest demand     ##
## first.                                                                ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


loadBalance <- function ( el, nodes){
	require(igraph)
	g <- graph.data.frame(el,directed=TRUE, vertices=nodes)	#builds the network in igraph form
	l <- layout.fruchterman.reingold(g)
	E(g)$width <- E(g)$Constraint / 5
	V(g)$color <- ifelse(V(g)$Supply > 0, "lightgreen","red")
	fin <- FALSE
 	plot(g, layout=l)
	nodePriority <- sort(V(g)$Demand, decreasing=TRUE, index.return=TRUE)$ix	#assigns priority by greatest demand
	
	for (i in 1:length(nodePriority)){
		s.nodes <- V(g)$name[V(g)$Supply>0]			# fetch first supply node to try to supply demand node k
		for(j in s.nodes){
			pathStart <- as.integer(s.nodes[j])			# assigns supply node
			pathEnd   <- nodePriority[i]						# assigns demand node
			x <- get.all.shortest.paths(g, pathStart, pathEnd, 
																	mode="out")	# get paths between supply and demand node
			xl <- length(x[["res"]])								# number of paths from supply node j to demand node i
			for( k in 1:xl){
				minFlow <- min(E(g,path=x[["res"]][[k]])$Constraint)  #retrieves the minimum flow constraint along the path
				# Determine if supply, demand, or flow is limiting
				supply <- V(g)$Supply[c(pathStart)]					# quantity available to supply
				demand <- V(g)$Demand[c(pathEnd)]						# quantity demanded
				m.min <- min(c(minFlow, supply, demand))		# minimum of minimums (supply, demand, smallest flow constraint)
				V(g)$Supply[pathStart] <- supply - min
				V(g)$Demand[pathEnd]  <- demand - min
				if(supply == min && demand == min)
				}
			}
			
			


				
				
		}
	}
	
	while (fin == FALSE){
		k <- which.max(V(g)$Demand)							# updates node with maximum demand
		s.nodes <- V(g)$name[V(g)$Supply>0]			# fetch first supply node to try to supply demand node k
		nn <- 1
		checked.nodes <- as.vector(c())
		while(checked.nodes != s.nodes){
			checked.nodes <- as.vector(s.nodes[nn])
			
			
			
			
		}
		while( V(g)$Demand[k]>0){								# need to update looping condition to include instance when demand cannot be reduced to 0
			
			
		}

	}
	x <- get.all.shortest.paths(g, 1, 5, mode="out")
	
	#once I find all the paths to and from my vertices of choice, I want to find the path
	#with the greatest available line capacity
	
	minFlow <- min(E(g,path=x[["res"]][[1]])$Constraint)    #retrieves the minimum flow constraint along the path
	
	# Determine if supply, demand, or flow is limiting
	
	pathStart <- x[["res"]][[1]][1] 											# assigns supply node
	pathEnd   <- x[["res"]][[1]][length(x[["res"]][[1]])] # assigns demand node
	supply <- V(g)$Supply[c(pathStart)]										# quantity available to supply
	demand <- V(g)$Demand[c(pathEnd)]											# quantity demanded
	min <- min(c(minFlow, supply, demand))
	
	if(min > 0){
		V(g)$Supply[pathStart] <- supply - min
		V(g)$Demand[pathEnd]   <- demand - min
	}
	
	g <- set.vertex.attribute(g, "Supply", index=V(g,), supply - min)
	V(g)$Demand[pathEnd]	  <- demand - min
	get.data.frame(g,what="edges")
	E(g,path=x[["res"]][[1]])$Constraint <- E(g,path=x[["res"]][[1]])$Constraint - min
	get.data.frame(g,what="edges")
}