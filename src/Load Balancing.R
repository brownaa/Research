# Load a test network -----------------------------------------------------
# in this section we load a test network in order to test the heuristic 
# load balancing 
library(RCurl)
# reading the 'toy2' network and attributes
el <- read.csv("./data/networks/toy2/edgelist.csv", header=TRUE)
nodes <- read.csv("./data/networks/toy2/node.attr.csv")

loadBalance(el,nodes)
# Load Balancing Process --------------------------------------------------
## The load should be balanced to meet the nodes with highest demand     ##
## first.                                                                ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
loadBalance <- function ( el, nodes){
	g <- graph.data.frame(el,directed=TRUE, vertices=nodes)	#builds the network in igraph format
	l <- layout.fruchterman.reingold(g)
	E(g)$width <- E(g)$Constraint / 5
	V(g)$color <- ifelse(V(g)$Supply > 0, "lightgreen","red")
	fin <- FALSE
	zzz <- 1
	png(file=paste("./reports/Load Balancing Results/", zzz, ".png", sep=""))
 	plot(g, layout=l, main=paste("State of the Network: ", zzz))
	dev.off()
	nodePriority <- sort(V(g)$Demand, decreasing=TRUE, index.return=TRUE)$ix	#assigns priority by greatest demand
	nodePriority <- nodePriority[!nodePriority==as.vector(V(g)[Supply>0]$name)]
	for (i in 1:length(nodePriority)){
		s.nodes <- as.integer(V(g)$name[V(g)$Supply>0])			# fetch first supply node to try to supply demand node k
		pathEnd   <- nodePriority[i]						# assigns demand node
		for(j in 1:length(s.nodes)){
			pathStart <- s.nodes[j]			# assigns supply node
			
			x <- get.all.shortest.paths(g, pathStart, pathEnd, 
																	mode="out")	# get paths between supply and demand node
			xl <- length(x[["res"]])								# number of paths from supply node j to demand node i
			if(xl !=0){
				for( k in 1:xl){
					minFlow <- min(E(g,path=x[["res"]][[k]])$Constraint)  #retrieves the minimum flow constraint along the path
					# Determine if supply, demand, or flow is limiting
					supply <- V(g)$Supply[c(pathStart)]					# quantity available to supply
					demand <- V(g)$Demand[c(pathEnd)]						# quantity demanded
					m.min <- min(c(minFlow, supply, demand))		# minimum of minimums (supply, demand, smallest flow constraint)
					V(g)$Supply[pathStart] <- supply - m.min 		# update supply remaining
					V(g)$Demand[pathEnd]  <- demand - m.min 		# update demand remaining
					E(g,path=x[["res"]][[k]])$Constraint <- E(g,path=x[["res"]][[k]])$Constraint - m.min
					E(g)$width <- E(g)$Constraint / 5
					
					zzz <- zzz + 1
					png(file=paste("./reports/Load Balancing Results/", zzz, ".png", sep=""))
					plot(g, layout=l, main=paste("State of the Network: ", zzz))
					dev.off()		
					
					if(demand == m.min){													# if demand is met we exit loops and switch demand nodes
						k <- xl	
						j <- length(s.nodes)
					}
				}
			}
			
		}	
	}
}