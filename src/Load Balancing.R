source("./src/01 Initialization.R")
# Load a test network -----------------------------------------------------
# in this section we load a test network in order to test the heuristic 
# load balancing 
# reading the 'toy1' network and attributes
# start <- proc.time()
el <- read.csv("./data/networks/toy1/edgelist.csv", header=TRUE)
nodes <- read.csv("./data/networks/toy1/node.attr.csv")

demand_met <- loadBalance(el,nodes)
# proc.time() - start
# Load Balancing Process --------------------------------------------------
## The load should be balanced to meet the nodes with highest demand     ##
## first.                                                                ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
loadBalance <- function ( el, nodes){
	g <- graph.data.frame(el,directed=TRUE, vertices=nodes)	#builds the network in igraph format
	#l <- layout.fruchterman.reingold(g)											#defines a layout; this one is fairly readible
	#later we can define the location of the network based on actual lat/long coordinates
	E(g)$width <- rescale(E(g)$Constraint,to=c(0.5,10))
	V(g)$color <- ifelse(V(g)$Supply > 0, "lightgreen","red")
	zzz <- 1
	png(file=paste("./reports/Load Balancing Results/", zzz, ".png", sep=""))
 	plot(g, layout=l, main=paste("State of the Network: ", zzz))
	dev.off()
	nodePriority <- sort(V(g)$Demand, decreasing=TRUE, index.return=TRUE)$ix	#assigns priority by greatest demand
	nodePriority <- nodePriority[as.numeric(V(g)[Demand>0]$name)]
	for (i in 1:length(nodePriority)){
# 		print(paste("i =", i))
		s.nodes <- as.integer(V(g)$name[V(g)$Supply>0])			# fetch first supply node to try to supply demand node k
		pathEnd   <- nodePriority[i]						# assigns demand node
		j <- 1
		while(j <= length(s.nodes)){
# 			print(paste("j =",j))
			pathStart <- s.nodes[j]			# assigns supply node
			
			x <- get.all.shortest.paths(g, pathStart, pathEnd, 
																	mode="out")	# get paths between supply and demand node
			xl <- length(x[["res"]])								# number of paths from supply node j to demand node i
			w <- 1
			while(w <= xl){
				if(length(x[["res"]][[w]]) != 1){
					minFlow <- min(E(g,path=x[["res"]][[w]])$Constraint)   # minimum constraint along path
					if(minFlow == 0){
						x[["res"]][[w]] <- NULL
						xl <- xl - 1
					} else
						w <- w + 1
				} else
					w <- w + 1

			}
			if(xl !=0){
				k <- 1
				while( k <= xl){
# 					print(paste("k =", k))
										# set the minimum flow along path 'k'
					if(pathStart != pathEnd){
						minFlow <- min(E(g,path=x[["res"]][[k]])$Constraint)  #retrieves the minimum flow constraint along the path
					} else
						minFlow <- Inf 		# set to be "Inf" as long as the path start and end node is the same
					# Determine if supply, demand, or flow is limiting
					supply <- V(g)$Supply[c(pathStart)]					# quantity available to supply
					demand <- V(g)$Demand[c(pathEnd)]						# quantity demanded
					m.min <- min(c(minFlow, supply, demand))		# minimum of minimums (supply, demand, smallest flow constraint)
					V(g)$Supply[pathStart] <- supply - m.min 		# update supply remaining
					V(g)$Demand[pathEnd]  <- demand - m.min 		# update demand remaining
					E(g,path=x[["res"]][[k]])$Constraint <- E(g,path=x[["res"]][[k]])$Constraint - m.min
					E(g)$width <- E(g)$Constraint / 2
					
					if(demand == m.min){													# if demand is met we exit loops and switch demand nodes
						k <- xl	
						j <- length(s.nodes)
					}
					if(pathStart != pathEnd)
						if(min(E(g,path=x[["res"]][[k]])$Constraint) == 0){
						k <- xl
						j <- j - 1
					}					
					path <- paste(x[["res"]][[k]],collapse="")
					
					## deleting edges that are 0
					g <- delete.edges(g,E(g)[E(g)$Constraint==0])
					
					zzz <- zzz + 1
					png(file=paste("./reports/Load Balancing Results/", zzz," - ", path, ".png", sep=""))
					plot(g, layout=l, main=paste("State of the Network: ", zzz))
					dev.off()		
					
					k <- k + 1
				}
			}
			j <- j + 1
		}	
	}
	x <- data.frame(Node=nodes$Node, Demand=nodes$Demand, 
									Remaining=get.data.frame(g,what="vertices")$Demand)
	return(x)
}