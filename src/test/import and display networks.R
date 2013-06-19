library(igraph)

oldwd <- getwd()
setwd("/Users/brownaa/Dropbox/Documents/School/2013/Research/R Proj/data/networks/")
EL <- as.matrix(read.csv("./toy2/edgelist.csv", header=TRUE))
plot(graph.edgelist(EL[,1:2]))

EL <- read.csv("./toy2/edgelist.csv", header=TRUE)
nodes <- read.csv("./toy2/node.attr.csv")

g <- graph.data.frame(EL,directed=TRUE, vertices=nodes)

get.data.frame(g, what="edges")
get.data.frame(g, what="vertices")

plot(g,layout=layout.kamada.kawai, edge.arrow.size=E(g)$Constraint/30, 
		 edge.width=E(g)$Constraint/5)

setwd(oldwd)
