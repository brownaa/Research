library(igraph)

## ~~~~~ Update the working directory to the location of my networks ~~~~~
oldwd <- getwd()		#preserve old working directory to permit switching
setwd("/Users/brownaa/Dropbox/Documents/School/2013/Research/R Proj/data/networks/")
## ~~~~~

EL <- read.csv("./toy2/edgelist.csv", header=TRUE)
nodes <- read.csv("./toy2/node.attr.csv")

g <- graph.data.frame(EL,directed=TRUE, vertices=nodes)
plot(g,layout=layout.kamada.kawai, edge.arrow.size=E(g)$Constraint/30, 
		 edge.width=E(g)$Constraint/5)

# get.data.frame(g, what="edges")				# returns the edges    dataframe with attributes
# get.data.frame(g, what="vertices")		# returns the vertecis dataframe with attributes

## ~~~~~ return the working directory to its original location
setwd(oldwd)