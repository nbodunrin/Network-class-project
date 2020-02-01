install.packages("rgexf")
library(rgexf)

# create a sample igraph object
g <- erdos.renyi.game(500, 0.13, directed = FALSE,loops = FALSE)

plot(pref)
# construct the nodes and edges data for gexf conversion
nodes <- data.frame(cbind(V(g), as.character(V(g))))
edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))

# do the conversion
gephiER<-write.gexf(nodes, edges)    
print(gephiER, file="mygraph1.gexf")

##preferential attachment
pref=sample_pa(50,directed = FALSE)
pref = sample_pa(50, power = 3, m = 2, out.dist = NULL, out.seq = NULL,
                 out.pref = FALSE, zero.appeal = 1, directed = FALSE,
                 algorithm =   "psumtree", start.graph = NULL)
nodes <- data.frame(cbind(V(pref), as.character(V(pref))))
edges <- t(Vectorize(get.edge, vectorize.args='id')(pref, 1:ecount(pref)))

# do the conversion
gephiER<-write.gexf(nodes, edges)    
print(gephiER, file="pref.gexf")