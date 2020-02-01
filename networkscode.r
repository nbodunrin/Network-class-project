a=erdos.renyi.game(100,1/4, type = "gnm", directed = FALSE,loops = FALSE)
#the number of edges
numberofedges = gsize(a)
degree_distribution(a)
sum(degree(a))
g2 <- make_full_graph(n=10)
g <- sample_gnm(225, 5040)
gsize(g)
betw=betweenness(g, directed = FALSE)
max(betw)### use it to get the node with highest bewtness
edge_density(g, loops=FALSE)    
multsm = sir(g, beta=5, gamma=1, no.sim = 100)
time_bins(sm, middle = TRUE)
quantile(sm, comp = c("NI", "NS", "NR"), 0.95)

sm <- sir(g, beta=5, gamma=1)
plot(multsm)
plot(sm, comp = c("NI", "NS", "NR"), median = TRUE,
     quantiles = c(0.1, 0.9), color = NULL, median_color = NULL,
     quantile_color = NULL, lwd.median = 2, lwd.quantile = 2,
     lty.quantile = 3, xlim = NULL, ylim = NULL, xlab = "Time",
     ylab = NULL)
lay <- layout_nicely(pref)

id <- tkplot(pref, layout=lay)
canvas <- tk_canvas(id)
tkpostscript(canvas, file="/tmp/output.eps")
#tk_close(id)

# Number of edges in a G(n,p) graph
rep= replicate(100, sample_gnp(100, 1/2), simplify = FALSE);
  #vapply(gsize, 0);
  hist(adj)

g <- sample_gnp(225, 4/10)
adj = as_adjacency_matrix(g)
V(g)$name <- letters[1:vcount(g)]
as_adjacency_matrix(g)
E(g)$weight <- runif(ecount(g))
as_adjacency_matrix(g, attr="weight")

## code from stack overflow
library(igraph)
setwd("/Volumes/NATASHKA/api/R files")
load("edgelist_one_mode.rda")
load("map.rda")
load ("result.rda")
el <- as.matrix(whatwewant)
el[,1] <- as.character(el[,1])
el[,2] <- as.character(el[,2])
g <- graph.data.frame(el, directed=FALSE)
plot(g, edge.arrow.size=.5)
indegreeG <- degree(g, mode="in")
outdegreeG <- degree(g, mode="out")
totaldegreeG <- degree(g)
inclosenessG <- closeness(g, mode='in')
outclosenessG <- closeness(g, mode='out')
totalclosenessG <- closeness(g)
betweennessG <- betweenness(g)
forumG <- data.frame(V(g)$name, indegreeG, outdegreeG, totaldegreeG, inclosenessG,    outclosenessG, totalclosenessG, betweennessG)
write.table(forumG,file="forumG.csv",sep=";")

g1 <- make_empty_graph(n=10)

pref = sample_pa(10, power = 1, m = 2, out.dist = NULL, out.seq = NULL,
          out.pref = FALSE, zero.appeal = 1, directed = FALSE,
          algorithm = c("psumtree", "psumtree-multiple", "bag"), start.graph = NULL)