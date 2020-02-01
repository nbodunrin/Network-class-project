install.packages("devtools")
library(devtools)
install.packages("rgl")
library(rgl)
## random networks
##using density of 13% m = 32435
rand= erdos.renyi.game(500, 32435, type = "gnm", directed = FALSE,
                 loops = FALSE) %>%
  plot()
summary(rand)
plot(degree.distribution(pref, mode="all"), log="xy")
replicate(100000, erdos.renyi.game(500, 0.13 , type = "gnp", directed = FALSE,loops = FALSE), simplify = FALSE) %>%
  vapply(gsize,0) %>%
  hist()
graph.density(rand)

A =as_adj(rand, type = "both", attr = NULL,
          edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))
Alist = as_adj_list(rand)

gsize(rand)
degrand= degree(rand, v = V(rand), mode ="all",loops = FALSE, normalized = FALSE)
hist(degrand)
degdrand=degree_distribution(rand)
plot(degdrand , type = "l")

randmul= sir(rand, beta = 5, gamma= 10, no.sim = 10)


plot(randmul, comp = c("NI", "NS", "NR"), median = TRUE,
     quantiles = c(0.1, 0.9), color = "black", median_color = "red",
     quantile_color = "green", lwd.median = 2, lwd.quantile = 2,
     lty.quantile = 3, xlim = NULL, ylim = NULL, xlab = "Time",
     ylab = NULL)


## preferential attachment 
pref = sample_pa(500, power = 1, m = 2, out.dist = NULL, out.seq = NULL,
          out.pref = FALSE, zero.appeal = 1, directed = FALSE,
          algorithm = c("psumtree", "psumtree-multiple", "bag"), start.graph = NULL) %>%
  plot()

A =as_adj(pref, type = "both", attr = NULL,
          edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices"))

gsize(pref)
degpref= degree(pref, v = V(pref), mode ="all",loops = FALSE, normalized = FALSE)
degdpref=degree_distribution(pref)
plot(degdpref, type= "l")
hist(degpref)

prefmul= sir(pref, beta = 5, gamma= 10, no.sim = 10)


plot(prefmul, comp = c("NI", "NS", "NR"), median = TRUE,
     quantiles = c(0.1, 0.9), color = "black", median_color = "red",
     quantile_color = "green", lwd.median = 2, lwd.quantile = 2,
     lty.quantile = 3, xlim = NULL, ylim = NULL, xlab = "Time",
     ylab = NULL)


replicate(100000, sample_pa(500, power = 1, m = 2, out.dist = NULL, out.seq = NULL,
                            out.pref = FALSE, zero.appeal = 1, directed = FALSE,
                            algorithm = c("psumtree", "psumtree-multiple", "bag"), start.graph = NULL), simplify = FALSE) %>%
  vapply(gsize,0) %>%
  hist()


## 3d
coords <- layout_with_fr(g, dim=3)
rglplot(g, layout=coords)


## interactive graph
lay <- layout_nicely(pref)

id <- tkplot(pref, layout=lay)
canvas <- tk_canvas(id)
tkpostscript(canvas, file="/tmp/output.eps")
tk_close(id)


plot(g, layout=layout.fruchterman.reingold,
     vertex.label=NA, vertex.size=5)

plot(g, layout=layout.reingold.tilford)

plot(degree.distribution(rand), xlab="node degree")
lines(degree.distribution(rand))


g <- barabasi.game(60, power=1, zero.appeal=1.3)

g<-network(25, directed=FALSE, density=0.1)

