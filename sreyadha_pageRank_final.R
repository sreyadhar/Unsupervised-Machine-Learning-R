rm(list = ls())
graphics.off()

library(igraph)

### Qs.1 : Compute the page rank vector the graph below for damping constants:
### p =0, p =.10, p=.15, p=.25, p = .50 and p=1.

# Construct an Igraph
nodes <- data.frame(names = c("Page1", "Page2", "Page3", "Page4", "Page5", "Page6"))
relations <- data.frame(
  from = c("Page2", "Page2", "Page3", "Page4", "Page4", "Page5", "Page6"), 
    to = c("Page3", "Page5", "Page1", "Page2", "Page5", "Page4", "Page3"))
g <- graph.data.frame(relations, directed = TRUE, vertices = nodes)

plot(g)

## Run the page rank algorithm with different damping coeff: ##
(pg_p0 <- page.rank(g, damping=1)$vector)  ## p = 0, d = 1-0 =1


(pg_p10 <- page.rank(g, damping=0.90)$vector)  ## p = 0.10, d = 1-0.1 =0.9

(pg_p15 <- page.rank(g, damping=0.85)$vector)  ## p = 0.15, d = 1-0.15 =0.85

(pg_p25 <- page.rank(g, damping=0.75)$vector)  ## p = 0.25, d = 1-0.25 =0.75

(pg_p50 <- page.rank(g, damping=0.5)$vector)  ## p = 0.5, d = 1-0.5 =0.5

(pg_p100 <- page.rank(g, damping=0.0)$vector)  ## p = 1.0, d = 1-1 =0


##########################################################################################################
### Qs.2 : Compute the page rank for the graph below. Use a damping constant p=0.15. Interpret
### the results. Are they within your expectation?

rm(list = ls())
graphics.off()

library(igraph)

# Construct an Igraph
nodes <- data.frame(names = c("Page1", "Page2", "Page3", "Page4", "Page5", "Page6",  "Page7", "Page8"))
relations <- data.frame(
  from = c("Page2", "Page3", "Page4", "Page5", "Page6", "Page7", "Page8"), 
  to   = c("Page1", "Page1", "Page2", "Page2", "Page3", "Page3", "Page3"))
g <- graph.data.frame(relations, directed = TRUE, vertices = nodes)

plot(g)

# Run the page rank algorithm with different damping coeff: ##
(pg_p <- page.rank(g, damping=0.85)$vector)  ## p = 0.15


### Pagerank vector sum ###
sum(pg_p) ## == 1

### Interpretation ###
(v1 <- page.rank(g, vids=V(g),
                directed=T, damping=.85,
                personalized = c(1,0,0,0,0,0,0,0))$vector) ## personalising a component 

(v2 <- page.rank(g, vids=V(g),
                directed=T, damping=.85,
                personalized = c(0,1,0,0,0,0,0,0))$vector)

(v3 <- page.rank(g, vids=V(g),
                directed=T, damping=.85,
                personalized = c(0,0,1,0,0,0,0,0))$vector)

(v4 <- page.rank(g, vids=V(g),
                directed=T, damping=.85,
                personalized = c(0,0,0,1,0,0,0,0))$vector)

(v5 <- page.rank(g, vids=V(g),
                directed=T, damping=.85,
                personalized = c(0,0,0,0,1,0,0,0))$vector)

(v6 <- page.rank(g, vids=V(g),
                directed=T, damping=.85,
                personalized = c(0,0,0,0,0,1,0,0))$vector)

(v7 <- page.rank(g, vids=V(g),
                directed=T, damping=.85,
                personalized = c(0,0,0,0,0,0,1,0))$vector)

(v8 <- page.rank(g, vids=V(g),
                directed=T, damping=.85,
                personalized = c(0,0,0,0,0,0,0,1))$vector)

(mean_v <- rowMeans(cbind(v1, v2, v3, v4, v5, v6, v7, v8)))

round(mean_v,2)==round(pg_p,2) ## same vectors


### end ###

