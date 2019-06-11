# SLow work in progress

# library(rdata.psych) # importing data from library

# Cluster analysis
dAct <- nodeMatrix[, 2:8]
kmed <- pam(dAct, 5)
nodes <- bind_cols(nodes, data.frame(cluster = kmed$cluster))

# Joining cluster
nodesBio <- left_join(nodesBio, nodes %>% select(student, cluster))
nodesSoc <- left_join(nodesSoc, nodes %>% select(student, cluster))
aaBio18 <- left_join(aaBio18, nodes %>% select(student, cluster))
aaSoc18 <- left_join(aaSoc18, nodes %>% select(student, cluster))

# Making graph structures
networkBio <- tbl_graph(nodes = nodesBio, edges = edgesBio) 
networkSoc <- tbl_graph(nodes = nodesSoc, edges = edgesSoc)
