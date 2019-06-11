# Creating list of students to render
library(tidyverse)
library(lubridate)
library(GGally)
library(ggraph)
library(ggthemes)
library(ggpubr)
library(lakit)
library(dendextend)
library(ggsci)
library(factoextra)
library(cluster)
library(rgl)
library(tsibble)
library(sugrrants)
library(gganimate)
library(tidygraph)

dirpath <- file.path('~','csu-psychology-engagement')
source(file.path(dirpath,'R','psych_functions.R'))
source(file.path(dirpath, 'R', 'import-data.R'))

# graph set up
bio_cc_nodes <- nodes_from_cc(ccBio18, subject = "Bio")
bio_cc_edges <- edges_from_cc(ccBio18)
soc_cc_nodes <- nodes_from_cc(ccSoc18, subject = "Soc")
soc_cc_edges <- edges_from_cc(ccSoc18)
bio_cc_graph <- tbl_graph(nodes = bio_cc_nodes, 
                          edges = bio_cc_edges)
soc_cc_graph <- tbl_graph(nodes = soc_cc_nodes,
                          edges = soc_cc_edges)
bio_cc_layout <- create_layout(bio_cc_graph, layout = "nicely")
soc_cc_layout <- create_layout(soc_cc_graph, layout = "nicely")
bio_np <- data.frame(x = bio_cc_layout$x, y = bio_cc_layout$y)
soc_np <- data.frame(x = soc_cc_layout$x, y = soc_cc_layout$y)

# Qual
qual_students <- studentList[studentList$qual_participant,]$Student
for (student in qual_students) {
  fn <- paste0(student, '_profile.html')
  rmarkdown::render(input = file.path(dirpath, 'Reports','student profile.Rmd'),
                           output_file = file.path(dirpath, 'Reports', fn))
}

# Medoids
medoids_nodes <- nodes[kmed$id.med,]
medoid_students <- medoids_nodes$Student
for (student in medoid_students) {
  cluster <- medoids_nodes[medoids_nodes$Student == student,]$cluster
  fn <- paste0('Medoid_cluster_', cluster, '_profile.html')
  rmarkdown::render(input = file.path(dirpath, 'Reports','student profile.Rmd'),
                    output_file = file.path(dirpath, 'Reports', fn))
}
