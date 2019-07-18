
# Examining early / late site activity comparison between  ----------------
# cluster 1 and cluster 2 Social Psychology students 
# Needs to have high quality but no colour (expensive to print in journals)
# -------------------------------------------------------------------------


# Load and build ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(GGally)
library(network)
library(tidygraph)
library(ggraph)
library(ggthemes)
library(lakit)
library(dendextend)
library(cluster)
library(factoextra)
library(ggsci)
library(data.table)

source(file.path( 'R', 'psych_functions.R'))
source(file.path( 'R', 'import-data.R'))

# Graphing ----------------------------------------------------------------
# Building underlying site structure
cc_nodes <-  nodes_from_cc(ccSoc18, subject = "Soc")
cc_edges <-  edges_from_cc(ccSoc18)
cc_edges <- bind_rows(cc_edges %>% mutate(cluster = 1), 
                      cc_edges %>% mutate(cluster = 2)) # might need to add time variable for faceting in a similar repeated manner
cc_graph <- tbl_graph(nodes = cc_nodes, edges = cc_edges)
cc_layout <- create_layout(cc_graph, layout = "nicely")
np <- data.frame(x = cc_layout$x, y = cc_layout$y)

# Student activity overlay
aaSocCluster12 <- aaSoc18 %>% 
  filter(cluster %in% 1:2, timestamp < as.Date('2018-10-30'))

# Single cluster edge compression
cluster_edge_compression <- function(x) {
  if (length(unique(x$cluster)) != 1) stop("Number of unique clusters not 1")
  
}

# Cluster summary fn - using cluster as pk instead of student
cluster_aa_compression <- function(aa) {
  aa %>% 
    filter(role == 'S', !is.na(content_pk1), timestamp < as.POSIXct('2018-10-30')) %>% 
    select(id, time = timestamp, content_pk1, session) %>%  
    #grade_quartile, cluster, qual) %>% 
    mutate(id = factor(id), content_pk1 = factor(content_pk1)) %>% 
    arrange(id, time)
}

aa_edges <-   
  x %>% 
  filter(role == 'S', !is.na(content_pk1), timestamp < as.POSIXct('2018-10-30')) %>% 
  select(id, time = timestamp, content_pk1, session) %>%  
  #grade_quartile, cluster, qual) %>% 
  mutate(id = factor(id), content_pk1 = factor(content_pk1)) %>% 
  arrange(id, time)
aa_edges <- tibble(from = aa$content_pk1[1:(nrow(aa) - 1)],
                   to =   aa$content_pk1[2:nrow(aa)],
                   time = aa$time[2:nrow(aa)],
                   student_from = aa$id[1:(nrow(aa) - 1)],
                   student = aa$id[2:nrow(aa)],
                   session_from = aa$session[1:(nrow(aa) - 1)],
                   session = aa$session[2:nrow(aa)])
#grade_quartile = aa$grade_quartile[1:(nrow(aa) - 1)],
#cluster = aa$cluster[1:(nrow(aa) - 1)],
#qual = aa$qual[1:(nrow(aa) - 1)]
aa_edges <- aa_edges %>% 
  filter(student == student_from, session == session_from) %>% 
  select(from, to, time, student, cluster) %>% # , grade_quartile, cluster, qual 
  mutate(weight = 2)
return(aa_edges)


aa_nodes <-  nodes_from_aa(aaSoc18 
                         %>% filter(cluster %in% c(1, 2)))
aa_edges <- edges_from_aa_cluster(aaSoc18 
                                 %>% filter(cluster %in% c(1, 2)))

aa_graph <- tbl_graph(nodes = cc_nodes, edges = aa_edges)
joined_graph <- cc_graph %>% graph_join(aa_graph) 
joined_graph <- joined_graph %>% 
  activate(edges) %>% 
  mutate(student = if_else(is.na(student), "_course structure", as.character(student)))
graph_list <- list(joined_graph, np)

draw_site_activity_graph_greyscale <- function(graph_list, alpha_low = 0.1) {
  g <- ggraph(graph_list[[1]], layout = "manual", node.positions = graph_list[[2]]) +
    geom_node_point(color = "#bbbbbb") + 
    geom_edge_link0(aes(color = as.numeric(time), 
                        width = weight, 
                        alpha = 2 - weight,
                        group = time)) +
    scale_edge_alpha_continuous(range = c(alpha_low, 1)) +
    scale_edge_width(range = c(0.6,1)) +
    scale_edge_color_gradientn(colors = c("#bbbbbb", "#666666", "#000000")) +
    theme_graph() +
    theme(legend.position = "none",
          text = element_text(family = "TT Times New Roman"))
  return(g)
}

draw_site_activity_graph_greyscale(
  graph_list = graph_list, alpha_low = 0.02) + facet_wrap(~ cluster) + ggtitle("Social Psychology activity by cluster")
