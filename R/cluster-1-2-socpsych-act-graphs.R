
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

library(rdata.psych)
ZERODATE <- '2018-06-04'
SESSIONSTART <- '2018-07-09'
SESSIONEND <- '2018-10-26'

# Graphing ----------------------------------------------------------------
# Building underlying site structure
content_primary_keys <- sort(unique(ccSoc18$pk1))
content_primary_keys <- factor(content_primary_keys, levels = content_primary_keys)
content_levels <- levels(content_primary_keys)

cc_nodes <-  ccSoc18 %>% 
  select(id = pk1, title, data, handle) %>% 
  mutate(id = factor(id, levels = content_levels))

cc_edges <-  tibble(
  from = factor(ccSoc18$parent_pk1, levels = content_levels),
  to = factor(ccSoc18$pk1, levels = content_levels),
  alfalfa = 1.0
) %>% 
  filter(!is.na(from), !is.na(to))

cc_edges <- bind_rows(cc_edges %>% mutate(cluster = 1), 
                      cc_edges %>% mutate(cluster = 2), 
                      cc_edges %>% mutate(cluster = 3), 
                      cc_edges %>% mutate(cluster = 4), 
                      cc_edges %>% mutate(cluster = 5)) 
cc_edges <- bind_rows(cc_edges %>% mutate(time = as_datetime('2018-07-01')),
                      cc_edges %>% mutate(time = as_datetime('2018-08-01')),
                      cc_edges %>% mutate(time = as_datetime('2018-09-01')),
                      cc_edges %>% mutate(time = as_datetime('2018-10-01')))
cc_graph <- tbl_graph(nodes = cc_nodes, edges = cc_edges)
cc_layout <- create_layout(cc_graph, layout = "nicely")
np <- data.frame(x = cc_layout$x, y = cc_layout$y)

# Student activity overlay
aaEdgePrep <- aaSoc18 %>% 
  data.table() %>% 
  .[timestamp <= as.Date('2018-10-30') & timestamp > as.Date('2018-07-02') , 
    .(timestamp, session, person, cluster, content_pk1)] %>% 
  setorder(person, timestamp)

i_to <- 2:nrow(aaEdgePrep)
i_fm <- 1:(nrow(aaEdgePrep) - 1)

aa_edges <- data.table(
  from =   factor(aaEdgePrep$content_pk1[i_fm], levels = content_levels),
  to =     factor(aaEdgePrep$content_pk1[i_to], levels = content_levels),
  pfrom =  aaEdgePrep$person[i_fm],
  pto =    aaEdgePrep$person[i_to],
  sfrom =  aaEdgePrep$session[i_fm],
  sto =    aaEdgePrep$session[i_to],
  time =   aaEdgePrep$timestamp[i_fm],
  cluster = aaEdgePrep$cluster[i_fm],
  alfalfa = 0.05
) %>% .[pfrom == pto & sfrom == sto, .(from, to, time, cluster, alfalfa)] %>% 
  .[!is.na(from) & !is.na(to)]


# Single cluster edge compression
edge_compression <- function(x) {
  x[!is.na(time) , time := floor_date(time, unit = "month")]
  x <- x %>% 
    group_by(from, to, time, cluster, alfalfa) %>% 
    summarise(weight = log(n() + 1))
  return(x)
}

aa_edges <- edge_compression(aa_edges)

aa_graph <- tbl_graph(nodes = cc_nodes, edges = aa_edges)

graph <- graph_join(cc_graph, aa_graph)

g1 <- graph %>%
  activate(edges) %>% 
  filter(cluster == 1) %>% 
  ggraph(layout = "manual", node.positions = np) +
  geom_node_point() +
  geom_edge_link(aes(color = as.numeric(time),
                     group = time,
                     alpha = alfalfa)) +
  scale_edge_color_gradientn(colours = c("#bbbbbb", "#666666", "#000000")) +
  theme_graph()+
  theme(legend.position = "none",
        text = element_text(family = "TT Times New Roman")) 

g <- graph %>% 
  ggraph(layout = "manual", node.positions = np) +
  geom_node_point(size = 0.2) +
  geom_edge_link(aes(color = alfalfa,
                     alpha = alfalfa)) +
  scale_edge_color_gradientn(colours = c("#000000", "#666666", "#888888")) +
  theme_graph() +
  theme(legend.position = "none",
        text = element_text(family = "TT Times New Roman")) +
  facet_grid(cluster ~ time)

g12 <- graph %>%
  activate(edges) %>% 
  filter(cluster %in% 1:2) %>% 
  ggraph(layout = "manual", node.positions = np) +
  geom_node_point(aes(color = handle), size = 0.2) +
  geom_edge_link(aes(color = alfalfa,
                     alpha = alfalfa)) +
  scale_edge_color_gradientn(colours = c("#000000", "#666666", "#888888")) +
  theme_graph() +
  theme(legend.position = "none",
        text = element_text(family = "TT Times New Roman")) +
  facet_grid(cluster ~ time)


