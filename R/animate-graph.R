# loading data
library(tidyverse)
library(ggraph)
library(tidygraph)
library(gganimate)
DATDIR <- file.path("~", "Data", "Psychology")
BIOAAFILE <- "aa Biopsychology 201860.csv"
BIOCCFILE <- "cc Biopsychology 201860.csv"


aa_full <- read_csv(file.path(DATDIR, BIOAAFILE))
cc_full <- read_csv(file.path(DATDIR, BIOCCFILE)) 

aa <- aa_full %>% 
  filter(role == 'S', !is.na(content_pk1)) %>% 
  select(id, time = timestamp, content_pk1, session = session_id) %>% 
  mutate(id = factor(id), content_pk1 = factor(content_pk1)) %>% 
  arrange(id, time)

cc <- cc_full %>% 
  select(pk1, parent_pk1, title) %>% 
  mutate(pk1 = factor(pk1), parent_pk1 = factor(parent_pk1))

# creating edgelist from aa
aa_edges <- tibble(from = aa$content_pk1[1:(nrow(aa) - 1)],
                   to =   aa$content_pk1[2:nrow(aa)],
                   time = aa$time[2:nrow(aa)],
                   student_from = aa$id[1:(nrow(aa) - 1)],
                   student = aa$id[2:nrow(aa)],
                   session_from = aa$session[1:(nrow(aa) - 1)],
                   session = aa$session[2:nrow(aa)])
# filter out any pairs not belonging to the same id and session
aa_edges <- aa_edges %>% 
  filter(student == student_from, session == session_from) %>% 
  select(from, to, time, student) %>% 
  mutate(weight = 2)

cc_edges <- cc %>% 
  select(from = parent_pk1, to = pk1) %>% 
  filter(!is.na(from)) %>% 
  mutate(weight = 1, time = as.POSIXct('2018-03-01')) # used manual set time - needs to be well before other data

# creating node lists
cc_nodes <- cc %>% select(id = pk1, title)
aa_nodes <- aa %>% select(id = content_pk1) %>% unique()

cc_graph <- tbl_graph(nodes = cc_nodes, edges = cc_edges)
cc_layout <- create_layout(cc_graph, layout = "nicely")
np <- data.frame(x = cc_layout$x, y = cc_layout$y)

aa_graph <- tbl_graph(nodes = cc_nodes, edges = aa_edges)
aa_sg <- aa_graph %>% activate(edges) %>% filter(student == sample(student, 1))

joined_graph <- cc_graph %>% graph_join(aa_sg) 


g <- ggraph(joined_graph, layout = "manual", node.positions = np) + 
  geom_node_point() + 
  geom_edge_link0(aes(color = as.numeric(time), width = weight, group = time, linetype = student), alpha = 0.2, show.legend = FALSE) +
  geom_edge_fan(aes(alpha = 2 - weight), color = 'black', spread = 0) + # not sure if this will work
  scale_edge_width(range = c(0.7,1)) +
  scale_edge_color_continuous(low = "black", high = "turquoise")+
  theme_graph() +
  transition_time(time) +
  shadow_mark(1, colour = "black")
g


# previously only the manual version was working
p <- ggraph(joined_graph, layout = "manual", node.positions = np) + 
  geom_node_point() + 
  geom_edge_link0(aes(color = as.numeric(time), width = weight, group = time), alpha = 0.2, show.legend = FALSE) +
  scale_edge_width(range = c(0.7,1)) +
  scale_edge_color_continuous(low = "Black", high = "Turquoise")+
  theme_graph() +
  transition_manual(time, cumulative = TRUE) + 
  labs(title = "Time: {frame}")
animate(p, end_pause = 5)
