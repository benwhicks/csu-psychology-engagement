# Analysis functions ------------------------------------------------------

tlttest <- function(t = timeSum, measure, subject) {
  t2 <- t[t$subject == subject & t$measure == measure & t$session == "201860", ]$score
  t1 <- t[t$subject == subject & t$measure == measure & t$session == "201760", ]$score
  return(t.test(t2, t1, paired = TRUE, na.rm = TRUE))
}

edges_from_aa <- function(x) {
  aa <- x %>% 
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
}

edges_from_aa_cluster <- function(x) {
  aa <- x %>% 
    filter(role == 'S', !is.na(content_pk1), timestamp < as.POSIXct('2018-10-30')) %>% 
    select(id, time = timestamp, content_pk1, session, cluster) %>%  
    #grade_quartile, cluster, qual) %>% 
    mutate(id = factor(id), content_pk1 = factor(content_pk1)) %>% 
    arrange(id, time)
  aa_edges <- tibble(from = aa$content_pk1[1:(nrow(aa) - 1)],
                     to =   aa$content_pk1[2:nrow(aa)],
                     time = aa$time[2:nrow(aa)],
                     student_from = aa$id[1:(nrow(aa) - 1)],
                     student = aa$id[2:nrow(aa)],
                     session_from = aa$session[1:(nrow(aa) - 1)],
                     session = aa$session[2:nrow(aa)],
                     #grade_quartile = aa$grade_quartile[1:(nrow(aa) - 1)])
                     cluster = aa$cluster[1:(nrow(aa) - 1)])
  #qual = aa$qual[1:(nrow(aa) - 1)]
  aa_edges <- aa_edges %>% 
    filter(student == student_from, session == session_from) %>% 
    select(from, to, time, student) %>% # , grade_quartile, cluster, qual 
    mutate(weight = 2)
  return(aa_edges)
}

edges_from_cc <- function(x) {
  cc_edges <- x %>% 
    select(from = parent_pk1, to = pk1) %>% 
    filter(!is.na(from)) %>% 
    mutate(weight = 1, time = as.POSIXct(ZERODATE)) # used manual set time - needs to be well before other data
  return(cc_edges)
}

nodes_from_cc <- function(x, subject) {
  x <- x %>% select(id = pk1, title, data) %>% 
    mutate(forum_bool = str_detect(tolower(data), "discus|thread|forum")) %>% 
    mutate(forum_bool = if_else(is.na(forum_bool), FALSE, forum_bool))
  if (str_detect(subject, "Bio|bio")) {
    x <- x %>% 
      mutate(PageType = if_else(str_detect(title, "lab|Lab"), "Lab", 
                                if_else(forum_bool, "Forum", "Other"))) %>%
      mutate(PageType = if_else(is.na(PageType), "Other", PageType)) %>% 
      mutate(PageType = factor(PageType, levels = c("Forum", "Lab", "Other")))
  }
  if (str_detect(subject, "Soc|soc")) {
    x <- x %>% 
      mutate(PageType = if_else(str_detect(title, "Week 2: The Social Self|Week 3:Perc|Week 4:Stereo|Week 5:Att|Week 6:Conf|Week 10:Att|Week 11:Help|Applied Social"), 
                                "Tutorial",
                                if_else(forum_bool, "Forum", "Other"))) %>%
      mutate(PageType = if_else(is.na(PageType), "Other", PageType)) %>% 
      mutate(PageType = factor(PageType, levels = c("Forum", "Tutorial", "Other")))
  }
  return(x %>% select(-data))
}

nodes_from_aa <- function(x) {
  x %>% 
    filter(!is.na(content_pk1)) %>% 
    left_join(nodes %>% select(id = ID, cluster, qual)) %>% 
    select(-id) %>% 
    select(id = content_pk1, cluster, qual) %>% # might need to calc grade quartile
    unique()
}

build_site_activity_graph_list <- function(cc_nodes, cc_edges, aa_nodes, aa_edges) {
  cc_graph <- tbl_graph(nodes = cc_nodes, edges = cc_edges)
  cc_layout <- create_layout(cc_graph, layout = "nicely")
  np <- data.frame(x = cc_layout$x, y = cc_layout$y)
  aa_graph <- tbl_graph(nodes = cc_nodes, edges = aa_edges)
  joined_graph <- cc_graph %>% graph_join(aa_graph) 
  joined_graph <- joined_graph %>% 
    activate(edges) %>% 
    mutate(student = if_else(is.na(student), "_course structure", as.character(student)))
  return(list(joined_graph, np))
}


# Visualisation functions -------------------------------------------------
draw_site_graph <- function(graph_list){
  graph <- graph_list[[1]] %>% 
    activate(edges) %>% 
    filter(weight == 1)
  np <- graph_list[[2]]
  g <- ggraph(graph, layout = "manual", node.positions = np) +
    geom_node_point(aes(color = PageType)) + 
    geom_node_text(aes(label = title), alpha = 0.4, repel = T, size = 1) +
    geom_edge_link0() +
    theme_graph() +
    theme(#legend.position = "none",
      text = element_text(family = "TT Times New Roman"))
  return(g)
}

draw_site_activity_graph <- function(graph_list, alpha_low = 0.1) {
  g <- ggraph(graph_list[[1]], layout = "manual", node.positions = graph_list[[2]]) +
    geom_node_point() + 
    geom_edge_link0(aes(color = as.numeric(time), 
                        width = weight, 
                        alpha = 2 - weight,
                        group = time)) +
    scale_edge_alpha_continuous(range = c(alpha_low, 1)) +
    scale_edge_width(range = c(0.6,1)) +
    scale_edge_color_gradientn(colors = c("black", "#47ADED", "#D81C41")) +
    theme_graph() +
    theme(legend.position = "none",
          text = element_text(family = "TT Times New Roman"))
  return(g)
}



buildGraph <- function(graph) {
  g <- ggraph(graph, layout = "kk") +
    geom_node_point(aes(size = grade_quartile, color = Accesses)) +
    scale_color_gradient(trans = "log1p", 
                         breaks = c(2,10,50,250,1250)) +
    geom_edge_fan(aes(alpha = weight), show.legend = F) +
    scale_size_continuous(name = "Grade Quartile", trans = "exp") +
    ggthemes::theme_tufte() +
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}

buildLurkerGraph <- function(graph) {
  g <- ggraph(graph, layout = "kk") +
    geom_node_point(aes(size = grade_quartile, color = ForumViews)) +
    scale_color_gradient2(name = "Forum Participation", 
                         low = "Black",
                          mid = "Dark Blue",
                         high = "Red",
                         midpoint = 5,
                         trans = "log1p",
                         breaks = c(2, 10, 50, 250, 1250)) +
    scale_size_continuous(name = "Grade Quartile", trans = "exp") +
    geom_edge_fan(aes(alpha = weight), show.legend = F) +
    ggthemes::theme_tufte() +
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}

buildGraphInteractions <- function(graph) {
  g <- ggraph(graph, layout = "kk") +
    geom_node_point(aes(size = grade_quartile, color = Interactions)) +
    scale_size_continuous(name = "Grade Quartile", trans = "exp") +
    geom_edge_fan(aes(alpha = weight), show.legend = F) +
    ggthemes::theme_tufte() +
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}

buildTrimGraph <- function(graph) {
  g <- ggraph(graph %>% 
                activate(nodes) %>%
                filter(!node_is_isolated()), 
              layout = "kk") +
    geom_node_point(aes(size = grade_quartile, color = Accesses)) +
    scale_size_continuous(name = "Grade Quartile", trans = "exp") +
    scale_color_gradient(trans = "log1p", 
                         breaks = c(2,10,50,250,1250)) +
    geom_edge_fan(aes(alpha = weight), show.legend = F) +
    ggthemes::theme_tufte() +
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}

buildTrimGraphInteractions <- function(graph) {
  g <- ggraph(graph %>% 
                activate(nodes) %>%
                filter(!node_is_isolated()), 
              layout = "kk") +
    geom_node_point(aes(size = grade_quartile, color = interactions)) +
    scale_size_continuous(name = "Grade Quartile", trans = "exp") +
    geom_edge_fan(aes(alpha = weight), show.legend = F) +
    ggthemes::theme_tufte() +
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}

buildImportanceGraph <- function(graph) {
  g <- ggraph(graph %>% 
                activate(nodes) %>% 
                filter(!node_is_isolated()) %>%
                mutate(importance = centrality_authority()), 
              layout = "kk") +
    geom_edge_fan(aes(alpha = weight), show.legend = F) +
    geom_node_point(aes(size = grade_quartile, color = importance)) +
    scale_size_continuous(name = "Grade Quartile", trans = "exp") +
    ggthemes::theme_tufte() + 
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}

buildClusterGraph <- function(graph) {
  g <- ggraph(to_undirected(graph) %>%
                filter(!node_is_isolated()) %>%
                activate(nodes) %>% 
                mutate(community = as.factor(group_infomap())), 
              layout = "kk") +
    geom_edge_link(aes(alpha = weight), show.legend = F) +
    geom_node_point(aes(size = grade_quartile, color = community), show.legend = F) +
    scale_size_continuous(name = "Grade Quartile", trans = "exp") +
    ggthemes::theme_tufte() + 
    scale_color_brewer(palette = "Dark2") +
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}

buildClusterKMetGraph <- function(graph) {
  g <- ggraph(to_undirected(graph), 
              layout = "kk") +
    geom_edge_link(aes(alpha = weight), show.legend = F) +
    geom_node_point(aes(size = grade_quartile, color = factor(cluster)), show.legend = T) +
    scale_size_continuous(name = "Grade Quartile", trans = "exp") +
    ggthemes::theme_tufte() + 
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}
