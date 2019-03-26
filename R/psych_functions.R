# network_functions


extractResourceTypeFromData <- function(dataString) {
  # Data string in the form:
  # [1] "{\"handler\":\"resource/x-bb-document\",\"title\":\"Notes\",\"parent\":\"_2223539_1\",\"synthetic\":true}"
  s <- str_remove(dataString, "^.*resource/")
  s <- str_remove(s, '\\".*$')
  return(s)
}

tidyDataField <- function(dataString) {
  s <- str_remove(dataString, '^.*title\\":\\"')
  s <- str_remove(s, '\\".*$')
  return(s)
}

na.falsify <- function(x) {
  return(if_else(is.na(x), FALSE, x))
}

buildGraph <- function(graph) {
  g <- ggraph(graph, layout = "kk") +
    geom_node_point(aes(size = grade_quartile, color = Accesses)) +
    scale_color_gradient(trans = "log1p", 
                         breaks = c(2,10,50,250,1250)) +
    geom_edge_fan(aes(alpha = weight), show.legend = F) +
    scale_size_continuous(name = "Grade Quartile (0 for no grade)", trans = "exp") +
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
    scale_size_continuous(name = "Grade Quartile (0 for no grade)", trans = "exp") +
    geom_edge_fan(aes(alpha = weight), show.legend = F) +
    ggthemes::theme_tufte() +
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}

buildGraphInteractions <- function(graph) {
  g <- ggraph(graph, layout = "kk") +
    geom_node_point(aes(size = grade_quartile, color = Interactions)) +
    scale_size_continuous(name = "Grade Quartile (0 for no grade)", trans = "exp") +
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
    scale_size_continuous(name = "Grade Quartile (0 for no grade)", trans = "exp") +
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
    scale_size_continuous(name = "Grade Quartile (0 for no grade)", trans = "exp") +
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
    scale_size_continuous(name = "Grade Quartile (0 for no grade)", trans = "exp") +
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
    scale_size_continuous(name = "Grade Quartile (0 for no grade)", trans = "exp") +
    ggthemes::theme_tufte() + 
    scale_color_brewer(palette = "Dark2") +
    theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  return(g)
}


tlttest <- function(t = timeSum, measure, subject) {
  t2 <- t[t$subject == subject & t$measure == measure & t$session == "201860", ]$score
  t1 <- t[t$subject == subject & t$measure == measure & t$session == "201760", ]$score
  return(t.test(t2, t1, paired = TRUE, na.rm = TRUE))
}
