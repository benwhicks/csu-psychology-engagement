# Various plots

library(rdata.psych)
library(ggraph)
library(tidygraph)
library(tidyverse)

forum_plot <- function(g, iso = T, teacher = T) {
  if (iso) {
    data <- g
  } else {
    data <- g %>% 
      activate(nodes) %>% 
      filter(!node_is_isolated())
  }
  if (!teacher) {
    data <- data %>% 
      activate(nodes) %>% 
      filter(str_detect(Person, "Student"))
  }
  plot <- data %>% 
    ggraph(layout = "nicely") +
    geom_node_point(aes(color = Role, size = Role)) +
    geom_edge_link(aes(color = weight), alpha = 0.2) +
    theme_graph() + 
    theme(legend.position = "none",
          text = element_text(family = "TT Times New Roman"))
  return(plot)
}

forum_plot(forumGraphBio17, iso = F, teacher = T)
forum_plot(forumGraphBio17, iso = F, teacher = F)
forum_plot(forumGraphBio18, iso = F, teacher = T)

# Graph analysis
analyse_graph <- function(g) {
  n <- length(V(g)) # number of nodes
  d <- edge_density(g)
  t <- transitivity(g)
  apl <- average.path.length(g)
  # triad census and comparison see - https://www.markanthonyhoffman.com/social_network_analysis/measuring-transitivity-and-structural-balance.html
  # Academic paper - https://arxiv.org/abs/1502.07016 
  Mutual_Asymmetric_Null_classes <-   c("003",  "012",  "102",  "021D",
                     "021U", "021C", "111D", "111U",
                     "030T", "030C", "201",  "120D",
                     "120U", "120C", "210",  "300")
  triads <- triad_census(g)
  trials <- 500
  trial_triads <- vector("list", length = trials)
  trial_transitivity <- c()
  for (i in 1:trials) {
    rg <- erdos.renyi.game(n, d, directed = T)
    trial_triads[[i]] <- triad_census(rg)
    trial_transitivity[[i]] <- transitivity(rg)
  }
  expected_triads <- Reduce("+", trial_triads) / trials
  expected_transitivity <- mean(trial_transitivity)
  triad_df <- tibble(MAN_class = Mutual_Asymmetric_Null_classes,
                     Triads = triads,
                     expected = expected_triads)
  return(list(nodes = n, desnsity = d, transitivity = t, expected_transitivity = expected_transitivity,
              triad_df = triad_df))
}

# Splitting forums
unique(fmSoc17$forum)
