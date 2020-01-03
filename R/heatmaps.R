# Making activity heatmaps based on time of day

library(tidyverse)
library(rdata.psych)
library(lubridate)

prep_heatmap <- function(x) {
  x %>% 
    filter(role == 'S') %>% 
    select(timestamp, qual, subject, forum_bool) %>% 
    mutate(day = lubridate::wday(timestamp, label = TRUE),
           hour = hour(timestamp)) %>% 
    group_by(day, hour, subject) %>% 
    tally()
}

draw_heatmap <- function(x, op = "B", leg = FALSE) {
  p <- x %>% 
    mutate(`Views (square root of)` = sqrt(n)) %>% 
    ggplot(aes(x = hour, 
               y = reorder(day, desc(day)))) +
    geom_tile(aes(fill = `Views (square root of)`)) +
    viridis::scale_fill_viridis(option = op) +
    facet_wrap(~ subject) +
    theme_minimal() +
    ylab("") + xlab("Hour of day") 
  if (leg == FALSE) {
    p <- p +
      theme(legend.position = "none") 
  }
  return(p)
    
}

heatmap_both <- bind_rows(aaBio18, aaSoc18) %>% 
  prep_heatmap() %>% 
  draw_heatmap(leg = T)

heatmap_bio <- aaBio18 %>% 
  prep_heatmap() %>% 
  draw_heatmap(leg = T)

heatmap_soc <- aaSoc18 %>% 
  prep_heatmap() %>% 
  draw_heatmap(leg = T)

# saving
ggsave(file.path('activity-heatmap_both-with-legend_01.png'), heatmap_both)
ggsave(file.path('activity-heatmap_Bio-with-legend_01.png'), heatmap_bio)
ggsave(file.path('activity-heatmap_Soc-with-legend_01.png'), heatmap_soc)
