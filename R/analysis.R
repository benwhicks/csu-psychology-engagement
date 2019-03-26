# Investigate missing accumulator data from early (mid July)
# Names above pages for "access to forum" 
# Minutes per site access or similar for smaller grainularity
# Group item activity usage more - possibly by grade quartile instead of grade
# compare to past years data



# Loading packages
library(tidyverse)
library(GGally)
library(network)
library(tidygraph)
library(ggraph)
library(ggthemes)

# Setting up directory and functions
datdir <- "D:/Users/bhicks/Documents/Data/Psych/"
source('R/psych_functions.R')

# importing data
snData <- read_csv(paste0(datdir, "PSY Posts and Replies.csv"),
                   col_types = "cccffn")
roleData <- read_csv(paste0(datdir, "PSY User Roles.csv"),
                     col_types = "ccfc")
roleData <- roleData %>% 
  filter(str_detect(Subject, "203|208")) %>% unique() # honours students are doubled up

marksData <- read_csv(paste0(datdir, "PSY Marks.csv"))
marksData$Score <- as.numeric(marksData$Score) # some of the mark data was NULL (missing) and this forces it into NA's, and the remainder into numeric form
accessData <- read_csv(paste0(datdir, "PSY Pyramid Course Accesses.csv"),
                       col_types = "cfcnn")
interactionsData <- read_csv(paste0(datdir, "PSY Pyramid Course Interactions.csv"),
                             col_types = "cfcnn")
pyramidData <- merge(interactionsData, accessData, all = T)
pyramidData <- pyramidData %>% plyr::rename(replace = c("CourseID" = "Subject", "ID" = "SISID"))
pyramidData <- pyramidData %>% filter(str_detect(Subject, "203|208"))
pyramidData$SISID <- NULL # it really is useless

studentNodeData <- merge(pyramidData, roleData)
studentNodeData <- studentNodeData %>% plyr::rename(replace = c("Avg Learn Grade" = "Grade",
                                                  "Course item Interactions" = "Interactions",
                                                  "Course Accesses" = "Accesses"))
teacherNodeData <- roleData %>% filter(ID %in% setdiff(roleData$ID, studentNodeData$ID) & ID %in% snData$MsgFrom)
teacherNodeData$Grade <- NA
teacherNodeData$Interactions <- NA
teacherNodeData$Accesses <- 1
nodeData <- rbind(studentNodeData, teacherNodeData)

# Building network
edgesAll <- data.frame(from = snData$MsgFrom, 
                       to = snData$MsgTo, 
                       weight = snData$Strength, 
                       Subject = snData$Subject)
questions <- edgesAll %>% 
  filter(to == "NULL") %>% 
  group_by(from, Subject) %>% 
  summarise(Questions = sum(weight))
edges <- edgesAll %>% filter(to != "NULL")
#edges$from <- as.numeric(edges$from)
#edges$to <- as.numeric(edges$to)

nodes <- data.frame(ID = as.factor(nodeData$ID), 
                Name = nodeData$Name, 
                Role = nodeData$Role,
                Accesses = nodeData$Accesses,
                Interactions = nodeData$Interactions,
                Grade = nodeData$Grade,
                Subject = nodeData$Subject
)


missingNodeDataIDs <- setdiff(union(edges$from, edges$to), nodes$ID)

# Editing subject field
nodes <- nodes %>% 
  mutate(Subject = ifelse(str_detect(Subject, "208|458"), "Biopsychology", "Social Psychology")) %>%
  unique() 
edges <- edges %>% 
  mutate(Subject = ifelse(str_detect(Subject, "208|458"), "Biopsychology", "Social Psychology")) %>%
  unique()

# New version of PSY data - testing
socpsyAct18 <- read_csv(file.path(datdir, "aa stats Social Psychology.csv")) %>% filter(!is.na(id))
socpsyAct18$id <- factor(socpsyAct18$id)
socpsyAct18 <- socpsyAct18 %>% mutate(forum_bool = str_detect(tolower(data), "discus|thread|forum"),
                                      content = tidyDataField(data),
                                      name = paste0(lastname, ', ', firstname))
socpsyAct18 <- socpsyAct18 %>% mutate(forum_bool = if_else(is.na(forum_bool), FALSE, forum_bool))


biopsyAct18 <- read_csv(file.path(datdir, "aa stats Biopsychology.csv")) %>% filter(!is.na(id))
biopsyAct18$id <- factor(biopsyAct18$id)
biopsyAct18 <- biopsyAct18 %>% mutate(forum_bool = str_detect(tolower(data), "discus|thread|forum"),
                                      content = tidyDataField(data),
                                      name = paste0(lastname, ', ', firstname))
biopsyAct18 <- biopsyAct18 %>% mutate(forum_bool = if_else(is.na(forum_bool), FALSE, forum_bool))


discussionViewsBio <- biopsyAct18 %>% 
  select(name, id, subject, forum_bool, data) %>%
  filter(  forum_bool |
           str_detect(tolower(data), "discus|thread|forum")
         ) %>% 
  group_by(subject, name, id) %>%
  summarise(ForumViews = n()) %>%
  group_by(name, id) %>%
  summarise(ForumViews = sum(ForumViews)) %>%
  plyr::rename(replace = c("id" = "ID", "name" = "Name"))


discussionViewsSoc <- socpsyAct18 %>% 
  select(name, id, subject, forum_bool, data) %>%
  filter(  forum_bool |
             str_detect(tolower(data), "discus|thread|forum")
  ) %>% 
  group_by(subject, name, id) %>%
  summarise(ForumViews = n()) %>%
  group_by(name, id) %>%
  summarise(ForumViews = sum(ForumViews)) %>%
  plyr::rename(replace = c("id" = "ID", "name" = "Name"))

# Generating accumulator sum for comparison with Pyramid Data
# - -  -  basically fields that you would think are identical have a high correlation...
#AccSumBio <- biopsyAct18 %>%
#  group_by(id, name, subject, event) %>%
#  summarise(n = n()) %>%
#  spread(key = event, value = n) %>%
#  plyr::rename(replace = c("id" = "ID", "subject" = "Subject", "name" = "Name"))

# Looking at what was clicked prior to forum access

bioForumExamine <- biopsyAct18 %>% filter(role == "S") %>% select(id, timestamp, content, forum_bool) %>% arrange(id, timestamp)
r = length(bioForumExamine$id)
bioForumAccess <- data.frame(from = bioForumExamine[1:r - 1, "content"],
                             to = bioForumExamine[2:r, "content"],
                             idFrom = bioForumExamine[1:r - 1, "id"],
                             idTo = bioForumExamine[2:r, "id"],
                             toForum = bioForumExamine[2:r, "forum_bool"])
names(bioForumAccess) <- c("from", "to", "idFrom", "idTo", "toForum") #shouldn't need to do this
#bioForumAccess$from <- as.character(bioForumAccess$from)
#bioForumAccess$to <- as.character(bioForumAccess$to)
bioForumAccess <- bioForumAccess %>% filter(idFrom == idTo) %>% mutate(toForum = na.falsify(toForum))# was filtering for forum true as well.

bioForumAccessEdges <- bioForumAccess %>% 
  group_by(from, to, toForum) %>% 
  summarise(weight = n()) 

bioForumAccessNodes <- bioForumExamine %>% 
  select(content, forum_bool, timestamp) %>%
  group_by(content, forum_bool) %>%
  summarise(first_access = min(timestamp), hits = n()) %>%
  mutate(PageType = if_else(str_detect(content, "lab|Lab"), "Lab", 
                            if_else(forum_bool, "Forum", "Other"))) %>%
  mutate(PageType = if_else(is.na(PageType), "Other", PageType)) %>%
  arrange(first_access)

bioForumAccessGraph <- tbl_graph(nodes = bioForumAccessNodes,
                                 edges = bioForumAccessEdges,
                                 directed = TRUE)

bioNavPlot <- bioForumAccessGraph %>% activate(nodes) %>% filter(!node_is_isolated()) %>% 
  ggraph(layout = "linear", circular = F) + 
  geom_node_point(aes(color = PageType, size = hits)) +
  geom_edge_arc2(aes(edge_width = weight, color = toForum), alpha = 0.1, lineend = "round", show.legend = F) +
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = "none") +
  scale_edge_alpha(guide = "none") +
  geom_node_text(aes(label = content, alpha = hits), size = 3,repel = T) +
  scale_alpha(guide = "none") +
  theme_graph()  

bioNavPlot + ggtitle("Biopsychology site navigation")

bioFAplot <- bioForumAccessGraph %>% activate(nodes) %>% filter(!node_is_isolated()) %>% activate(edges) %>% filter(toForum) %>% 
  ggraph(layout = "linear", circular = F) + 
  geom_node_point(aes(color = PageType, size = hits)) +
  geom_edge_arc2(aes(color = toForum, edge_width = weight), alpha = 0.5, lineend = "round", show.legend = F) +
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = "none") +
  scale_edge_alpha(guide = "none") +
#  geom_node_text(aes(label = content), size = 3,repel = T) +
  scale_alpha(guide = "none") +
  theme_graph()  

bioFAplot + ggtitle("Biopsychology navigation to Forums")




socForumExamine <- socpsyAct18 %>% filter(role == "S") %>% select(id, timestamp, content, forum_bool) %>% arrange(id, timestamp)
r = length(socForumExamine$id)
socForumAccess <- data.frame(from = socForumExamine[1:r - 1, "content"],
                             to = socForumExamine[2:r, "content"],
                             idFrom = socForumExamine[1:r - 1, "id"],
                             idTo = socForumExamine[2:r, "id"],
                             toForum = socForumExamine[2:r, "forum_bool"])
names(socForumAccess) <- c("from", "to", "idFrom", "idTo", "toForum")
socForumAccess <- socForumAccess %>% filter(from != to, idFrom == idTo) # was filtering for forum true as well.

socForumAccessEdges <- socForumAccess %>% 
  group_by(from, to, toForum) %>% 
  summarise(weight = n())

socForumAccessNodes <- socForumExamine %>% 
  select(content, forum_bool, timestamp) %>%
  group_by(content, forum_bool) %>%
  summarise(first_access = mean(timestamp), hits = n()) %>%
  mutate(PageType = if_else(str_detect(content, "tutorial|Tutorial"), "Tutorial", 
                            if_else(forum_bool, "Forum", "Other"))) %>%
  mutate(PageType = if_else(is.na(PageType), "Other", PageType)) %>%
  arrange(first_access)

socForumAccessGraph <- tbl_graph(nodes = socForumAccessNodes,
                                 edges = socForumAccessEdges,
                                 directed = TRUE)

socNavPlot <- socForumAccessGraph %>% activate(nodes) %>% filter(!node_is_isolated()) %>% 
  ggraph(layout = "linear", circular = T) + 
  geom_node_point(aes(color = PageType, size = hits)) +
  geom_edge_arc2(aes(edge_width = weight, color = toForum), alpha = 0.5, lineend = "round", show.legend = F) +
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = "none") +
  scale_edge_alpha(guide = "none") +
  geom_node_text(aes(label = title, alpha = hits), size = 3,repel = T) +
  scale_alpha(guide = "none") +
  theme_graph()  

socNavPlot + ggtitle("Social Psychology site navigation")

socFAplot <- socForumAccessGraph %>% activate(nodes) %>% filter(!node_is_isolated()) %>% activate(edges) %>% filter(toForum) %>% 
  ggraph(layout = "linear", circular = T) + 
  geom_node_point(aes(color = PageType, size = hits)) +
  geom_edge_arc2(aes(color = toForum, edge_width = weight), alpha = 0.5, lineend = "round", show.legend = F) +
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = "none") +
  scale_edge_alpha(guide = "none") +
  geom_node_text(aes(label = title), size = 3,repel = T) +
  scale_alpha(guide = "none") +
  theme_graph()  

socFAplot + ggtitle("Social Psychology navigation to Forums")


# Graphing in tidygraph and ggraph

nodesBio <- nodes %>% filter(str_detect(Subject, "Bio"))
edgesBio <- edges %>% filter(str_detect(Subject, "Bio"))
nodesSoc <- nodes %>% filter(str_detect(Subject, "Social"))
edgesSoc <- edges %>% filter(str_detect(Subject, "Social"))

# Filtering strange IDs
#nodesBio <- nodesBio %>% filter(ID %in% discussionViewsBio$ID | !(Name %in% discussionViewsBio$Name))
#nodesSoc <- nodesSoc %>% filter(ID %in% discussionViewsSoc$ID | !(Name %in% discussionViewsSoc$Name))

# getting grade quartiles
nodesBio <- nodesBio %>% mutate(grade_quartile = ntile(Grade, 4)) 
nodesBio[is.na(nodesBio$grade_quartile),]$grade_quartile <- 0

nodesSoc <- nodesSoc %>% mutate(grade_quartile = ntile(Grade, 4))
nodesSoc[is.na(nodesSoc$grade_quartile),]$grade_quartile <- 0


# merging with discussion data
nodesBio <- merge(nodesBio, discussionViewsBio, all.x = T)
nodesBio[is.na(nodesBio$ForumViews),]$ForumViews <- 0
nodesSoc <- merge(nodesSoc, discussionViewsSoc, all.x = T)
nodesSoc[is.na(nodesSoc$ForumViews),]$ForumViews <- 0

# converting NAs in Accesses to 0
nodesBio <- nodesBio %>% mutate(Accesses = if_else(is.na(Accesses), 0, Accesses))
nodesSoc <- nodesSoc %>% mutate(Accesses = if_else(is.na(Accesses), 0, Accesses))

# Making graph structures
BioPsyNet <- tbl_graph(nodes = nodesBio, edges = edgesBio) #netFetch(nd = nodesBio, ed = edgesBio, questions = T, lecturer = T)
SocPsyNet <- tbl_graph(nodes = nodesSoc, edges = edgesSoc)


# Graphs for Biopsychology
gBio <- buildGraph(BioPsyNet)
gBio + ggtitle("All Posts for Biopsychology") #+ geom_node_text(aes(label = id), size = 2)

gBioTrim <- buildTrimGraph(BioPsyNet)
gBioTrim  + ggtitle("All Posts for Biopsychology (isolated points removed)")

gBioStudent <- buildTrimGraph(BioPsyNet %>% activate(nodes) %>% filter(Role =="S")) 
gBioStudent + ggtitle("Student only interactions for Biopsychology (isolated points removed)")

gBioImportance <- buildImportanceGraph(BioPsyNet %>% activate(nodes) %>% filter(Role =="S"))
gBioImportance + ggtitle("Student only interaction by Importance for Biopsychology (isolated points removed)")

gBioCommunity <- buildClusterGraph(BioPsyNet %>% activate(nodes) %>% filter(Role =="S"))
gBioCommunity + ggtitle("Student only Communities for Biopsychology (isolated points removed)")

# Graphs for Social Psychology
gSoc <- buildGraph(SocPsyNet)
gSoc + ggtitle("All Posts for Social Psychology") #+ geom_node_text(aes(label = id), size = 2)

gSocTrim <- buildTrimGraph(SocPsyNet)
gSocTrim  + ggtitle("All Posts for Social Psychology (isolated points removed)")

gSocStudent <- buildTrimGraph(SocPsyNet %>% activate(nodes) %>% filter(Role =="S")) 
gSocStudent + ggtitle("Student only interactions for Social Psychology (isolated points removed)")

gSocImportance <- buildImportanceGraph(SocPsyNet %>% activate(nodes) %>% filter(Role =="S"))
gSocImportance + ggtitle("Student only interaction by Importance for Social Psychology (isolated points removed)")

gSocCommunity <- buildClusterGraph(SocPsyNet %>% activate(nodes) %>% filter(Role =="S"))
gSocCommunity + ggtitle("Student only Communities for Social Psychology (isolated points removed)")


timelineDataRaw <- read_csv(paste0(datdir, "PSY Pyramid Weekly Activity.csv"))
timelineData <- timelineDataRaw %>% mutate(subject = substr(`subject code`, 3, 8),
                                           session = as.factor(substr(`subject code`, 10, 15)))
timeSum <- timelineData[timelineData$subject != "PSY458",] %>% 
  group_by(week, measure, subject, session) %>%
  summarise(score = sum(score, na.rm = T))


# linear modelling, with networking 

# fetching centrality data
nodeCentrality203 <- as.data.frame(psy203studentNet %>% 
                                     activate(nodes) %>% 
                                     mutate(importance = centrality_authority()))
nodeCentrality208 <- as.data.frame(psy208studentNet %>% 
                                     activate(nodes) %>% 
                                     mutate(importance = centrality_authority()))
nodeCentrality <- rbind(nodeCentrality203, nodeCentrality208) %>% select(id, subject, importance)
# merging with node data
df <- left_join(nodes, nodeCentrality)

# hack to fix multiple centralities - FIX THIS!!!
df <- df %>% 
  group_by_at(vars(-importance)) %>% 
  summarise(importance = mean(importance)) %>%
  filter(role == "student") %>%
  select(id, accesses, interactions, grade, subject, importance)

fit <- lm(grade ~ importance + interactions + accesses,data = df)











# last session...in development
snNodeData2017 <- read_csv(paste0(datdir, "PSY Kumu Information 201760.csv"),
                           col_names = c("Subject", "Name", "Role", "ID"))
snNodeData2017$ID <- NULL






bioUsage <- biopsyAct18 %>%
  select(id, timestamp, title, role, forum_bool) %>%
  filter(role == "S" ) %>%
  #& timestamp < as.Date('2018-11-01')) %>%
  mutate(Activity = if_else(str_detect(tolower(title), "game"), "Game",
                            if_else(str_detect(tolower(title), "in the lab"), "In the lab",
                                    if_else(forum_bool, "Forum", "Other")))) %>%
  mutate(Activity = if_else(is.na(Activity), "Other", Activity)) %>%
  plyr::rename(replace = c("id" = "ID")) %>%
  merge(nodesBio %>% select(ID, Accesses, Interactions, Grade, ForumViews), all.x = T)



bioGameActPlot <- bioUsage %>% 
  ggplot(aes(x = timestamp, y = Grade, color = ForumViews)) +
  scale_color_gradient2(name = "Forum Participation", 
                        low = "Black",
                        mid = "Dark Blue",
                        high = "Red",
                        midpoint = 5,
                        trans = "log1p",
                        breaks = c(2, 10, 50, 250, 1250, 6250)) +  
  geom_jitter(alpha = 0.002) +
  theme(axis.title.x = element_blank()) + 
  ggthemes::theme_tufte() + 
  facet_grid(Activity ~ .) +
  ggtitle("Usage of items in Biopsychology")

bioUsage %>% group_by(Activity) %>% tally() %>% knitr::kable()

bioGameActPlot








