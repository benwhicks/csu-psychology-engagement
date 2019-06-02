suppressPackageStartupMessages(library(tidyverse))

# Parameters --------------------------------------------------------------

DATDIR <- file.path("~", "Data", "Psychology")
BIOAAFILE <- "aa Biopsychology 201860.csv"
SOCAAFILE <- "aa Social Psychology 201860.csv"
MARKSFILE <- "marks Psychology 201860.csv"
BIOCCFILE <- "cc Biopsychology 201860.csv"
SOCCCFILE <- "cc Social Psychology 201860.csv"
POSTSFILE <- "posts Psychology 201860.csv"
QUALFILE <- "qual participants.csv"
ZERODATE <- "2018-05-01" # pre-session date used to colour the site activity structure paths. Affects animation time before student activity

# Wranglers ---------------------------------------------------------------

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

snData <- read_csv(file.path(DATDIR, POSTSFILE), col_types = "cccffn")

# Activity Accumulator Data --------------------------------------
tidy_aa <- function(df) {
  df <- df %>%
    filter(!is.na(id), role == "S") %>%
    mutate(forum_bool = str_detect(tolower(data), "discus|thread|forum"),
           content = tidyDataField(data),
           name = paste0(lastname, ', ', firstname)) %>%
    rename(session = session_id, id = s_id, login_id = id)
  df <- df %>% mutate(forum_bool = if_else(is.na(forum_bool), FALSE, forum_bool))
  return(df)
} # --------------------------------------------------------------
aaSoc18 <- read_csv(file.path(DATDIR, SOCAAFILE), col_types = "ffTcccccffcccf") # can cause performance hit, if so do not specity columns and add as.factor lines to tidy_aa 
aaSoc18 <- tidy_aa(aaSoc18)

aaBio18 <- read_csv(file.path(DATDIR, BIOAAFILE), col_types = "ffTcccccffcccf")
aaBio18 <- tidy_aa(aaBio18)

# Course contents
tidy_cc <- function(x) {
  x %>% 
    select(pk1, parent_pk1, title, data = main_data) %>% 
    mutate(pk1 = factor(pk1), parent_pk1 = factor(parent_pk1)) %>% 
    mutate(title = str_trunc(title, width = 45))
}
ccSoc18 <- read_csv(file.path(DATDIR, SOCCCFILE)) %>% tidy_cc()
ccBio18 <- read_csv(file.path(DATDIR, BIOCCFILE)) %>% tidy_cc()


# Node preparation (i.e. data for each student)

# Mark data
marks18 <- read_csv(file.path(DATDIR, MARKSFILE)) %>%
  mutate(mark = score / possible, id = factor(id)) 
marksSummary18 <- marks18 %>% 
  group_by(subject_name, student, id) %>%
  summarise(avg_grade = mean(mark, na.rm = T)) %>%
  rename(name = student, subject = subject_name)
marksSoc18 <- marksSummary18 %>% filter(subject == "Social Psychology")
marksBio18 <- marksSummary18 %>% filter(subject == "Biopsychology")

# Discussion views from aa -------------------------------------------
discussion_views <- function(df) {
  df %>%
    select(name, subject, forum_bool, data) %>%
    filter(  forum_bool |
               str_detect(tolower(data), "discus|thread|forum")
    ) %>% 
    group_by(subject, name) %>%
    summarise(ForumViews = n()) %>%
    group_by(name) %>%
    summarise(ForumViews = sum(ForumViews))
} # ------------------------------------------------------------------
discussionViewsBio <- discussion_views(aaBio18)
discussionViewsSoc <- discussion_views(aaSoc18)

# Other interaction data from aa
aaSummaryBio <- aa_summary_by_user(aaBio18)
aaSummaryBio <- merge(aaSummaryBio, unique(aaBio18 %>% select(id, name)))
aaSummarySoc <- aa_summary_by_user(aaSoc18)
aaSummarySoc <- merge(aaSummarySoc, unique(aaSoc18 %>% select(id, name)))


# Node assembly ----------------------------------------------------------
node_merge_and_make <- function(aaData, marksData, discussionsData) {
  df <- aaData %>% select(-id) %>%
    merge(marksData) %>%
    merge(discussionsData, all.x = T) %>%
    select(ID = id, Name = name, Subject = subject, 
           Grade = avg_grade, Accesses = accesses, 
           mean_clicks_per_access, sd_clicks, 
           median_time_per_access, sd_time, 
           TotalTime = total_time,
           ForumViews) %>%
    mutate(grade_quartile = ntile(Grade, 4), ID = factor(ID))
  if (any(is.na(df$ForumViews))) {
    df[is.na(df$ForumViews),]$ForumViews <- 0
  }
  if (any(is.na(df$Accesses))) {
    df[is.na(df$Accesses),]$Accesses <- 0  
  }
  if (any(is.na(df$TotalTime))) {
    df[is.na(df$TotalTime),]$TotalTime <- 0
  }
  df$Role <- "S"
  return(df)
} # ----------------------------------------------------------------------
nodesBio <- node_merge_and_make(aaSummaryBio, marksBio18, discussionViewsBio)
nodesSoc <- node_merge_and_make(aaSummarySoc, marksSoc18, discussionViewsSoc)

# -----------------------------------------------------------------------------
subject_code_to_name <- function(x) {
  ifelse(str_detect(x, "208|458"), "Biopsychology", "Social Psychology")
} # ---------------------------------------------------------------------------

## ADD ##
allStudentIDs <- unique(marks18$id)
allForumIDs <- unique(snData$msgfrom)
teacherIDs <- setdiff(allForumIDs, allStudentIDs)
teacherNodes <- snData %>% 
  filter(msgfrom %in% teacherIDs) %>% 
  select(Name = poster, ID = msgfrom, Subject = subject) %>% 
  mutate(ID = as.factor(ID), 
         Grade = 0, Accesses = 100, ForumViews = 200, Role = "T", Subject = subject_code_to_name(Subject), 
         TotalTime = as.duration(200000), median_time_per_access = as.duration(2000), sd_time = as.duration(200),
         mean_clicks_per_access = 0, sd_clicks = 0, grade_quartile = 0) %>%
  unique()
teacherNodesBio <- teacherNodes %>% filter(Subject == "Biopsychology")
teacherNodesSoc <- teacherNodes %>% filter(Subject == "Social Psychology")

nodesBio <- rbind(nodesBio, teacherNodesBio)
nodesSoc <- rbind(nodesSoc, teacherNodesSoc)

# Edge assembly
edgesAll <- data.frame(from = snData$msgfrom, 
                       to = snData$msgto, 
                       weight = snData$strength, 
                       Subject = snData$subject) %>%
  mutate(Subject = subject_code_to_name(Subject))
questions <- edgesAll %>% 
  filter(to == "NULL") %>% 
  group_by(from, Subject) %>% 
  summarise(Questions = sum(weight))
edges <- edgesAll %>% filter(to != "NULL")
edgesBio <- edges %>% filter(str_detect(Subject, "Bio"))
edgesSoc <- edges %>% filter(str_detect(Subject, "Social"))


# Cluster analysis
make_normed_matrix <- function(df, return_ids = FALSE) {
  ids <- df$ID
  df$median_time_per_access <- as.numeric(as_duration_from_string(df$median_time_per_access))
  df$sd_time <- as.numeric(as_duration_from_string(df$sd_time))
  df <- as.data.frame(sapply(df %>% 
                               filter(Role == "S") %>% 
                               select(-ID, -Name, -Subject, -Role, -grade_quartile), 
                             as.numeric))
  #gq <- df$grade_quartile # does not need to be normed
  df <- scale(df)
  if (return_ids) {
    df$ID <- ids
  }
  #df$grade_quartile <- gq
  return(df)
}

nodeMatrixBio <- make_normed_matrix(nodesBio)
nodeMatrixSoc <- make_normed_matrix(nodesSoc)
nodes <- rbind(nodesBio, nodesSoc) %>% filter(Role == "S")
nodeMatrix <- make_normed_matrix(nodes)
dAct <- nodeMatrix[, 2:8]

kmed <- pam(dAct, 5)

nodes <- bind_cols(nodes, data.frame(cluster = kmed$cluster))

# Adding Qual participants
qual <- read_csv(file.path(DATDIR, QUALFILE))
qual <- qual %>% 
  rename(qual = Method) %>% 
  select(-Subject)
nodes <- nodes %>% left_join(qual, by = c("Name"))
nodes$qual_participant <- !is.na(nodes$qual)
nodes[is.na(nodes$qual), ]$qual <- "None"

# Joining cluster and qual back with subject ndoes
nodesBio <- left_join(nodesBio, nodes %>% select(ID, Name, cluster, qual))
nodesSoc <- left_join(nodesSoc, nodes %>% select(ID, Name, cluster, qual))

# Joining grade_quartile, cluster and qual back with aa 
nodeInfo <- nodes %>% 
  select(name = Name, grade_quartile, cluster, qual, qual_participant)
aaBio18 <- left_join(aaBio18, nodeInfo)
aaSoc18 <- left_join(aaSoc18, nodeInfo)

# Making graph structures
networkBio <- tbl_graph(nodes = nodesBio, edges = edgesBio) 
networkSoc <- tbl_graph(nodes = nodesSoc, edges = edgesSoc)

