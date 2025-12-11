##############################################
#  PACKAGES
##############################################

install.packages(c("tidyverse", "lubridate", "janitor", "skimr", 
                   "corrplot", "GGally", "reticulate"))

library(reticulate)   # pour utiliser du Python dans R
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(corrplot)
library(GGally)

##############################################
#  DOWNLOAD DATA FROM KAGGLE WITH kagglehub
##############################################

library(reticulate)
library(readr)

kagglehub <- import("kagglehub")

# Download dataset
path <- kagglehub$dataset_download("roccoli/gpx-hike-tracks")
print(path)

# Correct CSV path
csv_path <- file.path(path, "gpx-tracks-from-hikr.org.csv")

# Load the data
raw_hikes <- read_csv(csv_path)

head(raw_hikes)

##############################################
#  LOAD & CLEAN RAW DATA
##############################################

raw_hikes <- read_csv(csv_path)

hikes <- raw_hikes %>%
  clean_names() %>%   # normalize column names
  distinct()          # remove duplicate rows

##############################################
#  BASIC QUALITY CHECKS
##############################################

skim(hikes)

miss_summary <- hikes %>%
  summarise(across(everything(), ~ sum(is.na(.))))
print(miss_summary)

##############################################
#  FIX DATA TYPES
##############################################

hikes <- hikes %>%
  mutate(
    start_time = ymd_hms(start_time, tz = "UTC"),
    end_time   = ymd_hms(end_time, tz = "UTC"),
    length_2d      = as.numeric(length_2d),
    length_3d      = as.numeric(length_3d),
    uphill         = as.numeric(uphill),
    downhill       = as.numeric(downhill),
    max_elevation  = as.numeric(max_elevation),
    min_elevation  = as.numeric(min_elevation),
    moving_time    = as.numeric(moving_time),
    max_speed      = as.numeric(max_speed)
  )

##############################################
#  OUTLIER & IMPOSSIBLE VALUE FILTERING
##############################################

hikes <- hikes %>%
  filter(
    length_2d > 100, length_2d < 100000,
    length_3d > 100, length_3d < 100000,
    min_elevation > -100,
    max_elevation < 5000,
    uphill < 5000,
    downhill < 5000,
    max_speed < 10
  )

##############################################
#  INSPECT DIFFICULTY & NORMALIZE T-SCALE
##############################################

hikes %>% count(difficulty, sort = TRUE)

hikes <- hikes %>%
  mutate(
    difficulty_raw = difficulty,
    difficulty = str_extract(difficulty, "T[1-6][+-]?")
  ) %>%
  mutate(
    difficulty_num = case_when(
      difficulty == "T1"  ~ 1.0,
      difficulty == "T2"  ~ 2.0,
      difficulty == "T3-" ~ 2.75,
      difficulty == "T3"  ~ 3.0,
      difficulty == "T3+" ~ 3.25,
      difficulty == "T4-" ~ 3.75,
      difficulty == "T4"  ~ 4.0,
      difficulty == "T4+" ~ 4.25,
      difficulty == "T5-" ~ 4.75,
      difficulty == "T5"  ~ 5.0,
      difficulty == "T5+" ~ 5.25,
      difficulty == "T6-" ~ 5.75,
      difficulty == "T6"  ~ 6.0,
      difficulty == "T6+" ~ 6.25,
      TRUE ~ NA_real_
    )
  )

##############################################
#  FIX DATES & CREATE TIME FEATURES
##############################################

hikes <- hikes %>%
  mutate(
    start_time = if_else(year(start_time) == 1970, NA_POSIXct_, start_time),
    end_time   = if_else(year(end_time) == 1970,   NA_POSIXct_, end_time)
  ) %>%
  filter(!(is.na(start_time) & is.na(end_time))) %>%
  mutate(
    year     = year(start_time),
    month    = month(start_time),
    weekday  = wday(start_time, label = TRUE),
    season   = case_when(
      month %in% c(12,1,2) ~ "Winter",
      month %in% c(3,4,5)  ~ "Spring",
      month %in% c(6,7,8)  ~ "Summer",
      month %in% c(9,10,11) ~ "Autumn",
      TRUE ~ NA_character_
    ),
    duration_hours = as.numeric(difftime(end_time, start_time, units = "hours"))
  )

##############################################
#  FEATURE ENGINEERING
##############################################

hikes_features <- hikes %>%
  filter(length_2d > 0, uphill >= 0) %>%
  mutate(
    dist_km        = length_2d / 1000,
    dist3d_km      = length_3d / 1000,
    uphill_per_km  = uphill / dist_km,
    duration_h_raw = duration_hours,
    duration_h_alt = moving_time / 3600,
    duration_h = if_else(!is.na(duration_h_raw), duration_h_raw, duration_h_alt),
    avg_speed_kmh = if_else(duration_h > 0, dist_km / duration_h, NA_real_),
    duration_h     = if_else(duration_h < 0 | duration_h > 24, NA_real_, duration_h),
    avg_speed_kmh  = if_else(avg_speed_kmh > 12, NA_real_, avg_speed_kmh),
    uphill_per_km  = if_else(uphill_per_km > 500, NA_real_, uphill_per_km),
    slope_index    = uphill_per_km
  )

# ... puis tout le reste de ton script (plots, similarity, etc.)

##############################################
#  EDA PLOTS (clean)
##############################################

# distributions
hikes_features %>% ggplot(aes(dist_km)) + geom_histogram(bins = 50)
hikes_features %>% ggplot(aes(uphill)) + geom_histogram(bins = 50)
hikes_features %>% ggplot(aes(uphill_per_km)) + geom_histogram(bins = 50)
hikes_features %>% ggplot(aes(duration_h)) + geom_histogram(bins = 50)
hikes_features %>% ggplot(aes(avg_speed_kmh)) + geom_histogram(bins = 50)

# relationships
ggplot(hikes_features, aes(dist_km, uphill))        + geom_point(alpha = 0.2)
ggplot(hikes_features, aes(dist_km, duration_h))   + geom_point(alpha = 0.2)
ggplot(hikes_features, aes(dist_km, avg_speed_kmh))+ geom_point(alpha = 0.2)
ggplot(hikes_features, aes(as.factor(difficulty_num), uphill_per_km)) + geom_boxplot()

# correlation matrix
cor_matrix <- hikes_features %>%
  select(dist_km, uphill, uphill_per_km, duration_h, avg_speed_kmh, difficulty_num) %>%
  cor(use = "complete.obs")

corrplot(cor_matrix, method = "color")

# pair plot
GGally::ggpairs(
  hikes_features %>%
    select(dist_km, uphill, uphill_per_km, duration_h, avg_speed_kmh, difficulty_num)
)

##############################################
#  IDENTIFY UNIQUE HIKES & SHARED HIKES
##############################################

hikes_by_name <- hikes %>%
  group_by(name) %>%
  summarise(
    n_records   = n(),
    n_users     = n_distinct(user),
    avg_dist_km = mean(length_2d) / 1000,
    avg_uphill  = mean(uphill),
    avg_diff    = mean(difficulty_num),
    .groups = "drop"
  ) %>%
  arrange(desc(n_users), desc(n_records))

shared_hikes <- hikes_by_name %>% filter(n_users > 1)

##############################################
#  HIKE CATALOG (FOR RECOMMENDER)
##############################################

hike_items <- hikes_by_name %>%
  select(
    name, n_records, n_users,
    avg_dist_km, avg_uphill, avg_diff
  )

##############################################
#  SIMILARITY ENGINE (KNN)
##############################################

hike_feature_matrix <- hike_items %>%
  select(avg_dist_km, avg_uphill, avg_diff) %>%
  scale()

dist_matrix <- as.matrix(dist(hike_feature_matrix))

recommend_similar <- function(hike_name, k = 5) {
  idx <- which(hike_items$name == hike_name)
  if (length(idx) == 0) stop("Hike not found.")
  dists <- dist_matrix[idx, ]
  nearest_idx <- order(dists)[2:(k + 1)]
  tibble(
    reference_hike = hike_name,
    recommended_hike = hike_items$name[nearest_idx],
    distance = dists[nearest_idx]
  )
}

recommend_similar("Piz Julier", 5)

##############################################
#  POPULARITY METRICS
##############################################

top_popular <- hike_items %>% arrange(desc(n_users))  %>% slice(1:20)
top_longest <- hike_items %>% arrange(desc(avg_dist_km)) %>% slice(1:20)
top_hardest <- hike_items %>% arrange(desc(avg_diff))   %>% slice(1:20)
