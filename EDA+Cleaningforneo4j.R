############################################################
#  PACKAGES
############################################################

# À faire une fois seulement :
# install.packages(c("tidyverse", "lubridate", "janitor", "skimr",
#                    "corrplot", "GGally", "reticulate", "geosphere"))

library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(corrplot)
library(GGally)
library(reticulate)
library(geosphere)
library(stringr)
library(purrr)

############################################################
#  DOWNLOAD DATA FROM KAGGLE
############################################################

kagglehub <- import("kagglehub")

path <- kagglehub$dataset_download("roccoli/gpx-hike-tracks")
print(path)

csv_path <- file.path(path, "gpx-tracks-from-hikr.org.csv")

raw_hikes <- read_csv(csv_path)

############################################################
#  LOAD PERSONAL DATASET (already in your environment)
strava_hike <- read_csv("strava_hike.csv")
strava_hike <- strava_hike %>%
  janitor::clean_names()
############################################################

# Expecting columns in strava_hike:
# user, name (or hike_name), activity_type, avg_dist_km, max_elevation,
# avg_uphill, moving_time (seconds), max_speed

# If your column is hike_name instead of name, uncomment:
# strava_hike <- strava_hike %>% rename(name = hike_name)

############################################################
#  MERGE BASE + PERSONAL WITH SOURCE FLAG
############################################################

raw_hikes <- raw_hikes %>%
  mutate(source = "base")

# Build a "Kaggle-compatible" version of the personal dataset
personal_hikes <- strava_hike %>%
  mutate(source = "personal") %>%
  transmute(
    source,
    user = as.character(user),
    name = as.character(name),
    difficulty = NA_character_,
    bounds = NA_character_,
    start_time = as.POSIXct(NA, tz = "UTC"),
    end_time   = as.POSIXct(NA, tz = "UTC"),

    # expected raw fields (meters, seconds)
    length_2d = as.numeric(avg_dist_km) * 1000,
    length_3d = as.numeric(avg_dist_km) * 1000,
    uphill    = as.numeric(avg_uphill),
    downhill  = NA_real_,

    max_elevation = as.numeric(max_elevation),
    min_elevation = NA_real_,

    moving_time = as.numeric(moving_time),   # seconds
    max_speed   = as.numeric(max_speed),     # keep as provided
    activity_type = as.character(activity_type)
  )

# Add missing activity_type column on base data (so bind_rows works)
base_hikes <- raw_hikes %>%
  mutate(activity_type = NA_character_)

# Merge
hikes <- bind_rows(base_hikes, personal_hikes) %>%
  clean_names() %>%
  distinct()

############################################################
#  BASIC QUALITY CHECKS (optional)
############################################################

skim(hikes)

miss_summary <- hikes %>%
  summarise(across(everything(), ~ sum(is.na(.))))
print(miss_summary)

############################################################
#  FIX DATA TYPES
############################################################

hikes <- hikes %>%
  mutate(
    start_time    = ymd_hms(start_time, tz = "UTC", quiet = TRUE),
    end_time      = ymd_hms(end_time,   tz = "UTC", quiet = TRUE),
    length_2d     = as.numeric(length_2d),
    length_3d     = as.numeric(length_3d),
    uphill        = as.numeric(uphill),
    downhill      = as.numeric(downhill),
    max_elevation = as.numeric(max_elevation),
    min_elevation = as.numeric(min_elevation),
    moving_time   = as.numeric(moving_time),  # seconds
    max_speed     = as.numeric(max_speed)
  )

############################################################
#  FILTER OUTLIERS & IMPOSSIBLE VALUES
#  (applied to both sources)
############################################################

hikes <- hikes %>%
  filter(
    is.na(length_2d) | (length_2d > 100 & length_2d < 100000),
    is.na(length_3d) | (length_3d > 100 & length_3d < 100000),
    is.na(min_elevation) | min_elevation > -100,
    is.na(max_elevation) | max_elevation < 5000,
    is.na(uphill) | uphill < 5000,
    is.na(downhill) | downhill < 5000,
    is.na(max_speed) | max_speed < 60
  )

############################################################
#  DIFFICULTY NORMALIZATION (T1-T6 → numeric) [base only]
############################################################

hikes <- hikes %>%
  mutate(
    difficulty_raw = difficulty,
    difficulty = if_else(
      source == "base",
      str_extract(difficulty, "T[1-6][+-]?"),
      NA_character_
    )
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

############################################################
#  FIX DATES & TIME FEATURES (base only; personal has NA times)
############################################################

hikes <- hikes %>%
  mutate(
    start_time = if_else(!is.na(start_time) & year(start_time) == 1970, NA_POSIXct_, start_time),
    end_time   = if_else(!is.na(end_time)   & year(end_time)   == 1970, NA_POSIXct_, end_time)
  ) %>%
  mutate(
    duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")),
    year    = if_else(!is.na(start_time), year(start_time), NA_integer_),
    month   = if_else(!is.na(start_time), month(start_time), NA_integer_),
    weekday = if_else(!is.na(start_time), wday(start_time, label = TRUE), NA)
  )

############################################################
#  FEATURE ENGINEERING (per activity / trace)
#  -> avg speed based on length_3d + moving_time
############################################################

hikes_features <- hikes %>%
  mutate(source=source) %>%
  filter(!is.na(length_3d), length_3d > 0, !is.na(uphill), uphill >= 0) %>%
  mutate(
    # Distances
    dist_km   = length_2d / 1000,
    dist3d_km = length_3d / 1000,

    # Moving time in hours
    moving_h  = moving_time / 3600,

    # Duration fallback (base has timestamps; personal usually not)
    duration_h_raw = duration_hours,
    duration_h_alt = moving_h,
    duration_h     = if_else(!is.na(duration_h_raw), duration_h_raw, duration_h_alt),

    # Robust avg speed = 3D distance / moving time
    avg_speed_kmh = if_else(moving_h > 0, dist3d_km / moving_h, NA_real_),

    # Terrain features
    uphill_per_km = if_else(dist3d_km > 0, uphill / dist3d_km, NA_real_),

    # Basic sanity filters
    duration_h    = if_else(duration_h < 0 | duration_h > 24, NA_real_, duration_h),
    avg_speed_kmh = if_else(avg_speed_kmh <= 0 | avg_speed_kmh > 60, NA_real_, avg_speed_kmh),
    uphill_per_km = if_else(uphill_per_km > 1500, NA_real_, uphill_per_km),

    slope_index = uphill_per_km
  )

############################################################
#  (OPTIONAL) EDA PLOTS
############################################################

# Uncomment if you want plots
# hikes_features %>% ggplot(aes(dist3d_km))     + geom_histogram(bins = 50)
# hikes_features %>% ggplot(aes(uphill))        + geom_histogram(bins = 50)
# hikes_features %>% ggplot(aes(uphill_per_km)) + geom_histogram(bins = 50)
# hikes_features %>% ggplot(aes(duration_h))    + geom_histogram(bins = 50)
# hikes_features %>% ggplot(aes(avg_speed_kmh)) + geom_histogram(bins = 50)

############################################################
#  GEO FEATURES + HIKE_ID (BASE ONLY)
#  -> personal data does NOT change the hike catalog
############################################################

hikes_geo <- hikes_features %>%
  filter(!is.na(bounds)) %>%
  mutate(
    bounds_nums = str_extract_all(bounds, "-?[0-9]+\\.?[0-9]*"),
    lon_min = map_dbl(bounds_nums, ~ as.numeric(.x[1])),
    lat_min = map_dbl(bounds_nums, ~ as.numeric(.x[2])),
    lon_max = map_dbl(bounds_nums, ~ as.numeric(.x[3])),
    lat_max = map_dbl(bounds_nums, ~ as.numeric(.x[4])),
    lon_center = (lon_min + lon_max) / 2,
    lat_center = (lat_min + lat_max) / 2
  ) %>%
  select(-bounds_nums) %>%   # ⚠️ do NOT drop source
  mutate(
    name_clean = name %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9 ]", " ") %>%
      str_squish(),
    lat_bin = round(lat_center, 2),
    lon_bin = round(lon_center, 2),
    hike_id = paste(name_clean, lat_bin, lon_bin, sep = "_")
  )

############################################################
#  AGGREGATION BY HIKE (BASE ONLY)
############################################################

hikes_by_id <- hikes_geo_base %>%
  group_by(hike_id) %>%
  summarise(
    name         = names(sort(table(name), decreasing = TRUE))[1],
    name_clean   = first(name_clean),
    n_records    = n(),
    n_users      = n_distinct(user),

    avg_dist_km   = mean(dist3d_km,      na.rm = TRUE),
    avg_uphill    = mean(uphill,         na.rm = TRUE),
    avg_diff      = mean(difficulty_num, na.rm = TRUE),
    avg_speed_kmh = mean(avg_speed_kmh,  na.rm = TRUE),
    avg_moving_h  = mean(moving_h,       na.rm = TRUE),

    lat_center   = mean(lat_center,     na.rm = TRUE),
    lon_center   = mean(lon_center,     na.rm = TRUE),
    .groups = "drop"
  )

hike_items <- hikes_by_id %>%
  select(
    hike_id, name, n_records, n_users,
    avg_dist_km, avg_uphill, avg_diff, avg_speed_kmh, avg_moving_h,
    lat_center, lon_center
  )

############################################################
#  USER–HIKE TABLE (BASE ONLY) for Neo4j
############################################################

user_hike <- hikes_geo_base %>%
  select(user, hike_id) %>%
  distinct()

############################################################
#  SIMILARITY MODEL 1: PHYS + GEO (BASE ONLY)
############################################################

hike_features_agg <- hike_items %>%
  filter(!is.na(avg_dist_km),
         !is.na(avg_uphill),
         !is.na(avg_diff),
         !is.na(avg_speed_kmh),
         !is.na(avg_moving_h),
         !is.na(lat_center),
         !is.na(lon_center))

# Physical features matrix
feat_mat <- hike_features_agg %>%
  select(avg_dist_km, avg_uphill, avg_diff, avg_speed_kmh, avg_moving_h) %>%
  scale() %>%
  as.matrix()

dist_phys <- as.matrix(dist(feat_mat))
rownames(dist_phys) <- hike_features_agg$hike_id
colnames(dist_phys) <- hike_features_agg$hike_id

# Geo distance (Haversine)
coords <- hike_features_agg %>%
  select(hike_id, lon_center, lat_center)

geo_mat <- distm(
  x = as.matrix(coords[, c("lon_center", "lat_center")]),
  fun = distHaversine
)

geo_dist_km <- geo_mat / 1000
rownames(geo_dist_km) <- coords$hike_id
colnames(geo_dist_km) <- coords$hike_id

# Combine
h_ids <- hike_features_agg$hike_id
dist_phys_aligned <- dist_phys[h_ids, h_ids]
geo_dist_aligned  <- geo_dist_km[h_ids, h_ids]

w_phys <- 1.0
w_geo  <- 1.0

total_dist <- w_phys * dist_phys_aligned + w_geo * (geo_dist_aligned / 50)
sim_mat <- 1 / (1 + total_dist)

sim_df <- as_tibble(sim_mat, rownames = "hike1") %>%
  pivot_longer(cols = -hike1, names_to = "hike2", values_to = "similarity") %>%
  filter(hike1 < hike2, similarity > 0.7)

k <- 20
sim_df_top <- sim_df %>%
  group_by(hike1) %>%
  slice_max(order_by = similarity, n = k, with_ties = FALSE) %>%
  ungroup()

############################################################
#  SIMILARITY MODEL 2: PHYS ONLY (BASE ONLY)
############################################################

hike_features_phys <- hike_items %>%
  filter(!is.na(avg_dist_km),
         !is.na(avg_uphill),
         !is.na(avg_diff),
         !is.na(avg_speed_kmh),
         !is.na(avg_moving_h))

feat_mat_phys <- hike_features_phys %>%
  select(avg_dist_km, avg_uphill, avg_diff, avg_speed_kmh, avg_moving_h) %>%
  scale() %>%
  as.matrix()

dist_phys_only <- as.matrix(dist(feat_mat_phys))
rownames(dist_phys_only) <- hike_features_phys$hike_id
colnames(dist_phys_only) <- hike_features_phys$hike_id

sim_mat_phys_only <- 1 / (1 + dist_phys_only)

sim_df_phys_only <- as_tibble(sim_mat_phys_only, rownames = "hike1") %>%
  pivot_longer(cols = -hike1, names_to = "hike2", values_to = "similarity") %>%
  filter(hike1 < hike2, similarity > 0.7)

sim_df_top_phys_only <- sim_df_phys_only %>%
  group_by(hike1) %>%
  slice_max(order_by = similarity, n = k, with_ties = FALSE) %>%
  ungroup()

############################################################
#  PERSONAL USERS EXPORT (separate, no influence on similarity)
############################################################

personal_activities <- hikes_features %>%
  filter(source == "personal") %>%
  mutate(
    personal_hike_id = paste0(
      "PERS_", str_to_lower(user), "_",
      str_replace_all(str_to_lower(name), "[^a-z0-9]+", "_")
    )
  ) %>%
  select(
    user,
    personal_hike_id,
    name,
    activity_type,
    dist3d_km,
    uphill,
    max_elevation,
    moving_h,
    avg_speed_kmh
  ) %>%
  distinct()

personal_user_hike <- personal_activities %>%
  select(user, personal_hike_id) %>%
  distinct()

############################################################
#  EXPORT CSVs FOR NEO4J
############################################################

write_csv(hike_items,               "hike_items.csv")
write_csv(user_hike,                "user_hike.csv")
write_csv(sim_df_top,               "hike_similarity_topk.csv")
write_csv(sim_df_top_phys_only,     "hike_similarity_topk_phys_only.csv")

# Personal demo data
write_csv(personal_activities,      "personal_hike_items.csv")
write_csv(personal_user_hike,       "personal_user_hike.csv")

cat("CSV files generated:\n",
    "- hike_items.csv\n",
    "- user_hike.csv\n",
    "- hike_similarity_topk.csv (model 1: phys+geo)\n",
    "- hike_similarity_topk_phys_only.csv (model 2: phys only)\n",
    "- personal_hike_items.csv\n",
    "- personal_user_hike.csv\n")
