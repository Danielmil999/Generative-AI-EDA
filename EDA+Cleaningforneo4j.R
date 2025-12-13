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
#  CLEAN & NORMALIZE
############################################################

hikes <- raw_hikes %>%
  clean_names() %>%
  distinct()

# Aperçu général
skim(hikes)

# Résumé des valeurs manquantes
miss_summary <- hikes %>%
  summarise(across(everything(), ~ sum(is.na(.))))
print(miss_summary)

############################################################
#  FIX DATA TYPES
############################################################

hikes <- hikes %>%
  mutate(
    start_time    = ymd_hms(start_time, tz = "UTC"),
    end_time      = ymd_hms(end_time,   tz = "UTC"),
    length_2d     = as.numeric(length_2d),
    length_3d     = as.numeric(length_3d),
    uphill        = as.numeric(uphill),
    downhill      = as.numeric(downhill),
    max_elevation = as.numeric(max_elevation),
    min_elevation = as.numeric(min_elevation),
    moving_time   = as.numeric(moving_time),  # secondes
    max_speed     = as.numeric(max_speed)
  )

############################################################
#  FILTER OUTLIERS & IMPOSSIBLE VALUES
############################################################

hikes <- hikes %>%
  filter(
    length_2d > 100, length_2d < 100000,
    length_3d > 100, length_3d < 100000,
    min_elevation > -100,
    max_elevation < 5000,
    uphill < 5000,
    downhill < 5000,
    max_speed < 25
  )

############################################################
#  DIFFICULTY NORMALIZATION (T1-T6 → numeric)
############################################################

hikes <- hikes %>%
  mutate(
    difficulty_raw = difficulty,
    difficulty     = str_extract(difficulty, "T[1-6][+-]?")
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
#  FIX DATES & TIME FEATURES
############################################################

hikes <- hikes %>%
  mutate(
    start_time = if_else(year(start_time) == 1970, NA_POSIXct_, start_time),
    end_time   = if_else(year(end_time)   == 1970, NA_POSIXct_, end_time)
  ) %>%
  filter(!(is.na(start_time) & is.na(end_time))) %>%
  mutate(
    duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")),
    year    = year(start_time),
    month   = month(start_time),
    weekday = wday(start_time, label = TRUE)
  )

############################################################
#  FEATURE ENGINEERING (per activity / trace)
#  -> vitesse basée sur length_3d + moving_time
############################################################

hikes_features <- hikes %>%
  filter(length_3d > 0, uphill >= 0) %>%
  mutate(
    # Distances
    dist_km    = length_2d / 1000,
    dist3d_km  = length_3d / 1000,

    # Temps : on privilégie moving_time (temps en mouvement)
    moving_h   = moving_time / 3600,

    # Durée fallback si moving_time manquant
    duration_h_raw = duration_hours,
    duration_h_alt = moving_h,
    duration_h     = if_else(!is.na(duration_h_raw), duration_h_raw, duration_h_alt),

    # Vitesse moyenne robuste = distance 3D / moving_time
    avg_speed_kmh = if_else(moving_h > 0, dist3d_km / moving_h, NA_real_),

    # Features terrain
    uphill_per_km = if_else(dist3d_km > 0, uphill / dist3d_km, NA_real_),

    # Nettoyage basique
    duration_h    = if_else(duration_h < 0 | duration_h > 24, NA_real_, duration_h),
    avg_speed_kmh = if_else(avg_speed_kmh <= 0 | avg_speed_kmh > 60, NA_real_, avg_speed_kmh),
    uphill_per_km = if_else(uphill_per_km > 1500, NA_real_, uphill_per_km),

    slope_index = uphill_per_km
  )

############################################################
#  EDA PLOTS (OPTIONNEL, VISUEL)
############################################################

hikes_features %>% ggplot(aes(dist3d_km))     + geom_histogram(bins = 50)
hikes_features %>% ggplot(aes(uphill))        + geom_histogram(bins = 50)
hikes_features %>% ggplot(aes(uphill_per_km)) + geom_histogram(bins = 50)
hikes_features %>% ggplot(aes(duration_h))    + geom_histogram(bins = 50)
hikes_features %>% ggplot(aes(avg_speed_kmh)) + geom_histogram(bins = 50)

ggplot(hikes_features, aes(dist3d_km, avg_speed_kmh)) + geom_point(alpha = 0.2)
ggplot(hikes_features, aes(dist3d_km, uphill))        + geom_point(alpha = 0.2)

cor_matrix <- hikes_features %>%
  select(dist3d_km, uphill, uphill_per_km, duration_h, avg_speed_kmh, difficulty_num) %>%
  cor(use = "complete.obs")

corrplot(cor_matrix, method = "color")

GGally::ggpairs(
  hikes_features %>%
    select(dist3d_km, uphill, uphill_per_km, duration_h, avg_speed_kmh, difficulty_num)
)

############################################################
#  GEO FEATURES + HIKE_ID (rando logique)
############################################################

hikes_geo <- hikes_features %>%   # IMPORTANT: on part de hikes_features pour garder avg_speed_kmh
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
  select(-bounds_nums) %>%
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
#  AGGREGATION PAR RANDONNÉE (HIKE_ID)
############################################################

hikes_by_id <- hikes_geo %>%
  group_by(hike_id) %>%
  summarise(
    name         = names(sort(table(name), decreasing = TRUE))[1],
    name_clean   = first(name_clean),
    n_records    = n(),
    n_users      = n_distinct(user),


    # IMPORTANT: distance moyenne basée sur 3D
    avg_dist_km  = mean(dist3d_km,      na.rm = TRUE),
    avg_uphill   = mean(uphill,         na.rm = TRUE),
    avg_diff     = mean(difficulty_num, na.rm = TRUE),

    # NOUVEAU: vitesse moyenne (permet marche vs vélo)
    avg_speed_kmh = mean(avg_speed_kmh, na.rm = TRUE),
    avg_moving_h = mean(moving_h, na.rm = TRUE),

    lat_center   = mean(lat_center,     na.rm = TRUE),
    lon_center   = mean(lon_center,     na.rm = TRUE),
    .groups = "drop"
  )

hike_items <- hikes_by_id %>%
  select(
    hike_id,
    name,
    n_records,
    n_users,
    avg_dist_km,
    avg_uphill,
    avg_diff,
    avg_speed_kmh,
    avg_moving_h,
    lat_center,
    lon_center
  )

############################################################
#  USER–HIKE TABLE (pour Neo4j)
############################################################

user_hike <- hikes_geo %>%
  select(user, hike_id) %>%
  distinct()

############################################################
#  SIMILARITY BETWEEN HIKES (features + geo) + vitesse
############################################################

hike_features_agg <- hike_items %>%
  filter(!is.na(avg_dist_km),
         !is.na(avg_uphill),
         !is.na(avg_diff),
         !is.na(avg_speed_kmh),
         !is.na(avg_moving_h),
         !is.na(lat_center),
         !is.na(lon_center))

# Matrice de features physiques (+ vitesse)
feat_mat <- hike_features_agg %>%
  select(avg_dist_km, avg_uphill, avg_diff, avg_speed_kmh, avg_moving_h) %>%
  scale() %>%
  as.matrix()

dist_phys <- as.matrix(dist(feat_mat))
rownames(dist_phys) <- hike_features_agg$hike_id
colnames(dist_phys) <- hike_features_agg$hike_id

# Distance géographique (Haversine)
coords <- hike_features_agg %>%
  select(hike_id, lon_center, lat_center)

geo_mat <- distm(
  x = as.matrix(coords[, c("lon_center", "lat_center")]),
  fun = distHaversine
)

geo_dist_km <- geo_mat / 1000
rownames(geo_dist_km) <- coords$hike_id
colnames(geo_dist_km) <- coords$hike_id

# Combinaison physique + géo
h_ids <- hike_features_agg$hike_id

dist_phys_aligned <- dist_phys[h_ids, h_ids]
geo_dist_aligned  <- geo_dist_km[h_ids, h_ids]

w_phys <- 1.0
w_geo  <- 1.0

total_dist <- w_phys * dist_phys_aligned + w_geo * (geo_dist_aligned / 50)

sim_mat <- 1 / (1 + total_dist)

############################################################
#  SIMILARITY BETWEEN HIKES (PHYS ONLY) + vitesse
#  -> NO GEO distance, new CSV
############################################################

hike_features_phys <- hike_items %>%
  filter(!is.na(avg_dist_km),
         !is.na(avg_uphill),
         !is.na(avg_diff),
         !is.na(avg_speed_kmh))

# Matrice de features physiques (+ vitesse)
feat_mat_phys <- hike_features_phys %>%
  select(avg_dist_km, avg_uphill, avg_diff, avg_speed_kmh, avg_moving_h) %>%
  scale() %>%
  as.matrix()

dist_phys_only <- as.matrix(dist(feat_mat_phys))
rownames(dist_phys_only) <- hike_features_phys$hike_id
colnames(dist_phys_only) <- hike_features_phys$hike_id

# Similarité = 1/(1 + distance)
sim_mat_phys_only <- 1 / (1 + dist_phys_only)

# Format long + top-k
sim_df_phys_only <- as_tibble(sim_mat_phys_only, rownames = "hike1") %>%
  pivot_longer(
    cols = -hike1,
    names_to = "hike2",
    values_to = "similarity"
  ) %>%
  filter(hike1 < hike2) %>%
  filter(similarity > 0.7)

k <- 20

sim_df_top_phys_only <- sim_df_phys_only %>%
  group_by(hike1) %>%
  slice_max(order_by = similarity, n = k, with_ties = FALSE) %>%
  ungroup()



############################################################
#  FORMAT LONG + TOP-K PAIRES SIMILAIRES
############################################################

sim_df <- as_tibble(sim_mat, rownames = "hike1") %>%
  pivot_longer(
    cols = -hike1,
    names_to = "hike2",
    values_to = "similarity"
  ) %>%
  filter(hike1 < hike2) %>%
  filter(similarity > 0.7)

k <- 20

sim_df_top <- sim_df %>%
  group_by(hike1) %>%
  slice_max(order_by = similarity, n = k, with_ties = FALSE) %>%
  ungroup()

############################################################
#  EXPORT CSV POUR NEO4J
############################################################

write_csv(hike_items, "hike_items.csv")
write_csv(user_hike,  "user_hike.csv")
write_csv(sim_df_top, "hike_similarity_topk.csv")
# Export CSV pour Neo4j (modèle 2)
write_csv(sim_df_top_phys_only, "hike_similarity_topk_phys_only.csv")

print("CSV files generated: hike_items.csv, user_hike.csv, hike_similarity_topk.csv, hike_similarity_topk_phys_only.csv")
