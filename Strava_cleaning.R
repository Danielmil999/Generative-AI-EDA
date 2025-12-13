library(readr)
library(dplyr)
library(stringr)
library(purrr)

files <- file.path(
  "data/raw",
  c(
    "activities_valentine.csv",
    "activities_Thibaud.csv",
    "activities_Matteo.csv",
    "activities_Jakob.csv"
  )
)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# helper: pick first existing column among candidates
pick_col <- function(df, candidates) {
  existing <- candidates[candidates %in% names(df)]
  if (length(existing) == 0) return(rep(NA, nrow(df)))
  Reduce(dplyr::coalesce, df[existing])
}

# helper: make numeric safely (handles "12,3" and "12.3" and blanks)
to_num <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

process_one <- function(path) {
  df <- read_csv(path, show_col_types = FALSE, name_repair = "unique")

  user <- str_match(path, "activities_([^\\.]+)\\.csv")[, 2]
  user <- str_to_title(user)

  if (!("Activity Type" %in% names(df)) && ("Type d'activité" %in% names(df))) {
    df <- rename(df, `Activity Type` = `Type d'activité`)
  }

  out <- df %>%
    mutate(
      user = user,

      name = pick_col(cur_data(), c("name", "Activity Name", "Nom de l'activité")),

      avg_dist_km = pick_col(cur_data(), c("avg_dist_km", "Distance...7", "Distance")),
      max_elevation = pick_col(cur_data(), c("max_elevation", "Altitude max.", "Elevation High")),
      avg_uphill = pick_col(cur_data(), c("avg_uphill", "Elevation Gain", "Dénivelé positif")),
      moving_time = pick_col(cur_data(), c("moving_time", "Moving Time", "Durée de déplacement", "Durrée de déplacement")),
      max_speed = pick_col(cur_data(), c("max_speed", "Max Speed", "Vitesse max.", "vitesse max."))
    ) %>%
    transmute(
      user,
      name,
      `Activity Type`,
      avg_dist_km = to_num(avg_dist_km),
      max_elevation = to_num(max_elevation),
      avg_uphill = to_num(avg_uphill),
      moving_time = to_num(moving_time),
      max_speed = to_num(max_speed)
    ) %>%
    filter(`Activity Type` %in% c("Hike", "Randonnée"))

  out
}

result <- map_dfr(files, process_one)

write_csv(result, "data/processed/strava_hike.csv")
