############################################################
# 02_reco_llm_demo.R
# - Charge l'historique & les reco exportés de Neo4j (CSV)
# - Construit un prompt
# - Appelle l'API OpenAI pour générer un texte explicatif
############################################################

library(tidyverse)
library(glue)
library(httr)
library(jsonlite)

#########################
# 1. CHARGER LES CSV
#########################

username <- "cristina"  # juste pour le texte du prompt

history <- read_csv("history_cristina.csv")
reco    <- read_csv("reco_cristina.csv")

print(history)
print(reco)

#########################
# 2. CONSTRUIRE LE PROMPT
#########################

build_prompt <- function(username, history_df, reco_df) {

  if (nrow(history_df) == 0) {
    history_txt <- "(aucune rando trouvée pour cet utilisateur)"
  } else {
    history_txt <- paste0(
      apply(history_df, 1, function(row) {
        sprintf(
          "- %s (%.1f km, %.0f m D+, diff=%.1f)",
          row[["hike_name"]],
          as.numeric(row[["dist_km"]]),
          as.numeric(row[["uphill"]]),
          as.numeric(row[["difficulty"]])
        )
      }),
      collapse = "\n"
    )
  }

  if (nrow(reco_df) == 0) {
    reco_txt <- "(aucune recommandation trouvée)"
  } else {
    reco_txt <- paste0(
      apply(reco_df, 1, function(row) {
        sprintf(
          "- %s (%.1f km, %.0f m D+, diff=%.1f, score=%.3f, commun=%.0f)",
          row[["hike_name"]],
          as.numeric(row[["dist_km"]]),
          as.numeric(row[["uphill"]]),
          as.numeric(row[["difficulty"]]),
          as.numeric(row[["score"]]),
          as.numeric(row[["common_hikes"]])
        )
      }),
      collapse = "\n"
    )
  }

  glue("
Tu es un expert en randonnée et en systèmes de recommandation.

On te donne l'historique de randonnées d'un utilisateur et une liste de randonnées
recommandées par un système de recommandation basé sur un graphe de similarité
(distance, dénivelé, difficulté, proximité géographique).

Utilisateur : {username}

Historique (randonnées déjà faites) :
{history_txt}

Recommandations proposées :
{reco_txt}

Tâches :
1. Résume le profil de cet utilisateur (niveau, style de randonnées, préférences).
2. Explique en quelques phrases pourquoi chaque randonnée recommandée est pertinente pour lui
   (distance, dénivelé, difficulté, ressemblance avec des randos déjà faites, zone géographique).
3. Suggère comment on pourrait améliorer ce système de recommandation
   (d'autres features à utiliser, données personnelles, saison, météo, etc.).
4. Termine par 1–2 phrases de conclusion motivantes pour cet utilisateur.
5. Réponds en français, de manière claire, structurée et naturelle.
")
}

prompt <- build_prompt(username, history, reco)
cat("===== PROMPT =====\n")
cat(prompt)
cat("\n==================\n")

#########################
# 3. APPEL À L'API OPENAI
#########################

api_key <- Sys.getenv("OPENAI_API_KEY")
if (api_key == "") {
  stop("Merci de définir ta clé OpenAI dans .Renviron (OPENAI_API_KEY).")
}

url <- "https://api.openai.com/v1/responses"

body <- list(
  model = "gpt-4.1-mini",      # ou un autre modèle compatible Responses API
  input = prompt,
  temperature = 0.7,
  max_output_tokens = 800
)

res <- httr::POST(
  url,
  add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type"  = "application/json"
  ),
  body = jsonlite::toJSON(body, auto_unbox = TRUE)
)

if (httr::status_code(res) >= 300) {
  cat("Erreur API:\n")
  print(content(res, as = "text", encoding = "UTF-8"))

} else {
  out <- content(res, as = "parsed", encoding = "UTF-8")

  # 1) Cas idéal : champ output_text direct
  if (!is.null(out$output_text)) {
    answer <- out$output_text

    # 2) Sinon : on va chercher le texte dans output[[1]]$content[[1]]$text
  } else if (!is.null(out$output) &&
             length(out$output) > 0 &&
             !is.null(out$output[[1]]$content) &&
             length(out$output[[1]]$content) > 0 &&
             !is.null(out$output[[1]]$content[[1]]$text)) {

    answer <- out$output[[1]]$content[[1]]$text

    # 3) Fallback : on affiche la structure brute pour debug
  } else {
    answer <- "Impossible d'extraire proprement le texte. Voici la structure brute :\n"
    answer <- paste0(answer, capture.output(str(out)), collapse = "\n")
  }

  cat("\n===== RÉPONSE DU LLM =====\n")
  cat(answer)
  cat("\n==========================\n")
}
