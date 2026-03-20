# ==========================================
# 1. HERSTEL & BIBLIOTHEKEN
# ==========================================
dev.off() # Reset de grafische motor om 'invalid graphics state' te fixen
library(tidyverse)
library(tidymodels)
library(ggdendro)
library(viridis)
# ==========================================
# 2. DATA INLADEN (Zorg dat de bestandsnamen kloppen!)
# ==========================================
# We dwingen Release Date naar 'character' om de combine-error te voorkomen
data_90s <- read_csv("1990s.csv") %>% 
  mutate(era = "1990s", `Release Date` = as.character(`Release Date`))

data_20s <- read_csv("2020s.csv") %>% 
  mutate(era = "2020s", `Release Date` = as.character(`Release Date`))

# Samenvoegen tot één corpus
rb_corpus <- bind_rows(data_90s, data_20s) %>%
  drop_na(Danceability, Energy, Valence, Tempo) %>%
  mutate(`Track Name` = str_trunc(`Track Name`, 30))

# ==========================================
# 3. PRE-PROCESSING (RECIPE & JUICE)
# ==========================================
rb_recipe <- recipe(`Track Name` ~ Danceability + Energy + Loudness + 
                      Speechiness + Acousticness + Instrumentalness + 
                      Liveness + Valence + Tempo, 
                    data = rb_corpus) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep()

rb_juice <- juice(rb_recipe) %>%
  column_to_rownames("Track Name")

# Maak de matrix voor de heatmap
rb_matrix <- as.matrix(rb_juice)

# ==========================================
# 4. CLUSTERING BEREKENEN
# ==========================================
rb_dist <- dist(rb_juice, method = "euclidean")
rb_clust <- hclust(rb_dist, method = "ward.D2")

# ==========================================
# 5. DE PLOTS (Kijk rechtsonder bij 'Plots')
# ==========================================

# PLOT 1: HET DENDROGRAM (De Boom)
# We zetten 'sub = ""' om de tekst onderaan (rb_dist etc.) te verwijderen
# We zetten 'xlab = ""' om ook de as-naam weg te halen voor een cleaner resultaat
plot(rb_clust, 
     hang = -1, 
     cex = 0.6, 
     sub = "", 
     xlab = "", 
     main = "Dendrogram: R&B 90s (Movement) vs 2020s (Mood)")

# --- PLOT 2: PLOTTEN IN DE GEWENSTE KLEUREN ---
# We herstellen de marges om 'figure margins too large' errors te voorkomen
# Zorg er ook voor dat je plot-venster in RStudio groot genoeg is!
par(mar=c(1,1,1,1))

heatmap(rb_matrix, 
        Colv = NA,                      # Kenmerken niet clusteren, alleen nummers
        col = viridis(256),             # HIER ZETTEN WE HET KLEURENSCHEMA OP VIRIDIS
        scale = "column",                # Normalisatie per kenmerk
        main = "R&B Feature Heatmap: Movement vs Mood",
        cexRow = 0.6, cexCol = 0.8)     # Lettergrootte aanpassen voor leesbaarheid


