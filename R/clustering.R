library(tidyverse)
library(tidymodels)

bg     <- "#0E0A1A"
era_fill <- c("1990s" = "#FF2D78", "2020s" = "#C77DFF")

# ── Data ──────────────────────────────────────────────────────────────────────
data_90s <- read_csv("1990s.csv") %>%
  mutate(era = "1990s", `Release Date` = as.character(`Release Date`))

data_20s <- read_csv("2020s.csv") %>%
  mutate(era = "2020s", `Release Date` = as.character(`Release Date`))

rb_corpus <- bind_rows(data_90s, data_20s) %>%
  mutate(
    era          = factor(era, levels = c("1990s", "2020s")),
    Danceability = as.numeric(Danceability),
    Energy       = as.numeric(Energy),
    Loudness     = as.numeric(Loudness),
    Speechiness  = as.numeric(Speechiness),
    Acousticness = as.numeric(Acousticness),
    Valence      = as.numeric(Valence),
    Tempo        = as.numeric(Tempo)
  ) %>%
  drop_na(era, Danceability, Energy, Loudness, Acousticness, Valence, Tempo)

# ── Recipe ────────────────────────────────────────────────────────────────────
rb_recipe <- recipe(era ~ Danceability + Energy + Loudness +
                      Speechiness + Acousticness + Valence + Tempo,
                    data = rb_corpus) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# ── kNN model with cross-validation ──────────────────────────────────────────
rb_cv    <- vfold_cv(rb_corpus, v = 5)

knn_model <- nearest_neighbor(neighbors = 5) %>%
  set_mode("classification") %>%
  set_engine("kknn")

rb_knn <- workflow() %>%
  add_recipe(rb_recipe) %>%
  add_model(knn_model) %>%
  fit_resamples(rb_cv, control = control_resamples(save_pred = TRUE))

# ── Confusion matrix ──────────────────────────────────────────────────────────
conf_mat_plot <- rb_knn %>%
  collect_predictions() %>%
  conf_mat(truth = era, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_fill_gradient(low = "#160D2A", high = "#FF2D78") +
  labs(
    title    = "Confusion Matrix: kNN Classifier",
    subtitle = "5-fold cross-validation | predicting era from audio features"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "white"),
    plot.subtitle    = element_text(size = 10, color = "grey60"),
    axis.text        = element_text(color = "white"),
    axis.title       = element_text(color = "white", face = "bold"),
    panel.background = element_rect(fill = bg, colour = NA),
    plot.background  = element_rect(fill = bg, colour = NA),
    legend.text      = element_text(color = "grey70"),
    legend.title     = element_text(color = "white"),
    panel.grid       = element_blank()
  )

plot(conf_mat_plot)

# ── Accuracy ──────────────────────────────────────────────────────────────────
rb_knn %>% collect_metrics()

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/confusion_matrix.png",
       plot = conf_mat_plot, width = 6, height = 5, dpi = 300, bg = bg)