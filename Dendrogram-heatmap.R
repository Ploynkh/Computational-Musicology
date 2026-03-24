library(tidyverse)
library(tidymodels)
library(ggdendro)

bg <- "#0E0A1A"

# ── Load data ─────────────────────────────────────────────────────────────────
data_90s <- read_csv("1990s.csv") %>%
  mutate(era = "1990s", `Release Date` = as.character(`Release Date`))

data_20s <- read_csv("2020s.csv") %>%
  mutate(era = "2020s", `Release Date` = as.character(`Release Date`))

rb_corpus <- bind_rows(data_90s, data_20s) %>%
  drop_na(Danceability, Energy, Valence, Tempo) %>%
  mutate(`Track Name` = str_trunc(`Track Name`, 30))

# ── Pre-processing ────────────────────────────────────────────────────────────
rb_recipe <- recipe(`Track Name` ~ Danceability + Energy + Loudness +
                      Speechiness + Acousticness + Instrumentalness +
                      Liveness + Valence + Tempo,
                    data = rb_corpus) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep()

rb_juice <- juice(rb_recipe) %>%
  column_to_rownames("Track Name")

rb_matrix <- as.matrix(rb_juice)

# ── Clustering ────────────────────────────────────────────────────────────────
rb_dist  <- dist(rb_juice, method = "euclidean")
rb_clust <- hclust(rb_dist, method = "ward.D2")

# ── Dendrogram with ggdendro ──────────────────────────────────────────────────
dend_data <- dendro_data(rb_clust, type = "rectangle")

# Add era colour to labels
label_df <- dend_data$labels %>%
  mutate(era = case_when(
    label %in% str_trunc(data_90s$`Track Name`, 30) ~ "1990s",
    TRUE ~ "2020s"
  ))

dendrogram_plot <- ggplot() +
  geom_segment(data = dend_data$segments,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = "grey50", linewidth = 0.4) +
  geom_text(data = label_df,
            aes(x = x, y = y - 0.3, label = label, colour = era),
            angle = 90, hjust = 1, size = 2.5) +
  scale_colour_manual(values = c("1990s" = "#FF2D78", "2020s" = "#C77DFF"),
                      name = NULL) +
  labs(
    title    = "Hierarchical Clustering of R&B Tracks",
    subtitle = "Clustered by audio features using Ward linkage"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "white"),
    plot.subtitle    = element_text(size = 10, color = "grey60"),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    legend.text      = element_text(color = "white"),
    legend.position  = "bottom",
    panel.background = element_rect(fill = bg, colour = NA),
    plot.background  = element_rect(fill = bg, colour = NA),
    plot.margin      = margin(12, 12, 40, 12)
  )

plot(dendrogram_plot)

# ── Heatmap with ggplot2 ──────────────────────────────────────────────────────
heatmap_df <- as.data.frame(rb_matrix) %>%
  rownames_to_column("Track") %>%
  pivot_longer(cols = -Track, names_to = "Feature", values_to = "Value") %>%
  mutate(era = case_when(
    Track %in% str_trunc(data_90s$`Track Name`, 30) ~ "1990s",
    TRUE ~ "2020s"
  ))

heatmap_plot <- ggplot(heatmap_df, aes(x = Feature, y = Track, fill = Value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "Value") +
  facet_grid(era ~ ., scales = "free_y", space = "free_y") +
  labs(
    title    = "Audio Feature Heatmap",
    subtitle = "Normalised feature values by track and era",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "white"),
    plot.subtitle    = element_text(size = 10, color = "grey60"),
    axis.text.x      = element_text(color = "grey70", angle = 30, hjust = 1),
    axis.text.y      = element_text(color = "grey70", size = 7),
    strip.text       = element_text(color = "white", face = "bold"),
    legend.text      = element_text(color = "grey70"),
    legend.title     = element_text(color = "white"),
    panel.grid       = element_blank(),
    panel.background = element_rect(fill = bg, colour = NA),
    plot.background  = element_rect(fill = bg, colour = NA),
    plot.margin      = margin(12, 12, 12, 12)
  )

plot(heatmap_plot)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/dendrogram.png",
       plot = dendrogram_plot, width = 12, height = 7, dpi = 300, bg = bg)

ggsave("images/heatmap.png",
       plot = heatmap_plot,    width = 10, height = 10, dpi = 300, bg = bg)