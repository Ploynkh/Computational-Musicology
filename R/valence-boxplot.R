library(tidyverse)

era_fill <- c("1990s" = "#E69F00", "2020s" = "#56B4E9")
era_color <- c("1990s" = "#E69F00", "2020s" = "#56B4E9")

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11, color = "grey30"),
      axis.title.x = element_text(face = "bold", size = 11),
      axis.title.y = element_text(face = "bold", size = 11),
      axis.text = element_text(color = "black"),
      legend.title = element_blank(),
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(12, 12, 12, 12)
    )
)
# load, combine, clean data

rnb_90s <- read_csv("1990s.csv", show_col_types = FALSE) %>%
  mutate(
    `Release Date` = as.character(`Release Date`),
    Era = "1990s"
  )

rnb_2020s <- read_csv("2020s.csv", show_col_types = FALSE) %>%
  mutate(
    `Release Date` = as.character(`Release Date`),
    Era = "2020s"
  )

rnb_tracks <- bind_rows(rnb_90s, rnb_2020s)

rnb_tracks <- rnb_tracks %>%
  mutate(
    Danceability = as.numeric(Danceability),
    Energy = as.numeric(Energy),
    Valence = as.numeric(Valence),
    Tempo = as.numeric(Tempo),
    Era = factor(Era, levels = c("1990s", "2020s"))
  ) %>%
  drop_na(Danceability, Energy, Valence, Tempo)

valence <- ggplot(rnb_tracks, aes(x = Era, y = Valence, fill = Era)) +
  geom_boxplot(
    width = 0.5,
    alpha = 0.75,
    outlier.shape = NA,
    color = "grey25"
  ) +
  geom_jitter(
    width = 0.08,
    size = 1.8,
    alpha = 0.45,
    color = "black"
  ) +
  scale_fill_manual(values = era_fill) +
  labs(
#    title = "Valence Across R&B Eras",
#    subtitle = "1990s tracks tend to have slightly higher emotional positivity",
    y = "Valence",
    x = NULL
  ) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

plot(valence)