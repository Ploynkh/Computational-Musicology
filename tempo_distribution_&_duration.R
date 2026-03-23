library(tidyverse)

# -----------------------
# Load, combine, and clean data
# -----------------------

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

# Clean only needed variables
rnb_tracks <- rnb_tracks %>%
  mutate(
    Tempo = as.numeric(Tempo),
    `Duration (ms)` = as.numeric(`Duration (ms)`),
    Era = factor(Era, levels = c("1990s", "2020s"))
  ) %>%
  drop_na(Tempo, `Duration (ms)`) %>%
  mutate(
    Duration_min = `Duration (ms)` / 60000
  )

# Colors
era_fill <- c("1990s" = "#F4A261", "2020s" = "#56B4E9")

# -----------------------
# 1. TEMPO HISTOGRAM
# -----------------------

tempo_histogram <- ggplot(rnb_tracks, aes(x = Tempo, fill = Era)) +
  geom_histogram(binwidth = 10, alpha = 0.9, color = "#0D1B2A") +
  scale_fill_manual(values = era_fill) +
  facet_wrap(~Era) +
  labs(
    x = "Tempo (BPM)",
    y = "Number of Tracks"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.background = element_rect(fill = "#0D1B2A"),
    panel.grid = element_line(color = "#1B263B"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "#E0E1DD"),
    strip.text = element_text(size = 11, face = "bold", color = "white"),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", size = 11)
  )

# -----------------------
# 2. DURATION BOXPLOT
# -----------------------

duration_boxplot <- ggplot(rnb_tracks, aes(x = Era, y = Duration_min, fill = Era)) +
  geom_boxplot(alpha = 0.8, width = 0.6) +
  scale_fill_manual(values = era_fill) +
  labs(
    x = NULL,
    y = "Duration (minutes)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.background = element_rect(fill = "#0D1B2A"),
    panel.grid = element_line(color = "#1B263B"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "#E0E1DD"),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", size = 11)
  )

# -----------------------
# SHOW PLOTS
# -----------------------

# plot(tempo_histogram)
plot(duration_boxplot)