library(tidyverse)
library(patchwork)

# ── Colours ───────────────────────────────────────────────────────────────────
era_fill   <- c("1990s" = "#FF2D78", "2020s" = "#C77DFF")
era_colour <- c("1990s" = "#FF2D78", "2020s" = "#C77DFF")
bg         <- "#0E0A1A"

# ── Load & combine data ───────────────────────────────────────────────────────
rnb_90s <- read_csv("1990s.csv", show_col_types = FALSE) %>%
  mutate(`Release Date` = as.character(`Release Date`), Era = "1990s")

rnb_2020s <- read_csv("2020s.csv", show_col_types = FALSE) %>%
  mutate(`Release Date` = as.character(`Release Date`), Era = "2020s")

rnb_tracks <- bind_rows(rnb_90s, rnb_2020s) %>%
  mutate(
    Danceability = as.numeric(Danceability),
    Energy       = as.numeric(Energy),
    Valence      = as.numeric(Valence),
    Loudness     = as.numeric(Loudness),
    Era          = factor(Era, levels = c("1990s", "2020s"))
  ) %>%
  drop_na(Danceability, Energy, Valence, Loudness)

# ── Shared dark theme ─────────────────────────────────────────────────────────
theme_density <- theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13, hjust = 0.5, color = "white"),
    axis.title.x       = element_text(face = "bold", size = 10, color = "white", margin = margin(t = 6)),
    axis.title.y       = element_text(face = "bold", size = 10, color = "white"),
    axis.text          = element_text(color = "grey70"),
    legend.title       = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10, color = "white"),
    legend.key         = element_rect(fill = bg, colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "#2E2E32", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.background   = element_rect(fill = bg, colour = NA),
    plot.background    = element_rect(fill = bg, colour = NA),
    plot.margin        = margin(10, 12, 10, 12)
  )

# ── Helper function ───────────────────────────────────────────────────────────
make_density <- function(data, var, x_label, x_limits = NULL) {
  p <- ggplot(data, aes(x = .data[[var]], fill = Era, colour = Era)) +
    geom_density(alpha = 0.45, linewidth = 0.6) +
    scale_fill_manual(values = era_fill) +
    scale_colour_manual(values = era_colour) +
    labs(title = var, x = x_label, y = "Density") +
    theme_density
  
  if (!is.null(x_limits)) p <- p + coord_cartesian(xlim = x_limits)
  p
}

# ── Four panels ───────────────────────────────────────────────────────────────
p_dance    <- make_density(rnb_tracks, "Danceability", "Danceability (0–1)", c(0, 1))
p_energy   <- make_density(rnb_tracks, "Energy",       "Energy (0–1)",       c(0, 1))
p_valence  <- make_density(rnb_tracks, "Valence",      "Valence (0–1)",      c(0, 1))
p_loudness <- make_density(rnb_tracks, "Loudness",     "Loudness (dB)")

# ── Combine ───────────────────────────────────────────────────────────────────
combined <- (p_dance | p_energy | p_valence | p_loudness) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = bg, colour = NA)
  )

combined

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/density_track_features.png",
       plot   = combined,
       width  = 14,
       height = 4,
       dpi    = 300,
       bg     = bg)