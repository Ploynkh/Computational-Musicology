library(tidyverse)

# ── Colours ───────────────────────────────────────────────────────────────────
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
    Era          = factor(Era, levels = c("1990s", "2020s"))
  ) %>%
  drop_na(Danceability, Energy)

# ── Plot ──────────────────────────────────────────────────────────────────────
dance_vs_energy <- ggplot(rnb_tracks, aes(x = Energy, y = Danceability, colour = Era)) +
  geom_point(size = 2.2, alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_colour_manual(values = era_colour) +
  labs(x = "Energy (0–1)", y = "Danceability (0–1)") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x     = element_text(face = "bold", size = 10, color = "white", margin = margin(t = 6)),
    axis.title.y     = element_text(face = "bold", size = 10, color = "white"),
    axis.text        = element_text(color = "grey70"),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 10, color = "white"),
    legend.key       = element_rect(fill = bg, colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#2E2E32", linewidth = 0.4),
    panel.background = element_rect(fill = bg, colour = NA),
    plot.background  = element_rect(fill = bg, colour = NA),
    plot.margin      = margin(10, 12, 10, 12)
  )

plot(dance_vs_energy)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/danceability_vs_energy.png",
       plot   = dance_vs_energy,
       width  = 7,
       height = 5,
       dpi    = 300,
       bg     = bg)