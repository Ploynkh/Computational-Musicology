library(tidyverse)

# ── Colours ───────────────────────────────────────────────────────────────────
era_fill   <- c("1990s" = "#FF2D78", "2020s" = "#C77DFF")
bg         <- "#0E0A1A"

# ── Load & combine data ───────────────────────────────────────────────────────
rnb_90s <- read_csv("1990s.csv", show_col_types = FALSE) %>%
  mutate(`Release Date` = as.character(`Release Date`), Era = "1990s")

rnb_2020s <- read_csv("2020s.csv", show_col_types = FALSE) %>%
  mutate(`Release Date` = as.character(`Release Date`), Era = "2020s")

rnb_tracks <- bind_rows(rnb_90s, rnb_2020s) %>%
  mutate(
    Tempo           = as.numeric(Tempo),
    `Duration (ms)` = as.numeric(`Duration (ms)`),
    Era             = factor(Era, levels = c("1990s", "2020s"))
  ) %>%
  drop_na(Tempo, `Duration (ms)`) %>%
  mutate(Duration_min = `Duration (ms)` / 60000)

# ── Shared dark theme ─────────────────────────────────────────────────────────
theme_dark <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = bg, color = NA),
    panel.grid.major = element_line(color = "#2E2E32", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    text             = element_text(color = "white"),
    axis.text        = element_text(color = "grey70"),
    axis.title.x     = element_text(face = "bold", size = 11, color = "white",
                                    margin = margin(t = 6)),
    axis.title.y     = element_text(face = "bold", size = 11, color = "white"),
    strip.text       = element_text(size = 11, face = "bold", color = "white"),
    legend.position  = "none",
    plot.margin      = margin(10, 12, 10, 12)
  )

# ── Tempo histogram ───────────────────────────────────────────────────────────
tempo_histogram <- ggplot(rnb_tracks, aes(x = Tempo, fill = Era)) +
  geom_histogram(binwidth = 10, alpha = 0.9, color = bg) +
  scale_fill_manual(values = era_fill) +
  facet_wrap(~Era) +
  labs(x = "Tempo (BPM)", y = "Number of Tracks") +
  theme_dark

# ── Duration boxplot ──────────────────────────────────────────────────────────
duration_boxplot <- ggplot(rnb_tracks, aes(x = Era, y = Duration_min, fill = Era)) +
  geom_boxplot(
    alpha         = 0.8,
    width         = 0.5,
    color         = "white",
    outlier.color = "white",
    outlier.size  = 2
  ) +
  scale_fill_manual(values = era_fill) +
  labs(x = NULL, y = "Duration (minutes)") +
  theme_dark

plot(tempo_histogram)
plot(duration_boxplot)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/tempo_histogram.png",
       plot = tempo_histogram,  width = 8, height = 5, dpi = 300, bg = bg)

ggsave("images/duration_boxplot.png",
       plot = duration_boxplot, width = 6, height = 5, dpi = 300, bg = bg)