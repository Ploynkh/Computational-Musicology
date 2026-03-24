library(tidyverse)

bg <- "#0E0A1A"

# ── Read tempogram CSV files ──────────────────────────────────────────────────
yeah_tempogram_raw    <- read_csv("yeah_act.csv",     show_col_types = FALSE)
gooddays_tempogram_raw <- read_csv("gooddays_act.csv", show_col_types = FALSE)

# ── Convert to long format ────────────────────────────────────────────────────
make_tempogram_long <- function(df, bpm_min = 60, bpm_max = 180) {
  df <- df %>% mutate(TIME = row_number())
  n_tempo_cols <- ncol(df) - 1
  colnames(df)[1:n_tempo_cols] <- seq(bpm_min, bpm_max, length.out = n_tempo_cols)
  df %>%
    pivot_longer(cols = -TIME, names_to = "tempo", values_to = "value") %>%
    mutate(
      tempo = as.numeric(tempo),
      TIME  = as.numeric(TIME),
      value = as.numeric(value)
    ) %>%
    drop_na(TIME, tempo, value)
}

yeah_tempogram_long     <- make_tempogram_long(yeah_tempogram_raw)
gooddays_tempogram_long <- make_tempogram_long(gooddays_tempogram_raw)

# ── Shared dark theme ─────────────────────────────────────────────────────────
theme_tempogram <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = bg, color = NA),
    panel.grid       = element_blank(),
    text             = element_text(color = "white"),
    axis.text        = element_text(color = "grey70"),
    axis.title       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(color = "white", face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey60", size = 10),
    plot.margin      = margin(12, 12, 12, 12)
  )

# ── Yeah! tempogram (1990s — magma) ──────────────────────────────────────────
yeah_tempogram <- ggplot(yeah_tempogram_long, aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", guide = "none") +
  labs(
    title    = "Yeah! — Autocorrelation Tempogram",
    subtitle = "1990s R&B · First 30 seconds",
    x        = "Time (s)",
    y        = "Tempo (BPM)"
  ) +
  theme_tempogram

# ── Good Days tempogram (2020s — magma) ──────────────────────────────────────
gooddays_tempogram <- ggplot(gooddays_tempogram_long, aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", guide = "none") +
  labs(
    title    = "Good Days — Autocorrelation Tempogram",
    subtitle = "2020s R&B · First 30 seconds",
    x        = "Time (s)",
    y        = "Tempo (BPM)"
  ) +
  theme_tempogram

print(yeah_tempogram)
print(gooddays_tempogram)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/tempogram_yeah.png",
       plot = yeah_tempogram,     width = 10, height = 5, dpi = 300, bg = bg)

ggsave("images/tempogram_gooddays.png",
       plot = gooddays_tempogram, width = 10, height = 5, dpi = 300, bg = bg)