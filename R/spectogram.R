library(tidyverse)

bg <- "#0E0A1A"

# ── Read spectrogram CSVs ─────────────────────────────────────────────────────
read_spectrogram <- function(path) {
  df <- read_csv(path, col_names = FALSE, show_col_types = FALSE)
  n_bins <- ncol(df)
  colnames(df) <- paste0("bin_", seq_len(n_bins))
  df %>%
    mutate(frame = row_number()) %>%
    pivot_longer(cols = starts_with("bin_"),
                 names_to  = "bin",
                 values_to = "value") %>%
    mutate(
      bin   = as.integer(str_remove(bin, "bin_")),
      value = log1p(value)
    )
}

nodiggity   <- read_spectrogram("nodiggity_spectogram.csv")
cellophane  <- read_spectrogram("cellophane_spectogram.csv")

# ── Helper function ───────────────────────────────────────────────────────────
make_spectrogram <- function(data, track_title, era_label) {
  
  # Keep only the lower frequency bins where musical content lives
  data <- data %>% filter(bin <= 100)
  
  # Cap extreme values so contrast is spread across the visible range
  q95 <- quantile(data$value, 0.95, na.rm = TRUE)
  data <- data %>% mutate(value = pmin(value, q95))
  
  ggplot(data, aes(x = frame, y = bin, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(option = "rocket", name = "Magnitude") +
    scale_y_reverse() +
    labs(
      title    = paste0("Spectrogram: ", track_title),
      subtitle = paste0("Frequency content over time | ", era_label),
      x        = "Time (frames)",
      y        = "Frequency (bins)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, color = "white"),
      plot.subtitle    = element_text(size = 10, color = "grey60"),
      axis.title.x     = element_text(face = "bold", size = 10, color = "white",
                                      margin = margin(t = 6)),
      axis.title.y     = element_text(face = "bold", size = 10, color = "white"),
      axis.text        = element_text(color = "grey70"),
      legend.title     = element_text(color = "white", size = 9),
      legend.text      = element_text(color = "grey70", size = 8),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = bg, colour = NA),
      plot.background  = element_rect(fill = bg, colour = NA),
      plot.margin      = margin(12, 12, 12, 12)
    )
}

# ── Generate plots ────────────────────────────────────────────────────────────
spec_nodiggity  <- make_spectrogram(nodiggity,  "No Diggity",  "1990s R&B — Blackstreet")
spec_cellophane <- make_spectrogram(cellophane, "Cellophane",  "2020s R&B — FKA Twigs")

plot(spec_nodiggity)
plot(spec_cellophane)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/Spectogram_Nodiggity.png",
       plot = spec_nodiggity,  width = 10, height = 5, dpi = 300, bg = bg)

ggsave("images/Spectogram_Cellophane.png",
       plot = spec_cellophane, width = 10, height = 5, dpi = 300, bg = bg)