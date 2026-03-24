library(tidyverse)

bg <- "#0E0A1A"

pitch_classes <- c("C", "C#", "D", "Eb", "E", "F",
                   "F#", "G", "Ab", "A", "Bb", "B")

# ── Load data ─────────────────────────────────────────────────────────────────
read_chroma <- function(path) {
  read_csv(path, col_names = pitch_classes, show_col_types = FALSE) %>%
    mutate(frame = row_number()) %>%
    pivot_longer(cols = all_of(pitch_classes),
                 names_to  = "pitch_class",
                 values_to = "value") %>%
    mutate(pitch_class = factor(pitch_class, levels = pitch_classes))
}

no_diggity <- read_chroma("nodiggity_chroma.csv")
good_days  <- read_chroma("gooddays_chroma.csv")

# ── Helper function ───────────────────────────────────────────────────────────
make_chromagram <- function(data, track_title, era_label) {
  ggplot(data, aes(x = frame, y = pitch_class, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(
      option = "magma",
      name   = "Intensity"
    ) +
    scale_y_discrete(limits = rev(pitch_classes)) +
    labs(
      title    = paste0("Chromagram: ", track_title),
      subtitle = paste0("NNLS Chroma features | ", era_label),
      x        = "Time (frames)",
      y        = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, color = "white"),
      plot.subtitle    = element_text(size = 10, color = "grey60"),
      axis.title.x     = element_text(face = "bold", size = 10, color = "white",
                                      margin = margin(t = 6)),
      axis.text.x      = element_text(color = "grey70"),
      axis.text.y      = element_text(color = "grey70", size = 10),
      legend.title     = element_text(color = "white", size = 9),
      legend.text      = element_text(color = "grey70", size = 8),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = bg, colour = NA),
      plot.background  = element_rect(fill = bg, colour = NA),
      plot.margin      = margin(12, 12, 12, 12)
    )
}

# ── Generate plots ────────────────────────────────────────────────────────────
chroma_90s   <- make_chromagram(no_diggity, "No Diggity", "1990s R&B")
chroma_2020s <- make_chromagram(good_days,  "Good Days",  "2020s R&B")

plot(chroma_90s)
plot(chroma_2020s)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/chroma_no_diggity.png",
       plot = chroma_90s,   width = 10, height = 5, dpi = 300, bg = bg)

ggsave("images/chroma_good_days.png",
       plot = chroma_2020s, width = 10, height = 5, dpi = 300, bg = bg)