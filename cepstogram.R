library(tidyverse)

bg <- "#0E0A1A"

mfcc_cols <- paste0("mfcc_", sprintf("%02d", 1:20))

read_timbre <- function(path) {
  read_csv(path, col_names = mfcc_cols, show_col_types = FALSE) %>%
    mutate(frame = row_number()) %>%
    pivot_longer(cols = all_of(mfcc_cols),
                 names_to  = "mfcc",
                 values_to = "value") %>%
    mutate(mfcc = factor(mfcc, levels = rev(mfcc_cols)))
}

creep  <- read_timbre("creep_timbre.csv")
snooze <- read_timbre("snooze_timbre.csv")

make_cepstrogram <- function(data, track_title, era_label) {
  ggplot(data, aes(x = frame, y = mfcc, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(option = "rocket", name = "Magnitude") +
    labs(
      title    = paste0("Cepstrogram: ", track_title),
      subtitle = paste0("MFCC timbre features | ", era_label),
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
      axis.text.y      = element_text(color = "grey70", size = 9),
      legend.title     = element_text(color = "white", size = 9),
      legend.text      = element_text(color = "grey70", size = 8),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = bg, colour = NA),
      plot.background  = element_rect(fill = bg, colour = NA),
      plot.margin      = margin(12, 12, 12, 12)
    )
}

cep_creep  <- make_cepstrogram(creep,  "Creep",  "1990s R&B — TLC")
cep_snooze <- make_cepstrogram(snooze, "Snooze", "2020s R&B — SZA")

plot(cep_creep)
plot(cep_snooze)

ggsave("images/cepstrogram_creep.png",
       plot = cep_creep,   width = 10, height = 5, dpi = 300, bg = bg)
ggsave("images/cepstrogram_snooze.png",
       plot = cep_snooze,  width = 10, height = 5, dpi = 300, bg = bg)