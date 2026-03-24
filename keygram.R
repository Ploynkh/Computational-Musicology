library(tidyverse)

bg <- "#0E0A1A"

pitch_classes <- c("C", "C#", "D", "Eb", "E", "F",
                   "F#", "G", "Ab", "A", "Bb", "B")

major_key <- c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09,
               2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53,
               2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

key_templates <- bind_rows(
  tibble(name     = paste0(pitch_classes, ":maj"),
         template = map(0:11, ~circshift(major_key, .))),
  tibble(name     = paste0(pitch_classes, ":min"),
         template = map(0:11, ~circshift(minor_key, .)))
)

read_chroma_keygram <- function(path) {
  read_csv(path, col_names = pitch_classes, show_col_types = FALSE) %>%
    mutate(frame = row_number())
}

match_keys <- function(chroma_df) {
  chroma_matrix <- chroma_df %>% select(-frame) %>% as.matrix()
  map_dfr(1:nrow(chroma_matrix), function(i) {
    row    <- chroma_matrix[i, ]
    scores <- map_dbl(key_templates$template, ~cor(row, .x))
    tibble(frame = i,
           key   = key_templates$name[which.max(scores)],
           score = max(scores))
  })
}

no_diggity_chroma <- read_chroma_keygram("nodiggity_chroma.csv")
pov_chroma        <- read_chroma_keygram("pov_chroma.csv")

keys_nodiggity <- match_keys(no_diggity_chroma)
keys_pov       <- match_keys(pov_chroma)

make_keygram <- function(key_df, track_title, era_label) {
  ggplot(key_df, aes(x = frame, y = key, fill = score)) +
    geom_tile() +
    scale_fill_viridis_c(option = "rocket", name = "Confidence") +
    labs(
      title    = paste0("Keygram: ", track_title),
      subtitle = paste0("Krumhansl-Schmuckler key estimation | ", era_label),
      x        = "Time (frames)",
      y        = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, color = "white"),
      plot.subtitle    = element_text(size = 10, color = "grey60"),
      axis.title.x     = element_text(face = "bold", size = 10, color = "white",
                                      margin = margin(t = 6)),
      axis.text.x      = element_text(color = "grey70"),
      axis.text.y      = element_text(color = "grey70", size = 8),
      legend.title     = element_text(color = "white", size = 9),
      legend.text      = element_text(color = "grey70", size = 8),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = bg, colour = NA),
      plot.background  = element_rect(fill = bg, colour = NA),
      plot.margin      = margin(12, 12, 12, 12)
    )
}

keygram_nodiggity <- make_keygram(keys_nodiggity, "No Diggity", "1990s R&B — Blackstreet")
keygram_pov       <- make_keygram(keys_pov,       "Pov",        "2020s R&B — Ariana Grande")

plot(keygram_nodiggity)
plot(keygram_pov)

ggsave("images/keygram_nodiggity.png",
       plot = keygram_nodiggity, width = 10, height = 7, dpi = 300, bg = bg)
ggsave("images/keygram_pov.png",
       plot = keygram_pov,       width = 10, height = 7, dpi = 300, bg = bg)