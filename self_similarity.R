library(tidyverse)

bg <- "#0E0A1A"

mfcc_cols <- paste0("mfcc_", sprintf("%02d", 1:20))

read_timbre_matrix <- function(path) {
  read_csv(path, col_names = mfcc_cols, show_col_types = FALSE) %>%
    as.matrix()
}

janet     <- read_timbre_matrix("janetjackson_timbre.csv")
good_days <- read_timbre_matrix("gooddays_timbre.csv")

downsample <- function(mat, n = 200) {
  idx <- round(seq(1, nrow(mat), length.out = min(n, nrow(mat))))
  mat[idx, ]
}

janet     <- downsample(janet)
good_days <- downsample(good_days)

compute_ssm <- function(mat) {
  n   <- nrow(mat)
  ssm <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      ssm[i, j] <- sum((mat[i, ] - mat[j, ])^2)
    }
  }
  ssm <- 1 - (ssm / max(ssm))
  ssm
}

ssm_janet     <- compute_ssm(janet)
ssm_good_days <- compute_ssm(good_days)

ssm_to_df <- function(ssm) {
  n <- nrow(ssm)
  expand.grid(x = 1:n, y = 1:n) %>%
    mutate(value = as.vector(ssm))
}

make_ssm_plot <- function(ssm, track_title, era_label) {
  ssm_to_df(ssm) %>%
    ggplot(aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(option = "rocket", name = "Similarity") +
    coord_fixed() +
    labs(
      title    = paste0("Self-Similarity Matrix: ", track_title),
      subtitle = paste0("Timbre (MFCC) | ", era_label),
      x        = "Time",
      y        = "Time"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, color = "white"),
      plot.subtitle    = element_text(size = 10, color = "grey60"),
      axis.title       = element_text(face = "bold", size = 10, color = "white"),
      axis.text        = element_text(color = "grey70"),
      legend.title     = element_text(color = "white", size = 9),
      legend.text      = element_text(color = "grey70", size = 8),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = bg, colour = NA),
      plot.background  = element_rect(fill = bg, colour = NA),
      plot.margin      = margin(12, 12, 12, 12)
    )
}

ssm_janet_plot    <- make_ssm_plot(ssm_janet,     "That's The Way Love Goes", "1990s R&B — Janet Jackson")
ssm_gooddays_plot <- make_ssm_plot(ssm_good_days, "Good Days",                "2020s R&B — SZA")

plot(ssm_janet_plot)
plot(ssm_gooddays_plot)

ggsave("images/ssm_janetjackson.png",
       plot = ssm_janet_plot,    width = 6, height = 6, dpi = 300, bg = bg)
ggsave("images/ssm_gooddays.png",
       plot = ssm_gooddays_plot, width = 6, height = 6, dpi = 300, bg = bg)