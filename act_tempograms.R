library(tidyverse)

# -----------------------------
# 1. READ TEMPOGRAM CSV FILES
# -----------------------------

yeah_tempogram_raw <- read_csv("yeah_act.csv", show_col_types = FALSE)
gooddays_tempogram_raw <- read_csv("gooddays_act.csv", show_col_types = FALSE)

# -----------------------------
# 2. CONVERT TO TEMPORAL MATRIX
# -----------------------------
# Because Sonic exported generic column names, we:
# - add a TIME column ourselves
# - assign BPM labels across the columns

make_tempogram_long <- function(df, bpm_min = 60, bpm_max = 180) {
  
  df <- df %>%
    mutate(TIME = row_number())
  
  n_tempo_cols <- ncol(df) - 1
  
  colnames(df)[1:n_tempo_cols] <- seq(
    bpm_min,
    bpm_max,
    length.out = n_tempo_cols
  )
  
  df_long <- df %>%
    pivot_longer(
      cols = -TIME,
      names_to = "tempo",
      values_to = "value"
    ) %>%
    mutate(
      tempo = as.numeric(tempo),
      TIME = as.numeric(TIME),
      value = as.numeric(value)
    ) %>%
    drop_na(TIME, tempo, value)
  
  return(df_long)
}

yeah_tempogram_long <- make_tempogram_long(yeah_tempogram_raw)
gooddays_tempogram_long <- make_tempogram_long(gooddays_tempogram_raw)

# -----------------------------
# 3. PLOT YEAH! TEMPOGRAM
# -----------------------------

yeah_tempogram <- ggplot(yeah_tempogram_long, aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", guide = "none") +
  labs(
    title = "Yeah! — Autocorrelation Tempogram",
    subtitle = "First 30 seconds",
    x = "Time (s)",
    y = "Tempo (BPM)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.grid = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "#E0E1DD"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "#F4A261", face = "bold"),
    plot.subtitle = element_text(color = "white")
  )

# -----------------------------
# 4. PLOT GOOD DAYS TEMPOGRAM
# -----------------------------

gooddays_tempogram <- ggplot(gooddays_tempogram_long, aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", guide = "none") +
  labs(
    title = "Good Days — Autocorrelation Tempogram",
    subtitle = "First 30 seconds",
    x = "Time (s)",
    y = "Tempo (BPM)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.grid = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "#E0E1DD"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "#F4A261", face = "bold"),
    plot.subtitle = element_text(color = "white")
  )

# -----------------------------
# 5. SHOW PLOTS
# -----------------------------

# print(yeah_tempogram)
print(gooddays_tempogram)