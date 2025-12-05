library(tidyverse)
library(patchwork)

set.seed(42)

kv_ratios <- seq(0.0, 1.0, length.out = 10)
methods <- c("WG-KV", "DuoAttention", "Local Attention")
y_labels <- c(
  "Accuracy", "Accuracy", "Accuracy", "Accuracy", "F1 Score", "F1 Score", "Accuracy",
  "NDCG@10", "F1 Score", "F1 Score", "Accuracy", "Accuracy", "Accuracy", "Accuracy"
)
panel_titles <- list(
  "Natural Questions", "TriviaQA", "PopQA", "HotpotQA",
  "MS MACRO",
  "NarrativeQA", "InfiniteBench QA", "InfiniteBench MC",
  "InfiniteBench Sum", "Multi-LexSum",
  "TREC Fine", "NLU", "BANKING77", "CLINC150"
)

plot_data <- tibble(panel_id = seq_along(y_labels), y_label = y_labels) %>%
  crossing(kv_ratio = kv_ratios, method = methods) %>%
  mutate(
    method = fct_relevel(method, "WG-KV", "DuoAttention", "Local Attention"),
    score = case_when(
      method == "WG-KV" ~ 0.55 + 0.35 * kv_ratio + rnorm(n(), sd = 0.02),
      method == "DuoAttention" ~ 0.50 + 0.30 * kv_ratio + rnorm(n(), sd = 0.02),
      TRUE ~ 0.45 + 0.25 * kv_ratio + rnorm(n(), sd = 0.02)
    ),
    score = pmin(pmax(score, 0), 1)
  )

line_colours <- c(
  "WG-KV" = "#6C8EBF",
  "DuoAttention" = "#D6B656",
  "Local Attention" = "#B85450"
)

plot_list <- map(seq_along(y_labels), \(id) {
  panel_label <- panel_titles[[id]]
  y_label <- y_labels[[id]]

  plot_data %>%
    filter(panel_id == id) %>%
    ggplot(aes(x = kv_ratio, y = score, colour = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_colour_manual(values = line_colours) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    labs(
      x = "KV Budget",
      y = y_label,
      title = panel_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text(size = 10, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 14)
    )
})

fig <- wrap_plots(plot_list, ncol = 7, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("benchmark.pdf", fig, width = 14.0, height = 4.5, units = "in")
