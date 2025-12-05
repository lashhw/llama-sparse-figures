library(tidyverse)
library(patchwork)

data <- read_csv("data/benchmark.csv") %>%
  mutate(
    method = fct_rev(fct_inorder(method))
  )

panel_info <- data %>%
  distinct(panel_id, panel_title, y_label) %>%
  arrange(panel_id)

plot_list <- map(seq_len(nrow(panel_info)), \(id) {
  panel_label <- panel_info$panel_title[[id]]
  y_label <- panel_info$y_label[[id]]

  data %>%
    filter(panel_id == id) %>%
    ggplot(aes(x = kv_ratio, y = score, colour = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_colour_manual(
      values = c("WG-KV" = "#6C8EBF", "DuoAttention" = "#D6B656", "Local Attention" = "#B85450")
    ) +
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
