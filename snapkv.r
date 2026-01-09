library(tidyverse)
library(patchwork)

make_bar_line_plot <- function(data, title, metric_col, left_label, bar_legend_label, bar_fill, left_ticks) {
  scale_factor <- max(left_ticks)

  data <- data %>%
    mutate(
      accuracy_scaled = accuracy * scale_factor,
      accuracy_label = scales::label_percent(accuracy = 0.1)(accuracy)
    )

  ggplot(data, aes(x = lambda)) +
    geom_col(
      aes(y = .data[[metric_col]], colour = bar_legend_label),
      width = 0.65,
      fill = bar_fill,
      linewidth = 0
    ) +
    geom_line(
      aes(y = accuracy_scaled, colour = "Accuracy", group = 1),
      linewidth = 1
    ) +
    geom_point(
      aes(y = accuracy_scaled, colour = "Accuracy"),
      size = 2
    ) +
    geom_text(
      aes(y = accuracy_scaled, label = accuracy_label, colour = "Accuracy"),
      vjust = -1,
      size = 3.5,
      show.legend = FALSE
    ) +
    scale_colour_manual(
      values = c(setNames(bar_fill, bar_legend_label), Accuracy = "#a6761d"),
      breaks = c(bar_legend_label, "Accuracy"),
      name = NULL
    ) +
    scale_y_continuous(
      name = left_label,
      limits = c(0, scale_factor),
      breaks = left_ticks,
      labels = scales::label_comma(accuracy = 1),
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = "Accuracy",
        breaks = seq(0, 1, 0.2),
        labels = scales::label_number(accuracy = 0.1)
      )
    ) +
    labs(
      x = expression("Lambda"~(lambda)),
      title = title
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 13),
      axis.text.x = element_text(size = 12, colour = "black"),
      axis.title.y = element_text(size = 13, color = "black"),
      axis.text.y = element_text(color = "black"),
      legend.position = "top",
      legend.text = element_text(size = 13),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

plot_a <- read_csv("data/snapkv_no.csv") %>%
  mutate(lambda = fct_inorder(lambda)) %>%
  make_bar_line_plot(
    "(a) WG-KV Only",
    "avg_kv",
    "Avg. KV Cache Size (Tokens)",
    "KV Cache Size",
    "#cbd5e8",
    c(0, 5000, 10000, 15000, 20000, 25000)
  )

plot_b <- read_csv("data/snapkv_4096.csv") %>%
  mutate(lambda = fct_inorder(lambda)) %>%
  make_bar_line_plot(
    "(b) WG-KV + SnapKV",
    "num_evict",
    "Avg. Eviction Count",
    "Eviction Counts",
    "#b3e2cd",
    c(0, 16, 32, 48, 64, 80)
  )

fig <- plot_a + plot_b + plot_layout(ncol = 2)

ggsave("snapkv.pdf", fig, width = 9, height = 4, units = "in")
