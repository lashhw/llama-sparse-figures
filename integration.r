library(tidyverse)
library(patchwork)

method_priority <- c("Quest Only", "WG-KV + Quest")

data <- read_csv("data/integration.csv") %>%
  mutate(
    method = fct_relevel(method, method_priority),
    score = case_when(
      panel_title == "NarrativeQA" ~ score * (100 / 3),
      panel_title == "InfiniteBench Sum" | panel_title == "Multi-LexSum" ~ score * 100,
      TRUE ~ score
    )
  )

panel_info <- data %>%
  distinct(panel_title, y_label)

fig_list <- map(seq_len(nrow(panel_info)), \(idx) {
  panel_label <- panel_info$panel_title[[idx]]
  axis_label <- panel_info$y_label[[idx]]

  panel_data <- data %>%
    filter(panel_title == panel_label, y_label == axis_label)

  full_attention_score <- panel_data %>%
    filter(method == "Quest Only") %>%
    arrange(desc(kv_attended)) %>%
    pull(score) %>%
    first()

  plot <- ggplot(panel_data, aes(x = kv_attended, y = score, colour = method, shape = method)) +
    geom_hline(
      aes(yintercept = full_attention_score, colour = "Full Attention"),
      linetype = "dashed",
      linewidth = 0.9
    )

  for (method_name in method_priority) {
    method_data <- panel_data %>% filter(method == method_name)
    plot <- plot +
      geom_line(data = method_data, linewidth = 1) +
      geom_point(data = method_data, size = 1.7)
  }

  plot +
    scale_colour_manual(
      breaks = c("WG-KV + Quest", "Quest Only", "Full Attention"),
      values = c(
        "WG-KV + Quest" = "#D79B00",
        "Quest Only" = "#82B366",
        "Full Attention" = "#666666"
      )
    ) +
    scale_shape_manual(
      values = c(
        "WG-KV + Quest" = 17,
        "Quest Only" = 16
      )
    ) +
    scale_x_log10(
      limits = range(panel_data$kv_attended, na.rm = TRUE),
      breaks = c(320, 1024, 4096, 16384),
      expand = c(0, 0)
    ) +
    guides(
      colour = guide_legend(
        override.aes = list(linetype = "solid", linewidth = 1.5, size = 3, shape = c(17, 16, NA)),
        keywidth = 1.7
      ),
      shape = "none"
    ) +
    labs(
      x = "# KV Attended / Token",
      y = axis_label,
      title = panel_label
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      legend.title = element_blank(),
      legend.text = element_text(size = 15),
      axis.text.x = element_text(size = 10, colour = "black", angle = 17, hjust = 1),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
    )
})

fig <- wrap_plots(fig_list, ncol = 7, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("integration.pdf", fig, width = 14.0, height = 4.7, units = "in")
