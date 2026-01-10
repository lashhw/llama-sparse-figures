library(tidyverse)

data <- read_csv("data/perf_2.csv") %>%
  mutate(
    tokens = fct_inorder(tokens),
    method = fct_relevel(method, "WG-KV", "Full"),
    component = fct_inorder(component)
  ) %>%
  group_by(tokens, method) %>%
  mutate(total_latency = sum(latency)) %>%
  ungroup()

reduction_labels <- data %>%
  distinct(tokens, method, total_latency) %>%
  group_by(tokens) %>%
  mutate(
    vanilla_latency = total_latency[method == "Full"],
    reduction = (total_latency - vanilla_latency) / vanilla_latency,
    label = if_else(
      method == "WG-KV",
      scales::label_percent(accuracy = 1)(reduction),
      ""
    )
  ) %>%
  ungroup()

fig <- ggplot(data, aes(x = latency, y = method, fill = component, colour = component)) +
  geom_col(width = 0.8, position = position_stack(reverse = TRUE), linewidth = 0.7) +
  geom_text(
    data = reduction_labels,
    aes(x = total_latency, y = method, label = label),
    hjust = -0.13,
    size = 5.5,
    inherit.aes = FALSE
  ) +
  facet_grid(tokens ~ ., switch = "y") +
  scale_y_discrete(expand = expansion(add = 0.67)) +
  scale_fill_manual(
    values = c("Non-Attention" = "#DAE8FC", "Attention" = "#F8CECC"),
  ) +
  scale_colour_manual(
    values = c("Non-Attention" = "#6C8EBF", "Attention" = "#B85450"),
  ) +
  labs(
    title = "(b) Decode Latency",
    x = "Time per Output Token (ms)",
    y = "Sequence Length",
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    plot.margin = margin(0, 50, 0, 0),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 19),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    strip.text.y = element_text(size = 20, colour = "black"),
    strip.placement = "outside",
    panel.spacing = unit(0, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
  )

ggsave("perf_2.pdf", fig, width = 5.5, height = 4.65, units = "in")
