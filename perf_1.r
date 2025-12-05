library(tidyverse)

data <- read_csv("data/perf_1.csv") %>%
  mutate(
    tokens = fct_inorder(tokens),
    method = fct_inorder(method),
    component = fct_inorder(component)
  ) %>%
  group_by(tokens, method) %>%
  mutate(total_latency = sum(latency)) %>%
  ungroup()

reduction_labels <- data %>%
  distinct(tokens, method, total_latency) %>%
  group_by(tokens) %>%
  mutate(
    vanilla_latency = total_latency[method == "Vanilla"],
    reduction = (total_latency - vanilla_latency) / vanilla_latency,
    label = if_else(
      method == "WG-KV",
      scales::label_percent(accuracy = 1)(reduction),
      ""
    )
  ) %>%
  ungroup()

fig <- ggplot(data, aes(x = method, y = latency, fill = component, colour = component)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = reduction_labels,
    aes(x = method, y = total_latency, label = label),
    vjust = -0.5,
    size = 5,
    inherit.aes = FALSE
  ) +
  facet_grid(. ~ tokens, switch = "x") +
  scale_fill_manual(
    values = c("Non-Attention" = "#DAE8FC", "Attention" = "#F8CECC"),
  ) +
  scale_colour_manual(
    values = c("Non-Attention" = "#6C8EBF", "Attention" = "#B85450"),
  ) +
  labs(
    title = "(a) Prefill Latency",
    x = NULL,
    y = "Time to First Token (s)",
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 28, hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16, colour = "black"),
    strip.text.x = element_text(size = 20),
    panel.spacing = unit(0, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("perf_1.pdf", fig, width = 5.0, height = 4.0, units = "in")
