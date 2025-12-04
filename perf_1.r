library(tidyverse)

data <- tribble(
  ~tokens,  ~method,    ~component,       ~latency,
  "100K",   "Vanilla", "Non-Attention",  2,
  "100K",   "Vanilla", "Attention",      10,
  "100K",   "Ours",     "Non-Attention",  2,
  "100K",   "Ours",     "Attention",      3,
  "200K",   "Vanilla", "Non-Attention",  4,
  "200K",   "Vanilla", "Attention",      40,
  "200K",   "Ours",     "Non-Attention",  4,
  "200K",   "Ours",     "Attention",      12,
  "300K",   "Vanilla", "Non-Attention",  6,
  "300K",   "Vanilla", "Attention",      90,
  "300K",   "Ours",     "Non-Attention",  6,
  "300K",   "Ours",     "Attention",      25,
  "400K",   "Vanilla", "Non-Attention",  8,
  "400K",   "Vanilla", "Attention",      160,
  "400K",   "Ours",     "Non-Attention",  8,
  "400K",   "Ours",     "Attention",      45,
)

data <- data %>%
  mutate(
    tokens = fct_inorder(tokens),
    method = fct_relevel(method, "Vanilla", "Ours"),
    component = fct_relevel(component, "Non-Attention", "Attention")
  ) %>%
  group_by(tokens, method) %>%
  mutate(total_latency = sum(latency)) %>%
  ungroup()

reduction_labels <- data %>%
  distinct(tokens, method, total_latency) %>%
  group_by(tokens) %>%
  mutate(
    banilla_latency = total_latency[method == "Vanilla"],
    reduction = (total_latency - banilla_latency) / banilla_latency,
    label = if_else(method == "Ours", scales::percent(reduction, accuracy = 1), "")
  ) %>%
  ungroup()

fig <- ggplot(data, aes(x = method, y = latency, fill = component, colour = component)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = reduction_labels,
    aes(x = method, y = total_latency, label = label),
    vjust = -0.5,
    size = 5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  facet_grid(. ~ tokens, switch = "x") +
  scale_fill_manual(
    values = c("Non-Attention" = "#DAE8FC", "Attention" = "#F8CECC"),
    breaks = c("Non-Attention", "Attention")
  ) +
  scale_colour_manual(
    values = c("Non-Attention" = "#6C8EBF", "Attention" = "#B85450"),
    breaks = c("Non-Attention", "Attention")
  ) +
  labs(
    title = "(a) Prefill Latency",
    y = "Time to First Token (s)",
    x = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 28, hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 20, colour = "black"),
    strip.text.x = element_text(size = 20, colour = "black"),
    panel.spacing = unit(0.3, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave("perf_1.pdf", fig, width = 8.7, height = 5.3, units = "in")
