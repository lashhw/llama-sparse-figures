library(tidyverse)

data <- tribble(
  ~stage,            ~tokens, ~component,   ~latency,
  "(a) Prefill Latency", "100K",  "MLP",         0.40,
  "(a) Prefill Latency", "100K",  "Other",       0.18,
  "(a) Prefill Latency", "100K",  "Attention",   0.42,
  "(a) Prefill Latency", "200K",  "MLP",         0.32,
  "(a) Prefill Latency", "200K",  "Other",       0.18,
  "(a) Prefill Latency", "200K",  "Attention",   0.50,
  "(a) Prefill Latency", "300K",  "MLP",         0.26,
  "(a) Prefill Latency", "300K",  "Other",       0.18,
  "(a) Prefill Latency", "300K",  "Attention",   0.56,
  "(a) Prefill Latency", "400K",  "MLP",         0.22,
  "(a) Prefill Latency", "400K",  "Other",       0.12,
  "(a) Prefill Latency", "400K",  "Attention",   0.66,
  "(b) Decode Latency",  "100K",  "MLP",         0.36,
  "(b) Decode Latency",  "100K",  "Other",       0.22,
  "(b) Decode Latency",  "100K",  "Attention",   0.42,
  "(b) Decode Latency",  "200K",  "MLP",         0.30,
  "(b) Decode Latency",  "200K",  "Other",       0.22,
  "(b) Decode Latency",  "200K",  "Attention",   0.48,
  "(b) Decode Latency",  "300K",  "MLP",         0.24,
  "(b) Decode Latency",  "300K",  "Other",       0.24,
  "(b) Decode Latency",  "300K",  "Attention",   0.52,
  "(b) Decode Latency",  "400K",  "MLP",         0.22,
  "(b) Decode Latency",  "400K",  "Other",       0.18,
  "(b) Decode Latency",  "400K",  "Attention",   0.60,
)

data <- data %>%
  mutate(
    stage = fct_relevel(stage, "(a) Prefill Latency", "(b) Decode Latency"),
    tokens = fct_rev(fct_inorder(tokens)),
    component = fct_relevel(component, "MLP", "Other", "Attention")
  )

fig <- ggplot(data, aes(x = latency, y = tokens, fill = component, colour = component)) +
  geom_col(width = 0.7, linewidth = 1, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = c(MLP = "#DAE8FC", Other = "#FFF2CC", Attention = "#F8CECC"),
  ) +
  scale_colour_manual(
    values = c(MLP = "#6C8EBF", Other = "#D6B656", Attention = "#B85450"),
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "0.25", "0.5", "0.75", "1"),
  ) +
  labs(x = "Normalized Time", y = NULL) +
  facet_grid(. ~ stage) +
  theme_minimal(base_size = 18) +
  theme(
    strip.text = element_text(size = 28),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
  )

ggsave("breakdown_1.pdf", fig, width = 8.7, height = 4.3, units = "in")
