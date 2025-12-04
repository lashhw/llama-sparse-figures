library(tidyverse)

data <- tribble(
  ~stage,            ~tokens, ~component,   ~latency,
  "Prefill Latency", "100K",  "MLP",         0.40,
  "Prefill Latency", "100K",  "Other",       0.18,
  "Prefill Latency", "100K",  "Attention",   0.42,
  "Prefill Latency", "200K",  "MLP",         0.32,
  "Prefill Latency", "200K",  "Other",       0.18,
  "Prefill Latency", "200K",  "Attention",   0.50,
  "Prefill Latency", "300K",  "MLP",         0.26,
  "Prefill Latency", "300K",  "Other",       0.18,
  "Prefill Latency", "300K",  "Attention",   0.56,
  "Prefill Latency", "400K",  "MLP",         0.22,
  "Prefill Latency", "400K",  "Other",       0.12,
  "Prefill Latency", "400K",  "Attention",   0.66,
  "Decode Latency",  "100K",  "MLP",         0.36,
  "Decode Latency",  "100K",  "Other",       0.22,
  "Decode Latency",  "100K",  "Attention",   0.42,
  "Decode Latency",  "200K",  "MLP",         0.30,
  "Decode Latency",  "200K",  "Other",       0.22,
  "Decode Latency",  "200K",  "Attention",   0.48,
  "Decode Latency",  "300K",  "MLP",         0.24,
  "Decode Latency",  "300K",  "Other",       0.24,
  "Decode Latency",  "300K",  "Attention",   0.52,
  "Decode Latency",  "400K",  "MLP",         0.22,
  "Decode Latency",  "400K",  "Other",       0.18,
  "Decode Latency",  "400K",  "Attention",   0.60
)

data <- data %>%
  mutate(
    stage = fct_relevel(stage, "Prefill Latency", "Decode Latency"),
    tokens = fct_rev(fct_inorder(tokens)),
    component = fct_relevel(component, "MLP", "Other", "Attention")
  )

component_fill <- c(MLP = "#cdddf4", Other = "#f3e6ad", Attention = "#e8b3b3")
component_outline <- c(MLP = "#8aa1d2", Other = "#c9ab46", Attention = "#b56f6f")

latency_plot <- ggplot(data, aes(x = latency, y = tokens, fill = component, colour = component)) +
  geom_col(width = 0.6, linewidth = 1, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = component_fill,
    breaks = c("MLP", "Other", "Attention")
  ) +
  scale_colour_manual(
    values = component_outline,
    breaks = c("MLP", "Other", "Attention")
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 1),
    labels = c("0", "0.25", "0.5", "1"),
    expand = c(0, 0.01)
  ) +
  labs(x = "Normalized Time", y = NULL) +
  facet_grid(. ~ stage) +
  theme_minimal(base_size = 18) +
  theme(
    strip.text = element_text(size = 28, face = "plain"),
    strip.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.spacing.x = unit(0.6, "cm"),
    axis.title.x = element_text(size = 24, margin = margin(t = 12)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 20),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave("breakdown_1.pdf", latency_plot, width = 8.7, height = 5, units = "in")
