library(tidyverse)

data <- tribble(
  ~tokens, ~component,       ~memory,
  "100K",  "Model Weights",   15,
  "100K",  "KV Cache",        12,
  "200K",  "Model Weights",   15,
  "200K",  "KV Cache",        30,
  "300K",  "Model Weights",   15,
  "300K",  "KV Cache",        60,
  "400K",  "Model Weights",   15,
  "400K",  "KV Cache",        75
)

data <- data %>%
  mutate(
    tokens = fct_rev(fct_inorder(tokens)),
    component = fct_relevel(component, "Model Weights", "KV Cache")
  )

memory_fill <- c("Model Weights" = "#cdddf4", "KV Cache" = "#e8b3b3")
memory_outline <- c("Model Weights" = "#8aa1d2", "KV Cache" = "#b56f6f")

memory_plot <- ggplot(data, aes(x = memory, y = tokens, fill = component, colour = component)) +
  geom_col(width = 0.6, linewidth = 1, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = memory_fill,
    breaks = c("Model Weights", "KV Cache")
  ) +
  scale_colour_manual(
    values = memory_outline,
    breaks = c("Model Weights", "KV Cache")
  ) +
  scale_x_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 15),
    expand = c(0, 0.5)
  ) +
  labs(x = "Size (GB)", y = NULL, title = "Memory Usage") +
  theme_minimal(base_size = 18) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 28, hjust = 0.5),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.spacing.x = unit(0.6, "cm"),
    legend.margin = margin(b = 6),
    axis.title.x = element_text(size = 24, margin = margin(t = 12)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave("breakdown_2.pdf", memory_plot, width = 4.7, height = 5, units = "in")
