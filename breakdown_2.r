library(tidyverse)

data <- tribble(
  ~tokens, ~component,       ~memory,
  "100K",  "Model Weights",  15,
  "100K",  "KV Cache",       12,
  "200K",  "Model Weights",  15,
  "200K",  "KV Cache",       30,
  "300K",  "Model Weights",  15,
  "300K",  "KV Cache",       60,
  "400K",  "Model Weights",  15,
  "400K",  "KV Cache",       75,
)

data <- data %>%
  mutate(
    tokens = fct_rev(fct_inorder(tokens)),
    component = fct_relevel(component, "Model Weights", "KV Cache")
  )

fig <- ggplot(data, aes(x = memory, y = tokens, fill = component, colour = component)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = c("Model Weights" = "#DAE8FC", "KV Cache" = "#F8CECC"),
  ) +
  scale_colour_manual(
    values = c("Model Weights" = "#6C8EBF", "KV Cache" = "#B85450"),
  ) +
  labs(x = "Size (GB)", y = NULL, title = "(c) Memory Usage") +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 28, hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
  )

ggsave("breakdown_2.pdf", fig, width = 8.0, height = 4.0, units = "in")
