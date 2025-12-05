library(tidyverse)

data <- read_csv("data/breakdown_1.csv") %>%
  mutate(
    stage = fct_inorder(stage),
    tokens = fct_rev(fct_inorder(tokens)),
    component = fct_inorder(component)
  )

fig <- ggplot(data, aes(x = latency, y = tokens, fill = component, colour = component)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  facet_grid(. ~ stage) +
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
  theme_minimal(base_size = 18) +
  theme(
    strip.text = element_text(size = 21, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    panel.grid.major.y = element_blank(),
  )

ggsave("breakdown_1.pdf", fig, width = 7.0, height = 4.0, units = "in")
