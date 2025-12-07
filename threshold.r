library(tidyverse)

data <- read_csv("data/threshold.csv") %>%
  arrange(lambda, threshold) %>%
  mutate(lambda = factor(lambda, levels = sort(unique(lambda))))

fig <- ggplot(data, aes(x = kv_size, y = distill_loss, colour = lambda)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.3) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_log10(
    breaks = scales::log_breaks(n = 6),
    labels = scales::label_number(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.02))
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    x = "KV Cache Size (log scale)",
    y = "Distillation Loss",
    colour = expression(lambda)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 13, colour = "black"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
  )

ggsave("threshold.pdf", fig, width = 6.0, height = 4.0, units = "in")
