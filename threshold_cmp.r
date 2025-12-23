library(tidyverse)

data1 <- read_csv("data/threshold.csv") %>%
  mutate(method = "WG-KV")
data2 <- read_csv("data/threshold_local1.csv") %>%
  mutate(method = "WG-KV w/o Local Cache")

data <- bind_rows(data1, data2) %>%
  arrange(lambda, threshold) %>%
  mutate(lambda = factor(lambda, levels = sort(unique(lambda))))

fig <- ggplot(data, aes(x = kv_size, y = distill_loss, colour = lambda, shape = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.3) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_log10(
    breaks = scales::log_breaks(n = 6),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.02))
  ) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(override.aes = list(size = 2.5))
  ) +
  labs(
    x = "Normalized KV Cache Size (log scale)",
    y = "Distillation Loss",
    colour = expression(lambda),
    shape = NULL
  ) +
  coord_cartesian(ylim = c(0, 0.34), clip = "on") +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 13, colour = "black"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
  )

ggsave("threshold_cmp.pdf", fig, width = 5, height = 4.5, units = "in")
