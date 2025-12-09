library(tidyverse)
library(ggstar)

kv_mid <- 0.3
kv_low <- 0.1

points <- tibble(
  point = factor(
    c("Full Attention", "WG-KV Only", "Quest Only", "WG-KV + Quest"),
    levels = c("Full Attention", "WG-KV Only", "Quest Only", "WG-KV + Quest")
  ),
  kv_size = c(1.0, kv_mid, 1.0, kv_mid),
  kv_read = c(1.0, kv_mid, kv_low, kv_low)
)

fig <- ggplot(points, aes(x = kv_size, y = kv_read, fill = point, colour = point, starshape = point, size = point)) +
  geom_star(starstroke = 1) +
  scale_fill_manual(
    values = c("Full Attention" = "#F5F5F5", "WG-KV Only" = "#DAE8FC", "Quest Only" = "#D5E8D4", "WG-KV + Quest" = "#FFE6CC")
  ) +
  scale_colour_manual(
    values = c("Full Attention" = "#666666", "WG-KV Only" = "#6C8EBF", "Quest Only" = "#82B366", "WG-KV + Quest" = "#D79B00")
  ) +
  scale_starshape_manual(
    values = c("Full Attention" = 15, "WG-KV Only" = 13, "Quest Only" = 11, "WG-KV + Quest" = 5)
  ) +
  scale_size_manual(
    values = c("Full Attention" = 3.2, "WG-KV Only" = 3.2, "Quest Only" = 3.7, "WG-KV + Quest" = 3.5)
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0)
  ) +
  labs(
    x = "Normalized KV Size",
    y = "Normalized KV Read"
  ) +
  coord_cartesian(ratio = 1, clip = "off") +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    colour = guide_legend(nrow = 2, byrow = TRUE),
    starshape = guide_legend(nrow = 2, byrow = TRUE),
    size = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
  )

ggsave("pareto.pdf", fig, width = 3.5, height = 3.0, units = "in")
