library(tidyverse)

label_map <- c(
  "Off" = "atop('SnapKV','Only')",
  "0.08" = "lambda*'='*0.08",
  "0.16" = "atop(lambda*'='*0.16,'WG-KV + SnapKV')",
  "0.32" = "lambda*'='*0.32",
  "0.64" = "lambda*'='*0.64",
  "1.28" = "atop('WG-KV','Only')"
)

baseline_accuracy <- read_csv("data/snapkv_no.csv") %>%
  filter(lambda == "Off") %>%
  pull(accuracy) %>%
  first()

left_breaks <- seq(0, 80, 20)
scale_factor <- max(left_breaks)

data <- read_csv("data/snapkv_4096.csv") %>%
  mutate(
    label = factor(lambda, levels = names(label_map), labels = unname(label_map)),
    accuracy_scaled = accuracy * scale_factor,
    accuracy_label = scales::percent(accuracy, accuracy = 0.1)
  )

plot <- ggplot(data, aes(x = label)) +
  geom_col(
    aes(y = num_evict, color = "# Eviction Triggers"),
    width = 0.65,
    fill = "#f8cecc"
  ) +
  geom_hline(
    aes(
      yintercept = baseline_accuracy * scale_factor,
      color = "AIME25 Accuracy Under Unbounded KV Cache"
    ),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = accuracy_scaled, color = "AIME25 Accuracy", group = 1),
    linewidth = 1
  ) +
  geom_point(
    aes(y = accuracy_scaled, color = "AIME25 Accuracy"),
    size = 2
  ) +
  geom_text(
    aes(y = accuracy_scaled, label = accuracy_label),
    colour = "#7b5715",
    vjust = 2.0,
    size = 3.8,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "# Eviction Triggers" = "#f8cecc",
      "AIME25 Accuracy" = "#a6761d",
      "AIME25 Accuracy Under Unbounded KV Cache" = "#6b6b6b"
    ),
    breaks = c(
      "# Eviction Triggers",
      "AIME25 Accuracy",
      "AIME25 Accuracy Under Unbounded KV Cache"
    ),
    name = NULL
  ) +
  guides(
    color = guide_legend(order = 2, nrow = 2, byrow = TRUE)
  ) +
  scale_x_discrete(labels = function(x) parse(text = x)) +
  scale_y_continuous(
    name = "# Eviction Triggers",
    limits = c(0, scale_factor),
    breaks = left_breaks,
    labels = scales::label_comma(),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Accuracy (Pass@1)",
      breaks = seq(0, 1, 0.25),
      labels = scales::label_number(accuracy = 0.01)
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.text.y = element_text(size = 12, colour = "black"),
    legend.position = "top",
    legend.text = element_text(size = 13),
    legend.key.width = grid::unit(0.9, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(0, 30, 0, 30)
  )

ggsave("snapkv_short.pdf", plot, width = 6.4, height = 3.2, units = "in")
