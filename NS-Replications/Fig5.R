library(tidyverse)

rt_data <- read_tsv("/Users/chenyanze/NS-analysis/naturalstories/naturalstories_RTS/processed_RTs.tsv")

filtered_data <- rt_data %>%
  filter(correct >= 5) %>%
  filter(RT >= 100 & RT <= 3000)

summary_stats <- filtered_data %>%
  group_by(item) %>%
  summarize(
    mean_rt = mean(RT, na.rm = TRUE),
    median_rt = median(RT, na.rm = TRUE),
    n_participants = n_distinct(WorkerId), 
    .groups = "drop"
  ) %>%
  mutate(
    mean_label = sprintf("mean: %.2f ms", mean_rt),
    median_label = sprintf("median: %.0f ms", median_rt),
    n_label = sprintf("n = %d", n_participants)
  )

my_plot <- ggplot(filtered_data, aes(x = RT)) +
  geom_histogram(binwidth = 25, fill = "#555555", color = NA) +

  geom_vline(data = summary_stats, aes(xintercept = mean_rt), color = "red", linewidth = 0.8) +
  geom_vline(data = summary_stats, aes(xintercept = median_rt), color = "blue", linewidth = 0.8) +
  geom_text(data = summary_stats, aes(x = 950, y = Inf, label = mean_label), 
            color = "red", hjust = 1, vjust = 1.5, size = 3) +
  geom_text(data = summary_stats, aes(x = 950, y = Inf, label = median_label), 
            color = "blue", hjust = 1, vjust = 3, size = 3) +
  geom_text(data = summary_stats, aes(x = 950, y = Inf, label = n_label), 
            color = "black", hjust = 1, vjust = 4.5, size = 3) +

  facet_wrap(~ item, ncol = 5) +
  coord_cartesian(xlim = c(0, 1000)) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000)) +

  labs(x = "RT", y = NULL) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "#D3D3D3", color = "black"),
    strip.text = element_text(color = "black", size = 11),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("Figure5_RT_Histograms_Final.pdf", plot = my_plot, width = 12, height = 5)
print("Plot successfully saved as Figure5_RT_Histograms_Final.pdf!")