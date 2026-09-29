library(tidyverse)

rt_data <- read_tsv("/Users/chenyanze/NS-analysis/naturalstories/naturalstories_RTS/processed_RTs.tsv")

clean_rt <- rt_data %>%
  #filter(correct >= 5) %>%
  #filter(RT >= 100 & RT <= 3000) %>%
  select(WorkerId, item, zone, RT)

word_stats <- clean_rt %>%
  group_by(item, zone) %>%
  summarize(
    total_rt = sum(RT, na.rm = TRUE),
    n_readers = n(),
    .groups = "drop"
  )

isc_data <- clean_rt %>%
  inner_join(word_stats, by = c("item", "zone")) %>%
  filter(n_readers > 1) %>%
  mutate(
    others_mean_rt = (total_rt - RT) / (n_readers - 1)
  ) %>%
  group_by(WorkerId, item) %>%
  filter(n() > 5) %>%  
  summarize(
    spearman_rho = cor(RT, others_mean_rt, method = "spearman", use = "pairwise.complete.obs"),
    .groups = "drop"
  ) %>%
  drop_na(spearman_rho)

plot_data <- isc_data %>%
  mutate(facet_name = factor(paste("Story", item), levels = paste("Story", 1:10)))

all_stories_data <- plot_data %>%
  mutate(facet_name = factor("All stories"))

final_plot_data <- bind_rows(plot_data, all_stories_data) %>%
  mutate(facet_name = fct_relevel(facet_name, "All stories", after = Inf))

summary_labels <- final_plot_data %>%
  group_by(facet_name) %>%
  summarize(
    mean_isc = mean(spearman_rho, na.rm = TRUE),
    max_count = max(table(cut(spearman_rho, breaks = seq(-1, 1, by = 0.05)))),
    .groups = "drop"
  ) %>%
  mutate(
    label_expr = sprintf("ISC[loo] == %.3f", mean_isc)
  )

my_plot <- ggplot(final_plot_data, aes(x = spearman_rho)) +
  
  geom_freqpoly(binwidth = 0.05, color = "black", linewidth = 0.6) +
  geom_text(data = summary_labels, 
            aes(x = -0.8, y = max_count * 0.7, label = label_expr), 
            parse = TRUE, hjust = 0, size = 3.5, color = "black") +
 
  facet_wrap(~ facet_name, ncol = 4, scales = "free") +
  
  scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  labs(x = expression(ISC[loo]), y = "# subjects") +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "black", size = 11, face = "bold"),
    panel.border = element_rect(color = "black", linewidth = 0.5)
  )

ggsave("Figure6_ISC_Reproduced2.pdf", plot = my_plot, width = 11, height = 8)
print("Pipeline execution complete! Plot saved as Figure6_ISC_Reproduced.pdf")