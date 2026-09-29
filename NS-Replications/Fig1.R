library(tidyverse)
library(readxl)
library(stringr)

excel_file <- "/Users/chenyanze/NS-analysis/naturalstories/parsed-natural-stories.xlsx"
sheet_names <- excel_sheets(excel_file)

ns_data <- map_df(sheet_names[1:10], ~read_excel(excel_file, sheet = .x)) %>%
  select(Sentence) %>%
  drop_na() %>%
  mutate(
    Corpus = "Natural Stories",
    Length = str_count(Sentence, "\\w+|[^\\w\\s]")
  ) %>%
  select(Corpus, Length)

dundee_data <- read_excel(excel_file, sheet = "Dundee Sentences") %>%
  select(Sentence) %>%
  drop_na() %>%
  mutate(
    Corpus = "Dundee (200 Sample)",
    Length = str_count(Sentence, "\\w+|[^\\w\\s]")
  ) %>%
  select(Corpus, Length)

plot_data <- bind_rows(dundee_data, ns_data) %>%
  mutate(Corpus = factor(Corpus, levels = c("Dundee (200 Sample)", "Natural Stories")))

summary_stats <- plot_data %>%
  group_by(Corpus) %>%
  summarize(
    mean_val = mean(Length),
    median_val = median(Length),
    .groups = "drop"
  ) %>%
  mutate(
    mean_label = sprintf("mean: %.2f", mean_val),
    median_label = sprintf("median: %.0f", median_val)
  )


my_plot <- ggplot(plot_data, aes(x = Length)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 2.5, 
                 fill = "#595959", color = "white", alpha = 0.9) +
  
  facet_grid(Corpus ~ .) +
  
  geom_vline(data = summary_stats, aes(xintercept = median_val), color = "blue", linewidth = 0.8) +
  geom_vline(data = summary_stats, aes(xintercept = mean_val), color = "red", linewidth = 0.8) +
  
  geom_text(data = summary_stats, aes(x = 80, y = 0.045, label = mean_label), 
            color = "red", size = 4.5, hjust = 1) +
  geom_text(data = summary_stats, aes(x = 80, y = 0.035, label = median_label), 
            color = "blue", size = 4.5, hjust = 1) +
  
  scale_x_continuous(limits = c(-5, 105), breaks = seq(0, 100, by = 25)) +
  scale_y_continuous(limits = c(0, 0.055), breaks = seq(0, 0.04, by = 0.01)) +
  
  labs(x = "Sentence length (tokens)", y = "density") +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5E5E5"),
    strip.background = element_rect(fill = "#EFEFEF", color = "black"),
    strip.text.y = element_text(angle = 270, size = 11, color = "black", margin = margin(l = 5, r = 5)),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave("Figure1_FinalCorrected.pdf", plot = my_plot, width = 7, height = 6)
print("Pipeline complete! Plot saved as Figure1_FinalCorrected.pdf")