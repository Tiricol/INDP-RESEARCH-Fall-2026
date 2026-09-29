library(tidyverse)

raw_lines <- read_lines("/Users/chenyanze/NS-analysis/naturalstories/parses/stanford/all-parses-aligned.txt.stanford.depfeatures")

sentence_level_data <- tibble(raw_text = raw_lines) %>%
  mutate(
    is_blank = str_trim(raw_text) == "",
    sentence_id = cumsum(is_blank)
  ) %>%
  filter(!is_blank) %>%

  separate(raw_text, into = c("token_code", "dependency_length", "embedding_depth"), 
           sep = "\\s+", convert = TRUE) %>%

  separate(token_code, into = c("item", "zone", "type"), sep = "\\.", fill = "right", remove = FALSE) %>%
  mutate(
    zone = as.numeric(zone),
    dependency_length = ifelse(dependency_length == zone, 0, dependency_length)
  ) %>%
  group_by(sentence_id) %>%
  summarize(
    sentence_length = n(), 
    max_embedding_depth = max(embedding_depth, na.rm = TRUE),
    sum_dependency_length = sum(dependency_length, na.rm = TRUE),
    .groups = "drop"
  ) %>%

  filter(sentence_length > 0 & sentence_length <= 25) %>%
  mutate(corpus = "Natural Stories")

long_data <- sentence_level_data %>%
  pivot_longer(
    cols = c(max_embedding_depth, sum_dependency_length),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = recode(metric, 
                         "max_embedding_depth" = "max embedding depth",
                         "sum_dependency_length" = "sum dependency length"))

my_plot <- ggplot(long_data, aes(x = sentence_length, y = value, color = corpus, fill = corpus)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.3, linewidth = 1.2) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_color_manual(values = c("Natural Stories" = "#00BFC4")) +
  scale_fill_manual(values = c("Natural Stories" = "#00BFC4")) +
  labs(x = "Sentence length", y = NULL, color = "corpus", fill = "corpus") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "#E5E5E5", color = "black"),
    strip.text = element_text(color = "black", size = 11),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("corpus_structure_plot_FINAL.pdf", plot = my_plot, width = 10, height = 6)
print("Plot successfully saved as corpus_structure_plot_FINAL.pdf!")