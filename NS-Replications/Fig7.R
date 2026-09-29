library(tidyverse)
library(readxl) 

feature_mapping <- c(
  "even…than" = "even.than", "so…that constr" = "so.that.constr",
  "tough mvt" = "tough.mvt", "inf VP subject" = "inf.VP.subject",
  "attachment ambig" = "attachment.ambig", "if...then constr" = "if...then.constr",
  "question_wh_subj" = "question_wh_subj", "question_wh_other" = "question_wh_other",
  "sent subj" = "sent.subj", "MV/RR ambig EASIER" = "MV.RR.ambig.EASIER",
  "MV/RR ambig HARD" = "MV.RR.ambig.HARD", "non-local verb-DO" = "non.local.verb.DO",
  "post-nominal adj" = "post.nominal.adj", "question_YN" = "question_YN",
  "topicalization" = "topicalization", "free relative" = "free.relative",
  "as…as constr" = "as.as.constr", "local VP conjunction" = "local.VP.conjunction",
  "no-relativizer ORC" = "no.relativizer.ORC", "ORC non-restr" = "ORC.non.restr",
  "it-cleft" = "it.cleft", "NP/S ambig" = "NP.S.ambig",
  "parenthetical" = "parenthetical", "adverbial RC" = "adverbial.RC",
  "non-local NP conjunction" = "non.local.NP.conjunction", "ORC restr" = "ORC.restr",
  "ORC non-canon" = "ORC.non.canon", "adj conjunction" = "adj.conjunction",
  "gerund modifier" = "gerund.modifier", "idiom" = "idiom",
  "SRC restr" = "SRC.restr", "local NP conjunction" = "local.NP.conjunction",
  "CP conjunctions" = "CP.conjunctions", "non-local SV" = "non.local.SV",
  "SRC non-restr" = "SRC.non.restr", "quote" = "quote",
  "non-local VP conjunction" = "non.local.VP.conjunction"
)

excel_file <- "/Users/chenyanze/NS-analysis/naturalstories/parsed-natural-stories.xlsx"
sheet_names <- excel_sheets(excel_file)

ns_data <- map_df(sheet_names[1:10], ~read_excel(excel_file, sheet = .x))
ns_total_sentences <- nrow(ns_data) 

dundee_data <- read_excel(excel_file, sheet = "Dundee Sentences")
dundee_total_sentences <- nrow(dundee_data) 

calc_rate <- function(df, total_n, mapping) {
  rates <- map_dbl(names(mapping), function(col_name) {
    if (col_name %in% names(df)) {
      sum(as.numeric(df[[col_name]]), na.rm = TRUE) / total_n
    } else { 0 }
  })
  tibble(Feature = unname(mapping), Rate = rates)
}

ns_rates <- calc_rate(ns_data, ns_total_sentences, feature_mapping) %>%
  mutate(Corpus = "Natural Stories")

dundee_rates <- calc_rate(dundee_data, dundee_total_sentences, feature_mapping) %>%
  mutate(Corpus = "Dundee")

plot_data <- bind_rows(ns_rates, dundee_rates)

feature_order <- ns_rates %>% arrange(Rate) %>% pull(Feature)

plot_data <- plot_data %>%
  mutate(
    Feature = factor(Feature, levels = feature_order),
    Corpus = factor(Corpus, levels = c("Dundee", "Natural Stories"))
  )

my_plot <- ggplot(plot_data, aes(x = Feature, y = Rate, fill = Corpus)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Dundee" = "#F8766D", "Natural Stories" = "#00BFC4")) +
  labs(x = "Feature", y = "Rate per sentence") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#EBEBEB"),
    legend.position = "right",
    legend.title = element_blank()
  )

ggsave("Figure7_Reproduced.pdf", plot = my_plot, width = 14, height = 7)
print("Pipeline complete! Plot saved as Figure7_Reproduced.pdf")