library(tidyverse)
library(lme4)
library(lmerTest)    
library(data.table)  
library(broom.mixed)
library(knitr)      

print("1. Loading RT Data...")
rt_data <- fread("/Users/chenyanze/NS-analysis/naturalstories/naturalstories_RTS/processed_RTs.tsv")

print("2. Loading Unigram data (Log frequency & Word length)...")
freq1_data <- fread("/Users/chenyanze/NS-analysis/naturalstories/freqs/freqs-1.tsv", header = FALSE, 
                    col.names = c("token_code", "n_order", "word", "freq_n", "freq_n_minus_1")) %>%
  filter(grepl("\\.word$", token_code)) %>%    
  separate(token_code, into = c("item", "zone", "type"), sep = "\\.") %>%
  mutate(
    item = as.numeric(item),
    zone = as.numeric(zone),
    word_length = nchar(word),
    log_freq = log(freq_n)                     
  ) %>%
  select(item, zone, word, word_length, log_freq)

print("3. Loading Trigram data (Log trigram probability)...")
freq3_data <- fread("/Users/chenyanze/NS-analysis/naturalstories/freqs/freqs-3.tsv", header = FALSE, 
                    col.names = c("token_code", "n_order", "word", "trigram_freq", "context_freq")) %>%
  filter(grepl("\\.word$", token_code)) %>%    
  separate(token_code, into = c("item", "zone", "type"), sep = "\\.") %>%
  mutate(
    item = as.numeric(item),
    zone = as.numeric(zone),
    trigram_prob = trigram_freq / context_freq,
    log_trigram_prob = log(trigram_prob)       
  ) %>%
  select(item, zone, word, log_trigram_prob)

print("4. Aligning and merging all predictors...")
final_model_data <- rt_data %>%
  inner_join(freq1_data, by = c("item", "zone", "word")) %>% 
  inner_join(freq3_data, by = c("item", "zone", "word")) %>% 
  filter(is.finite(log_freq) & is.finite(log_trigram_prob))                  

print("5. Fitting the full mixed-effects regression model...")

model <- lmer(RT ~ log_freq + log_trigram_prob + word_length + (1 | WorkerId) + (1 | item), 
              data = final_model_data)

print("6. Extracting and formatting the results...")
model_results <- tidy(model, effects = "fixed") %>%

  filter(term != "(Intercept)") %>%
  mutate(

    Predictor = case_when(
      term == "log_freq" ~ "Log frequency",
      term == "log_trigram_prob" ~ "Log trigram probability",
      term == "word_length" ~ "Word length",
      TRUE ~ term
    ),

    Estimate = round(estimate, 2),
    `Std. error` = round(std.error, 2),
    `t value` = round(statistic, 2)
  ) %>%

  select(Predictor, beta = Estimate, `Std. error`, `t value`)

print(kable(model_results, format = "markdown", align = c("l", "l", "l", "l")))



# |Predictor               |beta  |Std. error |t value |
# |:-----------------------|:-----|:----------|:-------|
# |Log frequency           |-0.12 |0.11       |-1.09   |
# |Log trigram probability |-0.13 |0.09       |-1.43   |
# |Word length             |2.13  |0.12       |17.39   |
