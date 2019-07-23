library(tidyverse)
library(magrittr)
library(tidytext)
library(readxl)
library(ranger)
library(lubridate)

# create vector of times that will be updated over each step
timer <- c(startup = now())

# setup -------------------------------------------------------------------

# pull in needed data and code
coded  <- read_excel("ms-working-dir/data/20190122-train-test-labelled-dispos.xlsx")
rawcps <- read_excel("input-data-dir/20180920-cps-dispofindings-merged.xlsx")
models <- read_rds("ms-working-dir/data/fitted-model-objects.rds")
assets <- read_rds("ms-working-dir/data/prevalence-assets.rds")

# place the functions in the global env, so we can refer to them normally
sud_pattern_match <- assets$sud_pattern_match
preprocess_text   <- assets$preprocess_text
tidy_ngrams_s     <- assets$tidy_ngrams_s
get_tfidf         <- assets$get_tfidf

# prepare the labelled/unlabelled data
# keep summaries from 2015-2017 that are either unlabelled or in the test data
raw_text <- rawcps %>%
  transmute(
    docid   = as.character(srcinvestcase),
    year    = year(complaint_date),
    summary = disp_findings
  ) %>%
  distinct() %>%
  left_join(
    coded %>%
      select(docid = id, train_test),
    by = "docid"
  ) %>%
  mutate(train_test = ifelse(is.na(train_test), "unlabelled", train_test)) %>%
  filter(
    !is.na(summary),
    year %in% 2015:2017,
    train_test != "train"
  )

# define the cases that should be excluded

# 34 cases that have < 50 words in their summary
excluded <- raw_text %>%
  mutate(
    summ_no_punct = str_remove_all(summary, "[[:punct:]]"),
    word_count    = str_count(summ_no_punct, "\\b") / 2
  ) %>%
  filter(word_count < 50) %>%
  select(docid) %>%
  distinct()

# 140 cases that were flagged for review
excluded <- excluded %>%
  bind_rows(
    map_df(
      dir(
        "input-data-dir/coded-data/",
        pattern = "disp-file|reliability-samp-sb\\.xlsx",
        full.names = TRUE
      ),
      ~read_excel(., col_types = "text")
    ) %>%
      filter(review == 1) %>%
      select(docid = id) %>%
      distinct()
  )

timer <- c(timer, import_finished = now())

# preprocessing -----------------------------------------------------------

proc_text <- preprocess_text(raw_text)

timer <- c(timer, preprocess_finished = now())

# prepare dictionary features ---------------------------------------------

pm_features <- sud_pattern_match(proc_text, assets$sud_dict)

timer <- c(timer, dict_features_finished = now())

# prepare ngrams & tf-idf features ----------------------------------------

# generate ngram combinations
ngrams <- map_df(
  lst(unigrams = 1, bigrams = 2, trigrams = 3),
  ~tidy_ngrams_s(proc_text, stops = assets$stops, ngram = .),
  .id = "ngram"
)

timer <- c(timer, ngram_extraction_finished = now())

# calculate tf/idf for each document, limited to the vocab in our model
tfidf <- ngrams %>%
  semi_join(assets$vocab, by = "term") %>%
  count(ngram, docid, term) %>%
  group_by(ngram, docid) %>%
  mutate(n = n / sum(n)) %>%
  ungroup() %>%
  left_join(assets$idf_weights, by = c("term", "ngram")) %>%
  mutate(tf_idf = n * idf)

timer <- c(timer, ngram_weighting_finished = now())

# convert tall data-frame into a wide model matrix and munge the names
# drop a set of terms that weren't ultimately included in the models
modmat <- tfidf %>%
  select(docid, term, tf_idf) %>%
  spread(term, tf_idf, fill = 0) %>%
  janitor::clean_names() %>%
  select(
    -one_of(unlist(assets$ngram_drop)),
    -contains("due"),
    -contains("central"),
    -contains("registry"),
    -contains("preponderance"),
    -contains("cps"),
    -contains("dhs"),
    -contains("ms_"), -contains("_ms")
  )

names(modmat)[2:ncol(modmat)] <-
  tibble(
    col = names(modmat)[2:ncol(modmat)],
    n   = str_count(col, "_"),
    new = case_when(
      n == 2 ~ str_c("tri_", col),
      n == 1 ~ str_c("bi_", col),
      TRUE   ~ str_c("uni_", col)
    )
  ) %>%
  pull(new)

modvars <- tibble(modvars = models$rf_fit_tri$forest$independent.variable.names) %>%
  anti_join(
    tibble(modvars = names(modmat))
  ) %>%
  pull(modvars)

# if there's a variable in the models that we didn't see in our data, set to 0
for (name in modvars) modmat[, name] <- 0

# drop the excluded cases
modmat <- anti_join(modmat, excluded, by = "docid")
pm_features <- anti_join(pm_features, excluded, by = "docid")

timer <- c(timer, feature_frame_finished = now())

# predict -----------------------------------------------------------------

preds <- tibble(
  docid = modmat$docid,
  rfuni = predict(models$rf_fit_uni, modmat)$predictions,
  rfbi  = predict(models$rf_fit_bi, modmat)$predictions,
  rftri = predict(models$rf_fit_tri, modmat)$predictions,
  logit = predict(models$dict_logit, pm_features, type = "response")
)

preds$logit <- ifelse(preds$logit > .5, 1, 0)

preds$heur <- proc_text %>%
  select(docid) %>%
  anti_join(excluded, by = "docid") %>%
  left_join(pm_features, by = "docid") %>%
  gather(var, val, -docid) %>%
  mutate(var = str_remove_all(var, "[[:punct:][:digit:]]")) %>%
  group_by(docid, var) %>%
  mutate(count = sum(val)) %>%
  ungroup() %>%
  select(-val) %>%
  distinct() %>%
  spread(var, count) %>%
  mutate(heur_class = ifelse(pos > neg, 1, 0)) %>%
  pull(heur_class)

timer <- c(timer, prediction_finished = now())

preds <- raw_text %>%
  select(docid, year, train_test) %>%
  inner_join(preds, by = "docid")

# for table 2
# build data frame with estimate and CIs
prevalence <- bind_rows(
  ovr_unlab = preds %>%
    filter(train_test != "test") %>%
    select(rfuni:heur) %>%
    map_df(~broom::tidy(prop.test(rev(table(.)))), .id = "model") %>%
    select(model, estimate, conf.low, conf.high) %>%
    mutate(year = "overall, unlabeled"),

  ovr_test = preds %>%
    filter(train_test == "test") %>%
    select(rfuni:heur) %>%
    map_df(~broom::tidy(prop.test(rev(table(.)))), .id = "model") %>%
    select(model, estimate, conf.low, conf.high) %>%
    mutate(year = "overall, test"),

  preds %>%
    select(rfuni:heur) %>%
    map_df(~broom::tidy(prop.test(rev(table(.)))), .id = "model") %>%
    select(model, estimate, conf.low, conf.high) %>%
    mutate(year = "overall, unlabeled & test"),

  preds %>%
    select(year, rfuni:heur) %>%
    group_by(year = as.character(year)) %>%
    group_map(~ {
      map_df(.x, ~broom::tidy(prop.test(rev(table(.)))), .id = "model")
    }) %>%
    ungroup() %>%
    select(year, model, estimate, conf.low, conf.high),

  rawcps %>%
    transmute(id = as.character(srcinvestcase), year = year(complaint_date)) %>%
    inner_join(coded, by = "id") %>%
    select(docid = id, year, any_sub) %>%
    distinct() %>%
    group_by(year = as.character(year)) %>%
    do(broom::tidy(prop.test(rev(table(.$any_sub))))) %>%
    ungroup() %>%
    transmute(
      year,
      model = "humans",
      estimate,
      conf.low,
      conf.high
    ),

  coded %>%
    select(docid = id, any_sub) %>%
    group_by(year = "overall, labeled") %>%
    do(broom::tidy(prop.test(rev(table(.$any_sub))))) %>%
    ungroup() %>%
    transmute(
      year,
      model = "humans",
      estimate,
      conf.low,
      conf.high
    ),

  coded %>%
    filter(train_test == "test") %>%
    select(docid = id, any_sub) %>%
    group_by(year = "overall, test") %>%
    do(broom::tidy(prop.test(rev(table(.$any_sub))))) %>%
    ungroup() %>%
    transmute(
      year,
      model = "humans",
      estimate,
      conf.low,
      conf.high
    )
)

prevalence <- prevalence %>%
  mutate_at(vars(estimate:conf.high), list(~round(., 3))) %>%
  mutate(model = factor(model, levels = c("humans", "heur", "logit", "rfuni", "rfbi", "rftri"))) %>%
  arrange(year, model) %>%
  select(year, model, estimate, conf.low, conf.high)

timer <- c(timer, prevalence_finished = now())

# done --------------------------------------------------------------------

# store the timer information
write_csv(
  tibble(event = names(timer), time = timer),
  "ms-working-dir/data/predicted-prevalence-system-processing-time.csv"
)

# store info for table 2
write_csv(prevalence, "ms-working-dir/data/predicted-prevalence.csv")
