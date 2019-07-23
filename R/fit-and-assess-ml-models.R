library(tidyverse)
library(magrittr)
library(tidytext)
library(readxl)

# setup -------------------------------------------------------------------

# first, repair instances where there isn't a space between the end of a period and a new sentence
# second, convert all contractions of 'not' into 'not'
# third, normalize a set of substance-use related verbs and titles
# next, remove roman numerals, numerics, and excess/leading/trailing whitespace
preprocess_text <- function(corpus) {

  corpus <- corpus %>%
    mutate(
      # make sure there is spacing after periods
      summary = str_replace_all(summary, "\\.(?=[^ \\W\\d])", ". "),

      # remove forward slashes from pairs of words (e.g. "yes/no" -> "yes no")
      # respects dates, but dates will be cleaned out later
      summary = str_replace_all(summary, "(?<!\\d)(\\/)(?!\\d)", " "),

      # undo contractions of "not" (e.g., "wouldn't" -> "would not")
      summary = str_replace_all(summary, "\\b([[:alpha:]]+)n't\\b", "\\1 not"),

      # remove misread unicode
      summary = str_remove_all(summary, "\"|\\r|\\n|\'"),

      # normalize substance abuse-related terms
      summary = summary %>%
        str_replace_all(regex("\\bsmok[ei]{1}[dgns]{0,2}\\b", ignore_case = T), "smoke") %>%
        str_replace_all(regex("\\bdr[ai]{1}nk[s]{0,1}\\b|\\bdrinking\\b", ignore_case = T), "drink") %>%
        str_replace_all(regex("\\bcannab[[:alpha:]]+\\b|\\bmarihuana\\b", ignore_case = T), "marijuana") %>%
        str_replace_all(regex("\\bcocaine\\b|\\bcrack\\b", ignore_case = T), "cocaine") %>%
        str_replace_all(regex("\\bsnort[[:alpha:]]{0,3}\\b", ignore_case = T), "snort") %>%
        str_replace_all(regex("\\bmeth\\b|\\b[[:alpha:]]*amphetamine[s]*\\b", ignore_case = T), "amphetamine") %>%
        str_replace_all(regex("\\bneedle[s]*\\b", ignore_case = T), "needle") %>%
        str_replace_all(regex("\\bopioid[s]*\\b|\\bopiate[s]*\\b", ignore_case = T), "opioid") %>%
        str_replace_all(regex("\\buse[ds]*\\b|\\busing\\b", ignore_case = T), "use") %>%
        str_replace_all(regex("\\babuse[ds]*\\b|\\babusing\\b", ignore_case = T), "abuse") %>%
        str_replace_all(regex("\\binject[[:alpha:]]{0,4}\\b", ignore_case = T), "inject"),

      # normalize titles
      summary = summary %>%
        str_replace_all(regex("\\bmr\\. ", ignore_case = T), "mr ") %>%
        str_replace_all(regex("\\bmrs\\. |\\bms\\. ", ignore_case = T), "ms ") %>%
        str_replace_all(regex("\\bdr\\. ", ignore_case = T), "dr ") %>%
        str_replace_all(regex("\\bsr\\. ", ignore_case = T), "sr ") %>%
        str_replace_all(regex("\\bjr\\. ", ignore_case = T), "jr ")
    )

  corpus <- corpus %>%
    unnest_tokens(sent, summary, to_lower = TRUE, strip_punct = TRUE, token = "sentences") %>%
    group_by(docid) %>%
    mutate(sentence_id = 1:n()) %>%
    ungroup() %>%
    unite(docid, c("docid", "sentence_id"), sep = "-")

  # handle roman numerals, single letters?, numerics, excess, trailing, and leading whitespace
  corpus <- corpus %>%
    mutate(
      # remove roman numerals
      sent = str_remove_all(sent, "\\bi{1,3}\\b"),

      # !!! problematic for the dictionary, can update dict patterns (e.g. "has a sponsor" -> "has sponsor")
      # remove single letters
      sent = str_remove_all(sent, "\\b[[:alpha:]]{1}\\b"),

      # remove numerics
      sent = str_remove_all(sent, "[0-9]"),

      # remove excess, trailing, and leading whitespace
      sent = str_squish(sent)
    )

  # collapse back into single paragraph, but add double spaces between sentence break
  # this enables the document to be split out again for the bag-of-words approach
  corpus <- corpus %>%
    separate(docid, c("docid", "sent_id"), sep = "-") %>%
    group_by(docid) %>%
    summarise(summary = str_c(sent, collapse = "  ")) %>%
    ungroup()

  corpus
}

# train & test
coded <- read_excel("ms-working-dir/data/20190122-train-test-labelled-dispos.xlsx")

# handle the target variable, ensure the document ID doesn't look like a unigram
coded <- coded %>%
  mutate(any_sub = factor(any_sub)) %>%
  rename(docid = id)

text <- select(coded, docid, summary)

text <- preprocess_text(text)

# list of stop words ------------------------------------------------------

stops <- map(
  excel_sheets("data/full-stops-list.xlsx"),
  ~read_excel("data/full-stops-list.xlsx", sheet = .)
)

ngram_drop <- stops[[1]] %>%
  mutate(cps_custom = str_replace_all(cps_custom, " ", "_"))

stops <- stops %>%
  map(distinct) %>%
  magrittr::extract(2:4) %>%
  unlist()

# list of expert terms ----------------------------------------------------

sud_dict <-
  map_df(
    excel_sheets("data/sud-dictionary.xlsx"),
    ~read_excel("data/sud-dictionary.xlsx", sheet = .)
  ) %>%
  mutate(
    Pattern = Pattern %>%
      str_to_lower() %>%
      str_replace_all("(.+)", "\\\\b\\1\\\\b")
  )

unique_dict_phrases <- sud_dict$Pattern %>%
  map(
    ~str_extract_all(
      text$summary %>%
        str_remove_all("emotional abuse|physical abuse|sexual abuse|child abuse|domestic abuse|spousal abuse"),
      regex(., ignore_case = TRUE)
    ) %>%
      unlist() %>%
      unique()
  ) %>%
  unlist() %>%
  str_to_lower() %>%
  unique()

# pattern match function, accepts a data frame of summaries and counts patterns from a dictionary
# a full matrix/data frame of documents*patterns is created and filled with str_count()
# patterns are searched successively, within 3 tiers
# after each pattern has been searched within a tier, the patterns are removed before iterating
sud_pattern_match <- function(corpus, dict) {

  # these strings will trip a few of the patterns we want to check first
  corpus$summary <- str_remove_all(
    corpus$summary,
    "emotional abuse|physical abuse|sexual abuse|child abuse|domestic abuse|spousal abuse"
  )

  # 3 tiers of patterns, each will be searched successively
  dict_split <- dict %>%
    split(.$Tier) %>%
    map(rownames_to_column, "pattern_id")

  # empty frame, for results
  pm <- tibble()

  # count & store each match within a tier, then remove them before moving to next iteration
  for (i in 3:1) {
    pm_i <- map_df(
      dict_split[[i]]$Pattern,
      ~tibble(
        Tier  = i,
        docid = corpus$docid,
        match = str_count(corpus$summary, regex(., ignore_case = TRUE))
      ),
      .id = "pattern_id"
    )

    pm <- bind_rows(pm, pm_i)

    for (pattern in dict_split[[i]]$Pattern) corpus$summary <- str_remove_all(corpus$summary, pattern)
  }

  # collapse the dictionary back down
  dict <- bind_rows(dict_split)

  # join the dictionary in order to relabel each feature, then cast to a wide frame
  pm <- dict %>%
    right_join(pm, by = c("pattern_id", "Tier")) %>%
    mutate(
      pat2 = case_when(
        SRP == "Positive" ~ str_c("pos", Tier, pattern_id, sep = "_"),
        SRP == "Negative" ~ str_c("neg", Tier, pattern_id, sep = "_"),
        SRP == "Neutral"  ~ str_c("neu", Tier, pattern_id, sep = "_"),
        TRUE              ~ NA_character_
      )
    ) %>%
    select(docid, pat2, match) %>%
    spread(pat2, match)

  # patterns that are never found in the text are retained as columns of 0s
  pm
}

pm <- sud_pattern_match(text, sud_dict)

# tokenization ------------------------------------------------------------

# tokenizer function, built to extract ngrams of different N
# documents are split into sentences to prevent cross-sentence/paragraph ngrams from being created
# next, the sentences are split into unigrams; unigrams that belong to a list of provided stops are discarded
# finally, the unigrams are reformed into single sentences, and split into the specified ngram
tidy_ngrams_s <- function(corpus, stops, ngram = 2) {

  # *this function will only run correctly if _corpus_ has been created using preprocess_text()*
  # split the document into sentences, to prevent cross-sentence or cross-paragraph
  # ngrams from being formed
  ngrams <- corpus %>%
    unnest_tokens(sent, summary, token = "regex", pattern = "  ") %>%
    group_by(docid) %>%
    mutate(sent_id = 1:n()) %>%
    ungroup()

  # tokenize as unigrams first, to drop stop words
  ngrams <- ngrams %>%
    unnest_tokens(word, sent) %>%
    filter(!word %in% stops)

  # return to sentences
  # then tokenize into ngrams specified in function argument
  ngrams <- ngrams %>%
    group_by(docid, sent_id) %>%
    summarise(sent = str_c(word, collapse = " ")) %>%
    unnest_tokens(term, sent, token = "ngrams", n = ngram) %>%
    ungroup() %>%
    select(-sent_id)

  ngrams
}

# compute TF/IDF weights for each term
get_tfidf <- function(tokens) {

  tokens %>%
    count(docid, term) %>%
    bind_tf_idf(term, docid, n)
}

# extract unigrams/bigrams
# this ensures that both the training/test data is extracted in the same way
unigrams <- tidy_ngrams_s(text, stops, 1)
bigrams  <- tidy_ngrams_s(text, stops, 2)
trigrams <- tidy_ngrams_s(text, stops, 3)

# feature reduction -------------------------------------------------------

# !!! term must be seen in a minimum of 5% of documents in the training data
# !!! or the term must be in our 'expert dictionary'
cutoff <- round(n_distinct(coded$docid[coded$train_test == "train"]) * .05)

# remove stops from the unique pattern matched phrases
# this ensures that some patterns can be found in the extracted ngrams
special_phrases <- unique_dict_phrases %>%
  str_trim() %>%
  str_remove_all(
    str_c("\\b", stops, "\\b") %>%
      str_c(collapse = "|")
  ) %>%
  str_squish()

kept_uni <- unigrams %>%
  select(term, docid) %>%
  distinct() %>%
  count(term) %>%
  filter(n >= cutoff & nchar(term) >= 4 | term %in% special_phrases)

kept_bi <- bigrams %>%
  select(term, docid) %>%
  distinct() %>%
  count(term) %>%
  filter(n >= cutoff & nchar(term) >= 4 | term %in% special_phrases)

kept_tri <- trigrams %>%
  select(term, docid) %>%
  distinct() %>%
  count(term) %>%
  filter(n >= cutoff & nchar(term) >= 4 | term %in% special_phrases)

# store vocabulary
vocab <-
  bind_rows(
    unigrams = kept_uni,
    bigrams  = kept_bi,
    trigrams = kept_tri,
    .id = "ngram"
  ) %>%
  rename(document_frequency = n) %>%
  mutate(below_cutoff = document_frequency < cutoff)

# weight ngrams -----------------------------------------------------------

# extract uni/bigrams by dataset & weight
uni_train_wt <- coded %>%
  filter(train_test == "train") %>%
  inner_join(unigrams, by = "docid") %>%
  semi_join(kept_uni, by = "term") %>%
  get_tfidf()

bi_train_wt  <- coded %>%
  filter(train_test == "train") %>%
  inner_join(bigrams, by = "docid") %>%
  semi_join(kept_bi, by = "term") %>%
  get_tfidf()

tri_train_wt <- coded %>%
  filter(train_test == "train") %>%
  inner_join(trigrams, by = "docid") %>%
  semi_join(kept_tri, by = "term") %>%
  get_tfidf()

uni_test_wt <- coded %>%
  filter(train_test == "test") %>%
  inner_join(unigrams, by = "docid") %>%
  semi_join(kept_uni, by = "term") %>%
  get_tfidf()

bi_test_wt  <- coded %>%
  filter(train_test == "test") %>%
  inner_join(bigrams, by = "docid") %>%
  semi_join(kept_bi, by = "term") %>%
  get_tfidf()

tri_test_wt <- coded %>%
  filter(train_test == "test") %>%
  inner_join(trigrams, by = "docid") %>%
  semi_join(kept_tri, by = "term") %>%
  get_tfidf()

# set up tf/idf matrices
uni_train <- uni_train_wt %>%
  select(docid, term, tf_idf) %>%
  spread(term, tf_idf, fill = 0)

bi_train <- bi_train_wt %>%
  select(docid, term, tf_idf) %>%
  spread(term, tf_idf, fill = 0)

tri_train <- tri_train_wt %>%
  select(docid, term, tf_idf) %>%
  spread(term, tf_idf, fill = 0)

# !!! test sets will be weighted by what we find in the training
uni_test <- uni_test_wt %>%
  select(docid, term, tf) %>%
  full_join(
    uni_train_wt %>%
      select(term, idf) %>%
      distinct(),
    by = "term"
  ) %>%
  transmute(docid, term, tf_idf = tf * idf) %>%
  spread(term, tf_idf, fill = 0)

bi_test <- bi_test_wt %>%
  select(docid, term, tf) %>%
  full_join(
    bi_train_wt %>%
      select(term, idf) %>%
      distinct(),
    by = "term"
  ) %>%
  transmute(docid, term, tf_idf = tf * idf) %>%
  spread(term, tf_idf, fill = 0)

tri_test <- tri_test_wt %>%
  select(docid, term, tf) %>%
  full_join(
    tri_train_wt %>%
      select(term, idf) %>%
      distinct(),
    by = "term"
  ) %>%
  transmute(docid, term, tf_idf = tf * idf) %>%
  spread(term, tf_idf, fill = 0)

idf_weights <- map_df(
  lst(unigrams = uni_train_wt, bigrams = bi_train_wt, trigrams = tri_train_wt),
  ~distinct(select(., term, idf)),
  .id = "ngram"
)

# prepare modeling frames -------------------------------------------------

# merge all the features
tf_train <- coded %>%
  filter(train_test == "train") %>%
  select(docid, any_sub) %>%
  left_join(uni_train, by = "docid") %>%
  left_join(bi_train, by = "docid") %>%
  left_join(tri_train, by = "docid") %>%
  janitor::clean_names()

tf_test <- coded %>%
  filter(train_test == "test") %>%
  select(docid, any_sub) %>%
  left_join(uni_test, by = "docid") %>%
  left_join(bi_test, by = "docid") %>%
  left_join(tri_test, by = "docid") %>%
  janitor::clean_names()

# drop the feature if it's part of our list of n-grams to be discarded
tf_train <- select(tf_train, -one_of(unlist(ngram_drop)))
tf_test  <- select(tf_test, -one_of(unlist(ngram_drop)))

# second sweep to get rid of permutations from the cps_custom list
# removes 53 columns that should be dropped
tf_train <- select(
  tf_train,
  -contains("due"),
  -contains("central"),
  -contains("registry"),
  -contains("preponderance"),
  -contains("cps"),
  -contains("dhs"),
  -contains("ms_"), -contains("_ms")
)

tf_test <- select(
  tf_test,
  -contains("due"),
  -contains("central"),
  -contains("registry"),
  -contains("preponderance"),
  -contains("cps"),
  -contains("dhs"),
  -contains("ms_"), -contains("_ms")
)

# label the names with what type of ngram each feature is
names(tf_train)[3:ncol(tf_train)] <-
  tibble(
    col = names(tf_train)[3:ncol(tf_train)],
    n   = str_count(col, "_"),
    new = case_when(
      n == 2 ~ str_c("tri_", col),
      n == 1 ~ str_c("bi_", col),
      TRUE   ~ str_c("uni_", col)
    )
  ) %>%
  pull(new)

names(tf_test)[3:ncol(tf_test)] <-
  tibble(
    col = names(tf_test)[3:ncol(tf_test)],
    n   = str_count(col, "_"),
    new = case_when(
      n == 2 ~ str_c("tri_", col),
      n == 1 ~ str_c("bi_", col),
      TRUE   ~ str_c("uni_", col)
    )
  ) %>%
  pull(new)

# !!! check for missing data
tf_train <- mutate_at(tf_train, vars(-docid, -any_sub), funs(ifelse(is.na(.), 0, .)))
tf_test  <- mutate_at(tf_test, vars(-docid, -any_sub), funs(ifelse(is.na(.), 0, .)))

# if there's a column on one side that the other doesn't have, make an empty column
for (name in setdiff(names(tf_test), names(tf_train))) tf_train[, name] <- 0
for (name in setdiff(names(tf_train), names(tf_test))) tf_test[, name]  <- 0

# must be TRUE before moving to modeling
# can't predict if there are different feature names on test side
all(names(tf_train) %in% names(tf_test)) & all(names(tf_test) %in% names(tf_train))

# drop features in the training data that are all 0s
constants <- map_lgl(tf_train, ~all(. == 0))

# store for review
all_zero_train <- names(tf_train[, constants])

tf_train <- tf_train[, !constants]

# build dictionary models -------------------------------------------------

# use pattern matching results to build frame for logit model
coded_pm <- coded %>%
  mutate(
    summ_no_punct = str_remove_all(summary, "[[:punct:]]"),
    word_count    = str_count(summ_no_punct, "\\b") / 2,
    any_sub       = as.numeric(as.character(any_sub))
  ) %>%
  select(train_test, docid, any_sub, word_count) %>%
  left_join(pm, by = "docid")

keep_cols <- colnames(coded_pm[coded_pm$train_test == "train", map_lgl(coded_pm, ~!all(. == 0))])

coded_pm <- coded_pm %>%
  select(one_of(keep_cols)) %>%
  select(
    -word_count,
    -starts_with("neu_")
  )

dict_train <- coded_pm %>%
  filter(train_test == "train") %>%
  select(-train_test, -docid)

dict_test <- coded_pm %>%
  filter(train_test == "test") %>%
  select(-train_test)

# simple heuristic classification
dict_heur <- coded_pm %>%
  gather(var, val, -c(train_test:any_sub)) %>%
  mutate(var = str_remove_all(var, "[[:punct:][:digit:]]")) %>%
  group_by(docid, var) %>%
  mutate(count = sum(val)) %>%
  ungroup() %>%
  select(-val) %>%
  distinct() %>%
  spread(var, count) %>%
  mutate(heur_class = ifelse(pos > neg, 1, 0))

# logit model
dict_logit <- glm(any_sub ~ ., family = "binomial", data = dict_train)

# logit classification
dict_test$pred_prob <- predict(dict_logit, dict_test, type = "response")
dict_test$logit_class <- ifelse(dict_test$pred_prob > 0.5, 1, 0)

# build tf/idf models -----------------------------------------------------

# !!! Random Forest
# !!! number of trees should equal the number of features in the model

library(ranger)

rf_fit_uni <- ranger(
  formula = any_sub ~ .,
  data = select(tf_train, any_sub, contains("uni_")),
  num.trees = sum(str_count(names(tf_train), "uni_")),
  seed = 20190207,
  importance = "impurity"
)

rf_fit_bi <- ranger(
  formula = any_sub ~ .,
  data = select(tf_train, any_sub, contains("uni_"), contains("bi_")),
  num.trees = sum(str_count(names(tf_train), "uni_|bi_")),
  seed = 20190207,
  importance = "impurity"
)

rf_fit_tri <- ranger(
  formula = any_sub ~ .,
  data = tf_train[, -1],
  num.trees = sum(str_count(names(tf_train), "uni_|bi_|tri_")),
  seed = 20190207,
  importance = "impurity"
)

# evaluate ----------------------------------------------------------------

# helper function to compute confusion matrix and get metrics of interest
confmat <- function(pred, actual) {

  conf <- table(pred, actual) %>% addmargins()

  lst(
    #conf,

    # sensitivity (true positives / all positives)
    sens = conf[2, 2] / conf[3, 2],

    # specificity (true negatives / all negatives)
    spec = conf[1, 1] / conf[3, 1],

    # accuracy
    acc  = (conf[1, 1] + conf[2, 2]) / conf[3, 3]
  )
}

tf_test$rf_uni_pred <- predict(rf_fit_uni, tf_test)$predictions
tf_test$rf_bi_pred  <- predict(rf_fit_bi, tf_test)$predictions
tf_test$rf_tri_pred <- predict(rf_fit_tri, tf_test)$predictions

tf_test_res <- tf_test %>%
  select(
    docid,
    any_sub,
    contains("_pred")
  ) %>%
  left_join(dict_heur %>% select(heur_class, docid), by = "docid") %>%
  left_join(dict_test %>% select(logit_class, docid), by = "docid")

# gather importance values from each RF model
rf_var_imp <- map_df(
  lst(rf_fit_uni, rf_fit_bi, rf_fit_tri),
  ~tibble(
    feature       = names(importance(.)),
    estimateType  = "Importance",
    estimateValue = importance(.),
    lowerCI       = NA,
    upperCI       = NA
  ),
  .id = "model"
)

write_csv(rf_var_imp, "ms-working-dir/data/rf-variable-importance.csv")

# save fitted models for later inspection
write_rds(
  lst(
    dict_logit,
    rf_fit_uni,
    rf_fit_bi,
    rf_fit_tri
  ),
  path = "ms-working-dir/data/fitted-model-objects.rds"
)

# save specific predictions on the test set
write_csv(tf_test_res, "ms-working-dir/data/ml-test-predictions.csv")

# print the confusion tables and associated performance statistics
perf <- map2_df(
  lst(
    tf_test_res$rf_uni_pred, tf_test_res$rf_bi_pred, tf_test_res$rf_tri_pred,
    tf_test_res$heur_class, tf_test_res$logit_class
  ),
  lst(
    tf_test_res$any_sub, tf_test_res$any_sub, tf_test_res$any_sub,
    tf_test_res$any_sub, tf_test_res$any_sub
  ),
  ~confmat(.x, .y)
) %>%
  mutate(model = c("rf_uni_pred", "rf_bi_pred", "rf_tri_pred",
                  "heur_class", "logit_class"))

perf <- perf %>% select(model, acc, sens, spec)

write_csv(perf, "ms-working-dir/data/model-performance.csv")


# kappa statistic analysis ------------------------------------------------

# include the reliability sample from SB
rel_files <- dir(
  "input-data-dir/coded-data/",
  pattern = "reliability-samp",
  full.names = TRUE
) %>%
  magrittr::extract(!str_detect(., "-bv.xlsx"))

names(rel_files) <- rel_files %>%
  str_extract("samp-(.*?)\\.") %>%
  str_remove_all("samp-|\\.")

# drop cases that have been flagged for review
rel_coded <- rel_files %>%
  map_df(
    ~read_excel(., col_types = c("text", "text", "logical", rep("numeric", 15), "text")),
    .id = "coder"
  ) %>%
  select(coder, id, alcohol, stumulants, heroin, rx_opioids, rx_other, unspecified, other)

# create any substance use indicator
rel_coders <- rel_coded %>%
  gather(key = sub, val = ind, alcohol:other) %>%
  group_by(coder, id) %>%
  summarise(any_sub = sum(ind, na.rm = TRUE)) %>%
  mutate(any_sub = factor(ifelse(any_sub >= 1, 1, 0))) %>%
  spread(key = coder, val = any_sub) %>%
  set_names(c("docid", "c1", "c2", "c3", "c4"))

rel_preds <- coded %>%
  filter(reliab == 1) %>%
  select(docid) %>%
  inner_join(tf_test_res, by = "docid") %>%
  select(
    docid,
    rf_uni_pred,
    rf_bi_pred,
    rf_tri_pred,
    heur_class,
    logit_class
  )

ratings <- inner_join(rel_coders, rel_preds, by = "docid") %>%
  select(
    c1, c2, c3, c4,
    contains("_pred"),
    contains("_class"),
    docid
  )

library(irr)

# take a baseline using just the human coders
baseline <- kappam.fleiss(ratings[, 1:4])

# create a list of vectors which reflect all the different combinations of each model/rater
# the first argument is the different combination of raters, leaving 1 out each time
# the second argument is the indices of the models
combos <- cross2(lst(1:3, 2:4, c(1, 3:4), c(1:2, 4)), 5:9) %>%
  map(unlist)

# name each of the permutations based on which model is included
names(combos) <- map_chr(
  combos,
  ~case_when(
    5 %in% .x ~ "rf-uni",
    6 %in% .x ~ "rf-bi",
    7 %in% .x ~ "rf-tri",
    8 %in% .x ~ "heur",
    9 %in% .x ~ "logit",
    TRUE      ~ NA_character_
  )
)

# calculate kappa statistics for each permutation, and store as data frame
kappas <- combos %>%
  map(~kappam.fleiss(ratings[, .])) %>%
  map_dbl("value") %>% {
    tibble(
      model = names(.),
      kappa = .
    )
  } %>%
  group_by(model) %>%
  mutate(permutation = row_number(model)) %>%
  ungroup() %>%
  add_row(model = "human baseline", kappa = baseline$value, permutation = 0)

write_csv(kappas, "ms-working-dir/data/permuted-model-kappas.csv")

# prevalence estimates ----------------------------------------------------

# store weights, vocab, stops, dictionaries, and functions used to clean data
# these objects will be imported in a separate script used to estimate prevalence
# on our full set of CPS records
write_rds(
  lst(
    vocab,
    idf_weights,
    stops,
    ngram_drop,
    unique_dict_phrases,
    sud_dict,
    preprocess_text,
    sud_pattern_match,
    tidy_ngrams_s,
    get_tfidf
  ),
  "ms-working-dir/data/prevalence-assets.rds"
)
