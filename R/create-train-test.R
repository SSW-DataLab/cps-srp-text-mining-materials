library(tidyverse)
library(readxl)
library(writexl)

# include the reliability sample from SB
files <- dir(
  "input-data-dir/coded-data/",
  pattern = "disp-file|reliability-samp-sb\\.xlsx",
  full.names = TRUE
)

# drop cases that have been flagged for review
coded <- files %>%
  map_df(
    ~read_excel(., col_types = c("text", "text", "logical", rep("numeric", 15), "text"))
  ) %>%
  select(
    id, summary, review, notes, ambiguity, contains("no_"),
    everything(), contains("marijuana")
  ) %>%
  arrange(id) %>%
  filter(is.na(review), completed == TRUE) %>%
  distinct(id, .keep_all = TRUE)

# determine whether any of the substance indicators were positive
# this is the target variable for classification
# indicators included:
#     1. alcohol
#     2. heroin
#     3. 'other'
#     4. rx_opioids
#     5. rx_other
#     6. stimulants
#     7. unspecified
coded <- coded %>%
  gather(sub, ind,  alcohol, heroin, other, rx_opioids, rx_other, stumulants, unspecified) %>%
  group_by(id) %>%
  summarise(any_sub = sum(ind, na.rm = TRUE)) %>%
  mutate(any_sub = factor(ifelse(any_sub >= 1, 1, 0))) %>%
  right_join(coded, by = "id")

# pull in the cases that were used for reliability assessment
reliability <- read_excel(
  "input-data-dir/20181008-reliability-samp.xlsx",
  col_types = c("text", "skip")
)

reliability <- inner_join(reliability, coded, by = "id")
coded       <- anti_join(coded, reliability, by = "id")

set.seed(20190122)

# assign each case to training/test set; 75/25 split

train <- sample_n(coded, size = 2217)

test  <- coded %>%
  anti_join(train, by = "id") %>%
  bind_rows(reliability)

coded <- bind_rows("train" = train, "test" = test, .id = "train_test")
coded <- mutate(coded, reliab = ifelse(id %in% reliability$id, 1, 0))

write_xlsx(coded, "ms-working-dir/data/20190122-train-test-labelled-dispos.xlsx")
