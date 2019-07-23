# README

This GitHub repository contains the R code and phrase dictionary files used to process data and fit the models reported in ["Detecting substance-related problems in narrative investigation summaries of child abuse and neglect using text mining and machine learning"](), currently under review. This README file provides an overview of the contents of each subdirectory, and instructions on how to utilize the processing functions.

There are two subdirectories:

- `data/`
- `R/`

### `data/` directory

The `data/` subdirectory contains two files (`full-stops-list.xlsx` and `sud-dictionary.xlsx`).

The first file is a list of words and names (Michigan specific) that our reseach team deemed to be non-informative to our classification task. Each list is stored as a specific tab within the excel file. These lists were used during the text preprocessing and tokenization for the random forest models presented in the manuscript, which rely on TF/IDF matrices of unigrams, bigrams, and trigrams. The list of first and last names were drawn from the US Social Security Administration, spanning several decades of birth years to provide adequate coverage of possible names that could be encountered in the text being analyzed. Our list of standard stop words come from three different commonly used lexicons ("onix", "SMART", & "snowball"), which are curated in the R package `tidytext`. We removed a small subset of terms from these lexicons (e.g. in order to permit the creation of bigrams/trigrams that would imply negations), and our final inclusions are retained in the excel file. Lastly, we compiled a list of custom phrases and terms that are topically specific to Michigan's CPS system, but we deemed uninformative to the problem of determining whether a document addressed/discussed problematic substance use. Examples include acronyms (e.g. "DHHS", "DHS", "CPS"), and administrative language referring to substantiating allegations or evidence concerning allegations (e.g. "preponderance evidence support" or "mandatory override"). 

The second file contains several lists of regex search patterns, organized by different substance type and/or topic. These lists are used to generate the features used in the rule-based and logistic regression models reported in the manuscript. Each tab of the excel sheet represents a different topic/substance. Individual search patterns are organized into "tiers", so that when pattern counts are calculated, constituent phrases are not counted multiple times (e.g. "fetal alcohol syndrome" is counted prior to "alcohol" alone, and held aside from the count for "alcohol").

### `R/` directory

The `R/` subdirectory contains R scripts used to prepare the raw text data, fit the different models presented in the manuscript, and prepare estimates underlying its figures.

The first file, `create-train-test.R`. prepares a train/test split from our labelled disposition summaries. This script combines the separate files that were rated by individual coders in our office, and creates our *target* variable which indicates whether a given document is rated positive or negative for containing language indicating problematic substance use.

The second file, `fit-and-assess-ml-models.R`, is the primary file for preparing the raw data and building the models that are reported in the manuscript. Major operations in the code are organized into functions, e.g. one function dedicating to text preprocessing (standardizing text casing, removing punctuation, removing honorifics/titles, converting permutations of substances to single terms (e.g. "opiates" and "opioids" are treated together as "opioids")), one function dedicated to generating counts of patterns from the `sud-dictionary.xlsx` file, one function dedicated to generating different types of N-grams, and another function dedicated to generating TF/IDF weights. The remaining code imports the raw data prepared by `create-train-test.R`, applies the previously mentioned functions to clean/prepare modeling frames, fits the different models examined in the project, and stores the results for review and summary. This script also contains the kappa-statistic analysis, in which each model's prediction is permuted through a different group of raters.

The third file, `predict-overall-prevalence.R`, applies the models that were fit to the remaining unlabelled cases that the training data were drawn from.

### Process/workflow

Assuming the user has an excel file with the following three columns

1. `id`, Unique document ID#
2. `any_sub`, target variable (0 : negative, 1 : positive)
3. `summary`, raw text data

`fit-and-assess-ml-models.R` should be able to run without many modifications. Users will need to alter the paths to all the commands that *read* or import data into the environment, and *write* results or model objects back to disk for later review. These functions/commands can be easily found using search functions in any text editor; all of these functions are prepended with `read_` or `write_` (with one exception being `readxl::excel_sheets()`, which is used to pull the names of each tab in an excel file). The scripts described above reference two directories (regarding `read_` or `write_` commands):

- `input-data-dir/` and
- `ms-working-dir/`.

The first directory represents the file location where data that will be cleaned, processed, and used for modeling is stored. The second directory represents a file location where the resulting models, statistics, and output are stored. These paths will need to be updated by any future users with paths that correspond to their files are stored.

Users should also update the supplemental data files included in the `data/` folder, specifically the sets of names used based on the data's geographic region, and add/remove specific custom phrases or words as needed. If the dictionary/pattern match approach will be used, reviewing the provided regex patterns may be needed to determine they match the user's needs. Importantly, column names for these files should not be altered; any changes to column names will likely generate errors when `fit-and-assess-ml-models.R` is run.

The R build/environment and package versions used for the results in our manuscript are available below.

```
- Session info -----------------------------------------------------
 setting  value                       
 version  R version 3.4.4 (2018-03-15)
 os       Windows 7 x64 SP 1          
 system   x86_64, mingw32 
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/New_York     
     

- Packages ---------------------------------------------------------
 package     * version   date       lib source        
 assertthat    0.2.0     2017-04-11 [1] CRAN (R 3.4.3)
 backports     1.1.2     2017-12-13 [1] CRAN (R 3.4.3)
 base64enc     0.1-3     2015-07-28 [1] CRAN (R 3.4.1)
 broom         0.5.2     2019-04-07 [1] CRAN (R 3.4.4)
 callr         3.0.0     2018-08-24 [1] CRAN (R 3.4.4)
 cellranger    1.1.0     2016-07-27 [1] CRAN (R 3.4.3)
 cli           1.1.0     2019-03-19 [1] CRAN (R 3.4.4)
 colorspace    1.3-2     2016-12-14 [1] CRAN (R 3.4.3)
 crayon        1.3.4     2017-09-16 [1] CRAN (R 3.4.3)
 desc          1.2.0     2018-05-01 [1] CRAN (R 3.4.4)
 devtools      2.0.1     2018-10-26 [1] CRAN (R 3.4.4)
 digest        0.6.15    2018-01-28 [1] CRAN (R 3.4.3)
 dplyr       * 0.8.0.1   2019-02-15 [1] CRAN (R 3.4.4)
 forcats     * 0.4.0     2019-02-17 [1] CRAN (R 3.4.4)
 fs            1.2.6     2018-08-23 [1] CRAN (R 3.4.4)
 generics      0.0.2     2018-11-29 [1] CRAN (R 3.4.4)
 ggplot2     * 3.1.1     2019-04-07 [1] CRAN (R 3.4.4)
 glue          1.3.1     2019-03-12 [1] CRAN (R 3.4.4)
 gtable        0.2.0     2016-02-26 [1] CRAN (R 3.4.3)
 haven         2.1.0     2019-02-19 [1] CRAN (R 3.4.4)
 hms           0.4.2     2018-03-10 [1] CRAN (R 3.4.4)
 httr          1.4.0     2018-12-11 [1] CRAN (R 3.4.4)
 janeaustenr   0.1.5     2017-06-10 [1] CRAN (R 3.4.4)
 jsonlite      1.6       2018-12-07 [1] CRAN (R 3.4.4)
 lattice       0.20-35   2017-03-25 [1] CRAN (R 3.4.4)
 lazyeval      0.2.1     2017-10-29 [1] CRAN (R 3.4.3)
 lubridate     1.7.4     2018-04-11 [1] CRAN (R 3.4.4)
 magrittr    * 1.5       2014-11-22 [1] CRAN (R 3.4.3)
 Matrix        1.2-15    2018-11-01 [1] CRAN (R 3.4.4)
 memoise       1.1.0     2017-04-21 [1] CRAN (R 3.4.3)
 modelr        0.1.4     2019-02-18 [1] CRAN (R 3.4.4)
 munsell       0.5.0     2018-06-12 [1] CRAN (R 3.4.4)
 nlme          3.1-131.1 2018-02-16 [1] CRAN (R 3.4.4)
 pillar        1.3.1     2018-12-15 [1] CRAN (R 3.4.4)
 pkgbuild      1.0.2     2018-10-16 [1] CRAN (R 3.4.4)
 pkgconfig     2.0.2     2018-08-16 [1] CRAN (R 3.4.4)
 pkgload       1.0.2     2018-10-29 [1] CRAN (R 3.4.4)
 plyr          1.8.4     2016-06-08 [1] CRAN (R 3.4.3)
 prettyunits   1.0.2     2015-07-13 [1] CRAN (R 3.4.4)
 processx      3.2.1     2018-12-05 [1] CRAN (R 3.4.4)
 ps            1.2.1     2018-11-06 [1] CRAN (R 3.4.4)
 purrr       * 0.3.2     2019-03-15 [1] CRAN (R 3.4.4)
 R6            2.2.2     2017-06-17 [1] CRAN (R 3.4.3)
 Rcpp          1.0.0     2018-11-07 [1] CRAN (R 3.4.4)
 readr       * 1.3.1     2018-12-21 [1] CRAN (R 3.4.4)
 readxl      * 1.3.1     2019-03-13 [1] CRAN (R 3.4.4)
 remotes       2.0.2     2018-10-30 [1] CRAN (R 3.4.4)
 rlang         0.3.4     2019-04-07 [1] CRAN (R 3.4.4)
 rprojroot     1.3-2     2018-01-03 [1] CRAN (R 3.4.3)
 rstudioapi    0.10      2019-03-19 [1] CRAN (R 3.4.4)
 rvest         0.3.2     2016-06-17 [1] CRAN (R 3.4.4)
 scales        1.0.0     2018-08-09 [1] CRAN (R 3.4.4)
 sessioninfo   1.1.1     2018-11-05 [1] CRAN (R 3.4.4)
 SnowballC     0.5.1     2014-08-09 [1] CRAN (R 3.4.4)
 stringi       1.1.7     2018-03-12 [1] CRAN (R 3.4.4)
 stringr     * 1.4.0     2019-02-10 [1] CRAN (R 3.4.4)
 testthat      2.0.1     2018-10-13 [1] CRAN (R 3.4.4)
 tibble      * 2.1.1     2019-03-16 [1] CRAN (R 3.4.4)
 tidyr       * 0.8.3     2019-03-01 [1] CRAN (R 3.4.4)
 tidyselect    0.2.5     2018-10-11 [1] CRAN (R 3.4.4)
 tidytext    * 0.2.0     2018-10-17 [1] CRAN (R 3.4.4)
 tidyverse   * 1.2.1     2017-11-14 [1] CRAN (R 3.4.4)
 tokenizers    0.2.1     2018-03-29 [1] CRAN (R 3.4.4)
 usethis       1.4.0     2018-08-14 [1] CRAN (R 3.4.4)
 withr         2.1.2     2018-03-15 [1] CRAN (R 3.4.4)
 xml2          1.2.0     2018-01-24 [1] CRAN (R 3.4.4)
 yaml          2.1.18    2018-03-08 [1] CRAN (R 3.4.4)
```
