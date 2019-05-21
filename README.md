# askNivi Text Mining Analysis

This is the public repository for "A text mining analysis of messages exchanged between users and agents of a digital health service in Kenya", a manuscript submitted to Gates Open Research.

# Reproduce Paper

To reproduce this paper, clone this repository to your local machine, install [R](https://cloud.r-project.org/) and (optionally) [RStudio](https://www.rstudio.com/products/rstudio/download/), and install/load the following packages:

```
  library(papaja)
  library(MASS)
  library(niviR)
  library(tidyverse)
  library(knitr)
  library(gridExtra)
  library(kableExtra)
  library(magrittr)
  library(alluvial)
  library(ggrepel)
  library(ggalt)
  library(egg)
  library(grid)
  library(igraph)
  library(ggraph)
  library(widyr)
```

Knitting the `manuscript.Rmd` file will pull in the data files, run the analysis, and output a PDF.

We conducted all data processing and analysis in `R` version 3.5 and compiled this manuscript using the `papaja` package [v0.1.0.9842]. 

* To detect the language of each message, we used the `cld2` package [v1.2] to access Google's Compact Language Detector 2, a Naïve Bayesian classifier that probabilistically detects 83 languages, including English and Swahili. 
* We used the `tidytext` package [v0.2.0] to analyze word frequency and relationships in all inbound messages. 
* We used the `hunspell` package [v3.0] to detect possible misspellings and suggest corrections but ultimately decided to only accept suggestions for English words that appeared fewer than 4 times in the corpus given our concerns about reliability. 
* We used the `textstem` package [v0.1.4] to conduct lemmatization on the English words and identify the base form of each word—its lemma. 

# License
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

[![DOI](https://zenodo.org/badge/184089813.svg)](https://zenodo.org/badge/latestdoi/184089813)

# Text Mining Example

We prepared a brief example of how to tokenize messages. Here is an outline of the process:

![alt text](https://raw.githubusercontent.com/ericpgreen/asknivi-text-mining-2019/master/input/example/process.png)

We recommend first reading [*Text Mining with R*](https://www.tidytextmining.com/) by Julia Silge and David Robinson. Our example is based on the approach they describe.

Start by loading the required packages.

```
  library(tidyverse)
  library(tidytext)
  library(cld2)
  library(hunspell)
  library(textstem)
```

Next, load a dataset of English stop words, a list of custom stop words we curated, and a sample of message data.

```
# load stop_words from tidytext package
# -----------------------------------------------------------------------------
  data(stop_words) # load stop_words
# remove some words
  stop_words <-
  stop_words %>%
    filter(word!="side") # for side effects

# load custom stop words
# -----------------------------------------------------------------------------
  stop <- read.csv("input/example/stop.csv", stringsAsFactors = FALSE)
  
# load custom modifications
# -----------------------------------------------------------------------------
  modEng <- read.csv("input/example/modifications-en.csv", 
                     stringsAsFactors = FALSE)
  modEng <- 
  modEng %>%
    filter(rename!="")
  
# load sample anonymized message data
# -----------------------------------------------------------------------------
  df <- read.csv("input/example/example.csv", stringsAsFactors = FALSE)
```

Message data can be quite messy, so we conduct a bit of pre-processing to replace periods with spaces to separate words (e.g., "end.beginning" to "end beginning") and convert all text to lowercase.

```
  df <-
  df %>%
  # replace periods with spaces to separate words
    mutate(message_ = gsub("\\.", " ", message)) %>%
    mutate(message_ = tolower(message_))
```

To detect the language of each message, we used the `cld2` package [v1.2] to access Google's Compact Language Detector 2, a Naïve Bayesian classifier that probabilistically detects 83 languages, including English and Swahili.

```
  df$msgLang <- cld2::detect_language(df$message_)   
  df$msgLang <- ifelse(df$msgLang=="en" |            # set other langs to NA
                       df$msgLang=="sw", 
                       df$msgLang, NA)
```

Now to the fun parts. The next step is to examine the frequency of consecutive words, bigrams. The key function is `unnest_tokens()` that breaks messages into pairs of words. `separate()` separates pairs into two columns so it's possible to remove stop words from each column before re-uniting and counting.

```
  df %>%
  # limit to incoming messages in english
    filter(msgLang=="en") %>%
  # tokenize
    unnest_tokens(word, message_, token = "ngrams", n = 2) %>%
    separate(word, c("word1", "word2"), sep = " ") %>% 
  # remove stop words
    anti_join(filter(stop_words), by = c(word1 = "word")) %>% 
    anti_join(filter(stop_words), by = c(word2 = "word")) %>%
    filter(!(word1 %in% stop$niviStop)) %>%
    filter(!(word2 %in% stop$niviStop)) %>%
    unite(word, word1, word2, sep = " ") %>%
  # custom spelling corrections, if necessary
    mutate(word = ifelse(word=="prostrate cancer", "prostate cancer", word)) %>%
    mutate(word = ifelse(word=="family planing", "family planning", word)) %>%
    mutate(word = ifelse(word=="planning method", "planning methods", word)) %>%
  # count
    count(word, sort = TRUE) %>%
    dplyr::select(word, n) %>%
    filter(word!="NA NA") 
```

Here's the result from our example data:

```
# A tibble: 46 x 2
   word                  n
   <chr>             <int>
 1 giving birth          2
 2 prostate cancer       2
 3 side effects          2
 4 3 sum                 1
 5 abnormal child        1
 6 abov sexualhealth     1
 7 ady ngozi             1
 8 age improve           1
 9 anal sex              1
10 blood clotting        1
# … with 36 more rows
```

When counting individual words, we wanted to preserve some bigrams. For instance, when the word "family" immediately preceded the word "planning", we wanted to tally "family" as part of the bigram "family planning". However, if "family" occurred on its own, e.g., "I do not want to start a family", then we wanted to tally family as an individual term.

Our approach is a bit hacky, but we take all observed spellings of the bigrams and concatenate without a space, "familyplanning". This will count instances of "familyplanning" separate from instances of "family" and "planning" that do not appear together.

```
  df <- 
  df %>%
    mutate(message_ = gsub("breast cancer|breast caancer", 
                           "breastcancer", 
                           message_)) %>%
    mutate(message_ = gsub("family planning", 
                           "familyplanning", 
                           message_, 
                           fixed=TRUE)) %>%
    mutate(message_ = gsub("prostate cancer", 
                           "prostatecancer", 
                           message_, 
                           fixed=TRUE)) %>%
    mutate(message_ = gsub("sexual health", 
                           "sexualhealth", 
                           message_, 
                           fixed=TRUE))   
  
  bigramKeep <- c("breastcancer", "familyplanning", "prostatecancer", 
                  "sexualhealth"
  )
```

The next example shows how to tokenize messages and count the frequency of individual words. This approach can be modified with `group_by()` functions (#commented out below) to count frequency of words by group.

[*Text Mining with R*](https://www.tidytextmining.com/) gives a simpler, cleaner example of how to conduct this analysis. Our approach will likely work for new examples, but the specifics may be somewhat unique to the challenges we observed in our message data.

We ran this process an initial time and used the results to make a custom list of modifications to incorporate into final processing. 

![alt text](https://raw.githubusercontent.com/ericpgreen/asknivi-text-mining-2019/master/input/example/modifications.png)

For instance, we wanted to collapse all instances of the words "sex" and "intercourse" into the word "sex". This is a step beyond lemmatization (shown below) that collapses words into lemma, e.g., "prenancy" to "pregnant".

```
  df %>%
  # limit to incoming messages in english
    filter(msgLang=="en") %>%
  
  # tokenize
  # ---------------------------------------------------------------------------
  # to tokenize by group, add group_by
  # group_by(genAge) %>%
    unnest_tokens(word, message_) %>%
  
  # get initial count
    count(word, sort = TRUE) %>%
  # remove numbers and punctuation from strings
    mutate(word = gsub('[[:digit:]]+', '', word)) %>%
    mutate(word = gsub('_', '', word)) %>%
    mutate(word = gsub(',', '', word)) %>%
  # remove blank or 1 character after number removal 
    filter(nchar(word)>1) %>%
    
  # spelling, lemmatization
  # ---------------------------------------------------------------------------
  # suggest spelling replacements
    mutate(suggest = unlist(lapply(hunspell_suggest(word), 
                                   function(x) x[1]))) %>%
  # keep spelling suggestion for words where n < ??
  # mutate(word = ifelse(n<??, suggest, word)) %>%  # turned off to ignore 
  # correct a few problems hunspell introduces in these replacements
    mutate(word = tolower(word)) %>%
    mutate(word = gsub(" ", "", word)) %>%
  # lemma
    mutate(lemma = lemmatize_words(word)) %>%
  # keep lemma unless word is in bigramKeep
    mutate(word = ifelse((word %in% bigramKeep), suggest, lemma)) %>%
  # another round of corrections
    mutate(word = gsub('[[:digit:]]+', '', word)) %>%
    filter(!is.na(word)) %>%
    filter(word!="") %>%
    
  # recount
  # ---------------------------------------------------------------------------
    dplyr::select(-lemma) %>%  #-suggest, -stem, 
    group_by(word) %>%   # if tokenized by group: group_by(genAge, word) %>% 
    summarise(n = sum(n)) %>%
    arrange(desc(n)) %>%
    
  # custom modifications (revert lemma, combine synonmyms)
  # ---------------------------------------------------------------------------
    left_join( dplyr::select(modEng, word, rename), by="word") %>%
    mutate(word = ifelse(!is.na(rename), rename, word)) %>%
    dplyr::select(-rename) %>%
    
  # recount
  # ---------------------------------------------------------------------------
    group_by(word) %>%  # if tokenized by group: group_by(genAge, word) %>% 
    summarise(n = sum(n)) %>%
    arrange(desc(n)) %>%
  # remove stop words again
    anti_join(stop_words) %>%
    filter(!(word %in% stop$niviStop)) 
```

Here's the result from our example data:

```
# A tibble: 121 x 2
   word                     n
   <chr>                <int>
 1 contraception/method    12
 2 sex                      8
 3 period                   7
 4 age                      6
 5 safe                     6
 6 sexual                   6
 7 sign/symptom             6
 8 blood/bleed              4
 9 cancer                   4
10 effect/side effect       4
# … with 111 more rows
```

These results can be piped to the visualization code as shown in the `manuscript.Rmd` file.
