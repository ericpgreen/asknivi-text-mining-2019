# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# text mining example
# @ericpgreen
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# load packages
# =============================================================================
  library(tidyverse)
  library(tidytext)
  library(cld2)
  library(hunspell)
  library(textstem)


# load english stop words and custom modifications files
# =============================================================================

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
# =============================================================================
  
  df <- read.csv("input/example/example.csv", stringsAsFactors = FALSE)
  
# pre-process
# -----------------------------------------------------------------------------
  df <-
  df %>%
  # replace periods with spaces to separate words
    mutate(message_ = gsub("\\.", " ", message)) %>%
    mutate(message_ = tolower(message_))
  
# classify language of each message
# -----------------------------------------------------------------------------
  df$msgLang <- cld2::detect_language(df$message_)   
  df$msgLang <- ifelse(df$msgLang=="en" |            # set other langs to NA
                       df$msgLang=="sw", 
                       df$msgLang, NA)
  

# examine bigrams, english
# =============================================================================
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
  
  
# preserve selected bigrams, english
# =============================================================================
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
  
  
# tokenize, english, no grouping
# =============================================================================
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
  
  