# requires also the RSQLite package!
require(jsonlite)
require(DBI)
require(dplyr)
require(tm)
require(readr)

preprocessWords <- function(words) {
  
  words %>%
    removePunctuation %>%
    removeNumbers %>%
    stripWhitespace %>%
    stemDocument %>%
    tolower
}

# paperDf: Data frame representing a paper object from MAG.
# stopwords: Words to exclude from the query
# titleWeight: Factor how much words from the paper title should be weighted stronger than words from the abstract.
# dbCon: Database connection
getConceptsForPaper <- function(paperDf, stopwords, titleWeight, dbCon) {
  
  abstract <- fromJSON(paperDf$abstract)
  titleWords <- Boost_tokenizer(paperDf$title)
  
  wordFrame <- data_frame(word = names(abstract$InvertedIndex), 
                          frequency = sapply(abstract$InvertedIndex, length)) %>%
    bind_rows(data_frame(word = titleWords, frequency = titleWeight)) %>%
    mutate(norm_word = preprocessWords(word)) %>%
    filter(!(word %in% stopwords))
  
  query <- paste0("SELECT t.term, a.article, s.tf_idf
                    FROM terms t, term_article_score s, articles a
                    WHERE t.term in (", paste0("'", wordFrame$norm_word, "'", collapse=","), ") 
                    AND t.id = s.term_id AND a.id = s.article_id")
          
  dbGetQuery(dbCon, query) %>%
    inner_join(wordFrame, by = c("term" = "norm_word")) %>%
    mutate(score = frequency * tf_idf) %>%
    group_by(article) %>%
    summarise_at(c("frequency", "tf_idf", "score"), sum) %>%
    filter(!startsWith(article , "List of"))# Without this step often 'list of ...' articles appear in the top results.
}

example <- function() {
  
  con <- dbConnect(RSQLite::SQLite(), "esa.db") # esa.db can be downloaded from https://github.com/collide-uni-due/esa_db
  
  papers <- read_delim("../datasets/lak_papers.tsv", delim = "\t", 
                       col_names = c("id", "title", "year", "venue", "abstract"))
  
  # get all concepts for paper 500
  i <- 500
  print(paste("Get concept vector for paper with title", papers$title[i]))
  getConceptsForPaper(papers[i,], stopwords = stopwords(), titleWeight = 2, con) %>%
    arrange(desc(score)) %>% View
  
  dbDisconnect(con)
}