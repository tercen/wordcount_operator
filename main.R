library(tercen)
library(dplyr)
library(tokenizers)

tokenize_words = function(docId){
  con = rawConnection(ctx$client$fileService$download(docId), "r+")
  lines = readLines(con, encoding="UTF-8")
  close(con)
  unlist(tokenizers::tokenize_words(lines))
}

ctx = tercenCtx()

if (!any(ctx$cnames == "documentId")) stop("Column factor documentId is required") 
 
ctx$cselect() %>% 
  mutate(.ci= 1:nrow(.)-1) %>%
  group_by(.ci) %>%
  summarize(word = tokenize_words(documentId)) %>%
  count(word, name = "word_count_int", sort = TRUE) %>%
  mutate(word_count = as.double(word_count_int), word_count_int=NULL) %>%
  ctx$addNamespace() %>%
  ctx$save()
  
