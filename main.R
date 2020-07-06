library(tercen)
library(dplyr)
library(tokenizers)

# options("tercen.serviceUri"="http://172.17.0.1:5400/api/v1/")
# options("tercen.workflowId"= "0efdb3a90d78f83913059a6d3a182958")
# options("tercen.stepId"= "25c081dd-4b6f-42e0-8339-72dc821f0bfd")
 
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
  
