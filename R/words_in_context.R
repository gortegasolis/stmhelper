## Label manyTopics results with pause according to the most relevant texts and words in the database
words_in_context <- function(model,
                             texts,
                             thresh = 0.5,
                             n_texts = 3,
                             n_words = 10,
                             word_type = "FREX",
                             frexweight = 0.5) {

  ## Define intermediate dataframe
  topic_labels <- data.frame(
    model = character(),
    K = character(),
    topic = character(),
    valid_topic = character(),
    New_label = character(),
    words = character()
  )

  if (word_type == "Highest Prob") {
    type_num <- 2
  }
  if (word_type == "FREX") {
    type_num <- 3
  }
  if (word_type == "Lift") {
    type_num <- 4
  }
  if (word_type == "Score") {
    type_num <- 5
  }


  ## Start nested for loops
  for (x in 1:NROW(model$out)) {
    ## Nested loop for findToughts function
    for (t in 1:model$out[[x]]$settings$dim$K) {
      temp <- labelTopics(model$out[[x]],
        topic = t,
        n = n_words,
        frexweight = frexweight
      ) %>%
        utils::capture.output()

      important_words <- temp[type_num] %>%
        gsub(".*:", "", x = .) %>%
        strsplit(., split = ",", fixed = TRUE) %>%
        unlist()


      temp2 <- findThoughts(model$out[[x]],
        texts,
        topics = t,
        n = n_texts,
        thresh = thresh
      )


      for (wrd in important_words) {
        temp2$docs[[1]] <- temp2$docs[[1]] %>%
          gsub(wrd, crayon::red(wrd), x = ., fixed = T)
      }

      temp2$docs[[1]] %>%
        str_wrap() %>%
        writeLines(sep = "\n---\n")


      valid_topic <- readline(prompt = "Enter Y(es) or N(o) (ESC to quit): ")



      if (valid_topic == "N") {
        new_name <- NA
        important_words <- NA
      }

      temp_labels <- data.frame(
        model = paste0("Model ", x),
        K = model$out[[x]]$settings$dim$K,
        topic = paste0("Topic ", t),
        valid_topic = valid_topic,
        New_label = new_name,
        words = important_words
      )

      topic_labels <- rbind(
        topic_labels,
        temp_labels
      )
    }
  }
  return(topic_labels)
}
