## Label topics with pause according to the most relevant words per topic
labelTopics.p <- function(model,
                          texts,
                          first = 1,
                          last = NULL,
                          n_words1 = 10,
                          n_words2 = 20,
                          frexweight = 0.5,
                          thresh_prob_texts = 0.5,
                          n_docs = 5,
                          word_type = "FREX",
                          view_labels = F) {
  topic_labels <- data.frame(
    topic = character(),
    New_label = character(),
    words = character(),
    attempts_score = numeric()
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

  if (is.null(last) == T) {
    last <- model$settings$dim$K
  }

  for (x in first:last) {
    temp <- labelTopics(model,
      topic = x,
      n = n_words1,
      frexweight = frexweight
    ) %>%
      utils::capture.output()

    temp[type_num] %>%
      paste0("Topic ", x, .) %>%
      cat()

    new_name <- readline(prompt = paste0("Enter name (ESC = quit/'?' show ", n_words2, " words): "))

    score <- 1

    if (new_name == "?") {
      temp2 <- labelTopics(model,
        topic = x,
        n = n_words2,
        frexweight = frexweight
      ) %>%
        utils::capture.output()

      temp2[type_num] %>%
        paste0("Topic ", x, .) %>%
        cat()

      new_name <- readline(prompt = "Enter name (ESC = quit/'?' show high. prob. text samples): ")

      score <- 2
    }

    if (new_name == "?") {
      temp3 <- findThoughts(model,
        texts,
        topics = x,
        thresh = thresh_prob_texts
      )

      important_words <- temp[type_num] %>%
        gsub(".*:", "", x = .) %>%
        strsplit(., split = ",", fixed = TRUE) %>%
        unlist()

      for (wrd in important_words) {
        temp3 <- temp3 %>%
          gsub(wrd, crayon::red(wrd), x = .)
      }

      cat(temp3)

      new_name <- readline(prompt = paste0("Enter name (ESC = quit/'?' show ", n_docs, " text samples): "))

      score <- 3
    }


    if (new_name == "?") {
      temp4 <- findThoughts(model,
        texts,
        topics = x,
        n = n_docs
      )

      for (wrd in important_words) {
        temp4 <- temp4 %>%
          gsub(wrd, crayon::red(wrd), x = .)
      }

      cat(temp4)

      new_name <- readline(prompt = paste0("Last attempt for topic ", x, ", enter name (ESC to quit/'?' to continue): "))

      score <- 4
    }

    if (new_name == "?") {
      score <- "NA"
    }

    temp_labels <- data.frame(
      topic = paste0("Topic ", x),
      New_label = new_name,
      words = temp[type_num],
      attempts_score = score
    )


    topic_labels <- rbind(
      topic_labels,
      temp_labels
    )

    if (isTRUE(view_labels) == TRUE) {
      unique(topic_labels$New_label) %>%
        cat()
    }
  }
  return(topic_labels)
}
