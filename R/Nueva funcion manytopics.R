labelTopics_df <- function(lab_top_result,
                           word_type = "frex",
                           pivot_wrd = FALSE) {
  lab_top_result[[word_type]] %>%
    as.data.frame() %>%
    rename_at(
      vars(starts_with("V")),
      funs(str_replace(., "V", "Word"))
    ) -> words

  words$Topic <- paste0("Topic ", lab_top_result[["topicnums"]])

  if (isTRUE(pivot_wrd) == TRUE) {
    words <- pivot_longer(words, cols = starts_with("Word"), values_to = "Words")
  }

  return(words)
}


#####################################################

findThoughts_df <- function(ft_result) {
  temp_df <- foreach(x = names(ft_result$docs), .combine = rbind) %do% {
    texts <- data.frame(
      "Texts" = ft_result$docs[[x]],
      "index" = ft_result$index[[x]]
    )

    texts$Topic <- x

    return(texts)
  }
  return(temp_df)
}

#####################################################

manyTopics_df <- function(models_list,
                          texts,
                          word_type = "frex",
                          n_words = 20,
                          frexweight = 0.5,
                          n_texts = 3,
                          thresh = 0.5,
                          pivot_wrd = FALSE) {
  mod_list <- foreach(x = 1:NROW(models_list), .combine = rbind) %do% {
    temp1 <- labelTopics(models_list[[x]],
      n = n_words,
      frexweight = frexweight
    )

    temp2 <- findThoughts(models_list[[x]],
      texts = texts,
      n = n_texts,
      thresh = thresh
    )

    words_df <- labelTopics_df(temp1,
      word_type = word_type,
      pivot_wrd = pivot_wrd
    )

    texts_df <- findThoughts_df(temp2)

    temp_wrd_txt <- join(words_df, texts_df, by = "Topic")

    temp_wrd_txt$K <- models_list[[x]]$settings$dim$K

    return(temp_wrd_txt)
  }
  return(mod_list)
} ## EOF

##################################################################

label.models_list <- function(models_list,
                              texts,
                              word_type = "frex",
                              n_words = 20,
                              frexweight = 0.5,
                              n_texts = 3,
                              thresh = 0.5,
                              summarise = TRUE,
                              propagate_labels = "strict",
                              distance = NULL,
                              cluster_method = "complete",
                              cut_height = 10,
                              show_used_labels = TRUE) {
  queries_df <- manyTopics_df(
    models_list = models_list,
    texts = texts,
    n_words = n_words,
    frexweight = frexweight,
    n_texts = n_texts,
    thresh = thresh,
    pivot_wrd = TRUE
  )

  if (propagate_labels == "strict") {
    code <- queries_df %>%
      reshape2::dcast(Topic + K + index ~ Words, fill = 0) %>%
      reshape2::dcast(... ~ index, fill = 0)

    code[, 3:NCOL(code)][code[, 3:NCOL(code)] > 0] <- "A"

    code$code <- do.call(paste, as.data.frame(code[, 3:NCOL(code)]))

    code <- code %>% select(Topic, K, code)
  } # End of propagate strict

  if (propagate_labels == "documents_strict") {
    code <- queries_df %>%
      reshape2::dcast(Topic + K ~ index, fill = 0)

    code[, 3:NCOL(code)][code[, 3:NCOL(code)] > 0] <- "A"

    code$code <- do.call(paste, as.data.frame(code[, 3:NCOL(code)]))

    code <- code %>% select(Topic, K, code)
  } # End of propagate documents_strict

  if (propagate_labels == "words_strict") {
    code <- queries_df %>%
      reshape2::dcast(Topic + K ~ words, fill = 0)

    code[, 3:NCOL(code)][code[, 3:NCOL(code)] > 0] <- "A"

    code$code <- do.call(paste, as.data.frame(code[, 3:NCOL(code)]))

    code <- code %>% select(Topic, K, code)
  } # End of propagate documents_strict



  queries_df <- join(queries_df, code, by = c("Topic", "K"))

  ### Queries loop
  labels_df <- data.frame(
    New_label = character(),
    Comment = character(),
    label_words = character(),
    code = character()
  )

  n_loop <- 0
  tot_loops <- NROW(unique(queries_df$code))
  tot_topics <- queries_df %>%
    select(Topic, K) %>%
    unique() %>%
    NROW()

  for (x in unique(queries_df$code)) {
    words_to_color <- queries_df %>%
      subset(code == x) %>%
      select(Words) %>%
      unique() %>%
      droplevels()

    texts_to_show <- queries_df %>%
      subset(code == x) %>%
      select(Texts) %>%
      unique() %>%
      droplevels()


    for (w in words_to_color$Words) {
      texts_to_show$Texts <- gsub(paste0("\\b", w), crayon::red(w), x = texts_to_show$Texts, ignore.case = T) # Add color to important words
    }

    texts_to_show$Texts %>% writeLines(sep = "\n---\n")

    n_loop <- n_loop + 1

    paste0("Loop ", n_loop, " of ", tot_loops, ". Total topics are ", tot_topics, ".\n") %>%
      crayon::blue() %>%
      cat() # Show current and total loops

    if (isTRUE(show_used_labels) == TRUE & n_loop > 1) {
      paste0("Used labels: ", paste0(unique(as.character(labels_df$New_label)), collapse = ", "), "\n") %>%
        crayon::yellow() %>%
        cat()
    }

    if (tot_loops == tot_topics & n_loop == 1) {
      cat("The current method for label propagation will not save you time.\n")
    }
    if (tot_loops < tot_topics & n_loop == 1) {
      less_loops <- tot_topics - tot_loops
      paste("The current method for label propagation will save you ", less_loops, " loops.\n")
    }

    New_label <- readline(prompt = "Write a label:")

    Comment <- readline(prompt = "Add a comment (press RETURN to leave empty):")

    temp_labels_df <- data.frame(
      New_label = New_label,
      Comment = Comment,
      label_words = paste0(words_to_color$Words, collapse = ", "),
      code = x
    )

    labels_df <- rbind(labels_df, temp_labels_df)
  }

  result <- join(queries_df, labels_df, by = "code")

  if (isTRUE(summarise) == TRUE) {
    result <- result %>%
      select(Topic, K, label_words, New_label, Comment) %>%
      unique()
  }

  return(result)
}






# test1<-label.manyTopics(results$art_STM_reptiles,
#                         texts = stm_df_reptiles$`dc:description`,
#                         propagate_labels = "distance_words",
#                         distance = "jaccard")
