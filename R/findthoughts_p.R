## Label topics with pause according to the most relevant texts in the database
findThoughts.p <- function(model,
                           texts,
                           first = 1,
                           last,
                           n = 3,
                           thresh = NULL) {
  topic_labels <- data.frame(
    topic = character(),
    New_label = character()
  )
  for (x in first:last) {
    temp <- findThoughts(model,
      texts,
      topics = x,
      n = n,
      thresh = thresh
    )
    print(temp)
    new_name <- readline(prompt = "Enter name (ESC to quit): ")
    temp_labels <- data.frame(
      topic = paste0("Topic ", x),
      New_label = new_name
    )

    topic_labels <- rbind(
      topic_labels,
      temp_labels
    )
  }
  topic_labels
}
