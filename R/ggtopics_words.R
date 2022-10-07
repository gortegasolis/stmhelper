## Plot topics and most important words with ggplot
ggtopics_word <- function(model,
                          topic_labels,
                          interest_words = NULL,
                          color = "steelblue",
                          head_and_tail = NULL,
                          show.legend = FALSE,
                          words_size = 3,
                          words_family = "IBMPlexSans",
                          col.int = "red") {
  # tidytext::tidy(model,
  #                matrix = "beta") %>%
  #   arrange(beta) %>%
  #   group_by(topic) %>%
  #   top_n(top_words, beta) %>%
  #   arrange(-beta) %>%
  #   select(topic, term) %>%
  #   mutate_at(.,"term", ~paste(., collapse = ", ")) %>%
  #   mutate(.,topic = as.factor(topic)) %>%
  #   unique()->   top_terms

  tidytext::tidy(model,
    matrix = "gamma"
  ) %>%
    select(topic, gamma) %>%
    group_by(topic) %>%
    mutate_at(., "gamma", ~ mean(gamma)) %>%
    unique() %>%
    arrange(desc(gamma)) %>%
    as.data.frame() %>%
    mutate(., topic = as.factor(topic)) %>%
    left_join(., topic_labels$label_words, by = "topic") -> gamma_terms

  if (is.null(interest_words) == FALSE) {
    tidytext::tidy(model) %>%
      filter(term %in% interest_words) %>%
      select(topic, beta) %>%
      group_by(topic) %>%
      mutate_at(., "int_beta" = sum(beta)) %>%
      mutate(., topic = as.factor(topic)) %>%
      left_join(gamma_terms, ., by = "topic") -> gamma_terms
  }

  gamma_terms$topic <- paste0("Topic ", gamma_terms$topic)

  gamma_terms %>%
    left_join(., topic_labels, by = "topic") -> gamma_terms

  if (is.null(head_and_tail) == FALSE) {
    gamma_terms <- gamma_terms[head_and_tail, ]
  }

  if (is.null(interest_words) == FALSE) { ## Interest_words TRUE
    gamma_terms %>%
      ggplot(., aes(reorder(New_label, gamma), gamma, label = term)) +
      geom_col(
        fill = color,
        show.legend = show.legend
      ) +
      geom_col(aes(y = int_beta * gamma),
        fill = col.int,
        show.legend = FALSE
      ) +
      geom_text(
        hjust = 0, nudge_y = 0.0005,
        size = words_size,
        family = words_family
      ) +
      coord_flip()
  } else { ## Interest_words FALSE
    gamma_terms %>%
      ggplot(., aes(reorder(New_label, gamma), gamma, label = term)) +
      geom_col(
        fill = color,
        show.legend = show.legend
      ) +
      geom_text(
        hjust = 0, nudge_y = 0.0005,
        size = words_size,
        family = words_family
      ) +
      coord_flip()
  }
}
