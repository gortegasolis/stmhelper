### Plot manyTopics results
plot.manyTopics <- function(model,
                            size.c = 5,
                            vline = NULL,
                            hline = NULL,
                            count = NULL,
                            scale = NULL) {

  ## Create main dataframe
  foreach(
    x = 1:NROW(model[["out"]]),
    .combine = rbind
  ) %do%
    data.frame(
      "Model" = paste0(x),
      "K" = NROW(model[["exclusivity"]][[x]]),
      "Topic" = c(1:NROW(model[["exclusivity"]][[x]])),
      "Exclusivity" = model[["exclusivity"]][[x]],
      "semcoh" = model[["semcoh"]][[x]]
    ) -> df


  ## Scale exclusivity and semantic coherence standard deviations
  if (scale == "sd") {
    df$Exclusivity <- scale(df$Exclusivity)
    df$semcoh <- scale(df$semcoh)
  }

  ## Scale exclusivity and semantic coherence to deviations around the median
  if (scale == "mad") {
    df$Exclusivity <- scale(df$Exclusivity, center = median(df$Exclusivity))
    df$semcoh <- scale(df$semcoh, center = median(df$semcoh))
  }

  ## Create main plot
  ggplot(df, aes(
    x = semcoh,
    y = Exclusivity,
    group = K
  )) +
    geom_point(aes(
      shape = as.factor(K),
      colour = as.factor(K)
    )) +
    xlab("Semantic coherence") +
    scale_shape_manual(values = c(1:NROW(model[["out"]]))) +
    scale_color_manual(values = c(1:NROW(model[["out"]]))) -> plot

  ## Count topics in top-right quadrant
  if (count == "topright") {
    df %>%
      select(Model, K, Exclusivity, semcoh) %>%
      filter(., Exclusivity > 0, semcoh > 0) %>%
      group_by(K) %>%
      tally(., name = "Top-right count") -> topright.df

    topright.df$`Top-right proportion` <- topright.df$`Top-right count` / topright.df$K
    topright.df$Rank <- rank(-topright.df$`Top-right proportion`, ties.method = "min")

    View(topright.df)
  }

  ## Count topics in bottom-left quadrant
  if (count == "bottomleft") {
    df %>%
      select(Model, K, Exclusivity, semcoh) %>%
      filter(., Exclusivity < 0, semcoh < 0) %>%
      group_by(Model, K) %>%
      tally(., name = "Bottom-left count") -> bottomleft.df

    bottomleft.df$`Bottom-left proportion` <- bottomleft.df$`Bottom-left count` / bottomleft.df$K
    bottomleft.df$Rank <- rank(bottomleft.df$`Bottom-left proportion`, ties.method = "min")

    View(bottomleft.df)
  }

  ## Rank topics combining top-right and bottomleft results
  if (count == "both") {
    df %>%
      select(Model, K, Exclusivity, semcoh) %>%
      filter(., Exclusivity > 0, semcoh > 0) %>%
      group_by(Model, K) %>%
      tally(., name = "Top-right count") -> topright.df

    topright.df$`Top-right proportion` <- topright.df$`Top-right count` / topright.df$K
    topright.df$`Top-right rank` <- rank(-topright.df$`Top-right proportion`, ties.method = "min")

    df %>%
      select(Model, K, Exclusivity, semcoh) %>%
      filter(., Exclusivity < 0, semcoh < 0) %>%
      group_by(Model, K) %>%
      tally(., name = "Bottom-left count") -> bottomleft.df

    bottomleft.df$`Bottom-left proportion` <- bottomleft.df$`Bottom-left count` / bottomleft.df$K

    combined.rank.df <- left_join(topright.df, bottomleft.df, by = c("Model", "K"))
    if (exists("centroids.df") == T) {
      combined.rank.df <- left_join(combined.rank.df, centroids.df, by = c("Model", "K"))
    }
    combined.rank.df[is.na(combined.rank.df)] <- 0
    combined.rank.df$`Bottom-left rank` <- rank(combined.rank.df$`Bottom-left proportion`, ties.method = "min")
    combined.rank.df$`Combined rank` <- cbind(combined.rank.df$`Top-right rank`, combined.rank.df$`Bottom-left rank`) %>%
      rowMeans() %>%
      rank(-., ties.method = "min")
    combined.rank.df$`Count proportion` <- combined.rank.df$`Bottom-left count` / combined.rank.df$`Top-right count`

    View(combined.rank.df)
  }

  ## Add vertical line
  if (is.numeric(vline) == T) {
    plot +
      geom_vline(xintercept = vline) -> plot
  }

  ## Add horizontal line
  if (is.numeric(hline) == T) {
    plot +
      geom_hline(yintercept = hline) -> plot
  }

  plot +
    labs(colour = "K", shape = "K")
}
