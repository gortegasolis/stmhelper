## MultiSearchK
MultiSearchK <- function(data,
                         documents,
                         vocab,
                         list_searchK = NULL,
                         times = 10,
                         from = 2,
                         to = 50,
                         by = 10,
                         init.type = "Spectral",
                         ncores = 1){
  if (is.null(list_searchK) == TRUE){
    temp_list <- list()
    a <- 1
  } else{
    temp_list <- list_searchK
    a <- NROW(temp_list)
    times <- a+times
}
  for(x in a:times){
    searchK(data = data,
            documents = documents,
            vocab = vocab,
            K = seq(from = from,
                    to = to,
                    by = by),
            init.type = init.type,
            cores = ncores) -> temp

temp_list[[x]] <- temp
  }
  return(temp_list)
}


## plot.multisearchK
plot.multisearchK <- function(data,
                              n = 100,
                              span = 0.5,
                              facet = TRUE,
                              points = TRUE,
                              stat_method = NULL,
                              param = NULL,
                              vertical_line = NULL,
                              area.min.k = NULL,
                              area.max.k = NULL){
  if (is.list(data) == TRUE){

    temp_df <- data.frame()

    for (x in 1:NROW(data)){
      temp_df <- plyr::rbind.fill(temp_df,data[[x]]$results)
    }

    data <- temp_df
  }

  if(isTRUE(facet) == FALSE){

    data$exclus <- data$exclus %>%
      scales::rescale()
    data$semcoh <- data$semcoh %>%
      scales::rescale()
    data$heldout <- data$heldout %>%
      scales::rescale()
    data$lbound <- data$lbound %>%
      scales::rescale()
    data$bound <- data$bound %>%
      scales::rescale()
    data$residual <- data$residual %>%
      scales::rescale()


    data %>%
      pivot_longer(-K, names_to = "Metric", values_to = "score") %>%
      filter(Metric %in% param) %>%
      ggplot(.,
             aes(x = K,
                 y = score,
                 fill = Metric,
                 colour = Metric)) +
      stat_smooth(method = stat_method) +
      xlab("K") +
      ylab("Scaled-score") -> plot

  }

  if(isTRUE(facet) == TRUE){
    data %>%
      pivot_longer(-K, names_to = "Metric", values_to = "score") %>%
      filter(Metric %in% param) %>%
      ggplot(.,
             aes(x = K,
                 y = score,
                 fill = Metric,
                 colour = Metric)) +
      stat_smooth(method = stat_method) +
      facet_wrap(~Metric, scales = "free", ncol = 2) +
      xlab("K") +
      ylab("Score") -> plot
  }

if (isTRUE(points) == TRUE){
  plot +
    geom_point() -> plot
}

    if(is.numeric(vertical_line) == TRUE){
      plot +
        geom_vline(xintercept = vertical_line) -> plot
    }

    if(is.numeric(area.min.k) == TRUE){
      plot +
        annotate(geom = "rect",
                 xmin = area.min.k,
                 xmax = area.max.k,
                 ymin = 0,
                 ymax = 1,
                 alpha = 0.2,
                 fill = "red") -> plot
    }

  plot
}
