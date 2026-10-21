# The original per-item modal ALD rule, kept as the reference for row_modes().
get_mode_reference <- function(x, corInf){
  # x <- ext_ALD[4,]
  # corInf <- ext_cors
  x <- x %>% unlist()

  cor_order <-
    data.frame(x, corInf) %>%
    dplyr::arrange(., dplyr::desc(corInf))

  a1 <- x
  a2 <- sort(table(a1), decreasing = T)

  if(sum(a2 == max(a2)) > 1){

    max_name <- names(which(a2 == max(a2)))

    a2 <-
      cor_order %>%
      dplyr::filter(x %in% max_name) %>%
      .[1,1]
  } else {
    modal_ALD <- names(a2)[1]
  }
}

