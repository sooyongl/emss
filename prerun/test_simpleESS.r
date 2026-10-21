rm(list = ls())
library(tidyverse)
library(data.table)
library(foreach)
library(glue)
library(mgcv)
library(tidymv)
library(ggrepel)
library(grid)
library(plotly)
root <- rprojroot::find_rstudio_root_file()

source_files <- fs::dir_ls(file.path(root, "R"))
data_path <- file.path(root, "prerun/data")

# library(EmStanS)
# for(i in 1:length(source_files)) { source(source_files[i])}

filePath <- fs::dir_ls(data_path)[1]
data <- read.csv(filePath)
data[[1]] <- 1:nrow(data)
names(data) <- c("OOD","location","ALD")


# Test run ------------------------------------------------

a1 <- emstans(data = data, lvname = c("Level1", "Level2", "Level3"),
              WESS = T, GAM = T)
a1[[1]]
a1[[2]]
a1[[3]]

launchEmStanS()

# Bootstrap test --------------------------------------------
boot_emstans <- function(data, lvname = NULL,
                         WESS = T, GAM = T,
                         n_rep = 10, keep.sample = F, empirical = F) {

  boot_sample <-
    data %>%
    sample_frac(., size = 1, replace = T) %>%
    arrange(location)


  boot_res<- lapply(1:n_rep, function(bi) {
    # Cut scores
    cut_info <- calCountWeight(new_data = data, n_cut = length(lvname), empirical = empirical)

    # GAM fitting
    if(WESS){
      cut_res <- cut_info %>% select(matches("_W$"), location)
      lv_nm <- paste0("L",(2):n_level, "_W")
    } else {
      cut_res <- cut_info %>% select(matches("_C$"), location)
      lv_nm <- paste0("L",(2):n_level, "_C")
    }

    lv <- names(cut_res)[which(names(cut_res) != "location")]
    # lv <- names(cut_res)[1:(n_level-1)]
    knots <- ifelse(nrow(cut_res) > 20, 10, round(nrow(cut_res)/2, 0))
    model_fit <- lapply(lv, function(x) {
      gam(formula(glue("{x} ~ s(location, k = {knots})")),
          data = cut_res, family = "gaussian")
    })

    pred_fit <- map(model_fit, ~ .x %>% predict_gam(length_out = a1))

    minimizer <- function(model_pred) {
      min_point <- which.min(model_pred$fit)
      y_point <- model_pred$fit[min_point]
      x_point <- model_pred$location[min_point]

      c(x_point, y_point)
    }

    res <-
      map(pred_fit, minimizer) %>%
      do.call('rbind', .) %>%
      data.frame() %>%
      set_names(c("x", "y")) %>%
      mutate(lv_nm = lv_nm) %>%
      mutate_if(is.numeric, round, 3)

    res
  })

  boot_res <- bind_rows(boot_res, .id = "boot_id")

  boot_res

}







# Test run ------------------------------------------------
SD = 1
EC = 0
digits = 3
WESS = T
GAM  = T
lvname = c("Level1", "Level2", "Level3")

a1 <- emstans(data, lvname = c("Level1", "Level2", "Level3"))
a1


emstans <- function(data, lvname = NULL, WESS = T, GAM = T,
                    SD = 1, EC = 0, digits= 3) {

  message("Note. Input data must be ordered by OOD, location, and ALD")

  names(data) <- c("OOD","location","ALD")

  if(is.null(lvname)) {
    if(!is.numeric(data$ALD)){
      stop("Levels must be specified! ex. Level1, Level2, Level3\nor, must be a numeric vector. ex. 1,2,3")
    } else {
      data$ALD <- paste0("Level", data$ALD)
    }
  }

  cut_scores <- calCountWeight(data)

  cut_scores <-
    cut_scores %>%
    mutate_at(vars(matches("_W$")), ~ .x /SD) %>%
    mutate_all(round, digits)

  cut_point  <- cal_minp(cut_scores)

  gam_est <- splineFit(cut_scores %>% mutate(location = data$location))
  gam_res <- extractgam(gam_est)

  cut_point <- append(cut_point, gam_res[["scp"]])

  selected_CP <- select_cp(cut_point, cut_scores, WESS,GAM)
  selected_CS <- select_cs(selected_CP, gam_res, new_data$location, WESS, GAM)

  selected_weights <- select_weight(selected_CP, gam_res, cut_scores, GAM)

  data_2 <- data %>% bind_cols(., cut_scores)

  cor_inc <- getCor(data_2, selected_CP, lvname)

  data2 <-
    data_2 %>%
    mutate(Operational_Lv = cor_inc[[2]])

  cutpoint_inp <- selected_CP
  dataUse_1 <- data2

  p <- plotting(dataUse_1, selected_CP, selected_CS, selected_weights, WESS, GAM, gam_est)

  ess_table <- dataUse_1 %>%
    select(OOD, location, ALD, Operational_Lv,
           ends_with("_C"),ends_with("_W")) %>%
    rename("Aligned_Lvl" = "ALD")

  # dataUse_Weight <-
  #   dataUse_1 %>%
  #   mutate(
  #     weightSum = dplyr::select(., ends_with("_W")) %>% rowSums()
  #   ) %>% pull(weightSum)

  target_names <- dataUse_1$ALD
  lv_name <- target_names %>% unique() %>% sort()
  #
  given_n   <- c(1:length(lv_name))
  length_n  <- ifelse(length(given_n) == 1, 2, length(given_n))
  targets_n <- match(target_names, lv_name)

  er <- length(target_names)
  review_table <-
    foreach(n = 2:length_n, .combine = 'rbind') %do% {
      # n = 2
      cr <- cutpoint_inp[(n-1)]

      cr_abo <- cr
      cr_bel <- cr

      c_bel <- which(targets_n[0:(cr - 1)] >= n)
      c_abo <- (which(targets_n[cr_abo:er] < n) - 1) + cr_abo

      a1 <- dataUse_1[c(c_bel, c_abo), ] %>%
        mutate(item_status = "inc")
      a2 <- dataUse_1[-c(c_bel, c_abo), ] %>%
        mutate(item_status = "cons")

      a3 <- bind_rows(a2, a1) %>% arrange(OOD)
      a3
    }

  # review_table <-
  #   review_table  %>%
  #   mutate(.,
  #          Weight = dplyr::select(., ends_with("_W")) %>% rowSums()
  #   )


  cs_inf <- data2 %>%
    slice(selected_CP) %>%
    select(OOD, ALD, location) %>%
    mutate(ALD = lvname[-1]) %>%
    rename(
      "Cut_Score" = "location"
    ) %>% select(-OOD)

  cs_inf_upper <-
    cs_inf %>%
    mutate(
      ALD = extract_num(ALD),
      ALD = paste0("Level", (ALD - 1))
    ) %>%
    rename(
      "Cut_Score_upper" = "Cut_Score"
    )


  review_table <- review_table %>%
    left_join(., cs_inf, by = c("ALD" = "ALD")) %>%
    left_join(., cs_inf_upper, by = c("ALD" = "ALD")) %>%
    mutate(AN = extract_num(ALD),
           ON = extract_num(Operational_Lv),
           Diff_LV = AN - ON,
           Distance = case_when(
             AN > ON ~ location - Cut_Score,
             AN < ON ~ location - (Cut_Score_upper - 1),
             AN == ON ~ 0
           ),
           `Std. Abs. Distance` = abs(round(Distance / SD, 3)),
           item_status = case_when(
             abs(Distance) <= EC & abs(Distance) > 0 ~ "Essentially Consistent",
             abs(Distance) > EC ~ "Inconsistent",
             abs(Distance) == 0 ~ "Consistent"
           ),
           item_status = factor(item_status)

    ) %>%
    select(-matches("L[[:digit:]]+"), -AN, -ON) %>%
    rename(
      "Aligned_Lvl" = "ALD",
      "Operational_Lvl" = "Operational_Lv",
      "Lvl_Diff" = "Diff_LV"
    )


  o <- list(ess_table = ess_table, review_table = review_table, ess.plot = p)
  class(o) <- append("ess", class(o))
  # o <- structure(o, class = "ess")

  o
}
a1 <- emstans(data, lvname = c("Level1", "Level2", "Level3"))


print.ess <- function(x) {
  list(
    ess_table = head(x$ess_table),
    review    = head(x$review)
  )
}

print(o)
