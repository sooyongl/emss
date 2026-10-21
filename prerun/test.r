rm(list = ls())
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(foreach)
library(stringi)
library(DT)
library(knitr)
library(kableExtra)
library(readxl)
library(officer)
library(flextable)
library(shinycssloaders)
library(waiter)

root <- rprojroot::find_rstudio_root_file()

source_files <- fs::dir_ls(file.path(root, "R"))
data_path <- file.path(root, "prerun/data/OneDrive_1_7-13-2022")


for(i in 1:length(source_files)) { source(source_files[i])}

filePath <- fs::dir_ls(data_path)[1]


# Test run ------------------------------------------------
new_data <- read.csv(filePath)

new_data$ALD <- factor(new_data$ALD, levels = paste0("Level",1:3))
new_data$ALD <- as.character(
  factor(new_data$ALD,
         labels = paste0("aaaaa",1:length(lvname))))

count_weight <- calESS(new_data)

cut_scores <-
  cut_scores %>%
  mutate_at(vars(matches("_W$")), ~ .x /SD_inp) %>%
  mutate_all(round, digits)

cut_point  <- cal_minp(cut_scores)

gam_est <- splineFit(cut_scores %>% mutate(!!locnm := location[[locnm]]))

gam_res <- extractgam(gam_est)
cut_point <- append(cut_point, gam_res[["scp"]])

selected_CP <- select_cp(cut_point, cut_scores, information$base_data$WESS,information$base_data$gamest)

selected_CS <- select_cs(selected_CP, gam_res, location, information$base_data$WESS, information$base_data$gamest)

selected_weights <- select_weight(selected_CP, gam_res,cut_scores, information$base_data$gamest)



# helper ------------------------------------------------------------------
# cal
calESS <- function(new_data, levels = NULL, n_cut=3, d_alpha = 1, SD = 1, empirical = T, EC = 0) {


  temp1 <- map(2:n_cut,
               ~ essCW(new_data, d_alpha, cut_level = .x, SD, EC = EC, empirical = empirical))

  incon_default <- map(temp1, ~ .x[["incon_default"]]) %>%
    map2(., 2:n_cut,~ .x %>% set_names("cut_score", paste0("L",.y, "_C"),paste0("L",.y,"_W"))) %>%
    bind_cols(.name_repair = "minimal") %>%
    select(-cut_score) %>%
    select(paste0("L",2:n_cut,"_C"), everything()) %>%
    data.frame()

  return(incon_default)
}
# ESS estimate for Count and Weight
essCW <- function(new_data, d_alpha = 1, cut_level = 2, SD = 1, empirical = T, EC = 0) {
  # new_data = new_data_0; cut_level = 2

  new_data$Item_ID <- 1:dim(new_data)[1]

  if(empirical) {
    cut_candi <- new_data$location
  } else {
    cut_candi <- seq(min(new_data$location), max(new_data$location), by = 1)
  }

  cut_candi_id <- rep(1:length(cut_candi), each = dim(new_data)[1])
  cut_candi_list <- rep(cut_candi, each = dim(new_data)[1])

  dt <- data.table(new_data)
  dt.expanded <- dt[ ,list(freq=1:length(cut_candi)), by=names(dt)][order(freq, Item_ID)][ ,freq := NULL]

  dt.expanded[, cut_score := cut_candi_list]
  dt.expanded[, cut_score_id := cut_candi_id]

  incon_default <- data.frame(dt.expanded)

  incon_default <- incon_default %>% data.table() %>% .[, above_ecd := abs(location - cut_score)^d_alpha >= 0]
  incon_default[, below_ecd := abs(location - cut_score)^d_alpha >= 0]

  incon_default[, above_c := location < cut_score & ALD >= cut_level]
  incon_default[, below_c := location >= cut_score & ALD < cut_level]

  incon_default[above_c == T, above_d := abs(location - cut_score)^d_alpha]
  incon_default[below_c == T, below_d := abs(location - cut_score)^d_alpha]

  incon_default <- incon_default[, .(
    above_c = sum(above_c),
    below_c = sum(below_c),
    above_w = sum(above_d, na.rm = T),
    below_w = sum(below_d, na.rm = T)), by=.(cut_score_id)]

  incon_default <- incon_default[, .(
    cut_score = cut_candi,
    counts = above_c + below_c,
    weights = above_w + below_w)
  ]

  return(list(incon_default = incon_default))
}










# -------------------------------------------------------------------------
runEmStan <- function(data_list, grade = c("M3"),ald = "ALD",location = "Loc_RP67",WESS = F,modal = F,threshold = F) {



}

calWeight <- function(information) {



}

res <- runESS(
  data_list,
  grade = c("M3"),
  ald = "ALD",
  location = "Loc_RP67",
  WESS = F,
  modal = F,
  threshold = F
)

res <- updateESS(
  ess_class = res,
  manual_cp = list(c(2,7,10))
)

# Individually -----------------------------------------------
imprt_data <- read_data(filePath)
data_list <- data_ready(imprt_data)
####################################################
information <-
  get_data_info(
    data_list,
    grade = c("M3"),
    ald = "ALD",
    location = "Loc_RP67",
    WESS = F,
    modal = F,
    threshold = F
  )

####################################################
# tab0 <- list()
tab0 <- gen_tab0(information)

####################################################
# tab1 <- list()
tab1 <- gen_tab1(tab0, information)

#############################################################
for_report <- list()
for_report$indi <-
  dt_table_out_indi(tab1$indi_table, table_options_new_1)
for_report$med <-
  dt_table_out_med(tab1$median_table, table_options_new_2)
for_report$mode <-
  dt_table_out_mode(tab1$modal_table, table_options_new_2)
#############################################################
# Tab 2 - Generate TAB & output panels inside generated Tabs
tab2 <- gen_tab2(tab1, information)

# need to be fixed for update ------------------------------------------
manual_cp <- list(c(2,7,10))
tab1 <- update_tab1(tab0, tab1, information, manual_cp)
tab2 <- update_tab2(tab1, information)


for_tab2_out <- tab2$for_tab2_out

dataUse_1 <- for_tab2_out[[1]][[1]][["t_out"]]
tab2_table(dataUse_1, information$base_data$WESS)

eff_data <- for_tab2_out[[1]][[1]][["eff_data"]]
tab2_table_effpage(eff_data)

crosstabs <- for_tab2_out[[1]][[1]][["crosst"]]
tab2_table_crosst(crosstabs)

p1 <- for_tab2_out[[1]][[1]][["p1"]]

#############################################################
# Tab 3 - Summary of Cut Scores and impact data
tab3 <- gen_tab3(tab1, information)

tab3_table_pagetb(tab3)
tab3_plots(tab3)
#############################################################
# Tab 4 - Item Review Ready
tab4 <- gen_tab4(tab1, tab2, tab3, information)
tab4_table_review(tab4)
