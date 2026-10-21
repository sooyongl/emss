#' #' Estimate a generalized additive model (GAM) for ESS cut scores
#' #'
#' splineFit <- function(data_inp, information = NULL) {
#'   # data_inp <- cut_scores %>% mutate(!!locnm := location[[locnm]])
#'   # data_inp <- tbl_data
#'
#'   if(is.null(information)) {
#'     loc_name <- names(data_inp)[ncol(data_inp)]
#'
#'     cs_candi <- data_inp[[loc_name]]
#'
#'     # if(cs_candi[length(cs_candi)] - cs_candi[1] < 10) {
#'     #   a1 <- seq(cs_candi[1], cs_candi[length(cs_candi)], by = 0.01)
#'     #   a1 <- length(a1)
#'     # } else {
#'     #   a1 <- length(cs_candi[1]:cs_candi[length(cs_candi)])
#'     # }
#'     a1 <- 200
#'
#'     knots <- ifelse(nrow(data_inp) > 20, 10, round(nrow(data_inp)/2, 0))
#'     inp_vars <- names(data_inp)
#'     container <- list()
#'     for(i in 1:(length(inp_vars)-1)){ # i = 1
#'
#'       inp_formula <- glue("{inp_vars[i]} ~ s({loc_name}, k = {knots})")
#'
#'       fit_w <- gam(formula(inp_formula), data = data_inp, family = "gaussian")
#'
#'       pred_w <- predict_gam(fit_w, length_out = a1)
#'       pred_w$`Cut Level` <- inp_vars[i]
#'
#'       min_c <- pred_w %>% filter(fit==min(fit)) # empirical_min
#'
#'       gam_min <- tibble(x = min_c[[1]], y = min_c[[2]], `Cut Level` = inp_vars[i], row.names = NULL)
#'
#'       container[[(i)]] <- list(pred_w = pred_w, gam_min = gam_min)
#'     }
#'
#'     res_pred <-
#'       map(container, ~ .x$pred_w) %>%
#'       bind_rows() %>%
#'       set_names(c(loc_name, "Inconsistency", "se", "Cut Level")) %>%
#'       mutate_at(vars(matches(loc_name)), round, 0)
#'
#'     res_min <-
#'       map(container, ~ .x$gam_min) %>%
#'       bind_rows()
#'
#'     res_cut_score <-
#'       data_inp %>%
#'       gather("Cut Level", "fit", -!!as.name(loc_name)) %>%
#'       left_join(res_min, by = "Cut Level") %>%
#'       mutate(a1 = x - !!as.name(loc_name)) %>%
#'       filter(a1 <=0 ) %>%
#'       group_split(`Cut Level`) %>%
#'       map(., ~ .x %>% head(1)) %>%
#'       bind_rows() %>%
#'       rename("selected_cs" = all_of(loc_name)) %>%
#'       select(selected_cs, `Cut Level`)
#'
#'     res_selected <-
#'       data_inp %>%
#'       gather("Cut Level", "fit", -!!as.name(loc_name)) %>%
#'       left_join(res_cut_score, by = "Cut Level") %>%
#'       group_by(`Cut Level`) %>%
#'       summarise(
#'         selected_cp = which(abs(!!as.name(loc_name) - selected_cs)  == 0)[1]
#'       )
#'
#'     res <- list(gam_pred = res_pred,
#'                 gam_min = res_min,
#'                 gam_selected_cs = res_cut_score,
#'                 gam_selected_cp = res_selected)
#'
#'   } else {
#'     loc_name <- information$base_data$loc_nm
#'     WESS <- information$base_data$WESS
#'
#'     a1 <- data_inp %>% pull(loc_name)
#'     a1 <- length(a1)
#'
#'     if(WESS) {
#'       data_inp <- data_inp %>% select(all_of(loc_name), matches("^L[1-9]_W"))
#'
#'     } else {
#'       data_inp <- data_inp %>% select(loc_name, matches("^L[1-9]_C$"))
#'     }
#'
#'     knots <- ifelse(nrow(data_inp) > 20, 10, round(nrow(data_inp)/2, 0))
#'     inp_vars <- names(data_inp)
#'     container <- list()
#'     container0 <- list()
#'     for(i in 2:length(inp_vars)){
#'       inp_formula <- glue("{inp_vars[i]} ~ s({loc_name}, k = {knots})")
#'
#'       fit_w <- gam(formula(inp_formula),
#'                    data = data_inp, family = "gaussian")
#'
#'       pred_w <- predict_gam(fit_w, length_out = a1)
#'       pred_w$`Cut Level` <- inp_vars[i]
#'
#'       container[[(i-1)]] <- pred_w
#'       container0[[(i-1)]] <- fit_w
#'     }
#'     res <- do.call('rbind', container)
#'     res0 <- container0
#'     names(res) <- c(loc_name, "Inconsistency", "se", "Cut Level")
#'
#'     res <- list(gam_value = res, gam_fit = res0)
#'   }
#'
#'   res
#' }
#'
#' #' Calculate counts and weights by GAM
#' #'
#' extractgam <- function(gam_est) {
#'
#'   a1 <-
#'     gam_est$gam_min %>%
#'     select(-x) %>%
#'     spread("Cut Level", "y") %>%
#'     mutate_all(round, 2)
#'
#'   gam.def.cs <- a1 %>% select(!matches("_W"))
#'   gam.wei.cs <- a1 %>% select(matches("_W"))
#'
#'   gam.cs <- list(default_gam_cs = gam.def.cs, weight_gam_cs = gam.wei.cs)
#'
#'   a1 <-
#'     gam_est$gam_min %>%
#'     select(-y) %>%
#'     spread("Cut Level", "x") %>%
#'     mutate_all(round, 2)
#'
#'   gam.def.cp <- a1 %>% select(!matches("_W"))
#'   gam.wei.cp <- a1 %>% select(matches("_W"))
#'
#'   cp <- list(default_gam = gam.def.cp, weight_gam = gam.wei.cp)
#'
#'   a2 <- gam_est$gam_selected_cp %>% spread('Cut Level', "selected_cp")
#'
#'   gam.def.scp <- a2 %>% select(!matches("_W"))
#'   gam.wei.scp <- a2 %>% select(matches("_W"))
#'
#'   scp <- list(default_gam = gam.def.scp, weight_gam = gam.wei.scp)
#'
#'   list(cp = cp, scp = scp, gam.cs = gam.cs)
#' }
