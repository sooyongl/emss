# source

# helper ---------------------------------------------------------------
extract_num <- function(vectorInp){
  as.numeric(str_extract(vectorInp, "[[:digit:]]"))
}

# cal
calCountWeight <- function(new_data, n_cut = 3, d_alpha = 1, SD = 1, empirical = T, EC = 0) {

  new_data <- new_data %>%
    mutate(ALD = as.numeric(str_remove(ALD, "Level")))

  temp1 <- map(2:n_cut,
               ~ .calCW(new_data, d_alpha, cut_level = .x, SD, EC = EC, empirical = empirical))

  incon_default <- map(temp1, ~ .x[["incon_default"]]) %>%
    map2(., 2:n_cut,~ .x %>% set_names("cut_score", paste0("L",.y, "_C"),paste0("L",.y,"_W"))) %>%
    bind_cols(.name_repair = "minimal") %>%
    select(-cut_score) %>%
    select(paste0("L",2:n_cut,"_C"), everything()) %>%
    data.frame()

  return(incon_default)
}

#' ESS estimate for Count and Weight
#'
.calCW <- function(new_data, d_alpha = 1, cut_level = 2, SD = 1, empirical = T, EC = 0) {
  # new_data = new_data_0; cut_level = 2

  new_data$OOD <- 1:dim(new_data)[1]

  if(empirical) {
    cut_candi <- new_data$location
  } else {
    cut_candi <- seq(min(new_data$location), max(new_data$location), by = 1)
  }

  cut_candi_id <- rep(1:length(cut_candi), each = dim(new_data)[1])
  cut_candi_list <- rep(cut_candi, each = dim(new_data)[1])

  dt <- data.table(new_data)
  dt.expanded <- dt[ ,list(freq=1:length(cut_candi)), by=names(dt)][order(freq, OOD)][ ,freq := NULL]

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



cal_minp <- function(data_inp) {
  est_lev <- data_inp
  minimun_points <-
    foreach(el = 1:ncol(est_lev)) %do% {
      # el = 1
      which(est_lev[el] == min(est_lev[el]))
    }
  o.ver <- matrix(
    unlist(cross(minimun_points[1:(length(minimun_points)/2)])),
    ncol = (length(minimun_points)/2),
    byrow = T
  ) %>%
    data.frame(.) %>%
    set_names(.,
              names(est_lev)[1:(length(est_lev)/2)])
  w.ver <- matrix(
    unlist(
      cross(
        minimun_points[(length(minimun_points)/2 + 1):length(minimun_points)]
      )
    ),
    ncol = (length(minimun_points)/2),
    byrow = T
  ) %>%
    data.frame(.) %>%
    set_names(.,
              names(est_lev)[
                (length(est_lev)/2 + 1):length(est_lev)
              ]
    )

  return(list(default = o.ver, weight = w.ver))
}

#' Select the minimum point and cut score
#'
select_cp <- function(cut_point, est_cutscore, WESS, gamest){
  # est_cutscore=cut_scores
  # WESS=information$base_data$WESS
  # gamest=information$base_data$gamest
  level_names <- map(cut_point, names)
  l_names <- level_names[["default"]]
  w_names <- level_names[["weight"]]

  data_use_1 <- est_cutscore

  if(!gamest) {
    if(WESS){
      cut_point <- cut_point[["weight"]]
    } else{
      cut_point <- cut_point[["default"]]
    }

    cut_candi <- cut_point

    ppp <-
      foreach(ii = 1:ncol(cut_candi), .combine = 'c') %do% {
        # ii <- 1
        li <- l_names[ii]
        wi <-  w_names[ii]
        cp1 <- unique(unlist(cut_candi[,ii]))

        pp <-
          data_use_1 %>%
          slice(cp1) %>%
          arrange(!!as.name(wi), !!as.name(li)) %>%
          slice(1) %>%
          select(all_of(li), all_of(wi)) %>% data.frame() %>%
          unname() %>% unlist()

        which( data_use_1[[li]] ==  pp[1] & data_use_1[[wi]] ==  pp[2])[1]

      }
  } else {

    if(WESS){
      ppp <- unlist(cut_point[["weight_gam"]])
    } else{
      ppp <- unlist(cut_point[["default_gam"]])
    }
  }
  return(ppp = ppp)
}
#'
select_cs <- function(selected_CP, gam_res, location, WESS, gamest) {

  est_cut_score <- if(gamest) {
    if(WESS) {
      unlist(gam_res$cp$weight_gam)
    } else {
      unlist(gam_res$cp$default_gam)
    }
  } else {
    temp <- location[selected_CP]
    if(WESS) {
      names(temp) <- paste0(2:(length(temp)+1), "_W")
    } else {
      names(temp) <- paste0(2:(length(temp)+1), "_C")
    }
    temp
  }
  est_cut_score
}
#'
select_weight <- function(selected_CP, gam_res,cut_scores, gamest) {
  # gamest=information$base_data$gamest
  if(!gamest) {
    weights <- cut_scores[selected_CP, ]
    w_pos <- which(str_detect(names(weights), "_W"))
    w_name <- names(weights[ ,w_pos])
    weights <- diag(as.matrix(weights[ ,w_pos]))
    names(weights) <- w_name
  } else {
    weights <- unlist(gam_res$gam.cs$weight_gam_cs)
  }

  return(weights)
}

#' Estimate a generalized additive model (GAM) for ESS cut scores
#'
splineFit <- function(data_inp, information = NULL) {
  # data_inp <- cut_scores %>% mutate(!!locnm := location[[locnm]])
  # data_inp <- tbl_data

  if(is.null(information)) {
    loc_name <- names(data_inp)[ncol(data_inp)]

    cs_candi <- data_inp[[loc_name]]

    # if(cs_candi[length(cs_candi)] - cs_candi[1] < 10) {
    #   a1 <- seq(cs_candi[1], cs_candi[length(cs_candi)], by = 0.01)
    #   a1 <- length(a1)
    # } else {
    #   a1 <- length(cs_candi[1]:cs_candi[length(cs_candi)])
    # }
    a1 <- 200

    knots <- ifelse(nrow(data_inp) > 20, 10, round(nrow(data_inp)/2, 0))
    inp_vars <- names(data_inp)
    container <- list()
    for(i in 1:(length(inp_vars)-1)){ # i = 1

      inp_formula <- glue("{inp_vars[i]} ~ s({loc_name}, k = {knots})")

      fit_w <- gam(formula(inp_formula), data = data_inp, family = "gaussian")

      pred_w <- predict_gam(fit_w, length_out = a1)
      pred_w$`Cut Level` <- inp_vars[i]

      min_c <- pred_w %>% filter(fit==min(fit)) # empirical_min

      gam_min <- tibble(x = min_c[[1]], y = min_c[[2]], `Cut Level` = inp_vars[i], row.names = NULL)

      container[[(i)]] <- list(pred_w = pred_w, gam_min = gam_min)
    }

    res_pred <-
      map(container, ~ .x$pred_w) %>%
      bind_rows() %>%
      set_names(c(loc_name, "Inconsistency", "se", "Cut Level")) %>%
      mutate_at(vars(matches(loc_name)), round, 0)

    res_min <-
      map(container, ~ .x$gam_min) %>%
      bind_rows()

    res_cut_score <-
      data_inp %>%
      gather("Cut Level", "fit", -!!as.name(loc_name)) %>%
      left_join(res_min, by = "Cut Level") %>%
      mutate(a1 = x - !!as.name(loc_name)) %>%
      filter(a1 <=0 ) %>%
      group_split(`Cut Level`) %>%
      map(., ~ .x %>% head(1)) %>%
      bind_rows() %>%
      rename("selected_cs" = all_of(loc_name)) %>%
      select(selected_cs, `Cut Level`)

    res_selected <-
      data_inp %>%
      gather("Cut Level", "fit", -!!as.name(loc_name)) %>%
      left_join(res_cut_score, by = "Cut Level") %>%
      group_by(`Cut Level`) %>%
      summarise(
        selected_cp = which(abs(!!as.name(loc_name) - selected_cs)  == 0)[1]
      )

    res <- list(gam_pred = res_pred,
                gam_min = res_min,
                gam_selected_cs = res_cut_score,
                gam_selected_cp = res_selected)

  } else {
    loc_name <- information$base_data$loc_nm
    WESS <- information$base_data$WESS

    a1 <- data_inp %>% pull(loc_name)
    a1 <- length(a1)

    if(WESS) {
      data_inp <- data_inp %>% select(all_of(loc_name), matches("^L[1-9]_W"))

    } else {
      data_inp <- data_inp %>% select(loc_name, matches("^L[1-9]_C$"))
    }

    knots <- ifelse(nrow(data_inp) > 20, 10, round(nrow(data_inp)/2, 0))
    inp_vars <- names(data_inp)
    container <- list()
    container0 <- list()
    for(i in 2:length(inp_vars)){
      inp_formula <- glue("{inp_vars[i]} ~ s({loc_name}, k = {knots})")

      fit_w <- gam(formula(inp_formula),
                   data = data_inp, family = "gaussian")

      pred_w <- predict_gam(fit_w, length_out = a1)
      pred_w$`Cut Level` <- inp_vars[i]

      container[[(i-1)]] <- pred_w
      container0[[(i-1)]] <- fit_w
    }
    res <- do.call('rbind', container)
    res0 <- container0
    names(res) <- c(loc_name, "Inconsistency", "se", "Cut Level")

    res <- list(gam_value = res, gam_fit = res0)
  }

  res
}

extractgam <- function(gam_est) {

  a1 <-
    gam_est$gam_min %>%
    select(-x) %>%
    spread("Cut Level", "y") %>%
    mutate_all(round, 2)

  gam.def.cs <- a1 %>% select(!matches("_W"))
  gam.wei.cs <- a1 %>% select(matches("_W"))

  gam.cs <- list(default_gam_cs = gam.def.cs, weight_gam_cs = gam.wei.cs)

  a1 <-
    gam_est$gam_min %>%
    select(-y) %>%
    spread("Cut Level", "x") %>%
    mutate_all(round, 2)

  gam.def.cp <- a1 %>% select(!matches("_W"))
  gam.wei.cp <- a1 %>% select(matches("_W"))

  cp <- list(default_gam = gam.def.cp, weight_gam = gam.wei.cp)

  a2 <- gam_est$gam_selected_cp %>% spread('Cut Level', "selected_cp")

  gam.def.scp <- a2 %>% select(!matches("_W"))
  gam.wei.scp <- a2 %>% select(matches("_W"))

  scp <- list(default_gam = gam.def.scp, weight_gam = gam.wei.scp)

  list(cp = cp, scp = scp, gam.cs = gam.cs)
}


#' Estimate the correlation
#'
getCor <- function(inp_data, selected_CP, lvname) {
  # inp_data; data_2; selected_CP; lv_vector; locnm; ald_vector

  # inp_data
  # selected_CP

  ald_vector <- inp_data$ALD
  loc_vector <- inp_data$location

  op_num <- rep(0, nrow(inp_data))
  op_num[ selected_CP  ] <- 1

  Operational_name <- get_opname1(inp_data, lvname, op_num)

  ald_num <- match(ald_vector, lvname)
  olv_num <- match(Operational_name, lvname)

  cor_inc <-
    tryCatch({
      cor(ald_num, loc_vector)
    },
    warning = function(w) {0},
    error = function(w) {0}
    )


  list(cor_inc, Operational_name)
}

#' get_opname1
get_opname1 <- function(datainp, lvname, opnum){

  # lvname = c("Level1", "Level2", "Level3")
  eff_name <- names(datainp)
  target_filter = "ALD"

  item_start <- 1
  cut_point <- which(opnum==1)
  num_item <-length(opnum)

  operational_1 <- c(item_start, cut_point, num_item)
  Operational_name <- rep(lvname[length(lvname)], length(opnum))

  for(i in 1:(length(cut_point)+1)){
    # i <- 2
    if(i == (length(cut_point)+1)) {
      Operational_name[operational_1[i]:(operational_1[(i+1)])] <- lvname[i]

    } else {
      Operational_name[operational_1[i]:(operational_1[(i+1)]-1)] <- lvname[i]
    }
  }

  return(Operational_name)
}
