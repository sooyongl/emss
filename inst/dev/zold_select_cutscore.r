#' #' Calculate minimum points
#' #'
#' cal_minp <- function(data_inp) {
#'   est_lev <- data_inp
#'   minimun_points <-
#'     foreach(el = 1:ncol(est_lev)) %do% {
#'       which(est_lev[el] == min(est_lev[el]))
#'     }
#'   o.ver <- matrix(
#'     unlist(cross(minimun_points[1:(length(minimun_points)/2)])),
#'     ncol = (length(minimun_points)/2),
#'     byrow = T
#'   ) %>%
#'     data.frame(.) %>%
#'     set_names(.,
#'               names(est_lev)[1:(length(est_lev)/2)])
#'   w.ver <- matrix(
#'     unlist(
#'       cross(
#'         minimun_points[(length(minimun_points)/2 + 1):length(minimun_points)]
#'       )
#'     ),
#'     ncol = (length(minimun_points)/2),
#'     byrow = T
#'   ) %>%
#'     data.frame(.) %>%
#'     set_names(.,
#'               names(est_lev)[
#'                 (length(est_lev)/2 + 1):length(est_lev)
#'               ]
#'     )
#'
#'   return(list(default = o.ver, weight = w.ver))
#' }
#'
#' #' Select the minimum point and cut score
#' #'
#' select_cp <- function(cut_point, est_cutscore, WESS, gamest){
#'
#'   level_names <- map(cut_point, names)
#'   l_names <- level_names[["default"]]
#'   w_names <- level_names[["weight"]]
#'
#'   data_use_1 <- est_cutscore
#'
#'   if(!gamest) {
#'     if(WESS){
#'       cut_point <- cut_point[["weight"]]
#'     } else{
#'       cut_point <- cut_point[["default"]]
#'     }
#'
#'     cut_candi <- cut_point
#'
#'     ppp <-
#'       foreach(ii = 1:ncol(cut_candi), .combine = 'c') %do% {
#'         # ii <- 1
#'         li <- l_names[ii]
#'         wi <-  w_names[ii]
#'         cp1 <- unique(unlist(cut_candi[,ii]))
#'
#'         pp <-
#'           data_use_1 %>%
#'           slice(cp1) %>%
#'           arrange(!!as.name(wi), !!as.name(li)) %>%
#'           slice(1) %>%
#'           select(all_of(li), all_of(wi)) %>% data.frame() %>%
#'           unname() %>% unlist()
#'
#'         which( data_use_1[[li]] ==  pp[1] & data_use_1[[wi]] ==  pp[2])[1]
#'
#'       }
#'   } else {
#'
#'     if(WESS){
#'       ppp <- unlist(cut_point[["weight_gam"]])
#'     } else{
#'       ppp <- unlist(cut_point[["default_gam"]])
#'     }
#'   }
#'   return(ppp = ppp)
#' }
#' #' #' select by cut scores
#' #'
#' select_cs <- function(selected_CP, gam_res, location, WESS, gamest) {
#'
#'   est_cut_score <- if(gamest) {
#'     if(WESS) {
#'       unlist(gam_res$cp$weight_gam)
#'     } else {
#'       unlist(gam_res$cp$default_gam)
#'     }
#'   } else {
#'     temp <- location[selected_CP]
#'     if(WESS) {
#'       names(temp) <- paste0(2:(length(temp)+1), "_W")
#'     } else {
#'       names(temp) <- paste0(2:(length(temp)+1), "_C")
#'     }
#'     temp
#'   }
#'   est_cut_score
#' }
#'
#' #' select by weight
#' #'
#' select_weight <- function(selected_CP, gam_res,cut_scores, gamest, WESS) {
#'
#'   if(WESS) {
#'     if(!gamest) {
#'       weights <- cut_scores[selected_CP, ]
#'       w_pos <- which(str_detect(names(weights), "_W"))
#'       w_name <- names(weights[ ,w_pos])
#'       weights <- diag(as.matrix(weights[ ,w_pos]))
#'       names(weights) <- w_name
#'     } else {
#'       weights <- unlist(gam_res$gam.cs$weight_gam_cs)
#'     }
#'
#'
#'   } else {
#'     if(!gamest) {
#'       weights <- cut_scores[selected_CP, ]
#'       w_pos <- which(str_detect(names(weights), "_C"))
#'       w_name <- names(weights[ ,w_pos])
#'       weights <- diag(as.matrix(weights[ ,w_pos]))
#'       names(weights) <- w_name
#'     } else {
#'       weights <- unlist(gam_res$gam.cs$default_gam_cs)
#'     }
#'
#'   }
#'
#'   return(weights)
#' }
