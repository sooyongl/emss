#' #' Estimate the operational levels and the correlation
#' #'
#' getCor <- function(inp_data, selected_CP, lvname) {
#'
#'   ald_vector <- inp_data$ALD
#'   loc_vector <- inp_data$location
#'
#'   op_num <- rep(0, nrow(inp_data))
#'   op_num[ selected_CP  ] <- 1
#'
#'   Operational_name <- get_opname(inp_data, lvname, op_num)
#'
#'   ald_num <- match(ald_vector, lvname)
#'   olv_num <- match(Operational_name, lvname)
#'
#'   cor_inc <-
#'     tryCatch({
#'       cor(ald_num, loc_vector)
#'     },
#'     warning = function(w) {0},
#'     error = function(w) {0}
#'     )
#'
#'
#'   list(cor_inc, Operational_name)
#' }
#'
#' #' Calculate operational levels
#' #'
#' get_opname <- function(datainp, lvname, opnum){
#'
#'   eff_name <- names(datainp)
#'   target_filter = "ALD"
#'
#'   item_start <- 1
#'   cut_point <- which(opnum==1)
#'   num_item <-length(opnum)
#'
#'   operational_1 <- c(item_start, cut_point, num_item)
#'   Operational_name <- rep(lvname[length(lvname)], length(opnum))
#'
#'   for(i in 1:(length(cut_point)+1)){
#'     # i <- 2
#'     if(i == (length(cut_point)+1)) {
#'       Operational_name[operational_1[i]:(operational_1[(i+1)])] <- lvname[i]
#'
#'     } else {
#'       Operational_name[operational_1[i]:(operational_1[(i+1)]-1)] <- lvname[i]
#'     }
#'   }
#'
#'   return(Operational_name)
#' }
