fake_data <- genFakeDataSet(ngca = 3,
                            cor_val = 0.2,
                            n = 30,
                            nlevel = 3,
                            ntable = 5,
                            npanelist = 5,
                            sdinp = 1,
                            ecinp = 0,
                            100,300)

res <- emstans(data = fake_data,
               tests = c(1,2),
               targets = "ALD",
               WESS = T,
               gamest = T,
               median = "modal",
               loc = "Loc_RP50",
               domain = "GCA")



#' #' Update Manual Cut Points in an ESS Object
#' #'
#' #' Updates an existing `ESS` object by replacing or adding manually specified
#' #' cut points. Manual cut points are provided through `...` as named numeric
#' #' vectors, where each name identifies a model, domain, scale, or other
#' #' component used in the `ESS` object.
#' #'
#' #' @param output An object of class `ESS`.
#' #' @param ... Named numeric vectors of manual cut points. Each argument should
#' #'   be supplied in the form `name = c(lower, upper)` or
#' #'   `name = c(cp1, cp2, ...)`. For example, `M1 = c(7, 23)` specifies manual
#' #'   cut points for `M1`.
#' #'
#' #' @return An updated object of class `ESS` containing the supplied manual cut
#' #'   points.
#' #'
#' #' @details
#' #' The values supplied through `...` are captured internally as a named list
#' #' using `list(...)`. Therefore, all arguments in `...` should be named.
#' #' These names are used to match the provided manual cut points to the
#' #' corresponding elements of the `ESS` object.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' ess2 <- update.ESS(
#' #'   ess,
#' #'   M1 = c(7, 23),
#' #'   M2 = c(10, 18),
#' #'   M3 = c(5, 15, 25)
#' #' )
#' #' }
#' #'
#' #' @export
#' update.ESS <- function(output, ...) {
#'   # ... : Manual cuts corresponding to the panel
#'   # extract(res, "cutpoint")
#'
#'   updates <- list(...)
#'
#'   if (length(updates) == 0) {
#'     stop("No manual cut point values were provided.")
#'   }
#'
#'   if (is.null(names(updates)) || any(names(updates) == "")) {
#'     stop("All manual cut point values must be named, for example `M1-All` = c(7, 23).")
#'   }
#'
#'   information <- output$information
#'   tab0 <- output$tab0
#'   tab1 <- output$tab1
#'
#'   n.of.gca <- information$data_ready$id_list$GCA
#'   modal_selected_cp_all <- output$tab1$modal_selected_cp_all
#'   names(modal_selected_cp_all) <-
#'     # map(tab1$modal_est_cs_all, ~ unique(.x[[1]])) %>% unlist()
#'     n.of.gca
#'
#'   manual_cp <-
#'     modal_selected_cp_all[which(names(modal_selected_cp_all) %in% names(manual_cutpoint))] <- updates
#'
#'
#'   # Tab1 Update ----------------------------------------
#'   tab1 <-  update_tab1(tab0, tab1, information, manual_cp)
#'   output <- tab1_output(tab1, output)
#'
#'   # Tab2 Update ----------------------------------------
#'   tab2 <- update_tab2(tab1, information)
#'   output <- tab2_output(tab2, output)
#'
#'   ## Tab 3: Cut Score Summary --------------------------------------
#'   ## Summary of Cut Scores and impact data -----------------
#'   tab3 <-
#'     gen_tab3(tab1, information) %>%
#'     tab3_table_pagetb(.) %>%
#'     tab3_plots(.)
#'
#'   output <- tab3_output(tab3, output)
#'
#'
#'   ## Tab 4: Item Review Ready --------------------------------
#'   tab4 <- gen_tab4(tab1, tab2, information)
#'
#'   output <- tab4_output(tab4, output)
#'
#'   output$input <- input
#'
#'   output <- structure(
#'     output,
#'     class = "ESS"
#'   )
#'
#'
#'   invisible(output)
#'
#' }

report(res, "detailed")


extract(res, "cutpoint")
res_1 <- update(res, `M1-All` = c(7, 21))
extract(res_1, "cutpoint")

report(res_1)

# modal_est_cs <- tab1$modal_est_cs_all[[1]] %>% print(n=100)
# modal_est_cp <- tab1$modal_est_cp_all
# modal_selected_cp <- tab1$modal_selected_cp_all
# modal_selected_cs <- tab1$modal_selected_cs_all
#
# modal_selected_cp <- tab1$modal_selected_cp_all
# modal_selected_cs <- tab1$modal_selected_cs_all

## Obtain Data for Display (Cut Scores) -------------------




