#
res <- emstans(fs::dir_ls("prerun/data/example_file")[1],
        targets = "ALD",
        WESS = T,
        gamest = F,
        median = "modal",
        loc = "RP67",
        domain = "GCA",
        select_domain = NULL,
        font_size = 14,
        digits = 3)

class(res)
summary(res)
report(res, what = "all")
report(res, what = "individual")
report(res, what = "setup")

emstans <- function(filePath = NULL,
                    data = NULL,

                    targets = "ALD",
                    WESS = T,
                    gamest = F,
                    median = "modal",
                    loc = "RP67",
                    domain = "GCA",
                    select_domain = NULL,
                    font_size = 14,
                    digits = 3) {

  if(is.null(filePath) & is.null(data)) {
    message("Either filePath or data must be provided")
  }

  # Containers ---------------
  input <- list()
  output <- list()

  input$targets <- targets #= "ALD"#
  input$WESS <- WESS # = T
  input$gamest <- gamest # = F
  input$median <- median # = "modal"
  input$loc <-  loc # = "RP67" # "RP50" #"Loc_RP50" #

  input$domain <- domain # =  c("GCA") # "GCA" ; "Domain"

  input$select_domain <- select_domain # = NULL
  input$font_size <- font_size # = 14

  # Data imported --------------------------------
  # filePath = fs::dir_ls("prerun/data/example_file")[1]
  input$setups$datapath <- filePath

  imprt_data <- if(is.null(filePath)) {
    data
  } else {

    sheets_name <- input$setups$datapath %>% excel_sheets()
    read_data(filePath = input$setups$datapath, sheets_name = sheets_name)
  }

  output$imprt_data <- imprt_data

  # Validate Data file ----------------------------
  validateData(imprt_data)

  # Data ready for analysis
  data_list <- data_ready(imprt_data)

  output$data_list <- data_list

  setup_data    <- data_list$setup_data
  panelist_data <- data_list$panelist_data
  rating_data   <- data_list$rating_data
  item_data     <- data_list$item_data
  examinee_data <- data_list$examinee_data


  # Information ready --------------------------------------

  input$tests <-  as.character(setup_data$GCA)

  information <-
    get_data_info(
      data_list = data_list,
      gca       = input$tests,
      ald       = "ALD",
      location  = input$loc,
      WESS      = input$WESS,
      gamest    = input$gamest,
      domain    = input$domain,
      select_domain = input$select_domain,
      modal     = F
    )

  output$information <- information


  # Tab 0 --------------------------
  ## This provides all fundamental results
  tab0 <- gen_tab0(information = information)


  # Tab 1: Compute Cut scores --------------------------
  tab1 <- gen_tab1(tab0, information)

  # Main output:
  # res : individual result
  # median: median_res_com
  # mode: modal_res_com
  # average: average_res_com

  output$tab1 <- tab1

  output$individual_cut <- tab1$res
  output$median_cut <- tab1$median_res_com
  output$modal_cut <- tab1$modal_res_com
  output$average_cut <- tab1$average_res_com

  ## Tab1 print
  output$tab1_indi <- dt_table_out_indi(tab1$indi_table, table_options_new_1)

  # modal or median cut score output
  output$tab1_group_mode <- dt_table_out_mode(tab1$modal_table, table_options_new_2)

  # modal or median cut score output
  output$tab1_group_median <- dt_table_out_med(tab1$median_table, table_options_new_2)

  # average cut score output
  output$tab1_group_average <- dt_table_out_med(tab1$average_table, table_options_new_2)

  ## Tab 2: Obtain Data for Display (Cut Scores)-------
  tab2 <- gen_tab2(tab1, information)

  output$tab2 <- tab2

  # tab2$for_tab2_out$`R0-All`[[1]]$eff_data
  # tab2$for_tab2_out$`R0-All`[[1]]$t_out
  # tab2$for_tab2_out$`R0-All`[[1]]$crosst
  # tab2$for_tab2_out$`R0-All`[[1]]$gam_fitted

  output$summary_table <-
    lapply(1:length(tab2$for_tab2_out), function(x) {
    xx <- tab2$for_tab2_out[[x]]
    lapply(1:length(xx), function(xi) {
      xx[[xi]]$eff_data
    })
  })

  output$cutscore_table <-
    lapply(1:length(tab2$for_tab2_out), function(x) {
      xx <- tab2$for_tab2_out[[x]]
      lapply(1:length(xx), function(xi) {
        xx[[xi]]$t_out
      })
    })

  output$crosstab_table <-
    lapply(1:length(tab2$for_tab2_out), function(x) {
      xx <- tab2$for_tab2_out[[x]]
      lapply(1:length(xx), function(xi) {
        xx[[xi]]$crosst
      })
    })

  output$gam_fit <-
    lapply(1:length(tab2$for_tab2_out), function(x) {
      xx <- tab2$for_tab2_out[[x]]
      lapply(1:length(xx), function(xi) {
        xx[[xi]]$gam_fitted
      })
    })

  ## Tab2 Print -----
  information <- information
  tab2 <- tab2

  loc_nm <- information$base_data$loc_nm
  WESS_nm <- information$base_data$WESS

  n.of.gca <- information$data_ready$id_list$GCA
  n.of.tb <- rep(1, length(n.of.gca))

  for_tab2_out <- tab2$for_tab2_out

  # put the results into each output
  # lapply(1:length(n.of.gca), function(vi) {
  for(vi in 1:length(n.of.gca)){
    # vi = 1; vvi = 1
    in_num <- n.of.tb[vi]
    # lapply(1:in_num, function(vvi) {
    for(vvi in 1:in_num) {
      t_outname <- paste("t1", vi, vvi, sep = "_")
      p_outname <- paste("p1", vi, vvi, sep = "_")
      ct_outname <- paste("ct", vi, vvi, sep = "_")
      ct_outname_e <- paste("ct_e", vi, vvi, sep = "_")

      sum_outname <- paste("sum", vi, vvi, sep = "_")
      sum_outname_e <- paste("sum_e", vi, vvi, sep = "_")

      gam_outname <-  paste("gam", vi, vvi, sep = "_")

      dataUse_1 <- for_tab2_out[[vi]][[vvi]][["t_out"]]
      crosstabs <- for_tab2_out[[vi]][[vvi]][["crosst"]]
      crosstabs_ec <- for_tab2_out[[vi]][[vvi]][["crosst_ec"]]
      p1 <- for_tab2_out[[vi]][[vvi]][["p1"]]
      gam_fitted <- for_tab2_out[[vi]][[vvi]][["gam_fitted"]]

      plot_data <- for_tab2_out[[vi]][[vvi]][["plot_data"]]
      selected_cp <- for_tab2_out[[vi]][[vvi]][["selected_cp"]]
      selected_cs <- for_tab2_out[[vi]][[vvi]][["selected_cs"]]

      output[[t_outname]] <- tab2_table(dataUse_1,information)
      output[[p_outname]] <-
        p1 %>% ggplotly_render(plot_data, selected_cp, selected_cs, information)


      output[[ct_outname]] <- tab2_table_crosst(crosstabs)[[1]]
      output[[ct_outname_e]] <- tab2_table_crosst(crosstabs_ec)[[1]]
      output[[sum_outname]] <- tab2_table_crosst(crosstabs)[[2]]
      output[[sum_outname_e]] <- tab2_table_crosst(crosstabs_ec)[[2]]

      output[[gam_outname]] <- gam_fitted_table(gam_fitted, selected_cs)
    }
  }

  ## Tab 3: Cut Score Summary --------------------------------------
  ## Summary of Cut Scores and impact data -----------------
  tab3 <-
    gen_tab3(tab1, information) %>%
    tab3_table_pagetb(.) %>%
    tab3_plots(.)

  output$tab3 <- tab3

  output$effective_table <- tab3$eff_page



  ## Tab3 print ------------
  information <- information
  effpage <- tab3$effpage
  tab3_plot <- tab3$tab3_plot

  gca_id <- information$data_ready$id_list$GCA
  tab_id <- unique(str_split(gca_id, "-", simplify = T)[,2])

  # put the results into each output
  # lapply(1:length(tab_id), function(vi) {
  for(vi in 1:length(tab_id)) {
    # vi = 1; vvi = 1
    table_outname <- paste("tab3table", vi, sep = "_")

    p1_outname <- paste("tab3p1", vi, sep = "_")
    p2_outname <- paste("tab3p2", vi, sep = "_")
    p3_outname <- paste("tab3p3", vi, sep = "_")

    output[[table_outname]] <- effpage[[vi]]

    output[[p1_outname]] <- tab3_plot[[vi]][[1]]
    output[[p2_outname]] <- tab3_plot[[vi]][[2]]
    output[[p3_outname]] <- tab3_plot[[vi]][[3]]
  }

  ## Tab 4: Item Review Ready --------------------------------
  tab4 <- gen_tab4(tab1, tab2, information)

  output$tab4 <- tab4

  output$distance_output <- tab4$for_tab4_out

  ## Tab4 print----------------
  GCA_split <-
    tab4$for_tab4_out %>%
    mutate(GCA = factor(GCA, levels = information$data_ready$id_list$GCA)) %>%
    group_split(GCA)

  n.of.gca <- information$data_ready$id_list$GCA
  n.of.tb <- rep(1, length(n.of.gca))

  # put the results into each output
  # lapply(1:length(n.of.gca), function(vi) {
  for(vi in 1:length(n.of.gca)){
    # vi = 1; vvi = 1
    in_num <- n.of.tb[vi]
    # lapply(1:in_num, function(vvi) {
    for(vvi in 1:in_num) {

      review_outname <- paste("out4_", vi, vvi, sep = "_")
      tab4_split <- GCA_split[[vi]]
      maxRow <- nrow(tab4_split)
      tab4_out <- tab4_split %>%
        mutate_if(is.numeric, round,4)
      item_review <-
        datatable(tab4_out,
                  escape = FALSE,
                  selection = 'multiple',
                  rownames = F,
                  extensions = 'FixedHeader',
                  # options = table_options_3(maxRow),
                  filter = 'top'
        ) %>%
        formatStyle(1:nrow(tab4_out),
                    target= 'row',
                    lineHeight='70%')

      output[[review_outname]] <- item_review # tab4_table_review(GCA_split[[vi]])
    }
  }


  output$input <- input

  output <- structure(
    output,
    class = "ESS"
  )

  return(output)
}
