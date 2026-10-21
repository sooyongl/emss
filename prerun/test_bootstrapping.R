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


#' @export
boostrapping <- function(output, ...) {
  UseMethod("boostrapping")
}

boostrapping.ESS <- function(output, gca_name, boot_num = 10) {
  # output <- res; gca_name = "M1-All";boot_num = 10
  if(length(gca_name) > 1) {
    stop("only one `gca_name` must be selected.")
  }

  selected_id <- gca_name
  information <- output$information

  ald <- information$base_data$target_nm
  loc_nm <- information$base_data$loc_nm
  WESS <- information$base_data$WESS
  levels <- if(WESS) { "_W$" }else { "_C$" }

  bootResults <- list()

  gca <- unique(information$base_data$filtered_data[["GCA"]]) %>%
    sort() %>% as.character()
  level_nm <- information$data_ready$level_nm

  gca_p <- which(gca == selected_id)

  n_level <- length(level_nm[,gca_p])
  selected_cp <- output$tab1$modal_selected_cp_all[[gca_p]]
  used_data <- output$tab2$for_tab2_out[[gca_p]][[1]]$t_out

  new_data <-
    used_data %>%
    select(all_of(loc_nm), all_of(ald)) %>%
    mutate(!!ald := as.numeric(str_remove(!!as.name(ald), "Level")))

  names(new_data)[1] <- "location"

  boot_res <-
    boot_ESS(
      new_data = new_data,
      WESS = WESS,
      n_rep = boot_num,
      b_prop = 1,
      replace = T,
      keep.sample = F,
      empirical = F,
      n_level = n_level)

  bootResults$boot_p <-
    boot_plot(boot_res = boot_res,
              est_data = used_data,
              selected_cp = selected_cp,
              information = information,
              font_size = 18)

  bootResults$table1 <-
    boot_res %>%
    select(-y) %>%
    spread("lv_nm","x") %>%
    select(-boot_rep)

  bootResults$table2 <-
    boot_res %>%
    select(-y) %>%
    spread("lv_nm","x") %>%
    select(-boot_rep) %>%
    psych::describe() %>%
    select(-vars, -trimmed, -mad) %>%
    mutate_if(is.numeric, round, 2)


  bootResults$selected_id <- selected_id

  output$bootResults <- bootResults

  output <- structure(
    output,
    class = "bootESS"
  )


  invisible(output)

}


res_boot <- boostrapping(res, "M1-All")


summary.bootESS <- function(x) {
  x$bootResults$table2
}

print.bootESS <- function(x) {
  summary.bootESS(x)

}

report(res_boot)



report.bootESS <- function(output) {
 # output <- res_boot
  bootResults <- output$bootResults
  n.of.gca <- bootResults$selected_id

  boot_p <- bootResults$boot_p
  table1 <- bootResults$table1
  table2 <- bootResults$table2

  table1 <- table1 %>%
    DT::datatable(
      caption = htmltools::tags$caption(
        "Results of Bootstrapping draws",
        style = "color:black; font-size: 200%;"
      ),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size':'120%', 'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      )
    )

  table2 <- table2 %>%
    DT::datatable(
      caption = htmltools::tags$caption(
        "Descriptive statistics of bootstrapping draws",
        style = "color:black; font-size: 200%;"
      ),
      options = list(
        dom = "t",
        scrollX = TRUE,
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size':'120%', 'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      )
    )


  if(T) {
    plot_file <- tempfile(fileext = ".png")

    ggplot2::ggsave(
      filename = plot_file,
      plot = bootResults$boot_p,
      width = 9,
      height = 6,
      dpi = 300
    )

    boot_p_tag <- htmltools::tags$img(
      src = knitr::image_uri(plot_file),
      style = "width:100%; height:auto;"
    )

  }



  htmltools::browsable(
    bslib::navset_tab(
      id = "gca_tabs",

      bslib::nav_panel(
        title = n.of.gca,

        bslib::navset_tab(
          id = paste0("boot_tabs_", n.of.gca),

          bslib::nav_panel(
            title = "Boot plot",
            bslib::card(
              full_screen = TRUE,
              fill = TRUE,
              bslib::card_body(
                boot_p_tag
              )
            )
          ),

          bslib::nav_panel(
            title = "Boot table",
            bslib::card(
              full_screen = TRUE,
              fill = TRUE,
              bslib::card_body(
                table1
              )
            )
          ),

          bslib::nav_panel(
            title = "Boot summary",
            bslib::card(
              full_screen = TRUE,
              fill = TRUE,
              bslib::card_body(
                table2
              )
            )
          )
        )
      )
    )
  )
}

# Bootstrapping Ouput -----------------------------------



# selected_ids <- gca[1]; input$boot_num = 10


report.bootESS <- function(output) {
  tab1 <- output$tab1
  tab2 <- output$tab2
  information <- output$information
  bootResults <- output$bootResults

  level_nm <- information$data_ready$level_nm

  ald <- information$base_data$target_nm
  loc_nm <- information$base_data$loc_nm
  WESS <- information$base_data$WESS
  levels <- if(WESS) { "_W$" }else { "_C$" }

  n.of.gca <- selected_id <- bootResults$selected_id


  boot_p <- bootResults$boot_p # ggplot2 object
  table1 <- bootResults$table1
  table2 <- bootResults$table2

  table1 <- table1 %>%
    DT::datatable(caption =
                    tags$caption("Results of Bootstrapping draws",
                                 style="color:black; font-size: 200%;"),
                  options = list(
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'font-size':'120%', 'background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))

  table2 <- table2 %>%
    DT::datatable(caption =
                    tags$caption("Descriptive statistics of bootstrapping draws",
                                 style="color:black; font-size: 200%;"),
                  options = list(
                    dom = 't',
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'font-size':'120%','background-color': '#000', 'color': '#fff'});",
                      "}")
                  ))


  bslib::nav_panel(
    title = n.of.gca,

    bslib::navset_tab(
      id = paste0("p1_boot"),

      bslib::nav_panel(
        title = "bootplot",

        bslib::card(
          full_screen = TRUE,
          fill = TRUE,
          height = "100%",

          bslib::card_body(
            fillable = TRUE,
            fill = TRUE,
            style = "overflow-y: auto;",
            boot_p
          )
        )
      ),

      bslib::navset_tab(
        id = paste0("tab1_boot"),

        bslib::nav_panel(
          title = "boottab1",

          bslib::card(
            full_screen = TRUE,
            fill = TRUE,
            height = "100%",

            bslib::card_body(
              fillable = TRUE,
              fill = TRUE,
              style = "overflow-y: auto;",
              boot_p
            )
          )
        ),

        bslib::navset_tab(
          id = paste0("tab2_boot"),

          bslib::nav_panel(
            title = "boottab2",

            bslib::card(
              full_screen = TRUE,
              fill = TRUE,
              height = "100%",

              bslib::card_body(
                fillable = TRUE,
                fill = TRUE,
                style = "overflow-y: auto;",
                boot_p
              )
            )
          )

        )
      )
    )
  )

}



output$boot_ui <- renderUI({
  # n.of.gca <- selected_ids <- input$boot_id
  # Tab generation + putting uiOutput
  TAB  <-
    do.call(
      tabsetPanel,
      c(id ='tab',lapply(1:length(n.of.gca), function(i) {
        tabPanel(
          title = paste0('GCA: ', n.of.gca),
          uiOutput(paste0("boot_",i))
        )
      }))
    )
  TAB
})

v <- vector("list", length(n.of.gca))
for(vi in 1:length(n.of.gca)) {
  in_num <- 1
  for(vvi in 1:in_num){
    v[[vi]][[vvi]] <-
      fluidPage(
        tabsetPanel(
          tabPanel("plot",
                   plotOutput(paste0(n.of.gca[vi],"_boot_graph"))),
          tabPanel("table",
                   DTOutput(paste0(n.of.gca[vi],"_boot_table1")),
                   br(),
                   DTOutput(paste0(n.of.gca[vi],"_boot_table"))
          )
        )
      )
  }
}

## Generate outputs inside each uioutput
lapply(1:length(n.of.gca), function(vi) {
  ui_outname <- paste0("boot_", vi)
  output[[ui_outname]] <-
    renderUI({
      in_num <- 1
      lapply(1:in_num, function(vvi){
        v[[vi]][[vvi]]
      })
    })
})



boot_p <- bootResults$boot_p
table1 <- bootResults$table1
table2 <- bootResults$table2

selected_id <- selected_id

boot_p

table1 %>%
  datatable(caption =
              tags$caption("Results of Bootstrapping draws",
                           style="color:black; font-size: 200%;"),
            options = list(
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'font-size':'120%', 'background-color': '#000', 'color': '#fff'});",
                "}")
            ))


table2 %>%
  datatable(caption =
              tags$caption("Descriptive statistics of bootstrapping draws",
                           style="color:black; font-size: 200%;"),
            options = list(
              dom = 't',
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'font-size':'120%','background-color': '#000', 'color': '#fff'});",
                "}")
            ))




# bootResultsReport<- eventReactive(input$run_boot, {
#
#   bootResults
#
# })

## Downloading bootsrapping----------------------------------------------------
output$boot_report <- downloadHandler(
  filename = function() {
    paste("boot_",Sys.Date(), ".xlsx", sep = "")
  },

  content = function(file) {
    shiny::withProgress(
      message = paste0("Downloading", " the document"),
      value = 0,
      {
        incProgress(1/10);Sys.sleep(1);incProgress(5/10)
        file_excel <- tempfile(fileext = ".xlsx")

        reportTables <- boot_excel(bootResults)

        # reportTables <- bootResults$table1[[1]]

        file_xlxs <- boot_exceldown(reportTables = reportTables)
        saveWorkbook(file_xlxs, file=file, overwrite = T)
        Sys.sleep(1);incProgress(4/10);Sys.sleep(1)
      })
  })
