#
# report <- function(x, ...) {
#   UseMethod("report")
# }
#
# report.ESS <- function(x, what = "all") {
#
#   if(what == "all") {
#
#     page_fillable(
#       navset_tab(
#         nav_panel(
#           "Individual",
#           report_tab1(x)
#         ),
#         nav_panel(
#           "Detailed",
#           report_tab2(x)
#         ),
#         nav_panel(
#           "CS Summary",
#           report_tab3(x)
#         ),
#
#         nav_panel(
#           "Item Review",
#           report_tab4(x)
#         )
#
#       )
#
#     )
#
#
#   } else if(what == "individual") {
#     report_tab1(x)
#
#   } else if(what == "detailed") {
#     report_tab2(x)
#
#   } else if(what == "summary") {
#     report_tab3(x)
#
#   } else if(what == "review") {
#     report_tab4(x)
#
#   } else if(what == "setup") {
#
#     o <- list()
#     o$setting    <- print_setting(x$imprt_data)
#     o$itemtable1 <- print_itemtable1(x$imprt_data)
#     o$itemtable2 <- print_itemtable2(x$imprt_data)
#
#     report_setup(o)
#   }
#
#   # invisible(x)
# }
#
# report_setup <- function(output) {
#   browsable(
#     tagList(
#       HTML(as.character(output$setting)),
#       br(),
#       HTML(as.character(output$itemtable1)),
#       br(),
#       HTML(as.character(output$itemtable2))
#     )
#   )
# }
#
# report_tab1 <- function(output) {
#   browsable(
#     tagList(
#       div(output$tab1_indi),
#       br(),
#       div(output$tab1_group_mode),
#       br(),
#       div(output$tab1_group_median),
#       br(),
#       div(output$tab1_group_average)
#     )
#   )
# }
#
# report_tab2 <- function(output) {
#
#   gca_name <- output$information$data_ready$id_list$GCA
#
#   tabs <- lapply(1:length(gca_name), function(i) {
#
#     nav_panel(
#       paste0(gca_name[i]),
#
#       navset_tab(
#         nav_panel("Table", output[[paste0("t1_", i, "_1")]]),
#         nav_panel("Plot", output[[paste0("p1_", i, "_1")]]),
#         nav_panel("Plot data", output[[paste0("gam_", i, "_1")]]),
#         nav_panel(
#           "Crosstab",
#           HTML(as.character(output[[paste0("ct_", i, "_1")]]))
#         ),
#         nav_panel(
#           "Crosstab eff",
#           HTML(as.character(output[[paste0("ct_e_", i, "_1")]]))
#         ),
#         nav_panel(
#           "Summary",
#           HTML(as.character(output[[paste0("sum_", i, "_1")]]))
#         )
#       )
#     )
#   })
#
#   do.call(navset_tab, tabs)
#
# }
#
# report_tab3 <- function(output) {
#
#   gca_id <- information$data_ready$id_list$GCA
#   tab_id <- unique(str_split(gca_id, "-", simplify = T)[,2])
#
#   tabs <- lapply(1:length(tab_id), function(i) {
#
#     nav_panel(
#       paste0(tab_id[i]),
#
#       navset_tab(
#         nav_panel("Table",
#                   HTML(as.character(output[[paste0("tab3table_", i)]]))
#         ),
#         nav_panel("Plot",
#                   plotly::ggplotly(output[[paste0("tab3p1_", i)]]),
#                   plotly::ggplotly(output[[paste0("tab3p2_", i)]]),
#                   plotly::ggplotly(output[[paste0("tab3p3_", i)]])
#         )
#
#       )
#     )
#   })
#
#   do.call(navset_tab, tabs)
# }
#
# report_tab4 <- function(output) {
#
#   n.of.gca <- information$data_ready$id_list$GCA
#   n.of.tb <- rep(1, length(n.of.gca))
#
#
#   tabs <- lapply(1:length(n.of.tb), function(i) {
#
#     nav_panel(
#       paste0(n.of.gca[i]),
#
#       navset_tab(
#         nav_panel("Table",
#                   output[[paste0("out4__", i, "_1")]]))
#     )
#   })
#
#   do.call(navset_tab, tabs)
# }
