information <- information
tab1 <- tab1

# Tab generation + putting uiOutput
output$tabs <- renderUI({  genMultipleTab(information, uiOut_name = 'out2_')})
# Place Output for each tab
v <- genOutputPlace(information, tab1)

## Generate outputs inside each uioutput
n.of.gca <- information$data_ready$id_list$GCA
n.of.tb <- rep(1, length(n.of.gca))

lapply(1:length(n.of.gca), function(vi) {
  ui_outname <- paste0("out2_", vi)
  output[[ui_outname]] <-
    renderUI({
      in_num <- n.of.tb[vi]
      lapply(1:in_num, function(vvi){
        v[[vi]][[vvi]]
      })
    })
})

output$tabs()


genMultipleTab(information, uiOut_name = 'out2_')

library(bslib)

str(output$t1_1_1, max.level = 2)


output$t1_1_1
output$p1_1_1
output$ct_1_1
output$ct_e_1_1
output$sum_1_1
output$gam_1_1

navset_tab(
  nav_panel(
   "T1",
   navset_tab(
     
     nav_panel("Table", 
               # fill = TRUE,
               output$t1_1_1
               
               ),
     nav_panel("Plot", output$p1_1_1),
     nav_panel("Plot data", output$gam_1_1),
     nav_panel("Crosstab", 
               HTML(as.character(output$ct_1_1))
               ),
     nav_panel("Crosstab eff", 
               HTML(as.character(output$ct_e_1_1))
               ),
     nav_panel("Summary", 
               HTML(as.character(output$sum_1_1))
     )
   )
  )
)


panels <- lapply(seq_along(outputs), function(i) {
  nav_panel(
    paste("Table", i),
    outputs[[i]]
  )
})

do.call(navset_tab, panels)




make_tab2 <- function(output) {
  
  gca_name <- output$information$data_ready$id_list$GCA
  
  tabs <- lapply(1:length(gca_name), function(i) {
    
    nav_panel(
      paste0(gca_name[i]),
      
      navset_tab(
        nav_panel("Table", output[[paste0("t1_", i, "_1")]]),
        nav_panel("Plot", output[[paste0("p1_", i, "_1")]]),
        nav_panel("Plot data", output[[paste0("gam_", i, "_1")]]),
        nav_panel(
          "Crosstab",
          HTML(as.character(output[[paste0("ct_", i, "_1")]]))
        ),
        nav_panel(
          "Crosstab eff",
          HTML(as.character(output[[paste0("ct_e_", i, "_1")]]))
        ),
        nav_panel(
          "Summary",
          HTML(as.character(output[[paste0("sum_", i, "_1")]]))
        )
      )
    )
  })
  
  do.call(navset_tab, tabs)
  
}


