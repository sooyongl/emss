#' #' Plot EmstanS plot
#' #'
#' plotting <- function(dataUse_1, selected_CP, selected_CS, selected_weights, WESS, GAM, gam_est = NA, font_size = 16) {
#'
#'   if(WESS) {
#'     picked <- names(dataUse_1)[grep("_W", names(dataUse_1))]
#'   } else {
#'     picked <- names(dataUse_1)[grep("_C", names(dataUse_1))]
#'   }
#'
#'   if(GAM) {
#'     fitted_val <- gam_est
#'   }
#'
#'
#'   geom.text.size = font_size * (5/14)
#'   theme_set(theme_bw(base_size = font_size) +
#'               theme(plot.margin = unit(c(.1,.1,.1,.1), "cm")) +
#'               theme(
#'                 legend.position = "bottom",
#'                 legend.justification = c(0, 1)))
#'
#'
#'   dataUse_2 <- dataUse_1 %>%
#'     select(all_of(picked), location) %>%
#'     gather("Cut Level","Inconsistency", -location)
#'
#'   x_range <- dataUse_2$location
#'   x_range <- c(min(x_range), max(x_range))
#'
#'   pp1 <- dataUse_2 %>%
#'     ggplot(aes(x = location, y = Inconsistency,
#'                yend = 0, xend = x_range[1])
#'     ) +
#'     geom_point(aes(colour = factor(`Cut Level`)),
#'                fill = "white", size = 2, stroke = .5, shape = 21)
#'
#'
#'   if(GAM) {
#'     pp1 <- pp1 +
#'       geom_line(
#'         data = fitted_val$gam_pred %>% filter(`Cut Level` %in% picked),
#'         aes(group = factor(`Cut Level`),
#'             linetype = factor(`Cut Level`),
#'             colour = factor(`Cut Level`)),
#'         size = 1.2,
#'         alpha = .5
#'       )
#'   }
#'
#'   cut_dt <-
#'     tibble(
#'       location = dataUse_1$location[selected_CP],
#'       `Cut Level` = names(selected_weights),
#'       cutcs = selected_CS,
#'       Inconsistency = selected_weights)
#'
#'   arrow_length <- max(dataUse_2[["Inconsistency"]])/6
#'
#'   pp1 <-pp1 +
#'     geom_point(
#'       data = cut_dt,
#'       aes(x = location,
#'           y = Inconsistency),
#'       fill = "white",
#'       size = rel(2.5),
#'       stroke = rel(1.5),
#'       shape = 21,
#'       alpha = .8
#'     ) +
#'     geom_segment(
#'       data = cut_dt,
#'       aes(yend = Inconsistency),
#'       lty = "dashed",
#'       colour = "grey30"
#'     ) +
#'     geom_segment(
#'       data = cut_dt,
#'       aes(xend = location),
#'       lty = "dashed",
#'       colour = "grey30"
#'     ) +
#'
#'     geom_label_repel(
#'       data = cut_dt,
#'       aes(x = location,
#'           y = Inconsistency,
#'           label = cutcs),
#'       fontface = 'bold',
#'       size = geom.text.size,
#'       nudge_y = arrow_length,
#'       arrow = arrow(length = unit(0.02, "npc")),
#'       box.padding = 1,
#'       alpha = 0.8
#'     ) +
#'     guides(linetype = "none") +
#'     scale_colour_brewer(palette = "Set1")  +
#'     labs(x = "Location",
#'          y = "Number of Inconsistent Items",
#'          colour = "Cut Level",
#'          title = "",
#'          caption = "")
#'
#'
#'   if(WESS){
#'     pp1 <-
#'       pp1 +
#'       scale_x_continuous(
#'         limits = x_range,
#'         breaks = round(seq(from = x_range[1],
#'                            to = x_range[2],
#'                            length.out = 10),0)
#'       ) +
#'       labs(y = "Total Weights",
#'            title = "") +
#'       scale_y_continuous(
#'         breaks = seq(from = round(min(dataUse_2$Inconsistency),0),
#'                      to = round(max(dataUse_2$Inconsistency),0),
#'                      length.out = 5)
#'       )
#'   } else {
#'     pp1 <- pp1 #+ scale_y_continuous(breaks = y_disc)
#'   }
#'
#'
#'   pp1 <- ggplotly_render(pp1, cut_dt)
#'   pp1
#' }
#'
#'
#'
#' ggplotly_render <- function(gg_obj,cut_dt,font_size = 12) {
#'   # gg_obj <- p1;
#'   loc_name <- "location"
#'
#'
#'   annotation <- list(yref = 'paper',
#'                      xref = "paper",
#'                      y = 1,
#'                      x = .95,
#'                      showarrow=FALSE,
#'                      text = paste0("Cut score: ",paste(cut_dt[["cutcs"]], collapse = ", ")))
#'
#'   a1 <- c(3, 6)
#'   if(length(gg_obj$layers) == 5) {
#'     a1 <- a1 - 1
#'   }
#'   gg_obj$layers[a1] <- NULL
#'
#'   gg_obj <- gg_obj + theme_bw(base_size = font_size)
#'
#'   ggplotly(gg_obj) %>%
#'     layout(
#'       legend = list(orientation = "h", x = 0, y = 1.15),
#'       annotations= list(annotation)
#'     )
#' }
