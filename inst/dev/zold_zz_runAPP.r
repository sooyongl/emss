#' #' @include EmStanS-package.r
#' NULL
#'
#' #' Launch EmStanS shiny app
#' #'
#' #' \code{\link{launchEmStanS}} is a caller function to open the Shiny interface of embedded standard setting.
#' #'
#' #' @param local a logical indicating whether the app is launched by the user's local
#' #' computer or online. (default = \code{FALSE})
#' #'
#' #' @description An interactive Shiny application.
#' #' @details This starts the EmStanS Shiny application on the user's local computer
#' #' or launch the online application.
#' #' @keywords Embedded Standard Setting
#' #' @examples
#' #' \dontrun{
#' #' if (interactive()) {
#' #'   launchEmStanS()
#' #' }
#' #' }
#' #' @export
#' launchEmStanS <- function(local = T) {
#'   # shiny_env <- new.env()
#'   # if(!missing(thedata)) {
#'   #   print('Setting parameters')
#'   #   assign('thedata', thedata, shiny_env)
#'   # }
#'   # environment(shiny_ui) <- shiny_env
#'   # environment(shiny_server) <- shiny_env
#'
#'    # if(local){
#'    #   if(requireNamespace("shiny", quietly = TRUE)){
#'    #     app <- shiny::shinyApp(
#'    #       ui = shiny_ui,
#'    #       server = shiny_server
#'    #     )
#'    #     shiny::runApp(app)
#'    #
#'    #   } else {
#'    #     stop('shiny package is not available. Please install.', call.=FALSE)
#'    #   }
#'    # } else {
#'
#'      # browseURL("https://creativemeasurementsolutionsllc.shinyapps.io/EmStanS_Lite")
#'    # }
#' }
