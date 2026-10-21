#' #' Generate a single data set containing ALD, OOD, and location given a correlation value
#' #'
#' #' @export
#' genFakeData <- function(fun, cor_value, nlevel, ...) {
#'
#'   location <- genLoc(fun, ...)
#'   new_ald <- simALD(location, cor_value, nlevel)
#'   new_ald <- paste0("Level",new_ald)
#'
#'   fake_data <- cbind(
#'     OOD = 1:length(location),
#'     location = round(location,2),
#'     ALD = new_ald)
#'
#'   fake_data <- data.frame(fake_data)
#'
#'   fake_data$OOD <- as.numeric(fake_data$OOD)
#'   fake_data$location <- as.numeric(fake_data$location)
#'   fake_data
#' }
#'
#'
#' #' generate theta
#' genTheta <- function(theta, mean_vec, cor_mat) {
#'
#'   stopifnot( length(mean_vec) == dim(cor_mat)[2] )
#'   stopifnot( eigen( cor_mat )$values > 0 )
#'
#'   shiftMean <- function(theta_mat, mean_vec){
#'     col_means <- colMeans(theta_mat)
#'     mean_centered <- theta_mat - matrix(rep(col_means, dim(theta_mat)[1]), nrow = dim(theta_mat)[1], byrow = T)
#'     theta_mat <- mean_centered + matrix(rep(mean_vec, dim(theta_mat)[1]), nrow = dim(theta_mat)[1], byrow = T)
#'     return(theta_mat)
#'   }
#'
#'   n_col <- ifelse(is.null(dim(theta)), 1, dim(theta)[2])
#'
#'   n <- ifelse(is.null(dim(theta)), length(theta), dim(theta)[1])
#'   k <- ncol(cor_mat)
#'   x <- matrix( rnorm(n*k), nc=k )
#'   x[,seq_len(n_col)] <- theta
#'
#'   y <- x %*% solve(chol(var(x))) %*% chol(cor_mat) # cor(y)
#'   y[,seq_len(n_col)] <- theta  #
#'
#'   y <- shiftMean(y, mean_vec)
#'
#'   return(y)
#' }
#'
#'
#' #' simulate ALD
#' simALD <- function(existing_theta, cor_value, nlevel) {
#'   given_correlation <- matrix(c(
#'     1, cor_value,
#'     cor_value, 1 ), 2, 2 )
#'   given_means <- c(mean(existing_theta), 0)
#'   new_theta <- genTheta(theta = existing_theta, mean_vec = given_means, cor_mat = given_correlation)
#'
#'   new_level <- cut(x = new_theta[,2], breaks = nlevel, labels = F)
#'   new_level[1] <- 1
#'
#'   return(new_level)
#' }
#'
#' #' Generate location values
#' genLoc <- function(fun, ...){
#'   existing_theta <- match.fun(fun)(...)
#'   existing_theta <- sort(existing_theta)
#'   existing_theta
#' }
