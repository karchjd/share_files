library(ggplot2)
bayesian_linear_regression <- function(y, X, alpha, beta) {
  # compute posterior values, see Equations 3.53 and 3.54
  posterior_covariance_inv <- alpha * diag(nrow = ncol(X)) + beta * t(X) %*% X
  posterior_mean <- solve(posterior_covariance_inv) %*% (beta * t(X) %*% y)


  result <- list(posterior_mean = posterior_mean, posterior_covariance = solve(posterior_covariance_inv))
  class(result) <- "bayesregml"
  return(result)
}

predict.bayesregml <- function(object, newdata, prediction = "latent") {
  # Ensure that newdata is a matrix
  if (is.vector(newdata)) {
    newdata <- matrix(newdata, nrow = 1)
  }

  # Calculate predictive means see Equation 3.58
  predictive_means <- newdata %*% object$posterior_mean

  # Calculate predictive variances see Equation 3.59 
  predictive_covariance <-  newdata %*% object$posterior_covariance %*% t(newdata)
  if(prediction != "latent"){
    predictive_covariance <-  diag(1 / beta) + predictive_covariance
  }
  result <- list(predictive_means = predictive_means, predictive_covariance = predictive_covariance, X = newdata, method = prediction)
  class(result) <- "preddist"
  return(result)
}

plot.preddist <- function(prediction, x = NULL, y = NULL) {
  if ("preddist" != class(prediction)) {
    stop("Input prediction must be of class 'preddist'.")
  }

  if (!is.vector(prediction$X) && (is.matrix(prediction$X) || is.data.frame(prediction$X)) && ncol(prediction$X) != 1) {
    stop("newdata must be a vector or a one-column matrix/data frame.")
  }

  # Create a data frame for plotting
  plot_data <- data.frame(
    x_pred = prediction$X,
    mean = as.vector(prediction$predictive_means),
    ymin = as.vector(prediction$predictive_means - 2 * sqrt(diag(prediction$predictive_covariance))),
    ymax = as.vector(prediction$predictive_means + 2 * sqrt(diag(prediction$predictive_covariance))))
  if(!is.null(y) && !is.null(x)){
    plot_data_2 <- data.frame(x = x, y = y)
  }
  
  # Create the ggplot
  p <- ggplot(plot_data, ggplot2::aes(x = x_pred, y = mean)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2) +
    geom_line(aes(y = mean), color = "blue") +
    xlab("x") +
    ylab("Predicted value") +
    ggtitle("Predictive Distribution")
  if(!is.null(y) && !is.null(x)){
    p <- p + geom_point(data = plot_data_2, aes(x = x, y = y))
  }
  return(p)
}