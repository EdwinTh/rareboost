#' Perform Classic AdaBoost
#'
#' This is the implementation of the classic AdaBoost algorithm.
#' @param x A dataframe.
#' @param yname A character vector indicating the column name of the binary target.
#' @param treedepth The maximal depth of the tree.
#' @param event Value that indicates the even in `y`.
#' @param m The number of trees.
#' @return A list containing:
#' * The predicted values of x.
#' * The confidence of the predictions.
#' * A list with the `m` trees.
#' * The vector with alpha values.
#' * The column name of `y`.
#' @details This is the AdaBoost implementation as described on page 258 of
#' the article.
#' @references
#' Joshi, Kumar, Agarwal 2001
#' @examples
#' adaboost(mtcars, "am")
adaboost <- function(x,
                     yname,
                     treedepth = 3,
                     event = 1,
                     m = 50) {
  stopifnot(is.data.frame(x))
  colnames(x)[colnames(x) == yname] <- "y"
  x$y <- format_y(x$y, event)

  return_object <- get_prediction(x, treedepth, m)
  return_object$yname <- yname
  return(return_object)
}

format_y <- function(y, event) {
  stopifnot(length(unique(y)) == 2)
  stopifnot(event %in% y)
  new_y    <- ifelse(y == event, 1, -1)
  return(new_y)
}

learn_model_m <- function(x, w, depth) {
  return( rpart::rpart(y ~ ., data = x, weights = w,
                       control = list(maxdepth = depth)))
}

get_yhat_m <- function(model_iter, x) {
  yhat <- predict(model_iter, x)
  return(yhat)
}

compute_err <- function(y, yhat, w) {
  return(sum(w * yhat * y))
}

get_alpha <- function(err) {
  return( 0.5 * log( (1 + err) / (1 - err)) )
}

update_weights <- function(y, yhat, w, alpha) {
  weights_raw <- w * exp(-alpha * y * yhat)
  return(weights_raw / sum(weights_raw))
}

final_mod <- function(y_hat_mat, alpha_vec) {
  prediction_mat <- t( t(y_hat_mat) * alpha_vec )
  return(list(prediction = sign(rowSums(prediction_mat)),
              confidence = rowSums(predictie_mat)))
}

get_prediction <- function(x, treedepth, m) {
  weights <- rep(1 / nrow(x), nrow(x))
  y_hat_mat <- matrix(0, nrow(x), m)
  models    <- vector("list", m)
  alpha_vec <- numeric(m)

  for(i in 1:m) {
    models[[i]]    <- learn_model_m(x, weights, treedepth)
    y_hat_mat[, i] <- get_yhat_m(models[[i]], x)
    err_iter       <- compute_err(x$y, y_hat_mat[ ,i], weights)
    alpha_vec[i]   <- get_alpha(err_iter)
    weights        <- update_weights(x$y, y_hat_mat[ ,i], weights, alpha_vec[i])
  }
  final_mod_obj <- final_mod(y_hat_mat, alpha_vec)
  ret <- list(prediction = final_mod_obj[[1]],
              confidence = final_mod_obj[[2]],
              models     = models,
              alpha_vec  = alpha_vec)
  return(ret)
}

get_prediction_mat_score <- function(models, x) {
  return(sapply(models, predict, x = x))
}

score_adaboost <- function(adaboost_obj,
                           x) {
  prediction_mat_score <- get_prediction_mat_score(adaboost_obj$models, x)
  return(final_mod(prediction_mat_score, adaboost_obj$alpha_vec))
}

