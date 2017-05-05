

rareboost <- function(x,
                      yname,
                      treedepth = 3,
                      event = 1,
                      m = 50) {
  stopifnot(is.data.frame(x))
  colnames(x)[colnames(x) == yname] <- "y"
  x$y <- format_y(x$y, event)

  return_object <- get_prediction_rare(x, treedepth, m)
}

get_prediction_rare <- function(x, treedepth, m) {
  weights <- rep(1 / nrow(x), nrow(x))
  y_hat_mat <- matrix(0, nrow(x), m)
  models    <- vector("list", m)
  alpha_pos <- numeric(m)
  alpha_neg <- numeric(m)

  for(i in 1:m) {
    models[[i]]    <- learn_model_m(x, weights, treedepth)
    y_hat_mat[, i] <- get_yhat_m(models[[i]], x)
    alpha_pos[i]   <- get_alpha_pos(x$y, y_hat_mat[, i], weights)
    alpha_neg[i]   <- get_alpha_neg(x$y, y_hat_mat[, i], weights)
    weights        <- update_weights_pos_neg(x$y, y_hat_mat[, i], weights,
                                             alpha_pos[i], alpha_neg[i])
  }

}

get_alpha_pos <- function(y, yhat, w) {
  y_yhat_pos <- y[yhat > 0]
  yhat_pos   <- yhat[yhat > 0]
  w_yhat_pos <- w[yhat > 0]
  TP <- sum(w_yhat_pos[y_yhat_pos > 0] * yhat_pos[y_yhat_pos > 0])
  FP <- sum(w_yhat_pos[y_yhat_pos < 0] * yhat_pos[y_yhat_pos < 0])
  return(0.5 * log(TP / FP))
}

get_alpha_neg <- function(y, yhat, w) {
  y_yhat_neg <- y[yhat < 0]
  yhat_neg   <- yhat[yhat < 0]
  w_yhat_neg <- w[yhat < 0]
  TN <- sum(w_yhat_neg[y_yhat_neg < 0] * yhat_neg[y_yhat_neg < 0])
  FN <- sum(w_yhat_neg[y_yhat_neg > 0] * yhat_neg[y_yhat_neg > 0])
  return(0.5 * log(TN / FN))
}

update_weights_pos_neg <- function(y, yhat, w, alpha_p, alpha_n) {
  p <- yhat > 0; n <- yhat < 0
  new_weights <- numeric(length(weights))
  new_weights[p] <- weights[p] * exp (-alpha_p * y[p] * yhat[p])
  new_weights[n] <- weights[n] * exp (-alpha_n * y[n] * yhat[n])
  return(new_weights / sum(new_weights))
}

get_prediction_mat_score_rare <- function(y_hat_mat, alpha_pos, alpa_neg) {

}
