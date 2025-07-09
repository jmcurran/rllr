#' Create an empirical cross-entropy (ECE) plot
#'
#' @param log.lrs
#' @param labels
#' @param nbins
#'
#' @returns a ggplot2 plot showing an ECE plot.
#' @importFrom dplyr group_by mutate summarise
#' @importFrom ggplot2 ggplot geom_col geom_line labs theme_minimal
#' @examples
#' @export
ece.plot = function(log.lrs, labels, method = c("binned", "smoothed"),
                    nbins = 10,
                    bandwidth = 0.05, resolution = 200){
  method = match.arg(method)

  if (method == "binned") {
    ece.plot.binned(log.lrs, labels, nbins = nbins)
  } else {
    ece.plot.smoothed(log.lrs, labels, bandwidth = bandwidth, resolution = resolution)
  }

}

ece.plot.binned = function(log.lrs, labels, nbins){
  ## Convert the log-likehood ratios to probabilities
  ## using the inverse logistic or 'expit' or 'sigmoidal function
  probs = 1 / (1 + exp(-log.lrs))

  ## Put data into data.frame for easier manipulation
  data.df = data.frame(prob = probs, label = labels)

  ## Compute the endpoints of the bins
  breaks = seq(0, 1, length.out = nBins + 1)

  ## Bin the data
  data.df$bin = cut(data.df$prob, breaks = breaks, include.lowest = TRUE)

  ## Use dplyr to compute the values we need for binary cross-entropy per bin
  bin_stats = df |>
    group_by(bin) |>
    summarise(
      p = mean(label),
      p.hat = mean(prob),
      .groups = "drop"
    ) |>
    mutate(ce = cross.entropy(p, p.hat))
}

ece.plot.smoothed = function(log.lrs, labels, bandwidth = 0.05, resolution = 200){
  probs = 1 / (1 + exp(-scores))
  grid = seq(0, 1, length.out = resolution)

  # Kernel weights and smoothed true probabilities
  smooth_p_true = sapply(grid, function(p) {
    w = dnorm(probs - p, sd = bandwidth)
    sum(w * labels) / sum(w)
  })

  # Compute cross-entropy vs predicted prob
  ce = ifelse(
    smooth_p_true %in% c(0, 1), 0,
    smooth_p_true * log(smooth_p_true / grid) +
      (1 - smooth_p_true) * log((1 - smooth_p_true) / (1 - grid))
  )

  ce.df = data.frame(prob = grid, ce = ce)

  ggplot(ce.df, aes(x = prob, y = ce)) +
    geom_line(color = "blue") +
    labs(
      title = "Smoothed ECE Plot",
      x = "Predicted Probability",
      y = "Cross Entropy"
    ) +
    theme_minimal()
}
