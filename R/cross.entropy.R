#' Compute (Binary) Cross Entropy
#'
#' Computes the (binary) cross entropy, or log loss for binned data.
#'
#' If \eqn{y \in \left\{0,1\right\}}{y in {0,1}} is the true value/label and \eqn{p} is the predicted probability that
#' the label is 1, then the binary cross entropy, or log-loss of \eqn{y} and \eqn{p} is
#' \deqn{CE(y,p)=-\left[y\log(p)+(1-y)\log(1-p)\right]}{CE(y,p) = -[y * log(p) + (1 - y) * log(1 - p)]}.
#'
#' Note that if \eqn{p = 0}, then \eqn{\log(p) = -\infty}{log(p) = -infinity}. Similarly if \eqn{p = 1}, then \eqn{\log(1 - p)
#' = -\infty}{log(1 - p) = -infinity}. This is problematic, so the statistic is set to zero if \eqn{p = \left\{0,1\right\}}{p = {0, 1}}.
#' If the data is binned, then \eqn{y} is replaced by the proportion of true values in the bin, or equivalently, the
#' average over the labels in the bin, and \eqn{p} is replaced by the average predicted probability. We will call these
#' new quantities \eqn{p} and \eqn{\hat{p}{p.hat}} respectively.
#'
#' Some intution for this quantity comes from looking at its construction. If \eqn{y=1} and \eqn{p} is close to one,
#' then \eqn{CE(y,p)} should be close to zero because \eqn{\log(p)}{log(p)} is close to zero (and \eqn{y-1 = 0}).
#' Similarly, if \eqn{y=0} and \eqn{p} is close to zero, then \eqn{log(1-p)} will be close to zero. In both cases, this
#' says if your prediction probability is close to one, noting that that \eqn{\Pr(Y=0) = 10\Pr(Y=1)}{Pr(Y = 0) = 1 -
#' Pr(Y = 1)}, then you should have low cross entropy, and vice-versa, if it is close to zero, then you should have high
#' cross entropy. The same statement is "true" when based in terms of being averaged over observations in a bin.
#'
#' This function is not really meant to be called in isolation, but from an empirical cross entropy (ECE) plot function.
#'
#' @param p the proportion of true positives within a "bin."
#' @param p.hat the mean predicted probability within a "bin.
#'
#' @returns the (binary) cross entropy, or log loss for binned data.
#' @export
#'
#' @examples
#' # High propotion of true positives, high average predicted probability that y = 1 => low cross entropy
#' cross.entropy(p = 0.9, p.hat = 0.9)
#'
#' # High propotion of true negatives, low average predicted probability that y = 1 => low cross entropy
#' cross.entropy(p = 0.1, p.hat = 0.1)
#'
#' # High propotion of true positive, low average predicted probability that y = 1 => high cross entropy
#' cross.entropy(p = 0.9, p.hat = 0.1)
#'
cross.entropy = function(p, p.hat){
  if (p %in% c(0,1)) {
    0
  } else {
    -(p * log(p.hat) + (1 - p) * log(1 - p.hat))
  }
}
