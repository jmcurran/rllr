#' Compute the cost of log-likelihood ratio (C_llr)
#'
#' The Cost of Log-Likelihood Ratio, \eqn{C_{\mathrm{llr}}}{C_llr}, is a scalar measure of calibration performance for
#' likelihood-ratio systems. It tells you how well the system's scores behave like true log-likelihood ratios - that is,
#' how well they represent the strength of evidence.
#'
#' If \eqn{\ell_i}{l_i} is the \eqn{i^\mathrm{th}}{ith} log-likelihood ratio from the system, and \eqn{y_i = 1} if the
#' \eqn{i^\mathrm{th}}{ith} log-likelihood ratio was obtained when the items were same source (e.g. two fingerprints
#' from the same person), and  \eqn{y_i = 0} if the \eqn{i^\mathrm{th}}{ith} log-likelihood ratio was obtained when the
#' items different source (e.g. two fingerprints from different people) the cost of log-Likelihood ratio is defined as
#'
#' \deqn{
#'   C_{\mathrm{llr}} = \frac{1}{2} \mathbb{E}_{y=1}[\log_2(1 + e^{-\ell})]
#'                    + \frac{1}{2} \mathbb{E}_{y=0}[\log_2(1 + e^{\ell})]
#' }{
#'   C_llr = 0.5 * E_{y=1}[log2(1 + exp(-l))] + 0.5 * E_{y=0}[log2(1 + exp(l))]
#' }
#'
#' This formula penalises log-likelihood ratios that are less than one when the items being compared truly are the same
#' source, penalises log-likelihood ratios that are greater than one when the items are different source. If your system
#' is working "perfectly" then the \eqn{C_{\mathrm{llr}}}{C_llr} should be closer to zero, and if your system is "not
#' working at all", that is, the log-likelihood ratios are uninformative, then \eqn{C_{\mathrm{llr}}}{C_llr} will be
#' closer to 1.
#'
#' The formula is senstive to calibration.
#'
#' @param log.lrs A vector of log-likelihood ratios from a set of same-source and different-source comparisons.
#' @param labels A vector of "ground truth" values, where it takes the value 1 if the comparision was same-source, and 0
#'   if the comparison was different-source.
#'
#' @returns A value between 0 and 1 which is the cost of log-likelihood ratios figure.
#' @export
#'
#' @examples
cllr = function(log.lrs, labels){
  log.lrs.ss = log.lrs[labels == 1]
  log.lrs.ds = log.lrs[labels == 0]
  cllr.ss = mean(log2(1 + exp(-log.lrs.ss)))
  cllr.ds = mean(log2(1 + exp(log.lrs.ds)))
  0.5 * (cllr.ss + cllr.ds)
}
