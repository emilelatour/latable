#' @title
#' latable: Tools for making tables of results
#'
#' @description
#' This package contains a variety of helper functions for making tables of
#' results easily.
#'
#' @examples
#' # Example usage:
#' library(latable)
#'
#' @keywords internal
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
## From infer package
## https://github.com/tidymodels/infer/blob/master/R/infer.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".",
      "Category", 
      "conf_high",
      "conf_low",
      "covariate",
      "est_ci",
      "est_ci_adjusted",
      "est_ci_unadjusted",
      "est_is_na",
      "est_is_na_adjusted",
      "est_is_na_unadjusted",
      "estimate",
      "estimate_adjusted",
      "estimate_unadjusted",
      "index",
      "level",
      "lower_ci",
      "lower_ci_adjusted",
      "lower_ci_unadjusted",
      "names_to_match",
      "p",
      "p_value",
      "p_value_adjusted",
      "p_value_lrt",
      "p_value_lrt_adjusted",
      "p_value_lrt_unadjusted",
      "p_value_unadjusted",
      "p_value_wald",
      "p_value_wald_adjusted",
      "p_value_wald_unadjusted",
      "pr_chisq",
      "pr_f", 
      "ref",
      "res_univ",
      "row_id",
      "rowname",
      "signif_adjusted",
      "signif_lrt_adjusted",
      "signif_unadjusted",
      "signif_wald_adjusted",
      "SMD",
      "term",
      "test",
      "upper_ci",
      "upper_ci_adjusted",
      "upper_ci_unadjusted",
      "vars"
    ))
  
}






