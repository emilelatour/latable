#' @title
#' Display logistic regression results (given variable names)
#'
#' @description
#' Given a string with an "outcome" and a vector string with "predictors",
#' univariable and (optionally) multivariable results from logistic regression
#' are displayed in a format suitable for presentation.
#'
#' This version allows you to explicitly provide the outcome and predictors in
#' case there are issues with getting a full model fit.
#'
#' @param data A tibble or data frame with the full data set.
#' @param outcome Character string. The dependent variable (outcome) for
#'   logistic regression.
#' @param predictors Character vector. Independent variables
#'   (predictors/covariates) for univariable and/or multivariable modelling.
#' @param ref_level Character string. The factor level of outcome variable that
#'   corresponds to the true condition (1). If not provided then default is
#'   `NULL` and the model fit will determine the referenc level.
#' @param add_multi Logical; include multivariable results. Default is `FALSE`
#' @param format Display format in case I need to escape some characters. A
#'   place holder for now in case I need it in the future. Default is "html".
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly greater than 0 and less than 1. Defaults to 0.95, which
#'   corresponds to a 95 percent confidence interval.
#' @param exponentiate Logical indicating whether or not to exponentiate the
#'   the coefficient estimates. This is typical for logistic and multinomial
#'   regressions, but a bad idea if there is no log or logit link. Defaults to
#'   `TRUE`.
#' @param include_last_row Adds a row at the end of each set of results to give
#'   some breathing room. Default is `TRUE`.
#'
#' @importFrom broom tidy
#' @importFrom car Anova
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr coalesce
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr mutate_at
#' @importFrom dplyr na_if
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom labelled var_label
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @importFrom stats as.formula
#' @importFrom stats binomial
#' @importFrom stats drop1
#' @importFrom stats formula
#' @importFrom stats glm
#' @importFrom stringr str_remove
#' @importFrom tibble as_tibble
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr crossing
#' @importFrom tidyr unnest
#'
#' @return A tibble
#'
#' @export
#'
#' @examples \dontrun{
#' library(epiDisplay)
#' library(tidyverse)
#' dplyr::glimpse(infert)
#'
#' case ~ induced + spontaneous + education
#'
#' display_logistic2(data = infert,
#'                   outcome = "case",
#'                   predictors = c("induced", "spontaneous", "education"))
#'
#' display_logistic2(data = infert,
#'                   outcome = "case",
#'                   predictors = c("induced", "spontaneous", "education"),
#'                   add_multi = TRUE)
#'
#' # How does an interaction term work?
#' # Doesn't work yet (2019-03-23)
#' # display_logistic2(data = infert,
#' #                   outcome = "case",
#' #                   predictors = c("induced * education"))
#'
#' #### Another data set --------------------------------
#' library(compareGroups)
#' data(predimed)
#' dplyr::glimpse(predimed)
#' predimed <- predimed %>%
#'   mutate_if(is.double, as.double)
#'
#' fit = glm(htn ~ sex + bmi + smoke,
#'           family = binomial(link = "logit"),
#'           data = predimed)
#'
#' display_logistic2(data = predimed,
#'                   outcome = "htn",
#'                   predictors = c("sex", "bmi", "smoke"),
#'                   add_multi = TRUE)
#'  }




display_logistic2 <- function(data,
                              outcome,
                              predictors,
                              ref_level = NULL,
                              add_multi = FALSE,
                              format = "html",
                              conf_level = 0.95,
                              exponentiate = TRUE,
                              include_last_row = TRUE) {

  # if (length(class(fit)) == 1) {
  #   stop("Model not from logistic regression")
  # }
  # if (class(fit)[1] != "glm" | class(fit)[2] != "lm" |
  #     fit$family$family != "binomial") {
  #   stop("Model not from logistic regression")
  # }

  ## Make the regression formula ----------------

  lhs <- if (!is.null(ref_level)) {

    glue::glue("({outcome} == '{ref_level}')")

  } else {

    glue::glue("{outcome}")

  }

  rhs <- paste(predictors, collapse = " + ")

  form <- glue::glue("{lhs} ~ {rhs}")
  outcome <- lhs


  ## Get the data set ----------------

  # if (add_multi == TRUE) {
  #
  #   df <- fit$model
  #
  # } else {
  #
  #   df <- data
  #
  # }
  # TODO -- not sure about the data set that should be used if doing uni vs
  # multi. For now just use the full data set.
  df <- data
  pred_lvls <- predictors

  ## Univariable results ----------------

  uni_res <- tidyr::crossing(outcome,
                             predictors) %>%
    mutate(predictors = factor(predictors,
                               levels = pred_lvls)) %>%
    dplyr::arrange(predictors) %>%
    mutate(predictors = as.character(predictors),
           formula = paste(outcome, "~", predictors),
           res_univ =
             purrr::map(.x = formula,
                        .f = ~ .do_logistic_univ(data = df,
                                                 formula = .x,
                                                 format = format,
                                                 conf_level = conf_level,
                                                 exponentiate = exponentiate,
                                                 include_last_row = include_last_row))) %>%
    dplyr::select(res_univ) %>%
    tidyr::unnest(cols = res_univ)


  if (add_multi == TRUE) {

    ## Fit the model ----------------

    fit <- glm(formula = as.formula(form),
               family = binomial(link = "logit"),
               data = df)

    ## Multivariable results ----------------


    multi_res <- fit %>%
      broom::tidy(.,
                  conf.int = TRUE,
                  conf.level = conf_level,
                  exponentiate = exponentiate) %>%
      janitor::clean_names(.) %>%
      dplyr::select(term,
                    estimate_adjusted = estimate,
                    lower_ci_adjusted = conf_low,
                    upper_ci_adjusted = conf_high,
                    p_value_wald_adjusted = p_value)

    multi_res_lrt <- car::Anova(fit,
                                test.statistic = "LR") %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      tibble::as_tibble(.) %>%
      janitor::clean_names() %>%
      dplyr::select(covariate = rowname,
                    p_value_lrt_adjusted = pr_chisq)


    ## Combine to univariable results ----------------

    res <- uni_res %>%
      mutate(
        names_to_match = dplyr::na_if(covariate, ""),
        names_to_match = .repeat_last(names_to_match),
        names_to_match = paste0(names_to_match, term),
        names_to_match = dplyr::if_else(estimate == "",
                                        "",
                                        names_to_match),
        names_to_match = trimws(names_to_match, which = "left"),
        covariate = trimws(covariate, which = "left")) %>%
      left_join(.,
                multi_res,
                by = c("names_to_match" = "term")) %>%
      dplyr::left_join(.,
                       multi_res_lrt,
                       by = "covariate") %>%
      mutate(signif_wald_adjusted =
               .calc_sig_ind(p_value_wald_adjusted, format),
             signif_lrt_adjusted =
               .calc_sig_ind(p_value_lrt_adjusted, format),
             signif_adjusted = dplyr::coalesce(signif_wald_adjusted,
                                               signif_lrt_adjusted)) %>%
      dplyr::select(-names_to_match,
                    -signif_wald_adjusted,
                    -signif_lrt_adjusted) %>%
      dplyr::rename(estimate_unadjusted = estimate,
                    lower_ci_unadjusted = lower_ci,
                    upper_ci_unadjusted = upper_ci,
                    p_value_wald_unadjusted = p_value_wald,
                    p_value_lrt_unadjusted = p_value_lrt,
                    signif_unadjusted = signif)

  } else {

    res <- uni_res
  }

  res

}
