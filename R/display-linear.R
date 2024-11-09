
#' @title
#' Display linear regression results (given a model fit)
#'
#' @description
#' Given a `fit` object, univariable and (optionally) multivariable results from
#' linear regression are displayed in a format suitable for presentation.
#'
#' @param fit An object of class `lm` or `glm` with `family = gaussian`.
#' @param data A tibble or data frame with the full data set.
#' @param add_multi Logical; include multivariable results. Default is `FALSE`.
#' @param format Display format in case I need to escape some characters. A
#'   placeholder for now in case I need it in the future. Default is "html".
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly greater than 0 and less than 1. Defaults to 0.95, which
#'   corresponds to a 95 percent confidence interval.
#' @param include_last_row Adds a row at the end of each set of results to give
#'   some breathing room. Default is `TRUE`.
#'   
#' @importFrom stats gaussian pf
#'
#' @return A tibble
#'
#' @export
#'
#' @examples \dontrun{
#' # Example with the `mtcars` dataset:
#' model <- glm(mpg ~ cyl + disp, family = gaussian, data = mtcars)
#' display_linear(fit = model, data = mtcars, add_multi = TRUE)
#' }
display_linear <- function(fit,
                           data,
                           add_multi = FALSE,
                           format = "html",
                           conf_level = 0.95,
                           include_last_row = TRUE) {

  if (!(inherits(fit, "lm") || (inherits(fit, "glm") && fit$family$family == "gaussian"))) {
    stop("Model not from linear regression")
  }

  predictors <- attr(fit$terms, "term.labels")
  outcome <- names(fit$model)[1]


  ## Get the data set ----------------

  df <- data


  ## Univariable results ----------------
  uni_res <- tidyr::crossing(outcome,
                             predictors) %>%
    mutate(predictors = factor(predictors,
                               levels = attr(fit$terms, "term.labels"))) %>%
    dplyr::arrange(predictors) %>%
    mutate(predictors = as.character(predictors),
           formula = paste(outcome, "~", predictors),
           res_univ =
             purrr::map(.x = formula,
                        .f = ~ .do_linear_univ(data = df,
                                                 formula = .x,
                                                 format = format,
                                                 conf_level = conf_level,
                                                 include_last_row = include_last_row))) %>%
    dplyr::select(res_univ) %>%
    tidyr::unnest(col = res_univ)


  if (add_multi == TRUE) {

    ## Multivariable results ----------------


    multi_res <- fit %>%
      broom::tidy(.,
                  conf.int = TRUE,
                  conf.level = conf_level) %>%
      janitor::clean_names(.) %>%
      dplyr::select(term,
                    estimate_adjusted = estimate,
                    lower_ci_adjusted = conf_low,
                    upper_ci_adjusted = conf_high,
                    p_value_wald_adjusted = p_value)

    multi_res_lrt <- car::Anova(fit,
                                test.statistic = "F") %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      tibble::as_tibble(.) %>%
      janitor::clean_names() %>%
      dplyr::select(covariate = rowname,
                    p_value_lrt_adjusted = pr_f)


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



#' @title
#' Do univariable linear regression and extract results in nice format.
#'
#' @description
#' A helper function to be used in a loop to perform univariable linear regression
#' and output nicely formatted results.
#'
#' @param data A data frame or tibble.
#' @param formula A character string specifying the formula for the regression.
#' @param format Display format in case I need to escape some characters. A
#'   placeholder for now in case I need it in the future. Default is "html".
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly greater than 0 and less than 1. Defaults to 0.95, which
#'   corresponds to a 95 percent confidence interval.
#' @param include_last_row Adds a row at the end of each set of results to give
#'   some breathing room. Default is `TRUE`.
#' @param ... Additional arguments.
#'
#' @return A tibble or data frame with regression results.
#'
#' @importFrom broom tidy
#' @importFrom dplyr arrange bind_rows filter mutate rename select
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom purrr map
#' @importFrom stringr str_detect str_remove str_remove_all
#' @importFrom tibble as_tibble rownames_to_column tibble
#' @importFrom tidyr crossing unnest
#' @importFrom stats gaussian pf
#'
.do_linear_univ <- function(data,
                            formula,
                            format = "html",
                            conf_level = 0.95,
                            include_last_row = TRUE, ...) {


  #### Fit the model --------------------------------

  fit <- glm(formula = as.formula(formula),
             family = gaussian(link = "identity"),
             data = data)

  # independent <- attr(fit$terms, "term.labels")[[1]]
  independent <- attr(fit$terms, "term.labels")
  outcome <- names(fit$model)[1]

  indep_split <- unlist(stringr::str_split(independent, ":"))
  indep_split <- paste0(indep_split, collapse = "|")

  #### Overall p-value --------------------------------

  f_stat_pval <- summary(fit)$fstatistic
  lrt_pval <- if (!is.null(f_stat_pval)) {
    pf(f_stat_pval[1], f_stat_pval[2], f_stat_pval[3], lower.tail = FALSE)
  } else {
    NA_real_
  }

  res_overall <- tibble::tibble(
    covariate = independent,
    term = NA_character_,
    ref = NA_character_,
    estimate = NA_real_,
    lower_ci = NA_real_,
    upper_ci = NA_real_,
    p_value_wald = NA_real_,
    p_value_lrt = lrt_pval,
    signif = .calc_sig_ind(p_value_lrt, format)
  )

  #### Results by level --------------------------------

  if (any(class(data[[independent]]) %in% c("factor",
                                            "ordered",
                                            "logical",
                                            "character"))) {
    
    x_levels <- fit %>% 
      purrr::pluck(., 
                   "xlevels", 
                   1) %>% 
      .[-c(1)]

    res_by_level <- fit %>%
      broom::tidy(.,
                  conf.int = TRUE,
                  conf.level = conf_level) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
      # mutate(term = stringr::str_remove(term, independent),
      mutate(# term = stringr::str_remove_all(term, indep_split),
             term = x_levels, 
             ref = NA_character_,
             covariate = "") %>%
      dplyr::select(covariate,
                    term,
                    estimate,
                    lower_ci = conf_low,
                    upper_ci = conf_high,
                    p_value_wald = p_value) %>%
      mutate(p_value_lrt = NA_real_,
             signif = .calc_sig_ind(p_value_wald, format))

    fct_ref_lev <- levels(data[[independent]])[[1]]

    row_one <- tibble::tibble(
      covariate = NA_character_,
      # term = as.character(glue::glue("Ref lvl = {fct_ref_lev}")),
      term = fct_ref_lev,
      ref = "Reference level",
      estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      p_value_wald = NA_real_,
      p_value_lrt = NA_real_,
      signif = NA_character_)

    res_by_level <- dplyr::bind_rows(row_one,
                                     res_by_level)

  } else if (any(stringr::str_detect(independent, ":"))) {


    res_by_level <- fit %>%
      broom::tidy(.,
                  conf.int = TRUE,
                  conf.level = conf_level) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
      # mutate(term = stringr::str_remove(term, independent),
      mutate(term = stringr::str_remove_all(term, indep_split),
             ref = NA_character_,
             covariate = "") %>%
      dplyr::select(covariate,
                    term,
                    estimate,
                    lower_ci = conf_low,
                    upper_ci = conf_high,
                    p_value_wald = p_value) %>%
      mutate(p_value_lrt = NA_real_,
             signif = .calc_sig_ind(p_value_wald, format))

    # fct_ref_lev <- levels(data[[independent]])[[1]]
    fct_ref_lev <- "TBD"

    row_one <- tibble::tibble(
      covariate = NA_character_,
      # term = as.character(glue::glue("Ref lvl = {fct_ref_lev}")),
      term = fct_ref_lev,
      ref = "Reference level",
      estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      p_value_wald = NA_real_,
      p_value_lrt = NA_real_,
      signif = NA_character_)

    res_by_level <- dplyr::bind_rows(row_one,
                                     res_by_level)

  } else {

    res_by_level <- fit %>%
      broom::tidy(.,
                  conf.int = TRUE,
                  conf.level = conf_level) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
      mutate(covariate = "",
             term = "",
             ref = "") %>%
      dplyr::select(covariate,
                    term,
                    ref,
                    estimate,
                    lower_ci = conf_low,
                    upper_ci = conf_high,
                    p_value_wald = p_value) %>%
      mutate(p_value_lrt = NA_real_,
             signif = .calc_sig_ind(p_value_wald, format))

  }



  #### Combine results --------------------------------

  if (include_last_row == TRUE) {

    last_row <- tibble::tibble(
      covariate = NA_character_,
      term = NA_character_,
      ref = NA_character_,
      estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      p_value_wald = NA_real_,
      p_value_lrt = NA_real_,
      signif = NA_character_)

    res <- dplyr::bind_rows(
      res_overall,
      res_by_level,
      last_row)

  } else {

    res <- dplyr::bind_rows(
      res_overall,
      res_by_level)

  }

  res

}


#' @title
#' Display linear regression results (given variable names)
#'
#' @description
#' Given a string with an "outcome" and a vector string with "predictors",
#' univariable and (optionally) multivariable results from linear regression
#' are displayed in a format suitable for presentation.
#'
#' This version allows you to explicitly provide the outcome and predictors in
#' case there are issues with getting a full model fit.
#'
#' @param data A tibble or data frame with the full data set.
#' @param outcome Character string. The dependent variable (outcome) for
#'   linear regression.
#' @param predictors Character vector. Independent variables
#'   (predictors/covariates) for univariable and/or multivariable modelling.
#' @param add_multi Logical; include multivariable results. Default is `FALSE`.
#' @param format Display format in case I need to escape some characters. A
#'   placeholder for now in case I need it in the future. Default is "html".
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly greater than 0 and less than 1. Defaults to 0.95, which
#'   corresponds to a 95 percent confidence interval.
#' @param include_last_row Adds a row at the end of each set of results to give
#'   some breathing room. Default is `TRUE`.
#'
#' @return A tibble with regression results.
#'
#' @importFrom broom tidy
#' @importFrom car Anova
#' @importFrom dplyr arrange bind_rows coalesce filter if_else left_join mutate rename select
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom purrr map pluck
#' @importFrom stats as.formula lm
#' @importFrom stringr str_remove
#' @importFrom tibble as_tibble rownames_to_column tibble
#' @importFrom tidyr crossing unnest
#'
#' @export
#'
#' @examples \dontrun{
#' display_linear2(data = mtcars,
#'                 outcome = "mpg",
#'                 predictors = c("cyl", "disp"))
#'
#' display_linear2(data = mtcars,
#'                 outcome = "mpg",
#'                 predictors = c("cyl", "disp"),
#'                 add_multi = TRUE)
#' }
display_linear2 <- function(data,
                            outcome,
                            predictors,
                            add_multi = FALSE,
                            format = "html",
                            conf_level = 0.95,
                            include_last_row = TRUE) {

  ## Make the regression formula ----------------
  lhs <- glue::glue("{outcome}")
  rhs <- paste(predictors, collapse = " + ")
  form <- glue::glue("{lhs} ~ {rhs}")
  
  ## Use the full data set ----------------
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
                        .f = ~ .do_linear_univ(data = df,
                                                 formula = .x,
                                                 format = format,
                                                 conf_level = conf_level,
                                                 include_last_row = include_last_row))) %>%
    dplyr::select(res_univ) %>%
    tidyr::unnest(cols = res_univ)


  if (add_multi == TRUE) {

    ## Fit the model ----------------

    fit <- glm(formula = as.formula(form),
               family = gaussian(link = "identity"),
               data = df)

    ## Multivariable results ----------------

    multi_res <- fit %>%
      broom::tidy(.,
                  conf.int = TRUE,
                  conf.level = conf_level) %>%
      janitor::clean_names(.) %>%
      dplyr::select(term,
                    estimate_adjusted = estimate,
                    lower_ci_adjusted = conf_low,
                    upper_ci_adjusted = conf_high,
                    p_value_wald_adjusted = p_value)

    multi_res_lrt <- car::Anova(fit,
                                test.statistic = "F") %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      tibble::as_tibble(.) %>%
      janitor::clean_names() %>%
      dplyr::select(covariate = rowname,
                    p_value_lrt_adjusted = pr_f)


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
