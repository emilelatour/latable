
#' @title Display Cox Proportional Hazards Regression Results
#'
#' @description
#' Given a `coxph` model fit object, displays univariable and (optionally) multivariable results 
#' from Cox proportional hazards regression in a format suitable for presentation.
#'
#' @param fit An object of class `coxph` from the `survival` package, representing a Cox proportional hazards model.
#' @param data A tibble or data frame with the full dataset used for modeling.
#' @param add_multi Logical; whether to include multivariable results. Default is `FALSE`.
#' @param format Display format in case characters need escaping. Placeholder for future use. Default is `"html"`.
#' @param conf_level The confidence level to use for confidence intervals. Must be between 0 and 1. Default is 0.95.
#' @param exponentiate Logical; whether to exponentiate coefficient estimates to show hazard ratios. Default is `TRUE`.
#' @param include_last_row Logical; if `TRUE`, adds a blank row at the end for spacing. Default is `TRUE`.
#'
#' @importFrom broom tidy
#' @importFrom survival coxph
#' @importFrom dplyr arrange bind_rows left_join mutate select
#' @importFrom purrr map
#' @importFrom tidyr crossing unnest
#' @importFrom janitor clean_names
#' @importFrom car Anova
#' @importFrom stats alias anova
#'
#' @return A tibble containing the Cox regression results.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' data(lung)
#' 
#' # Fit a Cox model
#' fit <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
#' 
#' # Display Cox regression results
#' display_coxph(fit, data = lung, add_multi = TRUE)
#' }
#'
#' @export
display_coxph <- function(fit, data, add_multi = FALSE, format = "html", conf_level = 0.95, 
                          exponentiate = TRUE, include_last_row = TRUE) {
  if (!inherits(fit, "coxph")) {
    stop("Model not from Cox proportional hazards regression")
  }
  
  # Silence no visible binding for global variable
  pr_chi <- NULL
  
  predictors <- attr(fit$terms, "term.labels")
  outcome <- stringr::str_extract(as.character(fit$formula), "^[^~]+")[2]
  
  ## Use the full dataset
  df <- data
  
  ## Univariable results
  uni_res <- tidyr::crossing(outcome, predictors) %>%
    mutate(predictors = factor(predictors, levels = attr(fit$terms, "term.labels"))) %>%
    dplyr::arrange(predictors) %>%
    mutate(predictors = as.character(predictors),
           formula = glue::glue("{outcome} ~ {predictors}"),
           res_univ = purrr::map(.x = formula,
                                 .f = ~ .do_coxph_univ(data = df,
                                                       formula = .x,
                                                       format = format,
                                                       conf_level = conf_level,
                                                       exponentiate = exponentiate,
                                                       include_last_row = include_last_row))) %>%
    dplyr::select(res_univ) %>%
    tidyr::unnest(col = res_univ)
  
  ## Multivariable results
  if (add_multi == TRUE) {
    
    # Extract multivariable results
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
    
    # Check for potential multicollinearity
    alias_check <- alias(fit)$Complete
    
    if (any(alias_check, na.rm = TRUE)) {
      
      aliased_terms <- names(alias_check[alias_check == TRUE])
      warning("Model contains aliased terms (perfect collinearity detected) in the following variables: ", 
              paste(aliased_terms, collapse = ", "))
    }
    
    
    # Likelihood ratio test
    multi_res_lrt <- anova(fit, test = "Chisq") %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "covariate") %>%
      janitor::clean_names() %>%
      dplyr::select(covariate, p_value_lrt_adjusted = pr_chi) |> 
      dplyr::filter(!is.null(covariate))
    
    ## Combine results
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
                       by = "covariate")%>%
      mutate(signif_adjusted = purrr::map_chr(.x = p_value_wald_adjusted, 
                                              .f = ~ .calc_sig_ind(.x, format))) %>%
      dplyr::rename(estimate_unadjusted = estimate,
                    lower_ci_unadjusted = lower_ci,
                    upper_ci_unadjusted = upper_ci,
                    p_value_wald_unadjusted = p_value_wald,
                    p_value_lrt_unadjusted = p_value_lrt)
  } else {
    res <- uni_res
  }
  
  res
}


#' @title Univariable Cox Proportional Hazards Regression Helper
#'
#' @description
#' A helper function to perform univariable Cox proportional hazards regression and 
#' extract results in a presentation-ready format.
#'
#' @param data A tibble or data frame containing the full dataset used for modeling.
#' @param formula A character string representing the model formula (e.g., "Surv(time, status) ~ age").
#' @param format Display format in case characters need escaping. Placeholder for future use. Default is `"html"`.
#' @param conf_level The confidence level to use for confidence intervals. Must be between 0 and 1. Default is 0.95.
#' @param exponentiate Logical; whether to exponentiate coefficient estimates to show hazard ratios. Default is `TRUE`.
#' @param include_last_row Logical; if `TRUE`, adds a blank row at the end for spacing. Default is `TRUE`.
#' @param test Type of overall test to perform Likelihood ratio test ("LRT"), Wald test ("Wald"), Score (logrank) test ("Score").
#'
#' @importFrom broom tidy
#' @importFrom survival coxph
#' @importFrom dplyr mutate select bind_rows
#' @importFrom purrr pluck
#' @importFrom tibble tibble
#' @importFrom janitor clean_names
#'
#' @return A tibble with the univariable Cox regression results for the specified predictor.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' data(lung)
#' 
#' # Perform univariable Cox regression on a single predictor
#' .do_coxph_univ(data = lung, formula = "Surv(time, status) ~ age")
#' }
.do_coxph_univ <- function(data, 
                           formula, 
                           format = "html", 
                           conf_level = 0.95, 
                           exponentiate = TRUE, 
                           include_last_row = TRUE, 
                           test = "LRT") {
  
  #### Fit the model --------------------------------
  
  fit <- coxph(as.formula(formula), data = data)
  
  # independent <- attr(fit$terms, "term.labels")[[1]]
  independent <- attr(fit$terms, "term.labels")
  outcome <- stringr::str_extract(as.character(fit$formula), "^[^~]+")[2]
  
  indep_split <- unlist(stringr::str_split(independent, ":"))
  indep_split <- paste0(indep_split, collapse = "|")
  
  #### Overall p-value --------------------------------
  
  if (test == "LRT") {
    lrt_pval <- purrr::pluck(summary(fit), "logtest", "pvalue")
  } else if (test == "Wald") {
    lrt_pval <- purrr::pluck(summary(fit), "waldtest", "pvalue")
  } else if (test == "Score") {
    lrt_pval <- purrr::pluck(summary(fit), "sctest", "pvalue")
  }
  
  lrt_pval <- ifelse(length(lrt_pval) == 0, NA, lrt_pval)
  
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
                  conf.level = conf_level,
                  exponentiate = exponentiate) %>%
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
             signif = purrr::map_chr(.x = p_value_wald, 
                                     .f = ~ .calc_sig_ind(.x, format)))
    
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
                  conf.level = conf_level,
                  exponentiate = exponentiate) %>%
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
             signif = purrr::map_chr(.x = p_value_wald, 
                                     .f = ~ .calc_sig_ind(.x, format)))
    
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
                  conf.level = conf_level,
                  exponentiate = exponentiate) %>%
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
             signif = purrr::map_chr(.x = p_value_wald, 
                                     .f = ~ .calc_sig_ind(.x, format)))
    
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
#' Display Cox Proportional Hazards Regression Results (given variable names)
#'
#' @description
#' Given a string with an "outcome" and a vector string with "predictors",
#' displays univariable and (optionally) multivariable results from Cox proportional hazards regression
#' in a format suitable for presentation.
#'
#' This version allows you to explicitly provide the outcome and predictors in
#' case there are issues with getting a full model fit.
#'
#' @param data A tibble or data frame with the full dataset.
#' @param outcome Character string. The dependent variable (outcome) for
#'   Cox regression, formatted as "Surv(time, event)".
#' @param predictors Character vector. Independent variables
#'   (predictors/covariates) for univariable and/or multivariable modeling.
#' @param add_multi Logical; include multivariable results. Default is `FALSE`
#' @param format Display format in case characters need escaping. Placeholder for future use. Default is "html".
#' @param conf_level The confidence level to use for confidence intervals.
#'   Must be between 0 and 1. Default is 0.95.
#' @param exponentiate Logical; whether to exponentiate coefficient estimates to show hazard ratios. Default is `TRUE`.
#' @param include_last_row Adds a row at the end of each set of results for spacing. Default is `TRUE`.
#'
#' @importFrom broom tidy
#' @importFrom car Anova
#' @importFrom dplyr arrange bind_rows coalesce filter left_join mutate select
#' @importFrom purrr map
#' @importFrom stats as.formula
#' @importFrom survival coxph Surv
#' @importFrom tidyr crossing unnest
#' @importFrom stats alias anova
#'
#' @return A tibble containing Cox regression results.
#'
#' @export
#'
#' @examples \dontrun{
#' library(survival)
#' data(lung)
#'
#' display_coxph2(data = lung,
#'                outcome = "Surv(time, status)",
#'                predictors = c("age", "sex", "ph.ecog"))
#'
#' display_coxph2(data = lung,
#'                outcome = "Surv(time, status)",
#'                predictors = c("age", "sex", "ph.ecog"),
#'                add_multi = TRUE)
#' }
display_coxph2 <- function(data,
                           outcome,
                           predictors,
                           add_multi = FALSE,
                           format = "html",
                           conf_level = 0.95,
                           exponentiate = TRUE,
                           include_last_row = TRUE) {
  
  # Silence no visible binding for global variable
  pr_chi <- NULL
  
  ## Create the regression formula
  rhs <- paste(predictors, collapse = " + ")
  form <- glue::glue("{outcome} ~ {rhs}")
  
  pred_lvls <- predictors
  
  ## Univariable results
  suppressWarnings(
    uni_res <- tidyr::crossing(outcome, predictors) %>%
      mutate(predictors = factor(predictors, levels = pred_lvls)) %>%
      dplyr::arrange(predictors) %>%
      mutate(predictors = as.character(predictors),
             formula = paste(outcome, "~", predictors),
             res_univ = purrr::map(.x = formula,
                                   .f = ~ .do_coxph_univ(data = data,
                                                         formula = .x,
                                                         format = format,
                                                         conf_level = conf_level,
                                                         exponentiate = exponentiate,
                                                         include_last_row = include_last_row))) %>%
      dplyr::select(res_univ) %>%
      tidyr::unnest(cols = res_univ)
  )
  
  ## Multivariable results
  if (add_multi == TRUE) {
    
    # Fit the Cox model
    fit <- coxph(as.formula(form), data = data)
    
    # Extract multivariable results
    multi_res <- fit %>%
      broom::tidy(conf.int = TRUE, conf.level = conf_level, exponentiate = exponentiate) %>%
      janitor::clean_names() %>%
      dplyr::select(term,
                    estimate_adjusted = estimate,
                    lower_ci_adjusted = conf_low,
                    upper_ci_adjusted = conf_high,
                    p_value_wald_adjusted = p_value)
    
    # Check for potential multicollinearity
    alias_check <- alias(fit)$Complete
    
    if (any(alias_check, na.rm = TRUE)) {
      
      aliased_terms <- names(alias_check[alias_check == TRUE])
      
      warning("Model contains aliased terms (perfect collinearity detected) in the following variables: ", 
              paste(aliased_terms, collapse = ", "))
    }
    
    # Likelihood ratio test
    suppressWarnings(
      multi_res_lrt <- anova(fit, test = "Chisq") %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "covariate") %>%
        janitor::clean_names() %>%
        dplyr::select(covariate, p_value_lrt_adjusted = pr_chi) |> 
        dplyr::filter(!is.null(covariate))
    )
    
    ## Combine with univariable results
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
                       by = "covariate")%>%
      mutate(signif_adjusted = purrr::map_chr(.x = p_value_wald_adjusted, 
                                              .f = ~ .calc_sig_ind(.x, format))) %>%
      dplyr::rename(estimate_unadjusted = estimate,
                    lower_ci_unadjusted = lower_ci,
                    upper_ci_unadjusted = upper_ci,
                    p_value_wald_unadjusted = p_value_wald,
                    p_value_lrt_unadjusted = p_value_lrt)
  } else {
    res <- uni_res
  }
  
  res
}


