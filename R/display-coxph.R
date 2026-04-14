#' @title Display Cox Proportional Hazards Regression Results
#'
#' @description
#' Given a `coxph` model fit object, displays univariable and (optionally)
#' multivariable results from Cox proportional hazards regression in a format
#' suitable for presentation.
#'
#' Handles stratified models (e.g., `strata(var)`) and counting process data
#' (e.g., `Surv(tstart, tstop, event)`) with an optional `id` argument for
#' correct variance estimation.
#'
#' @param fit An object of class `coxph` from the `survival` package.
#' @param data A tibble or data frame with the full dataset used for modeling.
#' @param id Optional. Character string naming the cluster/subject ID variable
#'   for counting process (time-varying) models. Passed to `coxph()` for
#'   correct robust variance estimation. Default is `NULL`.
#' @param add_multi Logical; whether to include multivariable results. Default
#'   is `FALSE`.
#' @param format Display format. Default is `"html"`.
#' @param conf_level Confidence level for intervals. Default is `0.95`.
#' @param exponentiate Logical; exponentiate estimates to HRs. Default is `TRUE`.
#' @param include_last_row Logical; add blank spacing row. Default is `TRUE`.
#'
#' @importFrom broom tidy
#' @importFrom survival coxph
#' @importFrom dplyr arrange bind_rows left_join mutate select
#' @importFrom purrr map
#' @importFrom tidyr crossing unnest
#' @importFrom janitor clean_names
#' @importFrom stringr str_detect str_extract
#' @importFrom stats alias complete.cases
#'
#' @return A tibble containing the Cox regression results.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' data(lung)
#'
#' fit <- coxph(Surv(time, status) ~ age + sex + strata(ph.ecog), data = lung)
#' display_coxph(fit, data = lung, add_multi = TRUE)
#'
#' # Counting process / time-varying exposure
#' fit_tv <- coxph(Surv(tstart, tstop, event) ~ trt + strata(grp),
#'                 data = df, id = subject_id)
#' display_coxph(fit_tv, data = df, id = "subject_id", add_multi = TRUE)
#' }
#'
#' @export
display_coxph <- function(fit,
                          data,
                          id            = NULL,
                          add_multi     = FALSE,
                          format        = "html",
                          conf_level    = 0.95,
                          exponentiate  = TRUE,
                          include_last_row = TRUE) {

  if (!inherits(fit, "coxph")) {
    stop("Model not from Cox proportional hazards regression")
  }

  # Silence no visible binding for global variable
  pr_chi <- NULL

  # Extract term labels and remove strata() terms — strata terms have no
  # coefficient and cannot be fit as univariable models
  all_terms  <- attr(fit$terms, "term.labels")
  strata_terms    <- all_terms[stringr::str_detect(all_terms, "^strata\\(")]
  predictors <- all_terms[!stringr::str_detect(all_terms, "^strata\\(")]

  outcome <- stringr::str_extract(as.character(fit$formula), "^[^~]+")[2]

  ## Use the full dataset
  df <- data

  ## Univariable results — strata terms excluded, id passed through
  uni_res <- tidyr::crossing(outcome, predictors) %>%
    mutate(
      predictors = factor(predictors, levels = predictors),
      predictors = as.character(predictors),
      formula    = glue::glue("{outcome} ~ {predictors}"),
      res_univ   = purrr::map(
        .x = formula,
        .f = ~ .do_coxph_univ(
          data             = df,
          formula          = .x,
          id               = id,
          format           = format,
          conf_level       = conf_level,
          exponentiate     = exponentiate,
          include_last_row = include_last_row
        )
      )
    ) %>%
    dplyr::select(res_univ) %>%
    tidyr::unnest(col = res_univ)

  ## Multivariable results
  if (add_multi == TRUE) {

    # broom::tidy on a stratified model includes a strata row that breaks
    # downstream joins — filter it out
    multi_res <- fit %>%
      broom::tidy(
        conf.int   = TRUE,
        conf.level = conf_level,
        exponentiate = exponentiate
      ) %>%
      janitor::clean_names() %>%
      dplyr::filter(!stringr::str_detect(term, "^strata\\(")) %>%
      dplyr::select(
        term,
        estimate_adjusted    = estimate,
        lower_ci_adjusted    = conf_low,
        upper_ci_adjusted    = conf_high,
        p_value_wald_adjusted = p_value
      )

    # Check for potential multicollinearity
    alias_check <- alias(fit)$Complete

    if (!is.null(alias_check) && any(alias_check, na.rm = TRUE)) {
      aliased_terms <- names(alias_check[alias_check == TRUE])
      warning(
        "Model contains aliased terms (perfect collinearity detected): ",
        paste(aliased_terms, collapse = ", ")
      )
    }

    # Type III LRT via drop1() — requires complete cases to avoid row-count
    # mismatch errors when missingness differs across variables. Refit on the
    # same complete-case subset that coxph used internally.
    model_vars   <- all.vars(fit$formula)
    data_complete <- df[complete.cases(df[, intersect(model_vars, names(df))]), ]

    if (!is.null(id)) {
      fit_cc <- coxph(fit$formula, data = data_complete,
                      id = get(id, envir = as.environment(data_complete)))
    } else {
      fit_cc <- coxph(fit$formula, data = data_complete)
    }

    suppressWarnings(
      multi_res_lrt <- drop1(fit_cc, test = "Chisq") %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "covariate") %>%
        janitor::clean_names() %>%
        dplyr::select(covariate, p_value_lrt_adjusted = pr_chi) %>%
        dplyr::filter(covariate != "<none>")
    )

    ## Combine results
    res <- uni_res %>%
      mutate(
        names_to_match = dplyr::na_if(covariate, ""),
        names_to_match = .repeat_last(names_to_match),
        names_to_match = paste0(names_to_match, term),
        names_to_match = dplyr::if_else(
          is.na(estimate) | estimate == "",
          "",
          names_to_match
        ),
        names_to_match = trimws(names_to_match, which = "left"),
        covariate      = trimws(covariate, which = "left")
      ) %>%
      dplyr::left_join(multi_res,     by = c("names_to_match" = "term")) %>%
      dplyr::left_join(multi_res_lrt, by = "covariate") %>%
      mutate(
        signif_adjusted = purrr::map_chr(
          .x = p_value_wald_adjusted,
          .f = ~ .calc_sig_ind(.x, format)
        )
      ) %>%
      dplyr::rename(
        estimate_unadjusted    = estimate,
        lower_ci_unadjusted    = lower_ci,
        upper_ci_unadjusted    = upper_ci,
        p_value_wald_unadjusted = p_value_wald,
        p_value_lrt_unadjusted  = p_value_lrt
      )

  } else {
    res <- uni_res
  }

  # Add note about stratification if strata terms were present
  if (length(strata_terms) > 0) {
    strata_note <- paste(strata_terms, collapse = ", ")
    message(
      "Note: The following strata term(s) were excluded from univariable ",
      "models and have no HR estimate: ", strata_note
    )
  }

  res
}


#' @title Univariable Cox Proportional Hazards Regression Helper
#'
#' @description
#' Internal helper. Fits a univariable Cox model and returns results in a
#' presentation-ready tibble. Handles continuous, categorical, and interaction
#' terms. Passes `id` through for counting process / time-varying data.
#'
#' @param data A tibble or data frame.
#' @param formula Character string model formula.
#' @param id Optional. Character string naming the cluster/subject ID variable.
#' @param format Display format. Default is `"html"`.
#' @param conf_level Confidence level. Default is `0.95`.
#' @param exponentiate Logical. Default is `TRUE`.
#' @param include_last_row Logical. Default is `TRUE`.
#' @param test Overall p-value type: `"LRT"`, `"Wald"`, or `"Score"`.
#'
#' @importFrom broom tidy
#' @importFrom survival coxph
#' @importFrom dplyr mutate select bind_rows
#' @importFrom purrr pluck map_chr
#' @importFrom tibble tibble
#' @importFrom janitor clean_names
#' @importFrom stringr str_split str_detect
#'
#' @return A tibble with univariable Cox results for the specified predictor.
.do_coxph_univ <- function(data,
                           formula,
                           id               = NULL,
                           format           = "html",
                           conf_level       = 0.95,
                           exponentiate     = TRUE,
                           include_last_row = TRUE,
                           test             = "LRT") {

  #### Fit the model --------------------------------

  # Build coxph call — pass id for counting process data if provided
  if (!is.null(id)) {
    fit <- coxph(as.formula(formula), data = data, id = get(id, envir = as.environment(data)))
  } else {
    fit <- coxph(as.formula(formula), data = data)
  }

  independent <- attr(fit$terms, "term.labels")
  # Remove any strata terms that sneak through (shouldn't happen in univariable
  # context but guards against edge cases)
  independent <- independent[!stringr::str_detect(independent, "^strata\\(")]

  outcome <- stringr::str_extract(as.character(fit$formula), "^[^~]+")[2]

  indep_split <- unlist(stringr::str_split(independent, ":"))
  indep_split <- paste0(indep_split, collapse = "|")

  #### Overall p-value --------------------------------

  if (test == "LRT") {
    lrt_pval <- purrr::pluck(summary(fit), "logtest",  "pvalue")
  } else if (test == "Wald") {
    lrt_pval <- purrr::pluck(summary(fit), "waldtest", "pvalue")
  } else if (test == "Score") {
    lrt_pval <- purrr::pluck(summary(fit), "sctest",   "pvalue")
  }

  lrt_pval <- ifelse(length(lrt_pval) == 0, NA, lrt_pval)

  res_overall <- tibble::tibble(
    covariate   = independent,
    term        = NA_character_,
    ref         = NA_character_,
    estimate    = NA_real_,
    lower_ci    = NA_real_,
    upper_ci    = NA_real_,
    p_value_wald = NA_real_,
    p_value_lrt = lrt_pval,
    signif      = .calc_sig_ind(p_value_lrt, format)
  )

  #### Results by level --------------------------------

  if (any(class(data[[independent]]) %in% c("factor", "ordered",
                                             "logical", "character"))) {

    x_levels <- fit %>%
      purrr::pluck("xlevels", 1) %>%
      .[-c(1)]

    res_by_level <- fit %>%
      broom::tidy(
        conf.int     = TRUE,
        conf.level   = conf_level,
        exponentiate = exponentiate
      ) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
      mutate(
        term      = x_levels,
        ref       = NA_character_,
        covariate = ""
      ) %>%
      dplyr::select(
        covariate, term, estimate,
        lower_ci    = conf_low,
        upper_ci    = conf_high,
        p_value_wald = p_value
      ) %>%
      mutate(
        p_value_lrt = NA_real_,
        signif = purrr::map_chr(.x = p_value_wald,
                                .f = ~ .calc_sig_ind(.x, format))
      )

    fct_ref_lev <- levels(data[[independent]])[[1]]

    row_one <- tibble::tibble(
      covariate    = NA_character_,
      term         = fct_ref_lev,
      ref          = "Reference level",
      estimate     = NA_real_,
      lower_ci     = NA_real_,
      upper_ci     = NA_real_,
      p_value_wald = NA_real_,
      p_value_lrt  = NA_real_,
      signif       = NA_character_
    )

    res_by_level <- dplyr::bind_rows(row_one, res_by_level)

  } else if (any(stringr::str_detect(independent, ":"))) {

    res_by_level <- fit %>%
      broom::tidy(
        conf.int     = TRUE,
        conf.level   = conf_level,
        exponentiate = exponentiate
      ) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
      mutate(
        term      = stringr::str_remove_all(term, indep_split),
        ref       = NA_character_,
        covariate = ""
      ) %>%
      dplyr::select(
        covariate, term, estimate,
        lower_ci    = conf_low,
        upper_ci    = conf_high,
        p_value_wald = p_value
      ) %>%
      mutate(
        p_value_lrt = NA_real_,
        signif = purrr::map_chr(.x = p_value_wald,
                                .f = ~ .calc_sig_ind(.x, format))
      )

    row_one <- tibble::tibble(
      covariate    = NA_character_,
      term         = "TBD",
      ref          = "Reference level",
      estimate     = NA_real_,
      lower_ci     = NA_real_,
      upper_ci     = NA_real_,
      p_value_wald = NA_real_,
      p_value_lrt  = NA_real_,
      signif       = NA_character_
    )

    res_by_level <- dplyr::bind_rows(row_one, res_by_level)

  } else {

    res_by_level <- fit %>%
      broom::tidy(
        conf.int     = TRUE,
        conf.level   = conf_level,
        exponentiate = exponentiate
      ) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
      mutate(covariate = "", term = "", ref = "") %>%
      dplyr::select(
        covariate, term, ref, estimate,
        lower_ci    = conf_low,
        upper_ci    = conf_high,
        p_value_wald = p_value
      ) %>%
      mutate(
        p_value_lrt = NA_real_,
        signif = purrr::map_chr(.x = p_value_wald,
                                .f = ~ .calc_sig_ind(.x, format))
      )
  }

  #### Combine results --------------------------------

  if (include_last_row == TRUE) {

    last_row <- tibble::tibble(
      covariate    = NA_character_,
      term         = NA_character_,
      ref          = NA_character_,
      estimate     = NA_real_,
      lower_ci     = NA_real_,
      upper_ci     = NA_real_,
      p_value_wald = NA_real_,
      p_value_lrt  = NA_real_,
      signif       = NA_character_
    )

    res <- dplyr::bind_rows(res_overall, res_by_level, last_row)

  } else {

    res <- dplyr::bind_rows(res_overall, res_by_level)
  }

  res
}


#' @title Display Cox Proportional Hazards Regression Results (given variable names)
#'
#' @description
#' Given a string `outcome` and a character vector `predictors`, displays
#' univariable and (optionally) multivariable Cox proportional hazards results
#' in a presentation-ready format.
#'
#' Handles stratified models and counting process data. Strata terms passed in
#' `predictors` are detected and routed to the multivariable model only — they
#' are excluded from univariable screening since they produce no HR estimate.
#'
#' @param data A tibble or data frame.
#' @param outcome Character string. Outcome formatted as `"Surv(time, event)"`
#'   or `"Surv(tstart, tstop, event)"`.
#' @param predictors Character vector of predictors. May include `strata()`
#'   terms (e.g., `"strata(ulcer_age_cat)"`); these will be excluded from
#'   univariable models automatically.
#' @param id Optional. Character string naming the cluster/subject ID variable
#'   for counting process models. Passed to `coxph()` for robust variance
#'   estimation. Default is `NULL`.
#' @param add_multi Logical; include multivariable results. Default is `FALSE`.
#' @param format Display format. Default is `"html"`.
#' @param conf_level Confidence level. Default is `0.95`.
#' @param exponentiate Logical; exponentiate to HRs. Default is `TRUE`.
#' @param include_last_row Logical; add blank spacing row. Default is `TRUE`.
#'
#' @importFrom broom tidy
#' @importFrom dplyr arrange bind_rows filter left_join mutate rename select
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom purrr map map_chr
#' @importFrom stringr str_detect
#' @importFrom survival coxph
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr crossing unnest
#' @importFrom stats alias complete.cases as.formula
#'
#' @return A tibble containing Cox regression results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(survival)
#' data(lung)
#'
#' # Standard Cox
#' display_coxph2(data = lung,
#'                outcome = "Surv(time, status)",
#'                predictors = c("age", "sex", "ph.ecog"),
#'                add_multi = TRUE)
#'
#' # Stratified Cox with counting process data
#' display_coxph2(data = data_tte,
#'                outcome = "Surv(tstart, tstop, healed)",
#'                predictors = c("sharp_debridement", "ulcer_size",
#'                               "age_at_current_visit", "diabetes",
#'                               "strata(ulcer_age_cat)"),
#'                id = "mrn",
#'                add_multi = TRUE)
#' }
display_coxph2 <- function(data,
                           outcome,
                           predictors,
                           id               = NULL,
                           add_multi        = FALSE,
                           format           = "html",
                           conf_level       = 0.95,
                           exponentiate     = TRUE,
                           include_last_row = TRUE) {

  # Silence no visible binding for global variable
  pr_chi <- NULL

  # Separate strata terms from modelable predictors
  # Strata terms are passed to the multivariable model but excluded from
  # univariable screening — they produce no HR estimate
  strata_terms   <- predictors[stringr::str_detect(predictors, "^strata\\(")]
  uni_predictors <- predictors[!stringr::str_detect(predictors, "^strata\\(")]

  ## Build full model formula (includes strata terms)
  rhs  <- paste(predictors, collapse = " + ")
  form <- glue::glue("{outcome} ~ {rhs}")

  ## Univariable results — strata-free predictors only, id passed through
  suppressWarnings(
    uni_res <- tidyr::crossing(outcome, predictors = uni_predictors) %>%
      mutate(
        predictors = factor(predictors, levels = uni_predictors),
        predictors = as.character(predictors),
        formula    = paste(outcome, "~", predictors),
        res_univ   = purrr::map(
          .x = formula,
          .f = ~ .do_coxph_univ(
            data             = data,
            formula          = .x,
            id               = id,
            format           = format,
            conf_level       = conf_level,
            exponentiate     = exponentiate,
            include_last_row = include_last_row
          )
        )
      ) %>%
      dplyr::select(res_univ) %>%
      tidyr::unnest(cols = res_univ)
  )

  ## Multivariable results
  if (add_multi == TRUE) {

    # Fit full model including strata terms, with id if provided
    if (!is.null(id)) {
      fit <- coxph(as.formula(form), data = data,
                   id = get(id, envir = as.environment(data)))
    } else {
      fit <- coxph(as.formula(form), data = data)
    }

    # Tidy — filter out strata rows which have no coefficient
    multi_res <- fit %>%
      broom::tidy(
        conf.int     = TRUE,
        conf.level   = conf_level,
        exponentiate = exponentiate
      ) %>%
      janitor::clean_names() %>%
      dplyr::filter(!stringr::str_detect(term, "^strata\\(")) %>%
      dplyr::select(
        term,
        estimate_adjusted     = estimate,
        lower_ci_adjusted     = conf_low,
        upper_ci_adjusted     = conf_high,
        p_value_wald_adjusted = p_value
      )

    # Check for potential multicollinearity
    alias_check <- alias(fit)$Complete

    if (!is.null(alias_check) && any(alias_check, na.rm = TRUE)) {
      aliased_terms <- names(alias_check[alias_check == TRUE])
      warning(
        "Model contains aliased terms (perfect collinearity detected): ",
        paste(aliased_terms, collapse = ", ")
      )
    }

    # Type III LRT via drop1() — requires complete cases to avoid row-count
    # mismatch errors when missingness differs across variables. Refit on the
    # same complete-case subset that coxph used internally.
    model_vars    <- all.vars(as.formula(form))
    data_complete <- data[complete.cases(data[, intersect(model_vars, names(data))]), ]

    if (!is.null(id)) {
      fit_cc <- coxph(as.formula(form), data = data_complete,
                      id = get(id, envir = as.environment(data_complete)))
    } else {
      fit_cc <- coxph(as.formula(form), data = data_complete)
    }

    suppressWarnings(
      multi_res_lrt <- drop1(fit_cc, test = "Chisq") %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "covariate") %>%
        janitor::clean_names() %>%
        dplyr::select(covariate, p_value_lrt_adjusted = pr_chi) %>%
        dplyr::filter(covariate != "<none>")
    )

    ## Combine with univariable results
    res <- uni_res %>%
      mutate(
        names_to_match = dplyr::na_if(covariate, ""),
        names_to_match = .repeat_last(names_to_match),
        names_to_match = paste0(names_to_match, term),
        names_to_match = dplyr::if_else(
          is.na(estimate) | estimate == "",
          "",
          names_to_match
        ),
        names_to_match = trimws(names_to_match, which = "left"),
        covariate      = trimws(covariate, which = "left")
      ) %>%
      dplyr::left_join(multi_res,     by = c("names_to_match" = "term")) %>%
      dplyr::left_join(multi_res_lrt, by = "covariate") %>%
      mutate(
        signif_adjusted = purrr::map_chr(
          .x = p_value_wald_adjusted,
          .f = ~ .calc_sig_ind(.x, format)
        )
      ) %>%
      dplyr::rename(
        estimate_unadjusted     = estimate,
        lower_ci_unadjusted     = lower_ci,
        upper_ci_unadjusted     = upper_ci,
        p_value_wald_unadjusted = p_value_wald,
        p_value_lrt_unadjusted  = p_value_lrt
      )

  } else {
    res <- uni_res
  }

  # Inform user if strata terms were present and excluded from univariable table
  if (length(strata_terms) > 0) {
    message(
      "Note: Strata term(s) excluded from univariable models (no HR estimate): ",
      paste(strata_terms, collapse = ", ")
    )
  }

  res
}
