#' @title Display Linear Regression Results
#'
#' @description
#' Given an `lm` or Gaussian `glm` model fit object, displays univariable and
#' (optionally) multivariable results from linear regression in a format
#' suitable for presentation.
#'
#' Overall per-predictor p-values are likelihood ratio tests, consistent with
#' the other `display_*` functions in this package. For moderate to large
#' samples LRT and F-test p-values are numerically nearly identical.
#'
#' @param fit An object of class `lm`, or `glm` with `family = gaussian`.
#' @param data A tibble or data frame with the full dataset used for modeling.
#' @param add_multi Logical; whether to include multivariable results. Default
#'   is `FALSE`.
#' @param format Display format. Default is `"html"`.
#' @param conf_level Confidence level for intervals. Default is `0.95`.
#' @param include_last_row Logical; add blank spacing row. Default is `TRUE`.
#'
#' @importFrom broom tidy
#' @importFrom dplyr arrange coalesce filter if_else left_join mutate na_if rename select
#' @importFrom janitor clean_names
#' @importFrom purrr map
#' @importFrom stats alias drop1
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr crossing unnest
#'
#' @return A tibble containing the linear regression results.
#'
#' @examples
#' \dontrun{
#' fit <- lm(mpg ~ cyl + disp + hp, data = mtcars)
#' display_linear(fit, data = mtcars, add_multi = TRUE)
#' }
#'
#' @export
display_linear <- function(fit,
                           data,
                           add_multi        = FALSE,
                           format           = "html",
                           conf_level       = 0.95,
                           include_last_row = TRUE) {

  if (!(inherits(fit, "lm") ||
        (inherits(fit, "glm") && fit$family$family == "gaussian"))) {
    stop("Model not from linear regression")
  }

  # Silence no visible binding for global variable
  pr_chi  <- NULL
  formula <- NULL

  # Extract term labels
  predictors <- attr(fit$terms, "term.labels")
  outcome    <- names(fit$model)[1]

  ## Use the full dataset
  df <- data

  ## Univariable results
  # Capture model order before crossing() alphabetizes
  pred_order <- predictors

  uni_res <- tidyr::crossing(outcome, predictors) %>%
    mutate(
      predictors = factor(predictors, levels = pred_order)
    ) %>%
    dplyr::arrange(predictors) %>%          # sort by factor = model order
    mutate(
      predictors = as.character(predictors),
      formula    = paste(outcome, "~", predictors),
      res_univ   = purrr::map(
        .x = formula,
        .f = ~ .do_linear_univ(
          data             = df,
          formula          = .x,
          format           = format,
          conf_level       = conf_level,
          include_last_row = include_last_row
        )
      )
    ) %>%
    dplyr::select(res_univ) %>%
    tidyr::unnest(col = res_univ)

  ## Multivariable results
  if (add_multi == TRUE) {

    multi_res <- fit %>%
      broom::tidy(
        conf.int   = TRUE,
        conf.level = conf_level
      ) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
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

    # Type III LRT via drop1(). lm()/glm() use complete cases internally
    # (na.action = na.omit by default), so no complete-case refit needed.
    suppressWarnings(
      multi_res_lrt <- drop1(fit, test = "Chisq") %>%
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
        signif_wald_adjusted = .calc_sig_ind(p_value_wald_adjusted, format),
        signif_lrt_adjusted  = .calc_sig_ind(p_value_lrt_adjusted,  format),
        signif_adjusted      = dplyr::coalesce(signif_wald_adjusted,
                                               signif_lrt_adjusted)
      ) %>%
      dplyr::select(-names_to_match,
                    -signif_wald_adjusted,
                    -signif_lrt_adjusted) %>%
      dplyr::rename(
        estimate_unadjusted     = estimate,
        lower_ci_unadjusted     = lower_ci,
        upper_ci_unadjusted     = upper_ci,
        p_value_wald_unadjusted = p_value_wald,
        p_value_lrt_unadjusted  = p_value_lrt,
        signif_unadjusted       = signif
      )

  } else {
    res <- uni_res
  }

  res
}


#' @title Univariable Linear Regression Helper
#'
#' @description
#' Internal helper. Fits a univariable linear regression and returns results
#' in a presentation-ready tibble. Handles continuous, categorical, and
#' interaction terms. Overall per-predictor p-values are likelihood ratio
#' tests.
#'
#' @param data A tibble or data frame.
#' @param formula Character string model formula.
#' @param format Display format. Default is `"html"`.
#' @param conf_level Confidence level. Default is `0.95`.
#' @param include_last_row Logical. Default is `TRUE`.
#'
#' @importFrom broom tidy
#' @importFrom dplyr bind_rows filter mutate select
#' @importFrom janitor clean_names
#' @importFrom purrr pluck
#' @importFrom stats as.formula drop1 gaussian glm
#' @importFrom stringr str_detect str_remove_all str_split
#' @importFrom tibble tibble
#'
#' @return A tibble with univariable linear results for the specified predictor.
.do_linear_univ <- function(data,
                            formula,
                            format           = "html",
                            conf_level       = 0.95,
                            include_last_row = TRUE) {

  #### Fit the model --------------------------------
  fit <- glm(formula = as.formula(formula),
             family  = gaussian(link = "identity"),
             data    = data)

  independent <- attr(fit$terms, "term.labels")
  outcome     <- names(fit$model)[1]

  indep_split <- unlist(stringr::str_split(independent, ":"))
  indep_split <- paste0(indep_split, collapse = "|")

  #### Overall p-value --------------------------------

  lrt_pval <- drop1(fit, test = "Chisq") %>%
    purrr::pluck("Pr(>Chi)", 2)

  lrt_pval <- ifelse(length(lrt_pval) == 0, NA, lrt_pval)

  res_overall <- tibble::tibble(
    covariate    = independent,
    term         = NA_character_,
    ref          = NA_character_,
    estimate     = NA_real_,
    lower_ci     = NA_real_,
    upper_ci     = NA_real_,
    p_value_wald = NA_real_,
    p_value_lrt  = lrt_pval,
    signif       = .calc_sig_ind(p_value_lrt, format)
  )

  #### Results by level --------------------------------

  if (any(class(data[[independent]]) %in% c("factor", "ordered",
                                             "logical", "character"))) {

    x_levels <- fit %>%
      purrr::pluck("xlevels", 1) %>%
      .[-c(1)]

    res_by_level <- fit %>%
      broom::tidy(
        conf.int   = TRUE,
        conf.level = conf_level
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
        signif      = .calc_sig_ind(p_value_wald, format)
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
        conf.int   = TRUE,
        conf.level = conf_level
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
        signif      = .calc_sig_ind(p_value_wald, format)
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
        conf.int   = TRUE,
        conf.level = conf_level
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
        signif      = .calc_sig_ind(p_value_wald, format)
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


#' @title Display Linear Regression Results (given variable names)
#'
#' @description
#' Given a string `outcome` and a character vector `predictors`, displays
#' univariable and (optionally) multivariable linear regression results in a
#' presentation-ready format.
#'
#' This version lets you specify outcome and predictors explicitly, without
#' needing a pre-built fit object.
#'
#' @param data A tibble or data frame.
#' @param outcome Character string. Outcome variable name.
#' @param predictors Character vector of predictor variable names.
#' @param add_multi Logical; include multivariable results. Default is `FALSE`.
#' @param format Display format. Default is `"html"`.
#' @param conf_level Confidence level. Default is `0.95`.
#' @param include_last_row Logical; add blank spacing row. Default is `TRUE`.
#'
#' @importFrom broom tidy
#' @importFrom dplyr arrange coalesce filter if_else left_join mutate na_if rename select
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom purrr map
#' @importFrom stats alias as.formula drop1 gaussian glm
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr crossing unnest
#'
#' @return A tibble containing linear regression results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' display_linear2(data = mtcars,
#'                 outcome = "mpg",
#'                 predictors = c("cyl", "disp", "hp"),
#'                 add_multi = TRUE)
#' }
display_linear2 <- function(data,
                            outcome,
                            predictors,
                            add_multi        = FALSE,
                            format           = "html",
                            conf_level       = 0.95,
                            include_last_row = TRUE) {

  # Silence no visible binding for global variable
  pr_chi  <- NULL
  formula <- NULL

  ## Build model formula --------------------------------
  rhs  <- paste(predictors, collapse = " + ")
  form <- glue::glue("{outcome} ~ {rhs}")

  ## Univariable results --------------------------------
  pred_order <- predictors

  uni_res <- tidyr::crossing(outcome, predictors) %>%
    mutate(
      predictors = factor(predictors, levels = pred_order)
    ) %>%
    dplyr::arrange(predictors) %>%          # sort by factor = input order
    mutate(
      predictors = as.character(predictors),
      formula    = paste(outcome, "~", predictors),
      res_univ   = purrr::map(
        .x = formula,
        .f = ~ .do_linear_univ(
          data             = data,
          formula          = .x,
          format           = format,
          conf_level       = conf_level,
          include_last_row = include_last_row
        )
      )
    ) %>%
    dplyr::select(res_univ) %>%
    tidyr::unnest(cols = res_univ)

  ## Multivariable results --------------------------------
  if (add_multi == TRUE) {

    fit <- glm(formula = as.formula(form),
               family  = gaussian(link = "identity"),
               data    = data)

    multi_res <- fit %>%
      broom::tidy(
        conf.int   = TRUE,
        conf.level = conf_level
      ) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
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

    # Type III LRT via drop1(). lm()/glm() use complete cases internally
    # (na.action = na.omit by default), so no complete-case refit needed.
    suppressWarnings(
      multi_res_lrt <- drop1(fit, test = "Chisq") %>%
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
        signif_wald_adjusted = .calc_sig_ind(p_value_wald_adjusted, format),
        signif_lrt_adjusted  = .calc_sig_ind(p_value_lrt_adjusted,  format),
        signif_adjusted      = dplyr::coalesce(signif_wald_adjusted,
                                               signif_lrt_adjusted)
      ) %>%
      dplyr::select(-names_to_match,
                    -signif_wald_adjusted,
                    -signif_lrt_adjusted) %>%
      dplyr::rename(
        estimate_unadjusted     = estimate,
        lower_ci_unadjusted     = lower_ci,
        upper_ci_unadjusted     = upper_ci,
        p_value_wald_unadjusted = p_value_wald,
        p_value_lrt_unadjusted  = p_value_lrt,
        signif_unadjusted       = signif
      )

  } else {
    res <- uni_res
  }

  res
}
