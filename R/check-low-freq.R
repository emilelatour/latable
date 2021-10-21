#' @title 
#' Check for low frequencies/counts when doing logistic regression
#' 
#' @description 
#' Low counts in certain categories and levels of a variable can cause issues in
#' logistic regression. This function helps to identify low counts that might be
#' problematic. Rule of thumb might be to collapse categories that contain <=
#' 1.0% of your data.
#'
#' @param fit An object of class `glm` inheriting from "glm" which inherits from
#'   the class "lm.
#' @param data A tibble or data frame with the full data set.
#' @param threshold The threshold to flag categories with frequencies/counts.
#'   Default is 0.01.
#' @param outcome Character string. The dependent variable (outcome) for
#'   logistic regression.
#' @param predictors Character vector. Independent variables
#'   (predictors/covariates) for univariable and/or multivariable modelling.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom purrr map2
#' @importFrom purrr map_lgl
#' @importFrom rlang sym
#' @importFrom tibble as_tibble
#' @importFrom tidyr crossing
#' @importFrom tidyr unnest
#' 
#' @return A tibble
#' @export
#'
#' @examples \dontrun{
#' library(epiDisplay)
#' library(dplyr)
#' 
#' dplyr::glimpse(infert)
#' model0 <- glm(case ~ induced + spontaneous + education,
#'               family = binomial,
#'               data = infert)
#' summary(model0)
#' 
#' 
#' check_low_freq(fit = model0,
#'                data = infert)
#' 
#' check_low_freq(fit = model0,
#'                data = infert, 
#'                threshold = 0.05)
#' 
#' check_low_freq2(data = infert,
#'                 outcome = "case",
#'                 predictors = c("induced", "spontaneous", "education"), 
#'                 threshold = 0.05)
#' 
#' 
#' #### Another data set --------------------------------
#' 
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
#' check_low_freq(fit = fit,
#'                data = predimed)
#' 
#' check_low_freq(fit = model0,
#'                data = infert, 
#'                threshold = 0.05)
#' 
#' check_low_freq2(data = predimed,
#'                 outcome = "htn",
#'                 predictors = c("sex", "bmi", "smoke"), 
#'                 threshold = 0.01)
#' 
#' 
#' check_low_freq2(data = predimed,
#'                 outcome = "htn",
#'                 predictors = c("sex", "bmi", "smoke"), 
#'                 threshold = 0.05)
#' 
#' 
#' 
#' }
check_low_freq <- function(fit,
                           data, 
                           threshold = 0.01) {
  
  freq_tab <- pct <- y <- x <- n <- NULL
  
  if (length(class(fit)) == 1) {
    stop("Model not from logistic regression")
  }
  if (class(fit)[1] != "glm" | class(fit)[2] != "lm" |
      fit$family$family != "binomial") {
    stop("Model not from logistic regression")
  }
  
  predictors <- attr(fit$terms, "term.labels")
  outcome <- names(fit$model)[1]
  
  df <- data
  
  class_check <- purrr::map_lgl(.x = predictors, 
                                .f = ~ class(df[[.]]) %in% c("factor",
                                                             "ordered",
                                                             "logical",
                                                             "character"))
  predictors <- predictors[class_check]
  
  tidyr::crossing(outcome,
                  predictors) %>%
    dplyr::mutate(predictors = factor(predictors,
                                      levels = attr(fit$terms, "term.labels"))) %>%
    dplyr::arrange(predictors) %>% 
    dplyr::mutate(predictors = as.character(predictors), 
                  freq_tab = purrr::map2(.x = predictors, 
                                         .y = outcome, 
                                         .f = ~ get_freq(data = df, 
                                                         y = .y, 
                                                         x = .x))) %>% 
    tidyr::unnest(freq_tab)  %>% 
    dplyr::mutate(y = outcome, 
                  x = predictors, 
                  y = glue::glue("{y} == {y_level}"), 
                  x = glue::glue("{x} == {x_level}")) %>% 
    dplyr::filter(pct <= threshold) %>% 
    dplyr::select(y, x, n, pct)
  
}


#' @rdname check_low_freq
#' @export
check_low_freq2 <- function(data,
                            outcome,
                            predictors, 
                            threshold = 0.01) {
  
  freq_tab <- pct <- y <- x <- n <- NULL
  
  df <- data
  
  class_check <- purrr::map_lgl(.x = predictors, 
                                .f = ~ class(df[[.]]) %in% c("factor",
                                                             "ordered",
                                                             "logical",
                                                             "character"))
  predictors <- predictors[class_check]
  
  tidyr::crossing(outcome,
                  predictors) %>%
    mutate(predictors = factor(predictors,
                               levels = predictors)) %>%
    dplyr::arrange(predictors) %>% 
    mutate(predictors = as.character(predictors), 
           freq_tab = purrr::map2(.x = predictors, 
                                  .y = outcome, 
                                  .f = ~ get_freq(data = df, 
                                                  y = .y, 
                                                  x = .x))) %>% 
    tidyr::unnest(freq_tab)  %>% 
    mutate(y = outcome, 
           x = predictors, 
           y = glue::glue("{y} == {y_level}"), 
           x = glue::glue("{x} == {x_level}")) %>% 
    dplyr::filter(pct <= threshold) %>% 
    dplyr::select(y, x, n, pct)
  
}


#### Helper function -------------------------------- 

get_freq <- function(data, 
                     y, 
                     x) { 
  
  n <- x_level <- y_level <- pct <- NULL
  
  x <- rlang::sym(x)
  y <- rlang::sym(y)
  
  
  data %>% 
    tibble::as_tibble() %>% 
    dplyr::count(!! y, 
                 !! x, 
                 .drop = FALSE) %>% 
    dplyr::mutate(pct = n / sum(n, na.rm = TRUE)) %>% 
    dplyr::rename("x_level" = !! x, 
                  "y_level" = !! y) %>% 
    dplyr::mutate(x_level = as.character(x_level), 
                  y_level = as.character(y_level)) %>% 
    dplyr::select(y_level, x_level, n, pct)
  
  
}


