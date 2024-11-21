
#' @title
#' Add formatting to a table of regression results
#'
#' @description
#' Add formatting to a table of regression results....
#'
#' @param tab A regression table. Data frame or tibble
#' @param digits Integer; number of digits to display (default is 2)
#' @param pval_fmt Which p-value to show. Options are (1) "combo", default,
#'   which shows the LRT and Wald but only if there is more than one level, (2)
#'   "all" shows LRT and Wald no matter what, (3) "lrt" shows just the LRT
#'   p-value, (4) "wald" shows just the Wald p-value.
#' @param format Doesn't do anything right now. Place holder for later. Default
#'   is "html".
#' @param separate_text Text to use in confidence intervals. Default is " to ".
#' @param var_labels Variable labels
#'
#' @importFrom dplyr case_when
#' @importFrom dplyr coalesce
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select
#' @importFrom labelled var_label
#' @importFrom purrr map_chr
#' @importFrom stringr str_detect
#'
#' @return A tibble or data frame
#' @export
#'
#' @examples \dontrun{
#' library(epiDisplay)
#' library(tidyverse)
#'
#' dplyr::glimpse(infert)
#' model0 <- glm(case ~ induced + spontaneous + education,
#'               family = binomial,
#'               data = infert)
#'
#' tab1 <- display_logistic(fit = model0,
#'                  data = infert)
#'
#' adorn_reg_table_format(tab1,
#'                          pval_fmt = "all")
#'
#' adorn_reg_table_format(tab1,
#'                          pval_fmt = "combo")
#'
#'
#' adorn_reg_table_format(tab1,
#'                          pval_fmt = "combo")
#'
#' tab2 <- display_logistic(fit = model0,
#'                          data = infert,
#'                          add_multi = TRUE)
#'
#' adorn_reg_table_format(tab2,
#'                          pval_fmt = "combo")
#'
#'
#'
#' # How does an interaction term work?
#' # This sort of works right now, but still needs work (2019-03-23)
#' # Does't work very well with the multivariable results
#' model0 <- glm(case ~ induced * education,
#'               family = binomial,
#'               data = infert)
#'
#' tab3 <- display_logistic(fit = model0,
#'                  data = infert,
#'                  add_multi = FALSE)
#'
#' adorn_reg_table_format(tab3,
#'                          pval_fmt = "all")
#'
#' display_logistic(fit = model0,
#'                  data = infert,
#'                  add_multi = TRUE) %>%
#'   adorn_reg_table_format(.,
#'                          pval_fmt = "combo") %>%
#'   print(n = Inf)
#'
#'
#'
#' ## With labels ----------------
#'
#' model0 <- glm(case ~ induced + spontaneous + education,
#'               family = binomial,
#'               data = infert)
#'
#' tab4 <- display_logistic(fit = model0,
#'                  data = infert,
#'                  add_multi = TRUE)
#'
#' adorn_reg_table_format(tab4,
#'                          pval_fmt = "combo",
#'                          var_labels = list(induced = "Induced (1/0)",
#'                                            spontaneous = "Spontaneous (1/0)",
#'                                            education = "Education levels"))
#'
#'
#' # Another way if the data set is already labelled
#'
#' labelled::var_label(infert) <- list(induced = "Induced (1/0)",
#'                                     spontaneous = "Spontaneous (1/0)",
#'                                     education = "Education levels")
#'
#' adorn_reg_table_format(tab4,
#'                          pval_fmt = "combo",
#'                          var_labels = labelled::var_label(infert))
#' }

adorn_reg_table_format <- function(tab,
                                   digits = 2,
                                   pval_fmt = "combo",  # "lrt" "wald" "all"
                                   format = "html",
                                   separate_text = " to ",
                                   var_labels = NULL) {
  
  
  #### Check the pval_fmt is acceptable -------------------------------- 
  
  pval_fmt <- tolower(pval_fmt)
  
  if (!pval_fmt %in% c("combo", "lrt", "wald", "all")) {
    
    stop('pval_fmt must be one of: "combo", "lrt", "wald", "all"')
  }
  
  #### Check if multivariable or univariable results -------------------------------- 
  
  # is_multi <- !(length(names(tab)) == 9)
  
  is_multi <- any(stringr::str_detect(names(tab), "adjusted"))
  
  
  #### Start function -------------------------------- 
  
  if (!is_multi) {
    
    
    ## Combine/select p-values ----------------
    
    if (pval_fmt == "combo") {
      
      tab <- tab %>%
        mutate(p_value_wald = dplyr::case_when(
          is.na(ref) ~ p_value_wald,
          ref == "Reference level" ~ p_value_wald,
          ref == "" ~ NA_real_),
          p_value = dplyr::coalesce(p_value_wald,
                                    p_value_lrt))
      
    } else if (pval_fmt == "lrt") {
      
      tab <- tab %>%
        mutate(p_value = p_value_lrt)
      
    } else if (pval_fmt == "wald") {
      
      tab <- tab %>%
        mutate(p_value = p_value_wald)
      
    } else if (pval_fmt == "all") {
      
      tab <- tab %>%
        mutate(p_value = dplyr::coalesce(p_value_wald,
                                         p_value_lrt))
      
    }
    
    ## Recalc signif indicator ----------------
    
    tab <- tab %>%
      mutate(signif = .calc_sig_ind(p_value,
                                    format = format))
    
    ## Format p-value ----------------
    
    tab <- tab %>%
      mutate(p_value = .mypval(p_value))
    
    
    ## Combine estimate and CI ----------------
    
    tab <- tab %>%
      mutate(est_is_na = dplyr::if_else(is.na(estimate),
                                        TRUE,
                                        FALSE)) %>%
      mutate_at(.vars = vars(estimate, lower_ci, upper_ci),
                .funs = list(~ .myround(.,
                                        digits = digits))) %>%
      mutate(est_ci = paste0(estimate, " (",
                             lower_ci, separate_text, upper_ci, ")"),
             est_ci = dplyr::if_else(est_is_na,
                                     NA_character_,
                                     est_ci)) %>%
      mutate(est_ci = dplyr::coalesce(est_ci,
                                      ref))
    
    
    ## Select columns and replace NAs ----------------
    
    tab <- tab %>%
      dplyr::select(covariate,
                    term,
                    est_ci,
                    p_value,
                    signif) %>%
      mutate_all(.,
                 list(~ dplyr::if_else(is.na(.), "", .)))
    
    
  } else if (is_multi) {
    
    
    ## Combine/select p-values ----------------
    
    # Univariable
    
    if (pval_fmt == "combo") {
      
      tab <- tab %>%
        mutate(p_value_wald_unadjusted = dplyr::case_when(
          is.na(ref) ~ p_value_wald_unadjusted,
          ref == "Reference level" ~ p_value_wald_unadjusted,
          ref == "" ~ NA_real_),
          p_value_unadjusted = dplyr::coalesce(p_value_wald_unadjusted,
                                               p_value_lrt_unadjusted))
      
    } else if (pval_fmt == "lrt") {
      
      tab <- tab %>%
        mutate(p_value_unadjusted = p_value_lrt_unadjusted)
      
    } else if (pval_fmt == "wald") {
      
      tab <- tab %>%
        mutate(p_value_unadjusted = p_value_wald_unadjusted)
      
    } else if (pval_fmt == "all") {
      
      tab <- tab %>%
        mutate(p_value_unadjusted = dplyr::coalesce(p_value_wald_unadjusted,
                                                    p_value_lrt_unadjusted))
      
    }
    
    
    # Multivariable
    
    if (pval_fmt == "combo") {
      
      tab <- tab %>%
        mutate(p_value_wald_adjusted = dplyr::case_when(
          is.na(ref) ~ p_value_wald_adjusted,
          ref == "Reference level" ~ p_value_wald_adjusted,
          ref == "" ~ NA_real_),
          p_value_adjusted = dplyr::coalesce(p_value_wald_adjusted,
                                             p_value_lrt_adjusted))
      
    } else if (pval_fmt == "lrt") {
      
      tab <- tab %>%
        mutate(p_value_adjusted = p_value_lrt_adjusted)
      
    } else if (pval_fmt == "wald") {
      
      tab <- tab %>%
        mutate(p_value_adjusted = p_value_wald_adjusted)
      
    } else if (pval_fmt == "all") {
      
      tab <- tab %>%
        mutate(p_value_adjusted = dplyr::coalesce(p_value_wald_adjusted,
                                                  p_value_lrt_adjusted))
      
    }
    
    
    
    
    ## Recalc signif indicator ----------------
    
    tab <- tab %>%
      mutate(signif_unadjusted = .calc_sig_ind(p_value_unadjusted,
                                               format = format),
             signif_adjusted = .calc_sig_ind(p_value_adjusted,
                                             format = format))
    
    ## Format p-value ----------------
    
    tab <- tab %>%
      mutate(p_value_unadjusted = .mypval(p_value_unadjusted),
             p_value_adjusted = .mypval(p_value_adjusted))
    
    
    ## Combine estimate and CI ----------------
    
    # Univariable
    tab <- tab %>%
      mutate(est_is_na_unadjusted = dplyr::if_else(is.na(estimate_unadjusted),
                                                   TRUE,
                                                   FALSE)) %>%
      mutate_at(.vars = vars(estimate_unadjusted,
                             lower_ci_unadjusted,
                             upper_ci_unadjusted),
                .funs = list(~ .myround(.,
                                        digits = digits))) %>%
      mutate(est_ci_unadjusted = paste0(estimate_unadjusted, " (",
                                        lower_ci_unadjusted, separate_text,
                                        upper_ci_unadjusted, ")"),
             est_ci_unadjusted = dplyr::if_else(est_is_na_unadjusted,
                                                NA_character_,
                                                est_ci_unadjusted)) %>%
      mutate(est_ci_unadjusted = dplyr::coalesce(est_ci_unadjusted,
                                                 ref))
    
    # Multivariable
    tab <- tab %>%
      mutate(est_is_na_adjusted = dplyr::if_else(is.na(estimate_adjusted),
                                                 TRUE,
                                                 FALSE)) %>%
      mutate_at(.vars = vars(estimate_adjusted,
                             lower_ci_adjusted,
                             upper_ci_adjusted),
                .funs = list(~ .myround(.,
                                        digits = digits))) %>%
      mutate(est_ci_adjusted = paste0(estimate_adjusted, " (",
                                      lower_ci_adjusted, separate_text,
                                      upper_ci_adjusted, ")"),
             est_ci_adjusted = dplyr::if_else(est_is_na_adjusted,
                                              NA_character_,
                                              est_ci_adjusted)) %>%
      mutate(est_ci_adjusted = dplyr::coalesce(est_ci_adjusted,
                                               ref))
    
    
    ## Select columns and replace NAs ----------------
    
    tab <- tab %>%
      dplyr::select(covariate,
                    term,
                    est_ci_unadjusted,
                    p_value_unadjusted,
                    signif_unadjusted,
                    est_ci_adjusted,
                    p_value_adjusted,
                    signif_adjusted) %>%
      mutate_all(.,
                 list(~ dplyr::if_else(is.na(.), "", .)))
    
    
  }
  
  
  ## Apply labels ----------------
  
  if (!is.null(var_labels)) {
    
    var_labels <- Filter(Negate(is.null), var_labels)
    
    tab$covariate <- purrr::map_chr(.x = tab$covariate, 
                                    .f = ~ var_labels[[.]] %||% .)
    
  }
  
  
  ## Return table ----------------
  
  tab
  
  
}
