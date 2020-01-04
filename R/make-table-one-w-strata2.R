
#' @title 
#' Make a Table 1 with strata.
#' 
#' @description 
#' Wrapper function for `tableone::CreateTableOne` that adds in formatting and
#' other preferences of mine. Per their documentation: The tableone package is
#' an R package that eases the construction of "Table 1", i.e., patient baseline
#' characteristics table commonly found in biomedical research papers. The
#' packages can summarize both continuous and categorical variables mixed within
#' one table. Categorical variables can be summarized as counts and/or
#' percentages. Continuous variables can be summarized in the “normal” way
#' (means and standard deviations) or "nonnormal" way (medians and interquartile
#' ranges).
#'
#' @param data A data frame in which these variables exist. All variables (both
#'   vars and strata) must be in this data frame.
#' @param strata Stratifying (grouping) variable name(s) given as a character
#'   vector. If omitted, the overall results are returned.
#' @param vars Variables to be summarized given as a character vector. Factors
#'   are handled as categorical variables, whereas numeric variables are handled
#'   as continuous variables. If empty, all variables in the data frame
#'   specified in the data argument are used.
#' @param fct_vars Numerically coded variables that should be handled as
#'   categorical variables given as a character vector. Do not include factors,
#'   unless you need to relevel them by removing empty levels. If omitted, only
#'   factors are considered categorical variables. The variables specified here
#'   must also be specified in the `vars` argument.
#' @param catDigits Number of digits to print for proportions. Default 1.
#' @param contDigits Number of digits to print for continuous variables. Default
#'   2.
#' @param pDigits Number of digits to print for p-values (also used for
#'   standardized mean differences). Default 3.
#' @param show_smd Whether to show standardized mean differences. FALSE by
#'   default. If there are more than one contrasts, the average of all possible
#'   standardized mean differences is shown.
#' @param keep_test Logical; Whether to keep the column named "test" FALSE by
#'   default.
#' @param var_labels Whether to replace variable names with variable labels
#'   obtained from `labelled::var_label()` function.
#' @param exact A character vector to specify the variables for which the
#'   p-values should be those of exact tests. By default all p-values are from
#'   large sample approximation tests (chisq.test).
#' @param nonnormal A character vector to specify the variables for which the
#'   p-values should be those of nonparametric tests. By default all p-values
#'   are from normal assumptionbased tests (oneway.test).
#' @param ... Optional parameters
#' 
#' @import tableone
#' 
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr pull
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom labelled labelled
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble
#' @importFrom tibble rowid_to_column
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#'
#' @return A tbl_df
#' 
#' @references 
#' https://cran.r-project.org/web/packages/tableone/tableone.pdf
#'
.create_table_one_strata2 <- function(data, 
                                      strata, 
                                      vars, 
                                      fct_vars, 
                                      catDigits = 1, 
                                      contDigits = 2, 
                                      pDigits = 3, 
                                      show_smd = FALSE, 
                                      keep_test = FALSE, 
                                      var_labels = TRUE, 
                                      exact = NULL, 
                                      nonnormal = NULL, ...) { 
  
  
  ## Make the overall table without strata ---------------- 
  
  
  t_0 <- tableone::CreateTableOne(vars = vars, 
                                  data = data, 
                                  factorVars = fct_vars, 
                                  includeNA = TRUE) %>% 
    print(., 
          showAllLevels = TRUE, 
          printToggle = FALSE, 
          noSpaces = TRUE, 
          missing = FALSE, 
          varLabels = var_labels, 
          nonnormal = nonnormal, 
          exact = exact, 
          smd = show_smd, 
          catDigits = catDigits, 
          contDigits = contDigits, 
          pDigits = pDigits)
  
  
  ## Make the table with strata ---------------- 
  
  
  t_1 <- tableone::CreateTableOne(vars = vars, 
                                  strata = strata, 
                                  data = data, 
                                  factorVars = fct_vars, 
                                  includeNA = TRUE) %>% 
    print(., 
          showAllLevels = TRUE, 
          printToggle = FALSE, 
          noSpaces = TRUE, 
          missing = FALSE, 
          varLabels = var_labels, 
          nonnormal = nonnormal, 
          exact = exact, 
          smd = show_smd, 
          catDigits = catDigits, 
          contDigits = contDigits, 
          pDigits = pDigits)
  
  
  ## Make table with p-values ---------------- 
  
  tab_w_pvals <- tableone::CreateTableOne(vars = vars, 
                                          strata = strata, 
                                          data = data, 
                                          factorVars = fct_vars, 
                                          includeNA = FALSE) %>% 
    print(., 
          showAllLevels = TRUE, 
          printToggle = FALSE, 
          noSpaces = TRUE, 
          missing = FALSE) 
  
  ## A few things for later ---------------- 
  
  names_for_tab <- row.names(t_0)
  names_for_tab_w_pvals <- row.names(tab_w_pvals)
  
  
  ## Clean up the tables ---------------- 
  
  t_0 <- t_0 %>% 
    as.data.frame(.) %>% 
    tibble::rownames_to_column(., var = "Category") %>%  
    mutate(Category = names_for_tab, 
           row_id = dplyr::row_number())
  
  
  t_1 <- t_1 %>% 
    as.data.frame(.) %>% 
    tibble::rownames_to_column(., var = "Category") %>%  
    mutate(Category = names_for_tab, 
           row_id = dplyr::row_number())
  
  
  tab_w_pvals <- tab_w_pvals %>% 
    as.data.frame(.) %>% 
    tibble::rownames_to_column(., var = "Category") %>%  
    mutate(Category = names_for_tab_w_pvals, 
           row_id = dplyr::row_number()) %>% 
    dplyr::select(Category, p) %>% 
    dplyr::filter(p != "")
  
  ## Join overall and stratified table and p-values ---------------- 
  
  tab <- t_0 %>% 
    dplyr::left_join(., 
                     t_1, 
                     by = c("row_id", "Category", "level")) %>% 
    dplyr::select(-row_id, -p) %>% 
    dplyr::left_join(., 
                     tab_w_pvals, 
                     by = "Category") %>% 
    mutate(p = tidyr::replace_na(p, "")) %>% 
    dplyr::select(dplyr::everything(), -test, test)
  
  
  ## Keep test or not ---------------- 
  
  if (!keep_test) { 
    
    tab <- tab %>% 
      dplyr::select(-test)
    
  }
  
  
  ## Convert all columns to character ---------------- 
  
  tab <- tab %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate_all(., 
                      as.character) %>% 
    tibble::rowid_to_column("index")
  
  
  ## Add space and adjust alignment of numbers ---------------- 
  
  na_index <- which(stringr::str_detect(tab$Category, "(%)"))
  
  cols_to_keep <- names(tab)
  
  
  if (length(na_index) > 0) {
    
    ds_blank <- tibble::tibble(
      index = na_index + .5,
      Category = rep("", length(na_index)))
    
    df_indexing <- tab %>%
      dplyr::select(index, Category) %>%
      dplyr::union(ds_blank) %>%
      dplyr::arrange(index) %>%
      mutate(index = trunc(index),
             index2 = dplyr::if_else(Category == "",
                                     NA_real_,
                                     index),
             index = dplyr::if_else(stringr::str_detect(Category, "(%)"),
                                    NA_real_,
                                    index))
    
    
    if (keep_test == FALSE & show_smd == FALSE) { 
      
      tab <- df_indexing %>%
        dplyr::left_join(.,
                         tab %>% dplyr::select(index,
                                               p),
                         by= c("index2" = "index")) %>%
        dplyr::left_join(.,
                         tab %>% dplyr::select(-Category,
                                               -p),
                         by = "index") %>%
        dplyr::select(cols_to_keep, -index)
      
    } else if (keep_test == TRUE & show_smd == FALSE) { 
      
      tab <- df_indexing %>%
        dplyr::left_join(.,
                         tab %>% dplyr::select(index,
                                               p, 
                                               test),
                         by= c("index2" = "index")) %>%
        dplyr::left_join(.,
                         tab %>% dplyr::select(-Category,
                                               -p, 
                                               -test),
                         by = "index") %>%
        dplyr::select(cols_to_keep, -index)
      
    } else if (keep_test == FALSE & show_smd == TRUE) { 
      
      tab <- df_indexing %>%
        dplyr::left_join(.,
                         tab %>% dplyr::select(index,
                                               p, 
                                               SMD),
                         by= c("index2" = "index")) %>%
        dplyr::left_join(.,
                         tab %>% dplyr::select(-Category,
                                               -p, 
                                               -SMD),
                         by = "index") %>%
        dplyr::select(cols_to_keep, -index)
      
    } else if (keep_test == TRUE & show_smd == TRUE) { 
      
      tab <- df_indexing %>%
        dplyr::left_join(.,
                         tab %>% dplyr::select(index,
                                               p, 
                                               test, 
                                               SMD),
                         by= c("index2" = "index")) %>%
        dplyr::left_join(.,
                         tab %>% dplyr::select(-Category,
                                               -p, 
                                               -test, 
                                               -SMD),
                         by = "index") %>%
        dplyr::select(cols_to_keep, -index)
      
    }
  }
  
  
  ## Padding for p-values ---------------- 
  
  tab <- tab %>% 
    mutate(p = stringr::str_replace(string = p, 
                                    pattern = "<", 
                                    replacement = "< ")) %>% 
    mutate(p = dplyr::if_else(nchar(p) > 0 & stringr::str_detect(string = p, 
                                                                 pattern = "<", 
                                                                 negate = TRUE), 
                              paste0("  ", p), 
                              p))
  
  
  ## End of function ---------------- 
  
  tab <- tab %>% 
    dplyr::mutate(level = dplyr::case_when(
      is.na(Overall) ~ NA_character_, 
      TRUE ~ tidyr::replace_na(level, "NA"))) %>% 
    dplyr::mutate_all(., 
                      .funs = list(~ replace(., is.na(.), "")))
  
  return(tab)
  
  
}



