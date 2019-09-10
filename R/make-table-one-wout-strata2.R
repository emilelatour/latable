
#' @title 
#' Make a Table 1 without strata.
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
#' @param var_labels Whether to replace variable names with variable labels
#'   obtained from `labelled::var_label()` function.
#' @param nonnormal A character vector to specify the variables for which the
#'   p-values should be those of nonparametric tests. By default all p-values
#'   are from normal assumptionbased tests (oneway.test).
#' @param ... Optional parameters
#' 
#' @import tableone
#' 
#' @importFrom dplyr case_when
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
#' @importFrom tidyr replace_na
#'
#' @return A tbl_df
#'
#' @references 
#' https://cran.r-project.org/web/packages/tableone/tableone.pdf

.create_table_one_no_strata2 <- function(data, 
                                         vars, 
                                         fct_vars, 
                                         catDigits = 1, 
                                         contDigits = 2, 
                                         pDigits = 3, 
                                         var_labels = TRUE, 
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
          catDigits = catDigits, 
          contDigits = contDigits, 
          pDigits = pDigits)
  
  
  ## A few things for later ---------------- 
  
  # num_not_miss <- apply(!is.na(data[, vars]), 2, sum)
  names_for_tab <- row.names(t_0)
  
  
  ## Clean up the tables ---------------- 
  
  t_0 <- t_0 %>% 
    as.data.frame(.) %>% 
    tibble::rownames_to_column(., var = "Category") %>%  
    mutate(Category = names_for_tab)
  
  tab <- t_0
  
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
    
    tab <- df_indexing %>%
      dplyr::left_join(.,
                       tab %>% dplyr::select(index),
                       by= c("index2" = "index")) %>%
      dplyr::left_join(.,
                       tab %>% dplyr::select(-Category),
                       by = "index") %>%
      dplyr::select(cols_to_keep, -index)
    
  }
  
  ## End of function ---------------- 
  
  tab <- tab %>% 
    dplyr::mutate(level = dplyr::case_when(
      is.na(Overall) ~ NA_character_, 
      TRUE ~ tidyr::replace_na(level, "NA"))) %>% 
    dplyr::mutate_all(., 
                      .funs = list(~ replace(., is.na(.), "")))
  
  return(tab)
  
  
}


