#' @title 
#' Make a Table 1.
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
#'   are from normal assumption based tests (oneway.test).
#' @param includeNA	If TRUE, NA is handled as a regular factor level rather than
#'   missing. NA is shown as the last factor level in the table. Only effective
#'   for categorical variables.
#' @param testApprox A function used to perform the large sample approximation
#'   based tests. The default is chisq.test. This is not recommended when some
#'   of the cell have small counts like fewer than 5.
#' @param argsApprox A named list of arguments passed to the function specified
#'   in testApprox. The default is list(correct = TRUE), which turns on the
#'   continuity correction for chisq.test.
#' @param testExact A function used to perform the exact tests. The default is
#'   fisher.test. If the cells have large numbers, it will fail because of
#'   memory limitation. In this situation, the large sample approximation based
#'   should suffice.
#' @param argsExact	A named list of arguments passed to the function specified
#'   in testExact. The default is list(workspace = 2*10^5), which specifies the
#'   memory space allocated for fisher.test.
#' @param testNormal A function used to perform the normal assumption based
#'   tests. The default is oneway.test. This is equivalent of the t-test when
#'   there are only two groups.
#' @param argsNormal A named list of arguments passed to the function specified
#'   in testNormal. The default is list(var.equal = TRUE), which makes it the
#'   ordinary ANOVA that assumes equal variance across groups.
#' @param testNonNormal A function used to perform the nonparametric tests. The
#'   default is kruskal.test (Kruskal-Wallis Rank Sum Test). This is equivalent
#'   of the wilcox.test (Man-Whitney U test) when there are only two groups.
#' @param argsNonNormal	A named list of arguments passed to the function
#'   specified in testNonNormal. The default is list(NULL), which is just a
#'   placeholder.
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
#' @importFrom stats chisq.test
#' @importFrom stats fisher.test
#' @importFrom stats kruskal.test
#' @importFrom stats oneway.test
#' @importFrom tibble as_tibble
#' @importFrom tibble rowid_to_column
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#'
#' @return A tbl_df
#' @export
#'
#' @references 
#' https://cran.r-project.org/web/packages/tableone/tableone.pdf
#'
#' @examples
#' # Load some packages for the example
#' library(survival)
#' library(dplyr)
#' library(tibble)
#' 
#' # Data set comes from the survival package
#' data(pbc)
#' 
#' ## Vector of variables to summarize
#' myVars <- c("time", "status", "trt", "age", "sex", "ascites", "hepato",
#'           "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos",
#'           "ast", "trig", "platelet", "protime", "stage")
#' ## Vector of categorical variables that need transformation
#' catVars <- c("status", "trt", "ascites", "hepato",
#'              "spiders", "edema", "stage")
#' 
#' #### Example 1 --------------------------------
#' 
#' # With strata
#' create_table_one(data = pbc,
#'                         strata = "trt",
#'                         vars = myVars,
#'                         fct_vars = catVars,
#'                         keep_test = FALSE,
#'                         show_smd = FALSE,
#'                         var_labels = FALSE)
#' 
#' # Without strata
#' create_table_one(data = pbc,
#'                         vars = myVars,
#'                         fct_vars = catVars,
#'                         keep_test = FALSE,
#'                         show_smd = FALSE,
#'                         var_labels = FALSE)
#' 
#' 
#' 
#' #### Example 2 --------------------------------
#' 
#' # With labels
#' library(labelled)
#' 
#' # Tibble with labels
#' var_labels <- tibble::tribble(
#'                      ~vars,                                      ~labels,
#'                       "id",                                "Case Number",
#'                     "time",          "Number of days since registration",
#'                   "status",                         "Status at endpoint",
#'                      "trt",                            "Treatment group",
#'                      "age",                              "Age, in years",
#'                      "sex",                                        "Sex",
#'                  "ascites",                        "Presence of ascites",
#'                   "hepato", "Presence of hepatomegaly or enlarged liver",
#'                  "spiders",     "Blood vessel malformations in the skin",
#'                    "edema",                          "Presence of edema",
#'                     "bili",                   "Serum bilirunbin (mg/dl)",
#'                     "chol",                  "Serum cholesterol (mg/dl)",
#'                  "albumin",                       "Serum albumin (g/dl)",
#'                   "copper",                      "Urine copper (ug/day)",
#'                 "alk.phos",             "Alkaline phosphotase (U/liter)",
#'                      "ast",          "Aspartate aminotransferase (U/ml)",
#'                     "trig",                      "Triglycerides (mg/dl)",
#'                 "platelet",                             "Platelet count",
#'                  "protime",           "Standardised blood clotting time",
#'                    "stage", "Histologic stage of disease (needs biopsy)"
#'                 )
#' 
#' labels_list <- setNames(as.list(var_labels$labels), var_labels$vars)
#' 
#' # Apply labels
#' labelled::var_label(pbc) <- labels_list
#' 
#' # Table one with strata
#' create_table_one(data = pbc,
#'                  strata = "trt",
#'                  vars = myVars,
#'                  fct_vars = catVars,
#'                  keep_test = FALSE,
#'                  show_smd = FALSE,
#'                  var_labels = TRUE)
#' 
#' # Table one without strata
#' create_table_one(data = pbc,
#'                  # strata = "trt",
#'                  vars = myVars,
#'                  fct_vars = catVars,
#'                  keep_test = FALSE,
#'                  show_smd = FALSE, 
#'                  var_labels = TRUE)
#' 

create_table_one <- function(data, 
                             strata = NULL, 
                             vars, 
                             fct_vars, 
                             catDigits = 1, 
                             contDigits = 2, 
                             pDigits = 3, 
                             show_smd = FALSE, 
                             keep_test = FALSE, 
                             var_labels = TRUE, 
                             exact = NULL, 
                             nonnormal = NULL, 
                             includeNA = FALSE,
                             testApprox = chisq.test,
                             argsApprox = list(correct = TRUE),
                             testExact = fisher.test,
                             argsExact = list(workspace = 2 * 10^5),
                             testNormal = oneway.test,
                             argsNormal = list(var.equal = TRUE),
                             testNonNormal = kruskal.test,
                             argsNonNormal = list(NULL), ...) { 
  
  
  if (is.null(strata)) { 
    
    .create_table_one_no_strata(data = data, 
                                vars = vars, 
                                fct_vars = fct_vars, 
                                catDigits = catDigits, 
                                contDigits = contDigits, 
                                pDigits = pDigits, 
                                var_labels = var_labels, 
                                nonnormal = nonnormal, 
                                includeNA = includeNA, ...)
    
  } else {
    
    .create_table_one_strata(data = data, 
                             strata = strata, 
                             vars = vars, 
                             fct_vars = fct_vars, 
                             catDigits = catDigits, 
                             contDigits = contDigits, 
                             pDigits = pDigits, 
                             show_smd = show_smd, 
                             keep_test = keep_test, 
                             var_labels = var_labels, 
                             exact = exact, 
                             nonnormal = nonnormal, 
                             includeNA = includeNA, 
                             testApprox = testApprox,
                             argsApprox = argsApprox,
                             testExact = testExact,
                             argsExact = argsExact,
                             testNormal = testNormal,
                             argsNormal = argsNormal, 
                             testNonNormal = testNonNormal,
                             argsNonNormal = argsNonNormal, ...)
  }
}


