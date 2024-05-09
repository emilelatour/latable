
#' @title Print table one using flextable
#' 
#' @description
#' Helper function for printing output from `latable::create_table_one()` using
#' flextable.
#' 
#'
#' @param x A tibble from `latable::create_table_one()` results
#' @param font_name single character value, the font family name. With Word and
#'   PowerPoint output, the value specifies the font to be used to format
#'   characters in the Unicode range (U+0000-U+007F). For "all" table parts.
#' @param font_size integer value (points). For "all" table parts.
#' @param align Column alignment: a character vector consisting of 'l' (left),
#'   'c' (center), 'r' (right), and/or 'j' (justify). By default or if align =
#'   NULL, numeric columns are right-aligned, and other columns are
#'   left-aligned. If length(align) == 1L, the string will be expanded to a
#'   vector of individual letters, e.g. 'clc' becomes c('c', 'l', 'c').
#' @param align_j Column selection for `align` argument. Default is
#'   `1:length(align)`.
#' @param auto_fit Logical (Default is FALSE). See `flextable::autofit()`
#' @param width width in inches as a character vector.
#' @param width_j columns selection for width.
#' @param width_unit unit for width, one of "in", "cm", "mm".
#' @param col_nms Names of columns to be labelled.
#' @param col_lbls Labels to apply to columns different from column names.
#' @param title The text for the title.
#' @param subtitle The text for the subtitle which will be displayed below the
#'   title.
#' @param footer The text for the footer which will be displayed below the body
#'   of the table.
#' @param title_size Font size for the title. Default is 16.
#' @param subtitle_size  Font size for the subtitle. Default is 11.
#' @param footer_size  Font size for the footer Default is 11.
#' @param strata_lbl The text to use for the strata title displayed above the
#'   groups.
#' @param t1_default_cols Character vector of default column names that come
#'   from `latable::create_table_one()`. Can be changed.
#' @param t1_default_lbls  Character vector of default column labels. Can be
#'   changed.
#'   
#' @importFrom flextable add_footer_lines
#' @importFrom flextable add_header_lines
#' @importFrom flextable align
#' @importFrom flextable align
#' @importFrom flextable autofit
#' @importFrom flextable bold
#' @importFrom flextable flextable
#' @importFrom flextable font
#' @importFrom flextable fontsize
#' @importFrom flextable fp_border_default
#' @importFrom flextable hline
#' @importFrom flextable hline_top
#' @importFrom flextable set_flextable_defaults
#' @importFrom flextable set_header_labels
#' @importFrom flextable width
#' @importFrom officer fp_border
#' @importFrom rlang list2
#' @importFrom stats setNames 
#' @importFrom stringr str_squish
#' @import knitr
#' @import survival
#' 
#' 
#' @return
#' A `flextable`
#' 
#' @export
#'
#' @examples
#' # Load some packages for the example
#' library(survival)
#' library(dplyr)
#' library(tibble)
#' 
#' library(latable)
#' 
#' # Data set comes from the survival package
#' data(pbc)
#' 
#' ## Vector of variables to summarize
#' myVars <- c("time", "status", "trt", "age", "sex", "ascites", "hepato",
#'             "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos",
#'             "ast", "trig", "platelet", "protime", "stage")
#' ## Vector of categorical variables that need transformation
#' catVars <- c("status", "trt", "ascites", "hepato",
#'              "spiders", "edema", "stage")
#' 
#' # With labels
#' library(labelled)
#' 
#' # Tibble with labels
#' var_labels <- tibble::tribble(
#'   ~vars,                                      ~labels,
#'   "id",                                "Case Number",
#'   "time",          "Number of days since registration",
#'   "status",                         "Status at endpoint",
#'   "trt",                            "Treatment group",
#'   "age",                              "Age, in years",
#'   "sex",                                        "Sex",
#'   "ascites",                        "Presence of ascites",
#'   "hepato", "Presence of hepatomegaly or enlarged liver",
#'   "spiders",     "Blood vessel malformations in the skin",
#'   "edema",                          "Presence of edema",
#'   "bili",                   "Serum bilirunbin (mg/dl)",
#'   "chol",                  "Serum cholesterol (mg/dl)",
#'   "albumin",                       "Serum albumin (g/dl)",
#'   "copper",                      "Urine copper (ug/day)",
#'   "alk.phos",             "Alkaline phosphotase (U/liter)",
#'   "ast",          "Aspartate aminotransferase (U/ml)",
#'   "trig",                      "Triglycerides (mg/dl)",
#'   "platelet",                             "Platelet count",
#'   "protime",           "Standardised blood clotting time",
#'   "stage", "Histologic stage of disease (needs biopsy)"
#' )
#' 
#' labels_list <- setNames(as.list(var_labels$labels), var_labels$vars)
#' 
#' # Apply labels
#' labelled::var_label(pbc) <- labels_list
#' 
#' # No Strata
#' foo2 <- create_table_one(data = pbc,
#'                          # strata = "trt",
#'                          vars = myVars,
#'                          fct_vars = catVars,
#'                          keep_test = FALSE,
#'                          show_smd = FALSE,
#'                          var_labels = TRUE)
#' 
#' 
#' # Table one with strata
#' foo <- create_table_one(data = pbc,
#'                         strata = "trt",
#'                         vars = myVars,
#'                         fct_vars = catVars,
#'                         keep_test = FALSE,
#'                         show_smd = FALSE,
#'                         var_labels = TRUE)
#' 
#' 
#' flex_print_table_one(x = foo)
#' 
#' flex_print_table_one(x = foo2)
#' 
#' flex_print_table_one(x = foo, 
#'                      font_name = "Lato", 
#'                      title = "Edgar Anderson's Iris Data", 
#'                      title_size = 16, 
#'                      subtitle = "Just the head")
#' 
#' flex_print_table_one(x = foo2, 
#'                      font_name = "Lato", 
#'                      title = "Edgar Anderson's Iris Data", 
#'                      title_size = 16)
#' 
flex_print_table_one <- function(x,
                                 font_name = "Arial",
                                 font_size = 11,
                                 align = c("llcr"),
                                 align_j = NULL,
                                 auto_fit = FALSE,
                                 width = NULL,
                                 width_j = NULL,
                                 width_unit = "in",
                                 col_nms = NULL,
                                 col_lbls = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 footer = NULL,
                                 title_size = 16,
                                 subtitle_size = 11,
                                 footer_size = 11, 
                                 strata_lbl = "Treatment group", 
                                 t1_default_cols = c("Category",
                                                     "level",
                                                     "num_not_miss",
                                                     "Overall", 
                                                     "p"), 
                                 t1_default_lbls = c("Characteristic",
                                                     "",
                                                     "Number available",
                                                     "All subjects",
                                                     "p-value")) {
  
  # Check for strata
  t1_cols <- names(x)
  t1_strata <- t1_cols[!t1_cols %in% t1_default_cols]
  n_strata <- length(t1_strata)
  
  
  
  # Get the dimensions of x
  x_dim <- dim(x)
  
  # Get column names for later maybe
  if (is.null(col_nms)) {
    col_nms <- names(x)
  }
  
  
  # Make a flextable object
  if (n_strata == 0) {
    
    header_list <- rlang::list2(!!! setNames(t1_default_lbls[c(1:4)], col_nms))
    
    x <- x |>
      flextable::flextable() |> 
      flextable::set_header_labels(values = header_list)
    
  } else {
    
    typology <- data.frame(
      col_keys = c(t1_default_cols[c(1:4)], t1_strata, t1_default_cols[5]), 
      row_2 = c(rep("", 4), rep(strata_lbl, n_strata), ""), 
      row_1 = c(t1_default_lbls[c(1:4)], t1_strata, t1_default_lbls[5]), 
      stringsAsFactors = FALSE)
    
    x <- x |>
      flextable::flextable() |> 
      flextable::set_header_df(mapping = typology, 
                               key = "col_keys") |> 
      flextable::merge_h(i = 1, 
                         part = "header") |> 
      flextable::fix_border_issues() 
    
  }
  
  
  
  # Set columns width
  if (!is.null(width)) {
    x <- x |>
      flextable::width(j = width_j,
                       width = width,
                       unit = width_unit)
  } else {
    
    width_j
    
    width <- c(2, 2, 0.5, 1.5)
    width_j = c(1, 2, 3, 4)
    width_unit <- "in"
    
    if (n_strata != 0) {
      width <- c(width, rep(1.5, n_strata), 1.0)
      width_j <- c(1:length(width))
      width_unit <- "in"
      
    }
    
    x <- x |>
      flextable::width(j = width_j,
                       width = width,
                       unit = width_unit)
    
    
  }
  
  
  # Set text alignment
  if (!is.null(align)) {
    
    align <- unlist(strsplit(align, ""))
    
    n_cols <- x_dim[[2]]
    
    # Pad the end with the last element in align
    align_pad <- n_cols - length(align)
    align_pad <- max(0, align_pad)
    
    align <- c(align,
               rep(align[length(align)], align_pad))
    
    align <- align[1:n_cols]
    
    if (is.null(align_j)) {
      align_j <- c(1:length(align))
    }
    
    # "left", "center", "right", "justify"
    align <- dplyr::case_match(align,
                               "l" ~ "left",
                               "c" ~ "center",
                               "r" ~ "right",
                               "j" ~ "justify") 
    
    if (n_strata != 0) {
      x <- x |>
        flextable::align(j = align_j,
                         align = align,
                         part = "body") |>
        flextable::align(i = 2, 
                         j = align_j,
                         align = align,
                         part = "header") |>
        flextable::align(i = 1, 
                         align = "center",
                         part = "header")
      
    } else {
      x <- x |>
        flextable::align(j = align_j,
                         align = align,
                         part = "body") |>
        flextable::align(i = 1, 
                         j = align_j,
                         align = align,
                         part = "header")
      
    }
    
    
  }
  
  
  # # Column labels
  # if (!is.null(col_lbls)) {
  #   
  #   header_list <- rlang::list2(!!! setNames(col_lbls, col_nms))
  #   
  #   x <- x |>
  #     flextable::set_header_labels(values = header_list)
  #   
  # }
  
  
  # Apply font size
  x <- x |>
    flextable::fontsize(size = font_size,
                        part = "all")
  
  # Apply font name
  x <- x |>
    flextable::font(fontname = font_name,
                    part = "all")
  
  
  # Bold column labels
  if (n_strata != 0) {
    x <- x |>
      flextable::bold(i = c(1, 2), bold = TRUE, part = "header")
    
  } else {
    x <- x |>
      flextable::bold(i = 1, bold = TRUE, part = "header")
    
  }
  
  
  
  # Adjust the header for title and subtitles
  
  if (n_strata == 0) {
    header_adj <- -1
  } else {
    header_adj <- 0
  }
  
  # No title and no subtitle
  if (is.null(subtitle) & is.null(title)) {
    x <- x |> 
      flextable::valign(i = 2 + header_adj, 
                        valign = "bottom", 
                        part = "header") |> 
      flextable::hline_top(part = "header",
                           border = officer::fp_border(color = "#666666",
                                                       width = 1.5,
                                                       style = "solid")) |> 
      flextable::hline_bottom(part = "header", 
                              border = officer::fp_border(color = "#666666", 
                                                          width = 1.5)) 
  }
  
  
  # Subtitle and title
  if (!is.null(subtitle) & !is.null(title)) {
    
    title <- stringr::str_squish(title)
    subtitle <- stringr::str_squish(subtitle)
    
    x <- x |> 
      flextable::valign(i = 2 + header_adj, 
                        valign = "bottom", 
                        part = "header") |> 
      flextable::add_header_lines(values = "") |>  # add Padding
      flextable::add_header_lines(values = subtitle) |>  # add subtitle
      flextable::add_header_lines(values = title) |>  # add title
      flextable::hline_top(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::border_inner_h(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::align(i = c(1, 2), j = 1, align = "left", part = "header") |>
      flextable::fontsize(i = 1, size = title_size, part = "header") |>
      flextable::fontsize(i = 2, size = subtitle_size, part = "header") |>
      flextable::bold(i = 2, bold = FALSE, part = "header") |>
      flextable::hline_top(part = "header",
                           border = officer::fp_border(color = "black",
                                                       width = 2.0,
                                                       style = "solid")) |>
      flextable::hline(i = 3,
                       part = "header",
                       border = officer::fp_border(color = "#666666",
                                                   width = 1.5,
                                                   style = "solid")) |> 
      flextable::hline_bottom(part = "header", 
                              border = officer::fp_border(color = "#666666", 
                                                          width = 1.5)) 
    
    
  }
  
  # title only
  if (is.null(subtitle) & !is.null(title)) {
    
    title <- stringr::str_squish(title)
    
    x <- x |>
      flextable::valign(i = 2 + header_adj, 
                        valign = "bottom", 
                        part = "header") |> 
      flextable::add_header_lines(values = "") |>  # add Padding
      flextable::add_header_lines(values = title) |>  # add title
      flextable::hline_top(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::border_inner_h(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::align(i = 1, j = 1, align = "left", part = "header") |>
      flextable::fontsize(i = 1, size = title_size, part = "header") |>
      flextable::hline_top(part = "header",
                           border = officer::fp_border(color = "black",
                                                       width = 2.0,
                                                       style = "solid")) |>
      flextable::hline(i = 2,
                       part = "header",
                       border = officer::fp_border(color = "#666666",
                                                   width = 1.5,
                                                   style = "solid")) |> 
      flextable::hline_bottom(part = "header", 
                              border = officer::fp_border(color = "#666666", 
                                                          width = 1.5)) 
    
  }
  
  if (!is.null(footer)) {
    
    title <- stringr::str_squish(footer)
    
    x <- x |>
      flextable::add_footer_lines(footer) |>
      # flextable::hline_bottom(part = "footer",
      #                         border = officer::fp_border(color = "#666666",
      #                                                     width = 0.25,
      #                                                     style = "solid")) |>
      flextable::font(fontname = font_name,
                      part = "footer") |>
      flextable::fontsize(i = 1, size = footer_size, part = "footer")
    
    
  }
  
  
  # Autofit if TRUE
  if (auto_fit) {
    x <- x |>
      flextable::autofit()
  }
  
  
  return(x)
  
}


