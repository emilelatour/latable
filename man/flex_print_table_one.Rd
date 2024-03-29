% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flex_print_table_one.R
\name{flex_print_table_one}
\alias{flex_print_table_one}
\title{Print table one using flextable}
\usage{
flex_print_table_one(
  x,
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
  t1_default_cols = c("Category", "level", "num_not_miss", "Overall", "p"),
  t1_default_lbls = c("Characteristic", "", "Number available", "All subjects",
    "p-value")
)
}
\arguments{
\item{x}{A tibble from \code{latable::create_table_one()} results}

\item{font_name}{single character value, the font family name. With Word and
PowerPoint output, the value specifies the font to be used to format
characters in the Unicode range (U+0000-U+007F). For "all" table parts.}

\item{font_size}{integer value (points). For "all" table parts.}

\item{align}{Column alignment: a character vector consisting of 'l' (left),
'c' (center), 'r' (right), and/or 'j' (justify). By default or if align =
NULL, numeric columns are right-aligned, and other columns are
left-aligned. If length(align) == 1L, the string will be expanded to a
vector of individual letters, e.g. 'clc' becomes c('c', 'l', 'c').}

\item{align_j}{Column selection for \code{align} argument. Default is
\code{1:length(align)}.}

\item{auto_fit}{Logical (Default is FALSE). See \code{flextable::autofit()}}

\item{width}{width in inches as a character vector.}

\item{width_j}{columns selection for width.}

\item{width_unit}{unit for width, one of "in", "cm", "mm".}

\item{col_nms}{Names of columns to be labelled.}

\item{col_lbls}{Labels to apply to columns different from column names.}

\item{title}{The text for the title.}

\item{subtitle}{The text for the subtitle which will be displayed below the
title.}

\item{footer}{The text for the footer which will be displayed below the body
of the table.}

\item{title_size}{Font size for the title. Default is 16.}

\item{subtitle_size}{Font size for the subtitle. Default is 11.}

\item{footer_size}{Font size for the footer Default is 11.}

\item{strata_lbl}{The text to use for the strata title displayed above the
groups.}

\item{t1_default_cols}{Character vector of default column names that come
from \code{latable::create_table_one()}. Can be changed.}

\item{t1_default_lbls}{Character vector of default column labels. Can be
changed.}
}
\value{
A \code{flextable}
}
\description{
Helper function for printing output from \code{latable::create_table_one()} using
flextable.
}
\examples{
# Load some packages for the example
library(survival)
library(dplyr)
library(tibble)

library(latable)

# Data set comes from the survival package
data(pbc)

## Vector of variables to summarize
myVars <- c("time", "status", "trt", "age", "sex", "ascites", "hepato",
            "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos",
            "ast", "trig", "platelet", "protime", "stage")
## Vector of categorical variables that need transformation
catVars <- c("status", "trt", "ascites", "hepato",
             "spiders", "edema", "stage")

# With labels
library(labelled)

# Tibble with labels
var_labels <- tibble::tribble(
  ~vars,                                      ~labels,
  "id",                                "Case Number",
  "time",          "Number of days since registration",
  "status",                         "Status at endpoint",
  "trt",                            "Treatment group",
  "age",                              "Age, in years",
  "sex",                                        "Sex",
  "ascites",                        "Presence of ascites",
  "hepato", "Presence of hepatomegaly or enlarged liver",
  "spiders",     "Blood vessel malformations in the skin",
  "edema",                          "Presence of edema",
  "bili",                   "Serum bilirunbin (mg/dl)",
  "chol",                  "Serum cholesterol (mg/dl)",
  "albumin",                       "Serum albumin (g/dl)",
  "copper",                      "Urine copper (ug/day)",
  "alk.phos",             "Alkaline phosphotase (U/liter)",
  "ast",          "Aspartate aminotransferase (U/ml)",
  "trig",                      "Triglycerides (mg/dl)",
  "platelet",                             "Platelet count",
  "protime",           "Standardised blood clotting time",
  "stage", "Histologic stage of disease (needs biopsy)"
)

labels_list <- setNames(as.list(var_labels$labels), var_labels$vars)

# Apply labels
labelled::var_label(pbc) <- labels_list

# No Strata
foo2 <- create_table_one(data = pbc,
                         # strata = "trt",
                         vars = myVars,
                         fct_vars = catVars,
                         keep_test = FALSE,
                         show_smd = FALSE,
                         var_labels = TRUE)


# Table one with strata
foo <- create_table_one(data = pbc,
                        strata = "trt",
                        vars = myVars,
                        fct_vars = catVars,
                        keep_test = FALSE,
                        show_smd = FALSE,
                        var_labels = TRUE)


flex_print_table_one(x = foo)

flex_print_table_one(x = foo2)

flex_print_table_one(x = foo, 
                     font_name = "Lato", 
                     title = "Edgar Anderson's Iris Data", 
                     title_size = 16, 
                     subtitle = "Just the head")

flex_print_table_one(x = foo2, 
                     font_name = "Lato", 
                     title = "Edgar Anderson's Iris Data", 
                     title_size = 16)

}
