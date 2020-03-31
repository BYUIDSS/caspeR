#' Pdf scrapes report period from the 2020 CASPER reports
#'
#' The 2020 CASPER reports are slightly different from the 2019 reports (the date format
#' is different), so this function specifically scrapes report period from the 2020
#' CASPER reports which is required for building a table
#' (see \code{get_table_info_old()}, \code{get_1_table_info_2020()}, or
#' \code{get_2_tables_info_2020()} for further usage). If it is a 2019 CASPER report
#' \code{get_report_period()} should be used instead.
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the report period date range from the pdf as a string
#' @param details may want to save as an object as shown in the example below in
#'    preparation for building a table
#'
#' @examples
#' #make sure you have these packages installed and loaded
#' pacman::p_load(tidyverse, devtools, caspeR, pdftools, roxygen2,zoo, stringr, glue)
#'
#' raw_text <- pdf_text("R/caspeR_example.pdf")
#'
#' report_pd <- get_report_period_2020(raw_text)
#'
#' @export

get_report_period_2020 <- function(text){
  ## Getting the Report Period
  Report_Period <- stringr::str_locate(text, "Report Period: ")
  Report_Period <- stringr::str_sub(text, Report_Period[2], Report_Period[2] + 25)
  Report_Period <- stringr::str_trim(Report_Period, "both")
  return(Report_Period)
}

#' Pdf scrapes comparison group from the 2020 CASPER reports
#'
#' The 2020 CASPER reports are slightly different from the 2019 reports (the date format
#' is different), so this function specifically scrapes the comparison group from the
#' 2020 CASPER reports. This is required for building a table
#' (see \code{get_table_info_old()}, \code{get_1_table_info_2020()}, or
#' \code{get_2_tables_info_2020()} for further usage). If it is a 2020 CASPER report
#' \code{get_report_period_2020()} should be used instead.
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the comparison group date range from the pdf as a string
#' @param details may want to save as an object with the same name as shown below as
#'    these functions are built to go into building a table
#'
#' @examples
#' #make sure you have these packages installed and loaded
#' pacman::p_load(tidyverse, devtools, caspeR, pdftools, roxygen2,zoo, stringr, glue)
#'
#' raw_text <- pdf_text("R/caspeR_example.pdf")
#'
#' comp_group <- get_comparison_group_2020(raw_text)
#'
#' @export

get_comparison_group_2020 <- function(text){
  ## Getting the Comparision Group
  Comparision_Group <- str_locate(text, "Comparison Group: ")
  Comparision_Group <- str_sub(text, Comparision_Group[2], Comparision_Group[2] + 25)
  Comparision_Group <- str_trim(Comparision_Group, "both")
  return(Comparision_Group)
}

