#' Pdf scrapes report period from the 2020 CASPER reports
#'
#' The 2020 CASPER reports are slightly different from the 2019 reports (the date format is
#' different), so this function specifically scrapes the 2020 CASPER reports
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the report period from the pdf
#' @param details may want to save as an object with the same name as shown below as these functions are built to go into building a table (see \code{\link{get_1_table_info_2020}})
#' @examples
#'     raw_text <- pdf_text("yourpdf.pdf")
#'
#'     Report_Period <- get_report_period_2020(raw_text)
#'
#' @export

get_report_period_2020 <- function(text){
  ## Getting the Report Period
  Report_Period <- str_locate(text, "Report Period: ")
  Report_Period <- str_sub(text, Report_Period[2], Report_Period[2] + 25)
  Report_Period <- str_trim(Report_Period, "both")
  return(Report_Period)
}

#' Pdf scrapes comparison group from the 2020 CASPER reports
#'
#' The 2020 CASPER reports are slightly different from the 2019 reports (the date format is
#' different), so this function specifically scrapes 2020 CASPER reports
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the comparison group from the pdf
#' @param details may want to save as an object with the same name as shown below as these functions are built to go into building a table (see \code{\link{get_1_table_info_2020}})
#' @examples
#'     raw_text <- pdf_text("yourpdf.pdf")
#'
#'     Comparison_Group <- get_comparison_group_2020(raw_text)
#'
#' @export

get_comparison_group_2020 <- function(text){
  ## Getting the Comparision Group
  Comparision_Group <- str_locate(text, "Comparison Group: ")
  Comparision_Group <- str_sub(text, Comparision_Group[2], Comparision_Group[2] + 25)
  Comparision_Group <- str_trim(Comparision_Group, "both")
  return(Comparision_Group)
}

