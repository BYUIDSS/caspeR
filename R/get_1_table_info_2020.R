#' Builds the scrapped table from the 2020 CASPER reports
#'
#' This function is for pdf reports from 2020 that only have one table in the pdf
#' (Jan 2020 reports have one table but Feb 2020 reports have two). If the report being
#' scrapped has two tables see \code{get_two_tables_2020()}. The arguments for this
#' function have the same arguments as \code{get_table_info_old()} other than the
#' Report_Period and the Comparision_Group. User should also be aware that all NA's
#' are represented by the word "null".
#'
#' @param details the easiest way to use this function is to use the other functions
#'    mentioned above to get the numbers from the pdf and assign variable names to
#'    their outputs
#' @param object requires character strings, the raw_text from the CASPER pdf
#' @return returns the all the information from the data table on the CASPER reports as
#'     a dataframe in R
#' @param Facility_ID Facility's ID Number in a string format, ex: "LC0011535", see
#'    \code{get_facility_id()}
#' @param CCN CMS's certification number in a string format, ex: "L002.03", see
#'    \code{get_CNN()}
#' @param Facility_Name Facility's name in string format, ex: "John's Fruit Stand", see
#'    \code{get_facility_name()}
#' @param Report_Period The report period put in string format, month/day/year, ex: "04
#'    /01/2023 - 09/30/2023", see \code{get_report_period_2020()}
#' @param Comparision_Group The comparison group put in string format, month/day/year,
#'    ex: "02/02/2023 - 07/31/2023", see \code{get_comparison_group_2020()}
#' @param Run_Date The date that the report was compiled on put in string format, month
#'    /day/year, ex: "10/06/2023", see \code{get_run_date()}
#' @param City_State This is the city and state that the facility is located in put in
#'    string format, ex: "Richland, WA", see \code{get_city_and_state()}
#' @param Version_Number The report version number of the CASPER report in string
#'    format, ex: "1.01", see \code{get_version_number()}
#'
#' @examples
#' #make sure you have these packages installed and loaded
#' pacman::p_load(tidyverse, devtools, caspeR, pdftools, roxygen2,zoo, stringr, glue)
#'
#' #there are two ways of using this function, putting the strings in by hand or naming
#' them as demonstrated in table1 and table2
#'
#' raw_text <- pdf_text("R/caspeR_example.pdf")
#'
#' table1 <- get_1_table_info_2020(raw_text, Facility_ID = "LC0011535", CCN = "L002.03"
#' , Facility_Name = "John's Fruit Stand", Report_Period = "04/01/2023 - 09/30/2023",
#' Comparision_Group = "02/02/2023 - 07/31/2023", Run_Date = "10/6/2023", City_State =
#' "Richland, WA",Version_Number = "1.01")
#'
#' View(table1)
#'
#' fac_ID <- get_facility_id(raw_text)
#' ccn <- get_CCN(raw_text)
#' fac_name <- get_facility_name(raw_text)
#' report_pd <- get_report_period_2020(raw_text)
#' comp_group <- get_comparison_group_2020(raw_text)
#' rundate <- get_run_date(raw_text)
#' citystate <- get_city_and_state(raw_text)
#' vnumber <- get_version_number(raw_text)
#'
#' table2 <- get_1_table_info_2020(raw_text, Facility_ID = fac_ID,
#' CCN = ccn, Facility_Name = fac_name, Report_Period = report_pd,
#' Comparision_Group = comp_group, Run_Date = rundate, Version_Number = vnumber,
#' City_State = citystate)
#'
#' View(table2)
#'
#' @export
get_1_table_info_2020 <- function(text, Facility_ID, CCN, Facility_Name, Report_Period, Comparision_Group, Run_Date, City_State, Version_Number){

  raw_text <- str_replace_all(text, "[:space:]{3,}", "   ")

  table_start <- str_locate(raw_text, "Average   Percentile\r\n|Percent   Average\r\n") # Finding the where the table should start based on the last column's name

  table_end <- str_locate(raw_text, "This report may contain privacy") # Finding the end of the table2


  table_info <- str_sub(raw_text, table_start[2] + 1, table_end[1] - 5) # grabing just the table's infomation


  table_info <- str_replace_all(table_info, "[:space:]{3,}", ",") # relacing all of the weird gaps with commams
  table_info <- str_split(table_info, "\r\n") # making the string a list of the rows that we want

  x <- table_info %>%
    map(~str_split_fixed(string = ., ",", n=10))

  x <- data.frame(x) # puting it into a data frame
  table_info <- x %>%
    rename(
      `Measure Description` = X1,
      `CMS ID` = X2,
      `Data` = X3,
      `Num` = X4,
      `Denom` = X5,
      `Facility Observed Percent` = X6,
      `Facility Adjusted Percent` = X7,
      `Comparison Group State Average` = X8,
      `Comparison Group National Average` = X9,
      `Comparison Group National Percentile` = X10
    ) # Renameing columns

  table_info$`National Average` <- NA
  table_info$`Facility ID` <- Facility_ID
  table_info$`CCN` <- CCN
  table_info$`Facility Name` <- Facility_Name
  table_info$`Report Period` <- Report_Period
  table_info$`Comparison Group` <- Comparision_Group
  table_info$`Report Run Date` <- Run_Date
  table_info$`City/State` <- City_State
  table_info$`Version Number` <- Version_Number

  table_info$`Facility Observed Percent` <- str_replace_all(table_info$`Facility Observed Percent`, "%", "") %>%
    as.numeric()

  table_info$`Facility Adjusted Percent` <- str_replace_all(table_info$`Facility Adjusted Percent`, "%", "") %>%
    as.numeric()

  table_info$`Comparison Group State Average` <- str_replace_all(table_info$`Comparison Group State Average`, "%", "") %>%
    as.numeric()

  table_info$`Comparison Group National Average` <- str_replace_all(table_info$`Comparison Group National Average`, "%", "") %>%
    as.numeric()

  table_info$`National Average` <- str_replace_all(table_info$`National Average`, "%", "") %>%
    as.numeric()

  table_info <- table_info %>%
    mutate(`Comparison Group National Average` = `Comparison Group National Average`/100 %>% as.double(),
           `Comparison Group State Average` = `Comparison Group State Average`/100 %>% as.double(),
           `Facility Adjusted Percent` = `Facility Adjusted Percent`/100 %>% as.double(),
           `Facility Observed Percent` = `Facility Observed Percent`/100 %>% as.double(),
           `National Average` = `National Average`/100) %>%
    na.fill("null")

  table_info <- table_info %>%
    as.data.frame()

  table_info <- table_info %>%
    mutate( Flagged = case_when(str_detect(`Comparison Group National Percentile`, "\\*") == TRUE ~ "flagged",
                                TRUE ~ "null"))


  table_info$`Comparison Group National Percentile` <- str_replace_all(table_info$`Comparison Group National Percentile`, "\\*", "") %>%
    str_replace_all("-", "null") %>%
    str_trim(side = "both")

  ### make a new column with short and long stays

  table_info <- table_info %>%
    mutate(`Stay Length` = case_when(
      str_detect(`Measure Description`, "(L)") ~ "Long",
      str_detect(`Measure Description`, "(S)") ~ "Short",
      TRUE ~ "null"
    ))

  return(table_info)

}
