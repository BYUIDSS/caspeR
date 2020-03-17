#' Builds the scrapped table from the 2020 CASPER reports with two tables
#'
#' This function is to be used for the pdf reports from 2020 that have two tables in the
#' pdf (Jan 2020 reports have one table but Feb 2020 reports have two). If the report being
#' scrapped has one table see \code{\link{get_1_table_info_2020}}. The arguments for
#' this function are the same as the outputs for \code{\link{get_table_info_old}} and can be
#' obtained from the following functions: \code{get_facility_id}, \code{get_CNN}, \code{get_
#' facility_name}, \code{get_report_period}, \code{get_comparison_group}, \code{get_run_date
#' }, \code{get_city_and_state},\code{get_version_number}. User should also be aware that
#' all NA's are represented by the word "null"
#'
#' @param object requires character strings, the raw_text from the CASPER pdf for the
#'    following: Facility_ID, CCN, Facility_Name, Report_Period, Comparision_Group,
#'    Run_Date, City_State, and the Version_Number
#' @return returns the all the information from the data table on the CASPER reports as a
#'    dataframe in R
#' @param details the easiest way to use this function is to use the other functions
#'    mentioned above to get the numbers from the pdf and assign variable names to their
#'    outputs
#' @examples
#'    raw_text <- pdf_text("yourpdf.pdf")
#'    get_table_info_old(raw_text, "MS0004", "123456", "Name of Facility", "09/01/2019 - 02
#'    /31/2020", "10/01/2019 - 03/31/2020", "11/03/2019", "Rexburg, ID","3.02")
#'
#'    raw_text <- pdf_text("yourpdf.pdf")
#'    Facility_ID <- get_facility_id(raw_text)
#'    CNN <- get_CNN(raw_text)
#'
#'    get_table_info_old(raw_text, Facility_ID, CCN, Facility_Name, Report_Period, Comparisio
#'    n_Group, Run_Date, City_State, Version_Number)
#'
#' @export

get_2_tables_info_2020 <- function(text, Facility_ID, CCN, Facility_Name, Report_Period, Comparision_Group, Run_Date, City_State, Version_Number){

  raw_text <- str_replace_all(text, "[:space:]{3,}", "   ")

  table_start <- str_locate_all(raw_text, "Average   Percentile\r\n|Percent   Average\r\n") # Finding the where the table should start based on the last column's name

  table_end <- str_locate_all(raw_text, "CMS")
  #table_end <- str_locate(raw_text, "This report may contain privacy") # Finding the end of the table2


  table_info <- str_sub(raw_text, table_start[[1]][1,2] + 1, table_end[[1]][2,1] - 3) # grabing just the table's information


  table_info <- str_replace_all(table_info, "[:space:]{3,}", ",") # replacing all of the weird gaps with commas
  table_info <- str_split(table_info, "\r\n") # making the string a list of the rows that we want

  x <- table_info %>%
    map(~str_split_fixed(string = ., ",", n=10))


  x <- data.frame(x) # putting it in a data frame
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
    ) # Renaming columns

  ### Getting the other part of the table

  table_info_2 <- str_sub(raw_text, table_start[[1]][2,2]+1)
  table_info_2 <- str_replace_all(table_info_2, "[:space:]{3,}", ",") # replacing all of the weird gaps with commas
  table_info_2 <- str_split(table_info_2, "\r\n")

  x <- table_info_2 %>%
    map(~str_split_fixed(string = ., ",", n = 7)) %>%
    data.frame()


  x <- apply(x, 2, function(x) gsub("^$|^ $", NA, x)) %>%
    na.omit() %>%
    as.data.frame()

  x <- x %>%
    rename(
      `Measure Description` = X1,
      `CMS ID` = X2,
      `Num` = X3,
      `Denom` = X4,
      `Facility Observed Percent` = X5,
      `Facility Adjusted Percent` = X6,
      `National Average` = X7
    )


  table_info$`National Average` <- NA
  x$`Data` <- NA
  x$`Comparison Group State Average` <- NA
  x$`Comparison Group National Average` <- NA
  x$`Comparison Group National Percentile` <- NA

  table_info <- rbind(table_info, x)

  ## This is adding the information that we found above in to the data set to identify the facility and other stuff

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
  #head(table_info)

  table_info <- table_info %>%
    mutate(`Stay Length` = case_when(
      str_detect(`Measure Description`, "(L)") ~ "Long",
      str_detect(`Measure Description`, "(S)") ~ "Short",
      TRUE ~ "null"
    ))

  return(table_info)

}
