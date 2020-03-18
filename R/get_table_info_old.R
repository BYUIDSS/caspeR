#' Builds the scrapped table from the CASPER reports prior to 2020
#'
#' The arguments for this function are the outputs obtained from using the following
#' functions: \code{get_facility_id}, \code{get_CNN}, \code{get_facility_name}, \code{get_rep
#' ort_period}, \code{get_comparison_group}, \code{get_run_date}, \code{get_city_and_state
#' },\code{get_version_number}.
#' User should also be aware that all NA's are represented by the word "null"
#'
#' @param object requires character strings, the raw_text from the CASPER pdf for the
#'    following: Facility_ID, CCN, Facility_Name, Report_Period, Comparision_Group,
#'    Run_Date, City_State, and the Version_Number
#'
#' @return returns the all the information from the data table on the CASPER reports prior to
#' 2020 as a dataframe in R
#' @param details the easiest way to use this function is to use the other functions
#'    mentioned above to get the numbers from the pdf and assign variable names to their
#'    outputs
#' @examples
#' \dontrun{
#'    raw_text <- pdf_text("yourpdf.pdf")
#'    get_table_info_old(raw_text, Facility_ID = "MS0004", CCN = "123456", Facility_Name =
#'    "Name of Facility", Report_Period = "09/01/2019 - 02/31/2020", Comparison_Group = "10
#'    /01/2019 - 03/31/2020", Run_Date = "11/03/2019", City_State = "Rexburg, ID",Version_Num
#'    ber = "3.02")
#'
#'    raw_text <- pdf_text("yourpdf.pdf")
#'    facility_id <- get_facility_id(raw_text)
#'    ccn <- get_CCN(raw_text)
#'    facility_name <- get_facility_name(raw_text)
#'    report_pd <- get_report_period(raw_text)
#'
#'    get_table_info_old(raw_text, Facility_ID = facility_id, CCN = ccn, Facility_Name =
#'    facility_name, Report_Period = report_pd, Comparision_Group = comp_group, Run_Date =
#'    run_date, City_State = citystate, Version_Number = vnumber)
#' }
#' @export

get_table_info_old <- function(text, Facility_ID, CCN, Facility_Name, Report_Period, Comparision_Group, Run_Date, City_State, Version_Number){

  raw_text <- str_replace_all(text, "[:space:]{3,}", "   ")

  table_start <- str_locate(raw_text, "Average   Percentile\r\n|Percent   Average\r\n") # Finding the where the table should start based on the last column's name

  table_end <- str_locate(raw_text, "This report may contain privacy") # Finding the end of the table2


  table_info <- str_sub(raw_text, table_start[2] + 1, table_end[1] - 5) # grabbing just the table's information


  table_info <- str_replace_all(table_info, "[:space:]{3,}", ",") # replacing all of the weird gaps with commas
  table_info <- str_split(table_info, "\r\n") # making the string a list of the rows that we want

  x <- table_info %>%
    map(~str_split_fixed(string = ., ",", n=10))

  x <- data.frame(x) # putting it into a data frame
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

  table_info <- apply(table_info, 2, function(x) gsub("^$|^ $", NA, x)) # adding NA's to the last two rows that aren't real rows in the dataset

  table_info <- as.data.frame(table_info)

  table_info <- table_info %>%
    filter(!is.na(`CMS ID`))

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
