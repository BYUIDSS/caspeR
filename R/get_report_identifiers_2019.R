#' Pulls the Facility ID from the 2019-2020 CASPER reports
#'
#' This function can be used to pull the Facility ID off the pdfs, which is required for
#' building the table (see \code{get_table_info_old}).
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the Facility ID from the CASPER reports
#' @param details may want to save as an object with the same name as shown in the example
#'     below as these functions are needed to build a table (see \code{\link{get_table_info_old}
#'     })
#'
#' @examples
#' raw_text <- pdf_text("yourpdf.pdf")
#'
#' Facility_ID <- get_facility_id(raw_text)
#'
#' @export

get_facility_id <- function(text){
  ## Geting the Facility ID
  Facility_ID <- str_locate(text, "Facility ID:") # finding the charactor location of the ID, this returns a start and the end spots of charaters
  Facility_ID <- str_sub(text, Facility_ID[2] + 1, (Facility_ID[2]+11)) # This is grabing the ID
  Facility_ID <- str_trim(Facility_ID, "both") # This was me cuting off the white space at the end
  return(Facility_ID) # checking to see if I got the correct number

}

#' Pulls the CCN from the 2019-2020 CASPER reports
#'
#' This function can be used to pull the CCN off the pdfs, which is required for
#' building the table (see \code{get_table_info_old}).
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the CCN from the CASPER reports
#' @param details may want to save as an object with the same name as shown in the example
#'     below as these functions are needed to build a table (see \code{\link{get_table_info_old}
#'     })
#'
#' @examples
#' raw_text <- pdf_text("yourpdf.pdf")
#'
#' CCN <- get_CCN(raw_text)
#'
#' @export

get_CCN <- function(text){
  CCN <- str_locate(text, "CCN: ")
  CCN <- str_sub(text, CCN[2], CCN[2]+7)
  CCN <- str_trim(CCN, "both")
  return(CCN)
}

#' Pulls the Facility Name from the 2019-2020 CASPER reports
#'
#' This function can be used to pull the Facility Name off the pdfs, which is required for
#' building the table (see \code{get_table_info_old}).
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the Facility Name from the CASPER reports
#' @param details may want to save as an object with the same name as shown in the example
#'     below as these functions are needed to build a table (see \code{\link{get_table_info_old}
#'     })
#'
#' @examples
#' raw_text <- pdf_text("yourpdf.pdf")
#'
#' Facility_Name <- get_facility_name(raw_text)
#'
#' @export

get_facility_name <- function(text){
  ## Getting the Facility Name
  Facility_Name <-str_locate(text, "Facility Name: ")
  Facility_Name <- str_sub(text, Facility_Name[2], Facility_Name[2]+50)
  Facility_Name <- str_trim(Facility_Name, "both")
  return(Facility_Name)
}

#' Pulls the Report Period ONLY from the 2019 CASPER reports
#'
#' This function can be used to pull the Report Period off the pdfs, which is required for
#' building the table (see \code{get_table_info_old}). If it is a 2020 CASPER report
#' \code{get_report_period_2020} should be used (the pdf date format changed from 2019 to
#' 2020).
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the Report Period from the 2019 CASPER reports
#' @param details may want to save as an object with the same name as shown in the example
#'     below as these functions are needed to build a table (see \code{\link{get_table_info_old}
#'     })
#'
#' @examples
#' raw_text <- pdf_text("yourpdf.pdf")
#'
#' Report_Period <- get_report_period(raw_text)
#'
#' @export

get_report_period <- function(text){
  ## Getting the Report Period
  Report_Period <- str_locate(raw_text, "Report Period: ")
  Report_Period <- str_sub(raw_text, Report_Period[2], Report_Period[2] + 20)
  Report_Period <- str_trim(Report_Period)
  return(Report_Period)
}

#' Pulls the Comparison Group ONLY from the 2019 CASPER reports
#'
#' This function can be used to pull the Comparison Group off the pdfs, which is required for
#' building the table (see \code{get_table_info_old}). If it is a 2020 CASPER report
#' \code{get_report_period_2020} should be used (the pdf date format changed from 2019 to
#' 2020).
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the Comparison Group from the 2019 CASPER reports
#' @param details may want to save as an object with the same name as shown in the example
#'     below as these functions are needed to build a table (see \code{\link{get_table_info_old}
#'     })
#'
#' @examples
#' raw_text <- pdf_text("yourpdf.pdf")
#'
#' Comparison_Group <- get_CCN(raw_text)
#'
#' @export

get_comparison_group <- function(text){
  ## Getting the Comparision Group
  Comparision_Group <- str_locate(raw_text, "Comparison Group: ")
  Comparision_Group <- str_sub(raw_text, Comparision_Group[2], Comparision_Group[2] + 20)
  Comparision_Group <- str_trim(Comparision_Group)
  return(Comparision_Group)
}

#' Pulls the Run Date from the 2019-2020 CASPER reports
#'
#' This function can be used to pull the Run Date off the pdfs, which is required for
#' building the table (see \code{get_table_info_old}).
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the Run Date from the CASPER reports
#' @param details may want to save as an object with the same name as shown in the example
#'     below as these functions are needed to build a table (see \code{\link{get_table_info_old}
#'     })
#'
#' @examples
#' raw_text <- pdf_text("yourpdf.pdf")
#'
#' Run_Date <- get_run_date(raw_text)
#'}
#' @export

get_run_date <- function(text){
  ## Run date
  Run_Date <- str_locate(text, "Run Date: ")
  Run_Date <- str_sub(text, Run_Date[2], Run_Date[2] + 10)
  Run_Date <- str_trim(Run_Date, "both")
  return(Run_Date)
}

#' Pulls the City and State from the 2019-2020 CASPER reports
#'
#' This function can be used to pull the City and State off the pdfs, which is required for
#' building the table (see \code{get_table_info_old}).
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the city and state from the CASPER reports
#' @param details may want to save as an object with the same name as shown in the example
#'     below as these functions are needed to build a table (see \code{\link{get_table_info_old}
#'     })
#'
#' @examples
#' raw_text <- pdf_text("yourpdf.pdf")
#'
#' City_State <- get_city_and_state(raw_text)
#'
#' @export

get_city_and_state <- function(text){
  ## City and State *I'll need to ask if they want it in separate columns
  City_State <- str_locate(text, "City/State: ")
  City_State <- str_sub(text, City_State[2], City_State[2] + 30 )
  City_State <- str_trim(City_State, "both")
  return(City_State)
}

#' Pulls the Version Number from the 2019-2020 CASPER reports
#'
#' This function can be used to pull the Version Number off the pdfs, which is required for
#' building the table (see \code{get_table_info_old}).
#'
#' @param object requires a character string (text) from the scrapped pdf
#' @return returns the Version Number from the CASPER reports
#' @param details may want to save as an object with the same name as shown in the example
#'     below as these functions are needed to build a table (see \code{\link{get_table_info_old}
#'     })
#'
#' @examples
#' raw_text <- pdf_text("yourpdf.pdf")
#'
#' Version_Number <- get_version_number(raw_text)
#'
#' @export

get_version_number <- function(text){
  Version_Number <- str_locate(text, "Version Number: ")
  Version_Number <- str_sub(text, Version_Number[2], Version_Number[2] + 5)
  Version_Number <- str_trim(Version_Number, "both")
  return(Version_Number)
}
