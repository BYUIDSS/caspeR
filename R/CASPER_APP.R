#' Shiny App Function
#'
#' There are no arguments for this function, users will simply run the code CASPER_APP
#' and it will run the code and open the Shiny App. Other functions from the caspeR
#' package are used in this app. See details on GitHub for instructions on how to use
#' the Shiny App.
#'
#' @export
CASPER_APP <- function(){
  require(shiny)
  require(DT); require(glue); require(tidyverse)
  require(stringr); require(ggthemes); require(pdftools)
  require(zoo); require(caspeR)
  shinyApp(

    ui = fluidPage(
      # Create a row
      fluidRow(
        # Include company logo
        column(3,
               #img(src = "cascadia_logo.png", height = 150, width = 150)
        ),  ## end of the column
        # include title
        column(6,
               h1("CASPER Report Scraper"),
               h3("Importing All Files")
        ) ## end of the column
      ), ## end of fluidRow
      # add some space
      br(),
      br(),
      # Panel for inputs ----
      sidebarPanel(
        # Input: Select a file ----
        fileInput("upload", "Choose PDF File",
                  multiple = TRUE,
                  accept = (".pdf")), ## end of the file input

        actionButton("createCSV", "Create CSV"), ## end of the action button
        br(),
        br(),
        downloadButton("downloadCSV", "Download CSV"), ## end of the action button
        br()
      ),  ## end of side bar panel
      #  Main panel for displaying outputs ----
      mainPanel(
        tabsetPanel(
          id= 'tabs',
          tabPanel('Uploaded Files',
                   fluidRow(
                     column(12, tableOutput("files"))
                   )), ## the end of the fluid row and tab pabel
          tabPanel('New CSV',
                   fluidRow(
                     column(12, dataTableOutput("newCSV"))
                   )) ## end of the fluid Row and tab Panel
        ) # end of tabest Panel
      )  ## end of the main panel
    ),  # end of the ui

    server = function(input, output, session) {

      ## should give a table of pdf names
      output$files <- renderTable(input$upload$name)

      ## making an empty table for the for loop
      table_all <-  tibble(`Measure Description` = character(),
                           `CMS ID` = factor(),
                           `Data` = factor(),
                           `Num` = factor(),
                           `Denom` = factor(),
                           `Facility Observed Percent` = factor(),
                           `Facility Adjusted Percent` = factor(),
                           `Comparison Group State Average` = factor(),
                           `Comparison Group National Average` = factor(),
                           `Comparison Group National Percentile` = as.character(),
                           `National Average` = factor(),
                           `Facility ID` = factor(),
                           `CCN` = factor(),
                           `Facility Name` = factor(),
                           `Report Period` = factor(),
                           `Comparison Group` = factor(),
                           `Report Run Date` = factor(),
                           `City/State` = character(),
                           `Version Number` = character(),
                           `Flagged` = character(),
                           `Stay Length` = character())

      ## should render a data table
      renderDataTable(table_all)

      ## should run the loop when the buton is clicked
      observeEvent(input$createCSV, {

        getdat <- reactive({
          inFile <- input$upload
          if (is.null(inFile)) return(NULL)
          pdf_list <- inFile$datapath
          return(pdf_list)
        })

        for (i in getdat()){
          if(str_detect(i, '2019')){

            raw_text <- pdf_text(i)

            Facility_ID <- get_facility_id(raw_text)

            CCN <- get_CCN(raw_text)

            Facility_Name <- get_facility_name(raw_text)

            Report_Period <- get_report_period(raw_text)

            Comparision_Group <- get_comparision_group(raw_text)

            Run_Date <- get_run_date(raw_text)

            City_State <- get_city_and_state(raw_text)

            Version_Number <- get_version_number(raw_text)

            table_stuff <- get_table_info_old(raw_text, Facility_ID, CCN, Facility_Name, Report_Period, Comparision_Group, Run_Date, City_State, Version_Number)

            table_all <- rbind(table_all, table_stuff)


          } else{
            raw_text <- pdf_text(i)

            Facility_ID <- get_facility_id(raw_text)

            CCN <- get_CCN(raw_text)

            Facility_Name <- get_facility_name(raw_text)

            Report_Period <- get_report_period_2020(raw_text)

            Comparision_Group <- get_comparison_group_2020(raw_text)

            Run_Date <- get_run_date(raw_text)

            City_State <- get_city_and_state(raw_text)

            Version_Number <- get_version_number(raw_text)

            if (str_count(raw_text, "CMS") == 2){

              table_stuff <- get_2_tables_info_2020(raw_text, Facility_ID, CCN, Facility_Name, Report_Period, Comparision_Group, Run_Date, City_State, Version_Number)

            } else {
              table_stuff <- get_1_table_info_2020(raw_text, Facility_ID, CCN, Facility_Name, Report_Period, Comparision_Group, Run_Date, City_State, Version_Number)
            }

            table_all <- rbind(table_all, table_stuff)
          }
        }

        isolate({table_all})
        output$newCSV <- renderDataTable(table_all)
        output$downloadCSV <- downloadHandler(
          filename = function(){
            paste("dataset-", Sys.Date(), ".csv", sep = "")
          },
          content = function(file) {
            write.csv(table_all, file)
          }
        )
      })
    }

  )

}
