
installed.packages("dplyr")
installed.packages("plotly")
installed.packages("validate")
installed.packages("rmarkdown")
installed.packages("readxl")
installed.packages("haven")
installed.packages("knitr")
installed.packages("shiny")
installed.packages("formatters")
installed.packages("gt")
installed.packages("tidyverse")
installed.packages("rlistings")
installed.packages("shinythemes")
installed.packages("DT")


library(dplyr)
library(plotly)
library(validate)
library(rmarkdown)
library(readxl)
library(haven)
library(knitr)
library(shiny)
library(formatters)
library(gt)
library(tidyverse)
library(rlistings)
library(shinythemes)
library(DT)


ui <-  navbarPage(theme = shinytheme("flatly"), "Tepo Analysis",
                  tabPanel("outdata",
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Subjects Data",
                                        mainPanel(
                                          dataTableOutput("s1")
                                        )),
                               tabPanel("Adverse enets",
                                        mainPanel(
                                          dataTableOutput("ae")
                                        )),
                               tabPanel("Table",
                                        mainPanel(
                                          tabPanel("LAB", mainPanel(verbatimTextOutput("Table1"),
                                                                    width = 12))
                                        )),
                               tabPanel("Plot",
                                        mainPanel(
                                          tabPanel("LAB", mainPanel(verbatimTextOutput("plot1"),
                                                                    width = 12))
                                        ))
                             )
                           )))



server <- function(input, output, session){
  output$s1 <- renderDT(ex_adsl, options=list(pageLength = 15))
  output$ae <- renderDT(ex_adae, options=list(pageLength = 15))
  
  data <- reactive({ex_adlb %>%
       group_by(USUBJID,PARAMCD) %>%
       arrange(USUBJID,PARAMCD, AVAL)%>%
       mutate(
         #first.
         MIN = if_else(row_number(AVAL) == 1, "Y", ""),
         #last.
         MAX = if_else(row_number(AVAL) == n(), "Y", "")
       )%>% filter(SUBJID =="id-105")%>% select(USUBJID,PARAMCD,AVAL,AVISIT, MIN, MAX)})
  output$Table1 <- renderPrint({
    lsting <- as_listing(
      df = data(),
      disp_cols = c( "PARAMCD","AVAL", "MIN", "MAX"),
      key_cols = c("USUBJID", "AVISIT"),
      main_title = "Lab listing",
      subtitles = c("Other sub titles1", "Other sub titles2"),
      main_footer = c("Footnote1", "Footnote2"),
      prov_footer = "Source:ADLB, data:"
    )
    lsting
  })
  output$plot1 <- renderPlotly({
    plot_ly(data = ex_adlb, x = ex_adlb$ADY, y = ex_adlb$AVAL, color = ex_adlb$PARAMCD, type = "scatter", mode = "markers")%>%
      layout(
        title = "Customized Scatter Plot",
        xaxis = list(title = "Analysis Day"),
        yaxis = list(title = "Analysis Result")
      )
  })
  
}

shinyApp(ui, server)
