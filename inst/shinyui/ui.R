library(shiny)
library(shinyWidgets)
library(shinyhelper)

srhelp <- function(x, content, ...){
  helper(x, content=content, colour="black", ...)
}

# Define UI for application that draws a histogram
shinyUI(
  navbarPage('Privatized Network Sampling (PNS)',
             #tag$script(HTML("updateFirstMultiInput = function(x){ var event = new Event('input'); $('.multi-wrapper .search-input').get(0).dispatchEvent(event);};Shiny.addCustomMessageHandler('updateFirstMultiInput', updateFirstMultiInput);")),
             tabPanel('Introduction',
                      #includeScript("updateMulti.js"),
                      h4('Welcome to the PNS estimation application'),
                      br(),
                      p("The purpose of this tool is the estimation of population size from an RDS/PNS survey that collects network connection infromation."),
                      br(),
                      h5('Please proceed to the', em('Load Data'), 'tab')
             ),
             tabPanel('Load Data',
                      fluidPage(
                        # Application title
                        titlePanel("Load Data") ,

                        # Sidebar with a slider input for number of bins
                        sidebarLayout(
                          sidebarPanel(
                            fileInput("file1", "Choose CSV File",
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                            ) |> srhelp(content="file1"),
                            conditionalPanel("output.table != null",
                                             selectizeInput("subject", "Subject ID:",
                                                            c("")) |> srhelp(content="subject"),
                                             tabsetPanel(
                                               tabPanel("Coupons",
                                                        selectizeInput("subject_coupon", "Subject Coupon:",
                                                                       c("")) |> srhelp(content="subject_coupon"),
                                                        pickerInput(inputId = "coupons",
                                                                    label = "Coupons:",
                                                                    choices = c(""),
                                                                    multiple=TRUE,
                                                                    options = list(`actions-box` = TRUE,
                                                                                   `live-search`=TRUE,
                                                                                   `none-selected-text`="Choose Variable")) |> srhelp(content="coupons")
                                               ),
                                               tabPanel("Recruiter ID",
                                                        selectizeInput("recruiter", "Recrutier ID:",
                                                                       c("")) |> srhelp(content="recruiter")
                                               )
                                             ),tags$hr(),
                                             selectizeInput("degree", "Network Size:",
                                                            c("")) |> srhelp(content="degree"),
                                             selectizeInput("subject_hash", "Subject's Hashed Identifier:",
                                                            c("")) |> srhelp(content="subject_hash"),
                                             pickerInput(inputId = "nbrs",
                                                         label = "Hashed Identifiers For Contacts:",
                                                         choices = c(""),
                                                         multiple=TRUE,
                                                         options = list(`actions-box` = TRUE,
                                                                        `live-search`=TRUE,
                                                                        `none-selected-text`="Choose Variable")) |> srhelp(content="nbrs")


                            ),
                            width=5),
                          mainPanel(
                            conditionalPanel("input.degree != \"\"",
                                             h2("Network Size Descriptives:"),
                                             plotOutput("degree_plot", width = "100%", height = "200px")
                            ),
                            dataTableOutput("table"),
                            width = 7
                          )
                        )
                      )
             ),
             tabPanel('Analysis',
                      fluidPage(
                        # Application title
                        titlePanel("Analysis"),

                        # Sidebar with a slider input for number of bins
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons(
                              inputId = "method",
                              label = "Method:",
                              choices = c("Network", "Sample","Alter")
                            ) |> srhelp(content="method"),
                            numericInput("rho",
                                         "Rho:",
                                         0, min=0)|> srhelp(content="rho"),
                            actionButton("calc_rho","Calculate rho From data"),
                            br(),
                            br(),
                            numericInput("nrep",
                                         "# of Bootstraps:",
                                         50, min=2) |> srhelp(content="nrep"),
                            br(),
                            checkboxInput("small_sample_fraction","Small Sample Fraction", value = FALSE) |> srhelp(content="small_sample_fraction"),
                            br(),
                            actionButton('run', 'Run'),
                            actionButton('cancel', 'Cancel')
                          ),
                          mainPanel(
                            h3("Descriptives"),
                            tableOutput("descriptives_table")  |> srhelp(content="descript"),
                            h3("Point Estimate"),
                            tableOutput("point_results"),
                            h3("Bootstrap Intervals"),
                            tableOutput("bootstrap1")
                          )
                        )
                      )
             )
  )
)
