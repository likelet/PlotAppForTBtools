library(shiny)
library(shinydashboard)
library(shinyBS)

# source("extraWedget.R")

dashboardPage(
  dashboardHeader(title = "Plot server for TBtools"),
  dashboardSidebar(
    #     sliderInput("rateThreshold", "Warn when rate exceeds",
    #                 min = 0, max = 50, value = 3, step = 0.1
    #     ),
    tags$link(rel="stylesheet",type="text/css",href="main.css"),
    sidebarMenu(
      menuItem("Introducction", tabName = "introduction"),
      menuItem("GOenrichment", tabName = "GOenrichment",icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("introduction", 
              
              p("This application was designed for plot data output by TBtools, which was developed by CJ from UCSC"),
              br(),
              br(),
              strong("This application was created by "),
              tags$a(href="mailto:zhaoqi3@mail2.sysu.edu.cn",strong("Qi Zhao")),
              strong(' from '),
              tags$a(href="http://gps.biocuckoo.org/",strong("Ren Lab")),
              strong(" in "),
              tags$a(href="http://www.sysu.edu.cn/2012/en/index.htm",strong("SYSU")),
              strong('. Please let us know if you find bugs or have new feature request.This application uses the'),
              tags$a(href="http://www.rstudio.com/shiny/",strong("shiny package from RStudio."))
      ),
      
      ###function 1
      tabItem("GOenrichment",
              fluidRow(
                # box(
                #   width = 8, status = "success",solidHeader = TRUE,collapsible = TRUE,
                #   titl="plot",
                #   plotOutput("GOplotRender")
                # ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,collapsible = TRUE,
                  title = "Data Input",
                  radioButtons("dataset", strong("Dataset"), c(Example = "example", Upload = "upload"),selected = 'example'),
                  
                  conditionalPanel(
                    condition = "input.dataset == 'upload'",
                    fileInput('file1', 'Choose CSV/text File',
                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                    checkboxInput('header', 'Header', FALSE),
                    radioButtons('sep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 '\t'),
                    radioButtons('quote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 '')
                  ),
                  # numericInput('geneMappedGo', 'Total Annotated Genes',13000,max=30000,min=0),
                  checkboxInput("Issorted","Sorted Counts",TRUE),
                  tags$button(id="goPlot",type="button",class="btn btn-primary action-button","GO PLOT!")
                 
                  
                  
                ),
                box(
                  width = 8, status = "success",solidHeader = TRUE,collapsible = TRUE,
                  title="Dataset",
                  DT::dataTableOutput("summaryGO")
                    
                  
                ),
                bsModal("GOPlotModal", h3("Fancy Plot"), "goPlot", size = "large",
                        div(
                        downloadLink('downloadDataPNG', 'Download PNG-file'),
                        downloadLink('downloadDataPDF', 'Download PDF-file',class="downloadLinkred"),
                        downloadLink('downloadDataEPS', 'Download EPS-file',class="downloadLinkblue")
                        ),
                        selectInput("theme", "Plot Theme:",
                                    c("Tufte","Economist","Solarized","Stata","Excel 2003","Inverse Gray","Fivethirtyeight","Tableau","Stephen","Wall Street","GDocs","Calc","Pander","High
                                      charts")),
                plotOutput("GOplotRender",height="100%",width=800)
                
                )
                
              )
      )
    )
  )
)