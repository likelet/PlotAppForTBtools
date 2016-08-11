library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Ploter for TBtools"),
  dashboardSidebar(
    #     sliderInput("rateThreshold", "Warn when rate exceeds",
    #                 min = 0, max = 50, value = 3, step = 0.1
    #     ),
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
      
      tabItem("datainput",
              fluidRow(
                box(
                  width = 2, status = "info", solidHeader = TRUE,collapsible = TRUE,
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
                  selectInput('pagination', 'Choose Pagination',c('two_button', 'full_numbers')),
                  img(src="CUCKOO.jpg", height = "200", width = "200")
                  
                ),
                box(
                  width = 10, status = "success",solidHeader = TRUE,collapsible = TRUE,
                  h2("Status"),
                  verbatimTextOutput("dim"),
                  h2("Dataset"),
                  DT::dataTableOutput("summary")
                    
                  
                )
              )
      ),
      tabItem("result",
              
              fluidRow(
                box(
                  width = 8, status = "success",
                  title = "PCA anlysis Result",
                  tableOutput('pcatable'),
                  verbatimTextOutput("pcasum")
                ),
                box(
                  width = 4, status = "success",
                  title = "Figure Option",
                  radioButtons('pcaresult', strong("View option"),
                               c("Eigenvalues" = "eigenvalues",
                                 "Variables" = "variables",
                                 "Variables Coordinates" = "vcoord",
                                 "Variables Correlations" = "vcor",
                                 "Variables Cos2" = "vcos2",
                                 "Variables Contributions" = "vcontri",
                                 "Individuals" = "individuals",
                                 "Individuals Coordinates" = "icoord",
                                 "Individuals Cos2" = "icos2",
                                 "Individuals Contributions" = "icontri",
                                 "Call" = "call",
                                 "Call Center" = "cc",
                                 "Call Ecart Type" = "cet",
                                 "Call Row Weight" = "crw",
                                 "Call Column Weight" = "ccw"),
                               "icoord"
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'eigenvalues'",
                    helpText("Eigenvalues")
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'eigenvalues'",
                    helpText('Eigenvalues')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'variables'",
                    helpText('Results for the variables')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'vcoord'",
                    helpText('Coordinates for the variables')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'vcor'",
                    helpText('Correlations variables - dimensions')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'vcos2'",
                    helpText('cos2 for the variables')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'vcontri'",
                    helpText('contributions of the variables')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'individuals'",
                    helpText('Results for the individuals')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'icoord'",
                    helpText('coord. for the individuals')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'icos2'",
                    helpText('cos2 for the individuals')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'icontri'",
                    helpText('contributions of the individuals')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'call'",
                    helpText('summary statistics')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'cc'",
                    helpText('mean of the variables')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'cet'",
                    helpText('standard error of the variables')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'crw'",
                    helpText('weights for the individuals')
                  ),
                  conditionalPanel(
                    condition = "input.pcaresult == 'ccw'",
                    helpText('weights for the variables')
                  )
                )
              )
              
              
      ),
      tabItem("2dplot",
              
              fluidRow(
                box(
                  width = 8, status = "success",
                  title = "2D plot",
                  plotOutput("plotPCAtwoD",width="auto",height=600)
                ),
                box(
                  width = 4, status = "success",
                  checkboxInput("showtext","Show Individual Names"),
                  sliderInput("pointSize",
                              "Size of points",
                              min = 1,
                              max = 10,
                              value = 3
                  )
                  
                ),
                box(
                  width = 4, status = "success",
                  title = "Figure Option",
                  downloadButton('downloadDataPNG', 'Download PNG-file'),
                  downloadButton('downloadDataPDF', 'Download PDF-file'),
                  downloadButton('downloadDataEPS', 'Download EPS-file')
                )
              )
      ),
      tabItem("3dplot",
              
              fluidRow(
                box(
                  width = 8, status = "success",
                  title = "3D plot",
                  plotOutput("plotPCAthreeDtext")
                ),
                box(
                  width = 4, status = "success",
                  checkboxInput("showtext2","Show Individual Names"),
                  conditionalPanel(
                    condition = "input.showtext2 == true",
                    sliderInput("threeDtextSize",
                                "Text Size",
                                min = 0.1,
                                max = 2,
                                value = 1
                    )
                  ), 
                  selectInput("plottype", "PlotType:",
                              c("Point" = "p",
                                "Line" = "l",
                                "Vertical lines" = "h")),
                  sliderInput("angle",
                              "View angle",
                              min = 1,
                              max = 360,
                              value = 45
                  ),
                  downloadLink('downloadData', 'Download')
                  
                ),
                box(
                  width = 4, status = "success"
                  
                )
              )
      )
    )
  )
)