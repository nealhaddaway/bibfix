library(shiny)
library(shinydashboard)
library(flexdashboard)
library(ggplot2)
library(dplyr)
library(stringr)
library(synthesisr)
library(shinybusy)
library(openalexR)
library(openalex)
library(highcharter)

source('scan_file.R')
source('plot_health.R')
source('repair_refs.R')
source('reconstruct_abstract.R')
source('search_openAlex.R')
source('build_ris.R')

# Set background colour
tags$head(tags$style(
    HTML('
                     #sidebar {
                        background-color: #ffbdbd;
                    }
            
                    body, label, input, button, select { 
                      font-family: "Arial";
                    }')))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("bibfix"),
    
    fluidRow(
        column(10,
               h3('Welcome to bibfix!'),
               br(),
               'bibfix helps you to repair your bibliographic data files!'),
        column(1,
               tags$img(height = 150, src = "https://raw.githubusercontent.com/nealhaddaway/bibfix/master/inst/extdata/bibfix%20hex.png")),
        column(8, align="center", offset = 2,
               br(),
               hr(),
               'Upload an RIS file below to check its health and repair missing fields.',
               br(),
               hr(),
               br())
        ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
            
            # Sidebar panel for inputs ----
            sidebarPanel(
                
                # Input: Select a file ----
                fileInput("file", 
                          "RIS file upload", 
                          multiple = FALSE, 
                          accept = c('.ris', '.txt')),
                uiOutput('fixbutton'),
                br(),
                uiOutput('downloadbutton'),
                add_busy_spinner(spin = "fading-circle", color = "#e43235", margins = c(70, 20), position='bottom-left')
            ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput('report'),
            br(),
            textOutput('repair_report'),
            br(),
            highchartOutput('plt1'),
            br(),
            plotOutput('health')
        )
    ),
    fluidRow(
        column(12,
               hr(),
               'bibfix searches ', tags$a(href='https://openalex.org/', 'OpenAlex'),' using the OA API and ', tags$a(href='https://github.com/massimoaria/openalexR', 'openalexR'), ' by Massimo Aria.',
               br(),
               br(),
               'Please cite as: Haddaway NR, Grainger MJ (2022). bibfix: An R package and Shiny app for repairing and enriching bibliographic data.', 
               tags$a(href='https://github.com/nealhaddaway/bibfix', 'https://github.com/nealhaddaway/bibfix'),
               br()))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    rv <- reactiveValues()
    
    rv$repair_report <- ''
    
    #on file upload
    observeEvent(input$file,{
        validate(need(input$file != "", "Select your bibliographic file to upload..."))
        
        #do nothing if file is not uploaded
        if (is.null(input$file)) {
            return(NULL)
        } else {
        
        #upload file
            rv$upload <- read_refs(input$file$datapath)
            rv$n_records <- nrow(rv$upload)
            rv$health <- scan_file(rv$upload)
            rv$start_health <- rv$health
            rv$repair_report <- ''
            
        #create report
            rv$report <- paste0(
                'Your uploaded file contained a total of ',
                rv$n_records,
                ' bibliographic records.'
            )
        }
        
    })
    
    #repair records
    observeEvent(input$repair,{
        rv$repaired <- repair_refs(rv$upload) 
        rv$upload <- rv$repaired
        rv$n_records <- nrow(rv$upload)
        rv$health <- scan_file(rv$upload)
        rv$new_health <- rv$health
        #prepare RIS for download
        rv$download_ris <- build_ris(select(rv$repaired, -c(intID)))
        
        #render report
        rv$repair_report <- 
        if(rv$new_health$n_complete - rv$start_health$n_complete == 0 && rv$new_health$n_abstract - rv$start_health$n_abstract == 0){
            "Sorry, we couldn't repair your references :("
        } else {
            paste0('Your record now contains ',
                   rv$new_health$n_complete - rv$start_health$n_complete,
                   ' additional complete records, and ',
                   rv$new_health$n_abstract - rv$start_health$n_abstract,
                   ' new abstracts')
            }
        
    })
    
    #render report
    output$repair_report <- renderText({
        rv$repair_report
    })
    
    #render report
    output$report <- renderText({
        rv$report
    })
    
    #render gauge plot
    output$plt1 <- renderHighchart({
        if (is.null(input$file)) {
            return(NULL)
        } else {
            #gauge(rv$health$n_complete, 
            #      min = 0, 
            #      max = rv$n_records, 
            #      label = paste("healthy"),
            #      gaugeSectors(colors = c("#e43235")))
            perfect <- round(rv$health$n_complete/rv$n_records*100, digits=0)
            good <- round(rv$health$n_abstract/rv$n_records*100, digits=0)
            ok <- round(rv$health$n_doi/rv$n_records*100, digits=0)
            
            highchart(width = 400, height = 400) %>% 
                hc_chart(type = "solidgauge", marginTop = 50) %>% 
                hc_tooltip(borderWidth = 0,backgroundColor = 'none',shadow = FALSE,style = list(fontSize = '16px'),
                           pointFormat = '<span style="font-size:1.5em; color: #000000; font-weight: bold">{point.y}%<t>{series.name}</span>',
                           positioner = JS("function (labelWidth, labelHeight) {return {x: 200 - labelWidth / 2,y: 200};}")) %>% 
                hc_pane(startAngle = 0,endAngle = 360,
                        background = list(
                            list(outerRadius = '112%',innerRadius = '88%',backgroundColor = JS("Highcharts.Color('#f0b4b5').setOpacity(0.1).get()"),borderWidth =  0),
                            list(outerRadius = '87%',innerRadius = '63%',backgroundColor = JS("Highcharts.Color('#f0b4b5').setOpacity(0.1).get()"),borderWidth = 0),
                            list(outerRadius = '62%',innerRadius =  '38%',backgroundColor = JS("Highcharts.Color('#f0b4b5').setOpacity(0.1).get()"),borderWidth = 0))) %>% 
                hc_yAxis(min = 0,max = 100,lineWidth = 0,tickPositions = list()) %>% 
                hc_plotOptions(solidgauge = list(borderWidth = '34px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>% 
                hc_add_series(name = " with DOI",borderColor = '#fae6e8',data = list(list(color = '#e43235',radius = "100%",innerRadius = "100%",y = ok))) %>% 
                hc_add_series(name = " with abstract",borderColor = '#f0b4b5',data = list(list(color = '#e43235',radius = "75%",innerRadius = "75%",y = good))) %>% 
                hc_add_series(name = " complete",borderColor = '#e43235',data = list(list(color = '#e43235',radius = "50%",innerRadius = "50%",y = perfect)))
            
        }
    })
    
    #render health bar plot
    output$health <- renderPlot({
        if (is.null(input$file)) {
            return(NULL)
        } else {
            plot_health(rv$health)
        }
    })
    
    #render fix button UI
    output$fixbutton <- renderUI({
        if (is.null(input$file)) return(NULL)
        tagList(
            br(),
            'Click the button below to repair your RIS file (this may take a minute)',
            br(),
            br(),
            actionButton('repair', 'Repair RIS')
        )
    })
    
    #render download button UI
    output$downloadbutton <- renderUI({
        if (is.null(input$repair)) return(NULL)
        tagList(
            downloadButton('download', 'Download RIS')
        )
    })
    
    #download repaired RIS file
    output$download <- downloadHandler(
        filename = function(){
            paste("REPAIRED-", Sys.Date(), ".ris", sep = "")
        },
        content = function(file) {
            write.table(rv$download_ris, file, row.names = FALSE, col.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
