#
#

library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggthemes)


# types of outlook surveys 
OutlookSurveys <-
    c(
        "Service Sector" = "servicesector",
        "Retail" = "retail",
        "Manufacturing" = "manufacturing"
    )
DisplayOutlookSurveys <-
    c(
        "servicesector" = "Service Sector",
        "retail" = "Retail",
        "manufacturing" =  "Manufacturing"
    )

# survey fields we care about
SurveyFields <-
    c(
        "Capital expenditures" = "Cexp",
        "Employment" = "Emp",
        "Company outlook" = "Colk",
        "Hours worked" = "Avgwk",
        "Wages and benefits" = "Wgs",
        "Outlook uncertainty" = "Uncr"
    )
DisplaySurveyFields <-
    c(
        "Cexp" = "Capital expenditures",
        "Emp" = "Employment",
        "Colk" = "Company outlook",
        "Avgwk" = "Hours worked",
        "Wgs" = "Wages and benefits",
        "Uncr" =" Outlook uncertainty (Starts Jan 2018)"
    )


# Load the data sets
ManufacturingSurveyExcel <- read_excel("index_sa.xls",range="A1:AH173")
RetailSurveyExcel <- read_excel("TROS_Index_SA.xls",range="A1:AB173" )
ServiceSectorSurveyExcel <- read_excel("TSSOS_index_SA.xls",range="A1:V173")

ManufacturingSurveyData = data.frame(ManufacturingSurveyExcel)
ManufacturingSurveyData <- transform(ManufacturingSurveyData, Date = sprintf("01-%s", Date))
ManufacturingSurveyData <- mutate(ManufacturingSurveyData, Date=as.Date(Date, format="%d-%b-%y"))

RetailSurveyData = data.frame(RetailSurveyExcel)
RetailSurveyData <- transform(RetailSurveyData, Date = sprintf("01-%s", Date))
RetailSurveyData <- mutate(RetailSurveyData, Date=as.Date(Date, format="%d-%b-%y"))

ServiceSectorSurveyData = data.frame(ServiceSectorSurveyExcel)
ServiceSectorSurveyData <- transform(ServiceSectorSurveyData, Date = sprintf("01-%s", Date))
ServiceSectorSurveyData <- mutate(ServiceSectorSurveyData, Date=as.Date(Date, format="%d-%b-%y"))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Texas Employment Survey Indices Comparison"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("dataset1",
                        "First Outlook Survey:",
                        OutlookSurveys,
                        selected="servicesector"),
            
            selectInput("dataset2",
                        "Second Outlook Survey:",
                        OutlookSurveys,
                        selected="retail"),
            
            selectInput("field1",
                        "Index to Compare:",
                        SurveyFields,
                        selected = "Avgwk"),
            sliderInput("yearRange",
                        "Date Range:",
                        min=2007, max=2021,
                        value=c(2017, 2018))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        
        if (input$dataset1 == "servicesector") {
            chartdata1 <- ServiceSectorSurveyData[, c("Date",input$field1)]
        }
        else if (input$dataset1 == "retail") {
            chartdata1 <- RetailSurveyData[, c("Date",input$field1)]
        }
        else if (input$dataset1 == "manufacturing") {
            chartdata1 <- ManufacturingSurveyData[, c("Date",input$field1)]
        }
        
        chartdata1$Survey <- rep(DisplayOutlookSurveys[match(input$dataset1,OutlookSurveys)],nrow(chartdata1))   
        names(chartdata1) <- c("Date", "Value1","Survey")
        
        if (input$dataset2 == "servicesector") {
            chartdata2 <- ServiceSectorSurveyData[, c("Date",input$field1)]
        }
        else if (input$dataset2 == "retail") {
            chartdata2 <- RetailSurveyData[, c("Date",input$field1)]
        }
        else if (input$dataset2 == "manufacturing") {
            chartdata2 <- ManufacturingSurveyData[, c("Date",input$field1)]
        }
        
        chartdata2$Survey <- rep(DisplayOutlookSurveys[match(input$dataset2,OutlookSurveys)],nrow(chartdata2))   
        names(chartdata2) <- c("Date", "Value1","Survey")
        
        chartdata <- rbind(chartdata1,chartdata2)
        chartdata <- filter(chartdata, format(as.Date(Date),"%Y") >= input$yearRange[1] & format(as.Date(Date),"%Y") <= input$yearRange[2])
        
        ggplot(data=chartdata, aes(x = Date, y = Value1, group=Survey)) +
            geom_line(size=1, aes(linetype=Survey, color = Survey)) +
            geom_point(size=3, aes(linetype=Survey, color = Survey)) +
            labs(
                title = paste("Comparing",DisplayOutlookSurveys[match(input$dataset1,OutlookSurveys)],"to",DisplayOutlookSurveys[match(input$dataset2,OutlookSurveys)]),
                caption = "Source: Federal Reserve Bank of Texas",
                y = DisplaySurveyFields[match(input$field1,SurveyFields)]
            ) +
            theme_economist() +
            theme(
                plot.title = (element_text(size=20, face = "bold"))
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
