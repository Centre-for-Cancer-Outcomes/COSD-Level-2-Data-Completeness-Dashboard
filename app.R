## load in packages
list.of.packages <- c("tidyverse","shiny","shinydashboard","lubridate","DT","scales","qicharts","shinyjs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE, repos='http://cran.rstudio.com/')

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)
library(scales)
library(qicharts)
library(shinyjs)

options(shiny.maxRequestSize=1000*1024^2)
jsCode <- 'shinyjs.winprint = function(){
window.print();
}'
## load in data set
setwd("C://Users/DEGAN001/Desktop/COSD_Level2")
cosd <- read.csv("COSD Level 2 data.csv",header = TRUE, sep = ",")
cosd <- filter(cosd, cosd$Cancer.Alliance %in% c("North Central And North East London", "England") & is.na(cosd$Trust) == FALSE)
cosd$Month.of.Diagnosis <- as.Date(cosd$Month.of.Diagnosis)
cosd$value[is.na(cosd$value)] <- 0

##dashboard ui
ui <- dashboardPage(
  dashboardHeader(title = "COSD Level 2 -Beta"),
  dashboardSidebar(
    selectInput("Cancer.Alliance","Cancer Alliance", choices = unique(factor(cosd$Cancer.Alliance)), selected = "North Central And North East London"),
    selectInput("Trust", label = "Trust Name",choices = unique(factor(cosd$Trust)), selected = "Ryoal Free London"),
    selectInput("cancertype" , label = "Cancer Type" , choices = unique(factor(cosd$cancertype)), selected = "Breast"),
    selectInput("Metric.Name", label = "Metric", choices = unique(factor(cosd$Metric.Name)), selected = "L2.1a: with a Cancer Diagnosis (diagnosis may not have been at this Trust)", width = 600),
     sidebarMenu(
      menuItem("Patient Numbers",tabName = "PatientNumbers",icon = icon("users")),
      menuItem("Data Completeness",tabName = "DataCompleteness", icon = icon("dashboard")),
      menuItem("Selected Data",tabName = "SelectedData",icon = icon("table"))
    ),
    helpText("Devloped by The North Central and East London Centre for Cancer Outcomes")
  ),
  dashboardBody(
    tabItems(
      ## Patients numbers tab content
      tabItem(tabName = "PatientNumbers",
              fluidRow(
              box(plotOutput("completenessplot",height = 500),status = "success",title = "Number of Patient Records",solidHeader = TRUE),
              box(DT::dataTableOutput("ptxnumtable"),status = "info",title = "Number of Records Data Table",solidHeader = TRUE)),
              fluidRow(
              box(plotOutput("changesinpatientnumbers",height = 500),status = "success",title = "% Change in Patient Records Time Series",solidHeader = TRUE),
              box(plotOutput("patientmakeup",height = 500),status = "success",title = "Patient Records by Cancer Type",solidHeader = TRUE))
              ),
      tabItem(tabName = "DataCompleteness",
              fluidRow(
                 box(plotOutput("trustdatacompleteness",height = 500),status = "success",title = "Data Completeness", solidHeader = TRUE),
                 box(DT::dataTableOutput("trustdatacompletnesstable"),status = "info",title = "Data Completeness table", solidHeader = TRUE) 
                 ),
              fluidRow(
                box(plotOutput("completenessboxplot",height = 500),status = "success",title = "Data Completeness boxplot", solidHeader = TRUE),
                box(plotOutput("cpmletespc",height = 500),status = "success",title = "Completeness SPC",solidHeader = TRUE)
              )
              ),
      tabItem(tabName = "SelectedData",
              fluidRow(
                DT::dataTableOutput("completedatatable")
              ),
              fluidRow(
                  box(selectInput("cancertype2" , label = "Cancer Type" , choices = unique(factor(cosd$cancertype)), selected = "Breast"),status = "warning",title = "Cancer Type 1", solidHeader = TRUE),
                  box(selectInput("cancertype3" , label = "Cancer Type" , choices = unique(factor(cosd$cancertype)), selected = "Gynaecology"),status = "warning",title = "Cancer Type 2", solidHeader = TRUE),
                  box( selectInput("cancertype4" , label = "Cancer Type" , choices = unique(factor(cosd$cancertype)), selected = "Total"),status = "warning",title = "Cancer Type 3", solidHeader = TRUE),
                  box( selectInput("cancertype5" , label = "Cancer Type" , choices = unique(factor(cosd$cancertype)), selected = "Melanoma"),status = "warning",title = "Cancer Type 4", solidHeader = TRUE)
                  ),
              fluidRow(
               plotOutput("questionablegraph",height = 300) 
              )
              )
              )
    )
  )


## dashboard server

server <-function(input, output,session) { 
####################################################################
##                 Patient Number Page                            ##
####################################################################
  
## Data Completeness patient numbers timne series  
output$completenessplot <-  renderPlot({
                    g1 <-   filter(cosd, cosd$Trust == input$Trust & cosd$Metric.Name == input$Metric.Name, cancertype == input$cancertype)   %>% 
                   select(Month.of.Diagnosis,cancertype,value) %>% 
                   group_by(Month.of.Diagnosis, cancertype) %>% 
                   summarise(count = sum(value)) 
    
                   ggplot(g1, aes(Month.of.Diagnosis,count)) + geom_line() +theme_classic() + expand_limits( y = 0)
  })


##Data table for completness numbers 

output$ptxnumtable <- DT::renderDataTable(
  
  
  filter(cosd, cosd$Trust == input$Trust & cosd$Metric.Name == input$Metric.Name)   %>% 
    select(Month.of.Diagnosis,cancertype,value) %>% 
    filter(cancertype == input$cancertype) %>% 
    group_by(Month.of.Diagnosis, cancertype) %>% 
    summarise(count = sum(value)) %>%
    filter(cancertype == input$cancertype) %>%  
    datatable(rownames = FALSE, colnames = c("Date","Cancer Type","Count"), extensions = c('Buttons', 'Scroller'),
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                buttons = c('copy','csv', 'excel', 'pdf'),
                deferRender = TRUE,
                scrollY = 400,
                scroller = TRUE
                
              )
              
    )
)

## changes in the number of patients over time

output$changesinpatientnumbers <- renderPlot({
  g4p1 <- cosd %>% 
    filter(cosd$Trust == input$Trust & cosd$Metric.Name == input$Metric.Name & cancertype == input$cancertype) %>% 
    select(Month.of.Diagnosis,Trust,value) %>% 
    group_by(Month.of.Diagnosis, Trust) %>% 
    summarise(count = sum(value)) %>% 
    arrange((Month.of.Diagnosis)) 
  
  g4p1$previousmonth <- lag(g4p1$count)
  
  g4p1$dif <- g4p1$count - g4p1$previousmonth
  
  g4p1$Percentagedif <- g4p1$dif/g4p1$previousmonth*100
  
  g4p2 <- cosd %>% 
    filter(cosd$Trust == input$Cancer.Alliance &  cosd$Metric.Name == input$Metric.Name & cancertype == input$cancertype) %>% 
    select(Month.of.Diagnosis,Trust,value) %>% 
    group_by(Month.of.Diagnosis, Trust) %>% 
    summarise(count = sum(value)) %>% 
    arrange((Month.of.Diagnosis)) 
  
  g4p2$previousmonth <- lag(g4p2$count)
  
  g4p2$dif <- g4p2$count - g4p2$previousmonth
  
  g4p2$Percentagedif <- g4p2$dif/g4p2$previousmonth*100
  
  
  g4p3 <- cosd %>% 
    filter(cosd$Trust == "London" & cosd$Metric.Name == input$Metric.Name & cancertype == input$cancertype) %>% 
    select(Month.of.Diagnosis,Trust,value) %>% 
    group_by(Month.of.Diagnosis, Trust) %>% 
    summarise(count = sum(value)) %>% 
    arrange((Month.of.Diagnosis)) 
  
  g4p3$previousmonth <- lag(g4p3$count)
  
  g4p3$dif <- g4p3$count - g4p3$previousmonth
  
  g4p3$Percentagedif <- g4p3$dif/g4p3$previousmonth*100
  
  g4p4 <- cosd %>% 
    filter(cosd$Trust == "England" & cosd$Metric.Name == input$Metric.Name & cancertype == input$cancertype) %>% 
    select(Month.of.Diagnosis,Trust,value) %>% 
    group_by(Month.of.Diagnosis, Trust) %>% 
    summarise(count = sum(value)) %>% 
    arrange((Month.of.Diagnosis)) 
  
  g4p4$previousmonth <- lag(g4p4$count)
  
  g4p4$dif <- g4p4$count - g4p4$previousmonth
  
  g4p4$Percentagedif <- g4p4$dif/g4p4$previousmonth*100
  
  
  g4 <- rbind(g4p1,g4p2,g4p3,g4p4)
  
  g4<- g4[complete.cases(g4$dif),]
  
  ggplot(g4, aes(Month.of.Diagnosis,Percentagedif, colour = Trust)) + geom_line(stat = "identity")+ theme_classic() +
    ylab("Percentage Change compared to the previous month") + xlab("Month of Diagnosis") + theme(legend.position="bottom")
  

  
})

output$patientmakeup <- renderPlot({
 g8  <-  cosd %>% 
            filter(cosd$Trust == "England" & cosd$Metric.Name == input$Metric.Name & cosd$cancertype != "Total "& 
                  cosd$cancertype != "Invasive Cancers" & cosd$cancertype != "Non.Invasive Cancers" & cosd$cancertype != "Total") %>% 
    select(Month.of.Diagnosis,cancertype,value) %>% 
    group_by(Month.of.Diagnosis,cancertype) %>% 
    summarise(count = sum(value)) 
  
  ggplot(g8,aes(Month.of.Diagnosis,count))+ geom_bar(stat = "Identity", aes(fill = cancertype))+ theme_classic()+ 
    theme(legend.position="bottom")+ scale_y_continuous(label = comma )  
  
})
  
####################################################################
##                 Data completeness Page                         ##
####################################################################  
  
output$trustdatacompleteness <- renderPlot({
  g2 <- cosd %>%
    filter(cosd$Trust %in% c(input$Trust,"London",input$Cancer.Alliance , "England") & cancertype == input$cancertype & cosd$Metric.Name == input$Metric.Name) %>%
    select(Trust,Month.of.Diagnosis,percentage.completeness) %>% 
    group_by(Trust,Month.of.Diagnosis) %>% 
    summarise(completeness = round(sum(as.numeric(sub("%","",percentage.completeness)))*100,1))
  
  ggplot(g2, aes(Month.of.Diagnosis, completeness)) + geom_line(aes(colour = Trust)) + theme_classic() + expand_limits( y = 0) + 
    ylab("Completeness (%)") +   theme(legend.position="bottom")
  
})     

output$trustdatacompletnesstable <- renderDataTable(
  cosd %>%
    filter(cosd$Trust == input$Trust & cancertype == input$cancertype & cosd$Metric.Name == input$Metric.Name) %>%
    select(Trust,Month.of.Diagnosis,cancertype,percentage.completeness) %>% 
    group_by(Trust,Month.of.Diagnosis,cancertype) %>% 
    summarise(completeness = round(sum(as.numeric(sub("%","",percentage.completeness)))*100,1)) %>% 
    arrange(desc(Month.of.Diagnosis)) %>% 
    datatable(rownames = FALSE,colnames = c("Date","Cancer Type","Completeness (%)"),extensions = c('Buttons', 'Scroller'),
              options = list(
                pageLength = 15,
                dom = 'Bfrtip',
                buttons = c('copy','csv', 'excel', 'pdf'),
                deferRender = TRUE,
                scrollY = 400,
                scroller = TRUE
              )
              
              
              
    ))


output$completenessboxplot <- renderPlot({
  
 g7 <-   cosd %>%
         filter(cosd$Trust %in% c(input$Trust,"London",input$Cancer.Alliance , "England") & cancertype == input$cancertype & cosd$Metric.Name == input$Metric.Name) %>%
         select(Trust,Month.of.Diagnosis,cancertype,percentage.completeness) %>% 
         group_by(Trust,Month.of.Diagnosis,cancertype) %>% 
         summarise(completeness = round(sum(as.numeric(sub("%","",percentage.completeness)))*100,1))
 
 ggplot(g7,aes(Trust,completeness,fill = "Trust"))+geom_boxplot() +theme_classic() + expand_limits( y = 0) +
   theme(legend.position="none") + ylab("Completeness (%)")
})

output$cpmletespc <- renderPlot({
  
spcchartdata <- cosd %>% 
                filter(cosd$Trust == input$Trust &  cosd$Metric.Name == input$Metric.Name &
                         cancertype == input$cancertype) %>% 
                select(Month.of.Diagnosis,Trust,cancertype,percentage.completeness) %>% 
                group_by(Month.of.Diagnosis,Trust,cancertype) %>% 
                summarise(completeness = round(sum(as.numeric(sub("%","",percentage.completeness)))*100,1))

spcchartdata <-spcchartdata[order(spcchartdata$Month.of.Diagnosis),]
spcchartdata <-head(spcchartdata,nrow(spcchartdata)-5)


qic(y = completeness,
    x = Month.of.Diagnosis,
    data = spcchartdata,
    chart = "i",
    ylab = "completeness (%)",
    xlab = "Date",
    decimals = 1,
    main = "")
  
})
####################################################################
##                 complete data table                            ##
####################################################################  

output$completedatatable <- renderDataTable({
      cosd %>% 
      filter(cosd$Trust == input$Trust & cancertype == input$cancertype) %>% 
      select(Month.of.Diagnosis, Cancer.Alliance, Trust, cancertype, Metric.Name, value,percentage.completeness) %>% 
      group_by(Month.of.Diagnosis, Cancer.Alliance, Trust, cancertype, Metric.Name) %>% 
      summarise(count = sum(value), completeness = round(sum(as.numeric(sub("%","",percentage.completeness)))*100,1)) %>% 
      datatable(rownames = FALSE, colnames = c("Date","Cancer.Alliance","Trust" , "Cancer Type","Metric Name",
                                                             "Count", "Completeness (%)"),extensions = c('Buttons','Scroller'),
              options = list(
              pageLength = 20,
              dom = 'Bfrtip',
              buttons = c('copy','csv', 'excel', 'pdf'),
              deferRender = TRUE,
              scrollY = 400,
              scroller = TRUE
            ))
})


output$questionablegraph <- renderPlot({
  g10 <- cosd %>% 
         filter(cosd$Trust == input$Trust & cosd$Metric.Name == input$Metric.Name &
                cancertype %in% c(input$cancertype2,input$cancertype3,input$cancertype4,input$cancertype5)) %>% 
    select(Month.of.Diagnosis,Trust,cancertype,percentage.completeness) %>% 
    group_by(Month.of.Diagnosis,Trust,cancertype) %>% 
    summarise(completeness = round(sum(as.numeric(sub("%","",percentage.completeness)))*100,1)) %>% 
    ungroup()
  
  g10 <- g10[order(g10$Month.of.Diagnosis),]
  g10<- g10 %>% 
    filter(g10$Month.of.Diagnosis < nth(unique(g10$Month.of.Diagnosis),length(unique(g10$Month.of.Diagnosis))-3))
  
  
  
  ggplot(g10, aes(Month.of.Diagnosis,cancertype))+ geom_tile(aes(fill= completeness )) +  theme_classic() +
    scale_fill_gradient2( low = "red", high = "deepskyblue", mid = "magenta")
  
})


session$onSessionEnded(function() {
  stopApp()
})
  }
## run app 

shinyApp(ui = ui, server = server)


