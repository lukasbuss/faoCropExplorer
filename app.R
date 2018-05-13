options(scipen = 30)

require(shiny); require(knitr); require(dplyr); require(rlang); require(plotly); require(tidyr);


# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
   ## HEADER
  titlePanel(fluidRow(column(4,offset = 4, h1(em("FAO Crop Explorer"))
                     ))),
   ## SIDEBAR
  sidebarLayout(
    sidebarPanel( width = 2,
      fluidRow(a(href='http://fao.org',target="_blank", img(src='FAO_logo.png', width = 200, height = 200))),
      fluidRow(a(href="https://github.com/elalemano/faoCropExplorer",target="_blank", align = "left", target="_blank",
              shiny::icon("code", lib = "font-awesome"), "Source Code")),
      fluidRow(a(href="mailto:l_busswinkel@hotmail.de", target="_blank", align = "left", shiny::icon("envelope", lib = "font-awesome"),
           "Contact Author")),
      fluidRow(a(href="https://www.linkedin.com/in/lukas-busswinkel/", target="_blank", align = "left", shiny::icon("linkedin", lib = "font-awesome"),
                 "LinkedIn")),
      br(),
      fluidRow(htmlOutput("aboutApp")),
      br(),
      fluidRow(htmlOutput("aboutMe"))  ,  
      br(),
      fluidRow(htmlOutput("disclaimer"))),

   ## BODY
   mainPanel(
     tabsetPanel(type = "tabs",
           tabPanel("Home", 
          fluidRow(column(12, wellPanel(plotlyOutput(outputId = "frontPlot")))), 
          fluidRow(h3(tags$blockquote("If you desire peace, cultivate justice, but at the same time cultivate the fields to produce more bread; otherwise there will be no peace"))                   ),
          fluidRow(column(3, offset = 9, h5(em("Norman Borlaug"))))),
           
           tabPanel( "By Country",fluidRow(column(6, wellPanel(uiOutput("selectCountry1"), uiOutput("selectYear1"), uiOutput("selectCrop1"),actionButton("viewButton1", "View", icon = icon("eye")))),
                                          column(6, wellPanel(plotlyOutput(outputId = "plotBarAvgYield1")))),
                    
                                 fluidRow(column(6, wellPanel(plotlyOutput(outputId = "plotBarArea1"))),
                                          (column(6, wellPanel(plotlyOutput(outputId = "plotBarProduction1")))))),
           tabPanel("By Crop",fluidRow(column(6,wellPanel(uiOutput("selectCrop2"),uiOutput("selectYear2"), uiOutput("selectCountries2"),actionButton("viewButton2", "View", icon = icon("eye")))),
                                     column(6,wellPanel(plotlyOutput(outputId = "plotBarAvgYield2")))),
              
                    fluidRow(column(6, wellPanel(plotlyOutput(outputId = "plotBarArea2"))),
                             column(6, wellPanel(plotlyOutput(outputId = "plotBarProduction2"))))),
           
           tabPanel("Across Years",wellPanel(style = "padding-top: 2px;padding-bottom: 2px;", 
                                             fluidRow(style = "height: 90px;", column(4,uiOutput("selectCountries3A")),column(4,uiOutput("selectCrop3A")),column(4,uiOutput("selectParmA"))),
                                             fluidRow(column(2, actionButton("viewButton3A", "View", icon = icon("eye"))))),
                    fluidRow(column(12, wellPanel(plotlyOutput(outputId = "acrossYearsPlotA")))),               
                    wellPanel(style = "padding-top: 2px;padding-bottom: 2px;",
                              fluidRow(style = "height: 90px;" , column(4, uiOutput("selectCountries3B")),column(4,uiOutput("selectCrop3B")),column(4,uiOutput("selectParmB"))),
                              fluidRow(column(2, actionButton("viewButton3B", "View", icon = icon("eye"))))),

                    fluidRow(column(12,wellPanel(plotlyOutput(outputId = "acrossYearsPlotB"))))
      )))))

#### Global Parms 

table_Europe <- read.table("Production_Crops_E_Europe.csv", sep=",", na.strings = "", header = TRUE, stringsAsFactors = F)

countries <- table_Europe$Area %>% unique
crops     <- table_Europe$Item %>% unique
years     <- colnames(table_Europe)[grepl("Y",colnames(table_Europe))] %>% substr(., 2,5) %>% unique

## For across years graphs
yearColumns <- NULL
for(i in 1961:2014){
  yearColumns[i-1960] <- paste("Y", i, sep="")
}

selectCols <- names(table_Europe)[(names(table_Europe) %in% c("Area", "Item", "Element", "Unit", yearColumns))]
acrossYears <- table_Europe[selectCols]

##########################################
#### Front page graph ####################
##########################################
plotDataFront <- gather(acrossYears, Year, Value,yearColumns[1]:yearColumns[length(yearColumns)], factor_key=TRUE) %>% dplyr::filter(., Item == "Maize")
plotDataFront[,"Year"] <- substr(plotDataFront$Year,2,5) %>% as.numeric

hectarsPerYear <- plotDataFront %>% dplyr::filter(., Element == "Area harvested") %>% group_by(Year) %>% summarise(TotalArea = sum(Value, na.rm = T))
productionPerYear <- plotDataFront %>% dplyr::filter(., Element == "Production") %>% group_by(Year) %>% summarise(TotalProduction = sum(Value, na.rm = T))

plotData <- merge(hectarsPerYear,productionPerYear) 
plotData$averageYield <- plotData$TotalProduction/plotData$TotalArea
plotData$linearModel <- lm(averageYield ~ Year, data = plotData) %>% fitted

yieldThen <- list(
  xref = 'paper',
  yref = 'y',
  x = 0.05,
  y = plotData$averageYield[1]+0.01,
  xanchor = 'right',
  yanchor = 'middle',
  text = paste(round(plotData$averageYield[1],2), "(t/ha)"),
  autosize = TRUE,
  font = list(family = 'Arial',
              size = 16,
              color = 'rgba(67,67,67,1)'),
  showarrow = FALSE)

yieldNow <- list(
  xref = 'paper',
  yref = 'y',
  x = 1.00,
  y = plotData$averageYield[54] - 0.3,
  xanchor = 'right',
  yanchor = 'middle',
  text = paste(round(plotData$averageYield[54],2), "(t/ha)"),
  autosize = TRUE,
  font = list(family = 'Arial',
              size = 16,
              color = 'rgba(67,67,67,1)'),
  showarrow = FALSE)


####################################################
# Define server logic ##############################
####################################################
server <- function(input, output, session) { 
  
  output$aboutApp <- renderUI({
    HTML(paste("<p style=text-align: justify;text-justify: inter-word;>","About the App:", 
               "This Shiny App was developed as a personal learning playground, feedback and suggestions are 
               therefore always welcome</p>", sep="<br/>"))
  })
  
  output$aboutMe <- renderUI({
    HTML(paste("<p style=text-align: justify;text-justify: inter-word;>","About the Author:", 
               "I work as a Data Analyst in the Agriculture sector. Part of the reason I wanted to design this
               App to illustrate the success story that modern agriculture represents</p>", sep="<br/>"))
  })

  output$disclaimer<- renderUI({
    HTML(paste("<p style=text-align: justify;text-justify: inter-word;>","Disclaimer:", 
               "This App is not an official FAO Site. The author is in no way affiliated with the FAO</p>", sep="<br/>"))
  })
  
 output$frontPlot <- renderPlotly({ 
    frontPlot <- plot_ly(data = plotData, x = as.character(plotData[["Year"]]), y = ~averageYield, type = "scatter",  name = "Average Yield") %>%
      add_trace(data = plotData, x = as.character(plotData[["Year"]]), y = ~linearModel, mode = "lines", name = "Fitted Yield") %>% 
      layout(showlegend = FALSE, yaxis = list(title = "Average Yield (t/ha)"), xaxis = list(tickangle = 45), title ="Average Corn Yield in Europe") %>%
      layout(annotations = yieldThen) %>% 
      layout(annotations = yieldNow) 
return(frontPlot)
})
 
 ##########################################
 ## First tab, by country and year ########
 ##########################################
 output$selectCountry1 <-  renderUI(selectInput("selectCountry1", "Select Country", choices = countries))
 output$selectYear1    <-  renderUI(selectInput("selectYear1", "Select Year", selected = "2014", choices = years))
 output$selectCrop1    <-  renderUI(selectInput("selectCrop1", "Select Crops" ,multiple = T, selected = c("Maize", "Wheat", "Barley"), choices = crops))
 
 getData1 <- reactive({
   req(input$selectCountry1)
   req(input$selectYear1)
   req(input$selectCrop1)

   table_filtered <- filter(table_Europe,Area == input$selectCountry1 & 
                            Item %in% input$selectCrop1) %>% 
                            select(., c(Item,Element,Unit,!!!rlang::syms(paste("Y",input$selectYear1,sep = ""))))
   
   table_filtered[table_filtered$Element=="Yield",4] <- table_filtered[table_filtered$Element=="Yield",4]/10000

   return(table_filtered)
 })
 
 
plotBarAvgYield1 <- eventReactive(input$viewButton1,{ 
    plotData <- getData1()
    plotData <- dplyr::filter(plotData,Element == "Yield")
    plotTitle <- paste("Average Yield in",input$selectCountry1,"in",input$selectYear1)
    plot <-plot_ly(data = plotData, x =  plotData[["Item"]], 
                   y=plotData[[paste("Y",input$selectYear1,sep = "")]], color= plotData[["Item"]], type = "bar") %>%
      layout(yaxis = list(title= "Average Yield (t/ha)"), title = plotTitle)
   return(plot)})

plotBarArea1 <- eventReactive(input$viewButton1,{ 
  plotData <- getData1()
  
  plotData <- dplyr::filter(plotData,Element == "Area harvested")
  plotTitle <- paste("Total area harvested in",input$selectCountry1,"in",input$selectYear1)
  plot <-plot_ly(data = plotData, x =  plotData[["Item"]], 
                 y=plotData[[paste("Y",input$selectYear1,sep = "")]], color= plotData[["Item"]], type = "bar") %>%
    layout(yaxis = list(title= "Total Area (ha)"), title = plotTitle)
  
  return(plot)})

plotBarProduction1 <- eventReactive(input$viewButton1,{ 
  plotData <- getData1()
  
  plotData <- dplyr::filter(plotData,Element == "Production")
  plotTitle <- paste("Total production in",input$selectCountry1,"in",input$selectYear1)
  plot <-plot_ly(data = plotData, x =  plotData[["Item"]],
                 y=plotData[[paste("Y",input$selectYear1,sep = "")]], color= plotData[["Item"]], type = "bar") %>%
    layout(yaxis = list(title= "Total Production (t)"), title = plotTitle)
  
  return(plot)})


output$plotBarAvgYield1 <- renderPlotly(plotBarAvgYield1())
output$plotBarArea1 <- renderPlotly(plotBarArea1())
output$plotBarProduction1 <- renderPlotly(plotBarProduction1())


########################################################
## Second tab, by crop and year, several countries #####
########################################################
output$selectCountries2 <-  renderUI(selectInput("selectCountries2", "Select Countries", choices = countries, multiple = T))
output$selectYear2    <-  renderUI(selectInput("selectYear2", "Select Year", selected = "2014", choices = years))
output$selectCrop2    <-  renderUI(selectInput("selectCrop2", "Select Crop" , selected = "Wheat", choices = crops))

getData2 <- reactive({
  req(input$selectCountries2)
  req(input$selectYear2)
  req(input$selectCrop2)

  table_filtered <- filter(table_Europe,Area %in% input$selectCountries2 & 
                             Item == input$selectCrop2) %>% 
    select(., c(Item,Element,Unit,!!!rlang::syms(paste("Y",input$selectYear2,sep = "")), Area))
  
  table_filtered[table_filtered$Element=="Yield",4] <- table_filtered[table_filtered$Element=="Yield",4]/10000

  return(table_filtered)
})

plotBarAvgYield2 <- eventReactive(input$viewButton2,{ 
  plotData <- getData2()
  
  plotData <- dplyr::filter(plotData,Element == "Yield")
  plotTitle <- paste("Average", input$selectCrop2, "Yield in ",input$selectYear2)

  plot <-plot_ly(data = plotData, x =  plotData[["Area"]], 
                 y=plotData[[paste("Y",input$selectYear2,sep = "")]], color= plotData[["Area"]], type = "bar") %>%
    layout(yaxis = list(title= "Average Yield (t/ha)"), title = plotTitle)
  return(plot)})

plotBarArea2 <- eventReactive(input$viewButton2,{ 
  plotData <- getData2()
  
  plotData <- dplyr::filter(plotData,Element == "Area harvested")
  plotTitle <- paste("Total", input$selectCrop2, "area harvested in",input$selectYear2)
  
  plot <-plot_ly(data = plotData, x =  plotData[["Area"]], 
                 y=plotData[[paste("Y",input$selectYear2,sep = "")]], color= plotData[["Area"]], type = "bar") %>%
    layout(yaxis = list(title= "Total Area (ha)"), title = plotTitle)

  return(plot)})

plotBarProduction2 <- eventReactive(input$viewButton2,{ 
  plotData <- getData2()
  
  plotData <- dplyr::filter(plotData,Element == "Production")
  plotTitle <- paste("Total ",input$selectCrop2, "Production in",input$selectYear2)
  
  plot <-plot_ly(data = plotData, x =  plotData[["Area"]], 
                 y=plotData[[paste("Y",input$selectYear2,sep = "")]], color= plotData[["Area"]], type = "bar") %>%
    layout(yaxis = list(title= "Total Production (t)"), title = plotTitle)
  return(plot)})


output$plotBarAvgYield2 <- renderPlotly(plotBarAvgYield2())
output$plotBarArea2 <- renderPlotly(plotBarArea2())
output$plotBarProduction2 <- renderPlotly(plotBarProduction2())

###############################
### Third tab, across years ###
###############################
output$selectCountries3A <-  renderUI(selectInput("selectCountries3A", "Select Countries",selected =  "Albania", choices = countries, multiple = T))
output$selectCrop3A      <-  renderUI(selectInput("selectCrop3A", "Select Crop" , choices = crops))
output$selectParmA       <-  renderUI(selectInput("selectParmA", "Select Parameter" , choices = c("Yield", "Production", "Area harvested")))

output$selectCountries3B <-  renderUI(selectInput("selectCountries3B", "Select Countries",selected =  "Austria", choices = countries, multiple = T))
output$selectCrop3B      <-  renderUI(selectInput("selectCrop3B", "Select Crop" , selected = "Wheat", choices = crops))
output$selectParmB       <-  renderUI(selectInput("selectParmB", "Select Crop" , selected = "Yield", choices = c("Yield", "Production", "Area harvested")))


#####Line graphs #####
getDataAcrossA <- reactive({
  req(input$selectParmA)
  req(input$selectCountries3A)
  req(input$selectCrop3A)

  acrossYearsData <- dplyr::filter(acrossYears, Element == input$selectParmA)%>% gather(., Year, Value,yearColumns[1]:yearColumns[length(yearColumns)], factor_key=TRUE)
  acrossYearsData[,"Year"] <- substr(acrossYearsData$Year,2,5) %>% as.numeric

  acrossYearsData <- filter(acrossYearsData, Area %in% input$selectCountries3A & Item == input$selectCrop3A)

  ifelse(input$selectParmA == "Yield", acrossYearsData$Value <- acrossYearsData$Value/10000, NA)

return(acrossYearsData)
})

plotAcrossYearsA <- eventReactive(input$viewButton3A,{
  plotData <- getDataAcrossA()
  
  if(input$selectParmA == "Yield"){
    labelY <- "Average Yield (t/ha)" 
  } else if (input$selectParmA == "Production"){
    labelY <- "Total Production (t)"
  } else if (input$selectParmA == "Area harvested"){
    labelY <- "Total Area harvested (ha)"
  }
 
  labelPlot <- paste("Historic trend in", input$selectParmA)
  plot <- plot_ly(data = plotData, x = ~Year, y = ~Value, type = "scatter", mode = "lines", color = ~Area) %>%
          layout(yaxis = list(title = labelY), title = labelPlot)
  return(plot)})

output$acrossYearsPlotA <- renderPlotly(plotAcrossYearsA())

### Right visualization ###
getDataAcrossB <- reactive({
  req(input$selectParmB)
  req(input$selectCountries3B)
  req(input$selectCrop3B)
  
  acrossYearsData <- dplyr::filter(acrossYears, Element == input$selectParmB)%>% gather(., Year, Value,yearColumns[1]:yearColumns[length(yearColumns)], factor_key=TRUE)
  acrossYearsData[,"Year"] <- substr(acrossYearsData$Year,2,5) %>% as.numeric
  
  acrossYearsData <- filter(acrossYearsData, Area %in% input$selectCountries3B & Item == input$selectCrop3B)
  
  ifelse(input$selectParmB == "Yield", acrossYearsData$Value <- acrossYearsData$Value/10000, NA)
  
  return(acrossYearsData)
})

plotAcrossYearsB <- eventReactive(input$viewButton3B,{ plotData <- getDataAcrossB()

if(input$selectParmB == "Yield"){
  labelY <- "Average Yield (t/ha)" 
} else if (input$selectParmB == "Production"){
  labelY <- "Total Production (t)"
} else if (input$selectParmB == "Area harvested"){
  labelY <- "Total Area harvested (ha)"
}

labelPlot <- paste("Historic trend in", input$selectParmB)
plot <- plot_ly(data = plotData, x = ~Year, y = ~Value, type = "scatter", mode = "lines", color = ~Area) %>%
  layout(yaxis = list(title = labelY), title = labelPlot)
return(plot)})

output$acrossYearsPlotB <- renderPlotly(plotAcrossYearsB())

}

# Run the application 
shinyApp(ui = ui, server = server)

#rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPPS_NAME"), token=Sys.getenv("SHINYAPPS_TOKEN"), secret=Sys.getenv("SHINYAPPS_SECRET"))
#rsconnect::deployApp("F:/GitHub/faoCropExplorer", appTitle = "FAO Crops Explorer")
