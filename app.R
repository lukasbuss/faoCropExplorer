options(scipen = 30)

require(shiny); require(knitr); require(dplyr); require(ggplot2); require(rlang); require(tidyr); require(Cairo); require(plotly);


# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
   ## HEADER
  titlePanel(fluidRow(column(4,offset = 4, h1(em("FAO Crop Explorer"))
                     ))),
   ## SIDEBAR
  sidebarLayout(
    sidebarPanel( width = 2,
      fluidRow(a(href='http://fao.org',target="_blank", img(src='FAO_logo.png', align = "center"))),
      fluidRow(a(href="https://github.com/elalemano/FAO.CropExplorer",target="_blank", align = "left", target="_blank",
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
          fluidRow(h3(em("If you desire peace, cultivate justice, but at the same time cultivate the fields to produce more bread; otherwise there will be no peace"))
                   ),
          fluidRow(column(3, offset = 9, h5(em("Norman Borlaug"))))),
           
           tabPanel( "By Country",fluidRow(column(6, wellPanel(uiOutput("selectCountry1"), uiOutput("selectYear1"), uiOutput("selectCrop1"),actionButton("viewButton1", "View", icon = icon("eye")))),
                                          column(6, wellPanel(plotlyOutput(outputId = "plotBarAvgYield1")))),
                     br(),
                                 fluidRow(column(6, wellPanel(plotlyOutput(outputId = "plotBarArea1"))),
                                          (column(6, wellPanel(plotlyOutput(outputId = "plotBarProduction1")))))),
           tabPanel("By Crop",fluidRow(column(6,wellPanel(uiOutput("selectCrop2"),uiOutput("selectYear2"), uiOutput("selectCountries2"),actionButton("viewButton2", "View", icon = icon("eye")))),
                                     column(6,wellPanel(plotlyOutput(outputId = "plotBarAvgYield2")))),
                    br(),
                    fluidRow(column(6, wellPanel(plotlyOutput(outputId = "plotBarArea2"))),
                             column(6, wellPanel(plotlyOutput(outputId = "plotBarProduction2"))))),
           
           tabPanel("Across Years",fluidRow(column(6,wellPanel(uiOutput("selectCountries3A"),uiOutput("selectCrop3A"),uiOutput("selectParmA"),actionButton("viewButton3A", "View", icon = icon("eye")))),
                                           column(6, wellPanel(uiOutput("selectCountries3B"),uiOutput("selectCrop3B"), uiOutput("selectParmB"),actionButton("viewButton3B", "View", icon = icon("eye"))))),
                    br(),              
                    fluidRow(column(6, wellPanel(plotlyOutput(outputId = "acrossYearsPlotA"))),
                               column(6,wellPanel(plotlyOutput(outputId = "acrossYearsPlotB")))))
      ))))

#### Global Parms 

mainTheme <- theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=16,face="bold"),
                   axis.text=element_text(size=12), 
                   axis.title=element_text(size=14,face="bold"),
                   plot.background = element_rect("white", colour = "black"),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor.y = element_line(colour ="grey", linetype = "dashed"), 
                   panel.grid.minor.x = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   panel.border = element_rect(colour = "grey", fill=NA, size=0.5)) ###set ggplot theme
lineTheme <- theme(plot.title = element_text(hjust = 0.5,size=16,face="bold"),
                   axis.text=element_text(size=12), 
                   axis.title=element_text(size=14,face="bold"),
                   plot.background = element_rect("white", colour = "black"),
                   panel.background = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.y = element_line(colour ="grey", linetype = "dashed"),
                   panel.grid.major.x = element_line(colour ="grey", linetype = "dashed"),
                   axis.line = element_line(colour = "black"),
                   legend.title = element_blank(),
                   legend.text = element_text(face = "bold", size =12),
                   legend.justification=c(0,1), legend.position=c(0.01,0.995),
                   legend.background =  element_rect(fill = "transparent", colour = "transparent"),
                   panel.border = element_rect(colour = "grey", fill=NA, size=0.5)) 

table_Europe <- read.table("Production_Crops_E_Europe.csv", sep=",", na.strings = "", header = TRUE, stringsAsFactors = F)


# Define server logic required to draw a histogram
server <- function(input, output, session) { 
  options(shiny.usecairo=T)
  
  output$aboutApp <- renderUI({
    HTML(paste("<p style=text-align: justify;text-justify: inter-word;>","About the App:", 
               "This Shiny App was developed as a personal learning playground, feedback and suggestions are 
               therefore always welcome. This is not an official FAO site.</p>", sep="<br/>"))
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
 plotDataFront <- gather(acrossYears, Year, Value,yearColumns[1]:yearColumns[length(yearColumns)], factor_key=TRUE) %>% dplyr::filter(., Element == "Yield" & Item == "Maize")
 plotDataFront[,"Year"] <- substr(plotDataFront$Year,2,5) %>% as.numeric
 plotDataFront$Value <- plotDataFront$Value / 10000 
 plotData <- plotDataFront %>% group_by(Year)


 output$frontPlot <- renderPlotly({ 
   ggplot(plotData, aes(Year, Value)) + lineTheme + geom_smooth(na.rm = T, se = T, size = 1.2, method = "loess") + scale_x_continuous(breaks = seq(1970, 2010, 10), minor_breaks = seq(1961,2014,1))+ coord_cartesian(xlim = c(1961, 2014), expand = F)+ 
     xlab("Year (y)")+ ylab("Average Yield (t/ha)")+ggtitle("Average Corn Yield in Europe across Time")
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
#   table_filtered[table_filtered$Element=="Production",4] <- table_filtered[table_filtered$Element=="Production",4]/1000
   
   return(table_filtered)
 })
 
 
plotBarAvgYield1 <- eventReactive(input$viewButton1,{ 
    plotData <- getData1()
    plotData <- dplyr::filter(plotData,Element == "Yield")
    plot <-plot_ly(data = plotData, x =  plotData[["Item"]], y=plotData[[paste("Y",input$selectYear1,sep = "")]], type = "bar")
             #    geom_col(width = 0.5)+xlab("Crop") + ylab("Yield in t/ha") + ggtitle(paste("Average Yield in",input$selectCountry1,"in",input$selectYear1))+ 
              #   mainTheme +  scale_fill_brewer(palette = "Paired")
   return(plot)})

plotBarArea1 <- eventReactive(input$viewButton1,{ 
  plotData <- getData1()
  plot <-ggplot(dplyr::filter(plotData,Element == "Area harvested"),aes_string("Item",paste("Y",input$selectYear1,sep = ""),fill="Item"))+
    geom_col(width = 0.5)+xlab("Crop") + ylab("Area (ha)") + ggtitle(paste("Total area harvested in",input$selectCountry1,"in",input$selectYear1))+ 
    mainTheme + scale_fill_brewer(palette = "Paired")
  return(plot)})

plotBarProduction1 <- eventReactive(input$viewButton1,{ 
  plotData <- getData1()
  
  plot <-ggplot(dplyr::filter(plotData,Element == "Production"),aes_string("Item",paste("Y",input$selectYear1,sep = ""),fill="Item"))+
    geom_col(width = 0.5)+xlab("Crop") + ylab("Production (t)") + ggtitle(paste("Total production in",input$selectCountry1,"in",input$selectYear1))+ 
    mainTheme +scale_fill_brewer(palette = "Paired")
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
 # table_filtered[table_filtered$Element=="Production",4] <- table_filtered[table_filtered$Element=="Production",4]/1000
  
  return(table_filtered)
})

plotBarAvgYield2 <- eventReactive(input$viewButton2,{ 
  plotData <- getData2()
  plot <-ggplot(dplyr::filter(plotData,Element == "Yield"),aes_string("Area",paste("Y",input$selectYear2,sep = ""),fill="Area"))+
    geom_col(width = 0.5)+xlab("Country") + ylab("Yield in t/ha") + ggtitle(paste("Average ", input$selectCrop2, "Yield in ",input$selectYear2))+ 
    mainTheme +  scale_fill_brewer(palette = "Paired")
  return(plot)})

plotBarArea2 <- eventReactive(input$viewButton2,{ 
  plotData <- getData2()
  plot <-ggplot(dplyr::filter(plotData,Element == "Area harvested"),aes_string("Area",paste("Y",input$selectYear2,sep = ""),fill="Area"))+
    geom_col(width = 0.5)+xlab("Country") + ylab("Area (ha)") + ggtitle(paste("Total ", input$selectCrop2, "area harvested in ",input$selectYear2))+ 
    mainTheme + scale_fill_brewer(palette = "Paired")
  return(plot)})

plotBarProduction2 <- eventReactive(input$viewButton2,{ 
  plotData <- getData2()
  
  plot <-ggplot(dplyr::filter(plotData,Element == "Production"),aes_string("Area",paste("Y",input$selectYear2,sep = ""),fill="Area"))+
    geom_col(width = 0.5)+xlab("Country") + ylab("Production (t)") + ggtitle(paste("Total ",input$selectCrop2, "production in ",input$selectYear2))+ 
    mainTheme +scale_fill_brewer(palette = "Paired")
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

plotAcrossYearsA <- eventReactive(input$viewButton3A,{ plotData <- getDataAcrossA()
  
  if(input$selectParmA == "Yield"){
    labelY <- "Average Yield (t/ha)" 
  } else if (input$selectParmA == "Production"){
    labelY <- "Production (t)"
  } else if (input$selectParmA == "Area harvested"){
    labelY <- "Area harvested (ha)"
  }

  
  plot <-ggplot(plotData,aes(Year, Value, colour = factor(Area))) + geom_line(size = 1.2, na.rm = T) + geom_point() + scale_x_continuous(breaks = seq(1960, 2010, 10))+ 
  ylab(labelY) + xlab("Year (y)") + ggtitle(paste("Historic trend in", input$selectParmA))+ lineTheme +scale_fill_brewer(palette = "Paired")
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
  labelY <- "Production (t)"
} else if (input$selectParmB == "Area harvested"){
  labelY <- "Area harvested (ha)"
}
#browser()
plot <-ggplot(plotData,aes(Year, Value, colour = factor(Area))) + geom_line(size = 1.2, na.rm = T) + geom_point() + lineTheme + scale_x_continuous(breaks = seq(1960, 2010, 10))+
  ylab(labelY) + xlab("Year (y)") + ggtitle(paste("Historic trend in", input$selectParmB)) +scale_fill_brewer(palette = "Paired")
return(plot)})

output$acrossYearsPlotB <- renderPlotly(plotAcrossYearsB())

}

# Run the application 
shinyApp(ui = ui, server = server)

#rsconnect::setAccountInfo(name='buzzwinkel', token='D2B6A38F5FBE4C51B5BACEC30E2DA66F', secret='ggsXQnt15mHBUfGefD9F8vd701FBhjqdGAkzlDGn')
#rsconnect::deployApp("F:/GitHub/FAO.CropExplorer", appTitle = "FAO Crops Explorer")
