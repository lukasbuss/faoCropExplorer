options(scipen = 30)

require(shiny); require(shinydashboard);require(rvest); require(knitr); require(dplyr); require(ggplot2); require(rlang); require(tidyr); require(Cairo);


# Define UI for application that draws a histogram
ui <- dashboardPage(
  ## HEADER
  dashboardHeader(title="FAO Crop Explorer",titleWidth = 250,
                  tags$li(a(href='http://fao.org',img(src='FAO_logo.png',title = "Company Home", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),class="dropdown"),
                  dropdownMenu(type = "notifications", headerText = "Additional information", icon = shiny::icon("info", lib = "font-awesome"),
                               notificationItem("Contact",icon = shiny::icon("envelope", lib = "font-awesome"), href = "mailto:l_busswinkel@hotmail.de"),
                               notificationItem("Source code",icon = shiny::icon("code", lib = "font-awesome"), href = "https://github.com/elalemano/FAO.CropExplorer"))
  ),
  ## SIDEBAR
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem("Home", tabName = "frontPage", icon = icon("home")),
                     menuItem("By Country and Year", tabName = "byCountry", icon = icon("flag")),
                     menuItem("By Crop and Year", tabName = "byCrop", icon = icon("pagelines",lib = "font-awesome")),
                     menuItem("Across Years", tabName = "acrossYears", icon = icon("line-chart",lib = "font-awesome")))
  ),
  
  ## BODY
  dashboardBody(fluidRow(
    tags$head(tags$style(HTML('.main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }'))),
         tabItems(
           tabItem(tabName = "frontPage", box(plotOutput(outputId = "frontPlot", width = "1400px", height = "750px"))),
           tabItem(tabName = "byCountry", box(menuItemOutput("selectCountry1"),menuItemOutput("selectYear1"),menuItemOutput("selectCrop1"),actionButton("viewButton1", "View", icon = icon("eye")),height = 442),
                   box(plotOutput(outputId = "plotBarAvgYield1"),collapsible = T),
                   box(plotOutput(outputId = "plotBarArea1"),collapsible = T),
                   box(plotOutput(outputId = "plotBarProduction1"),collapsible = T)
                   
           ),
           tabItem(tabName = "byCrop",box(menuItemOutput("selectCrop2"),menuItemOutput("selectYear2"), menuItemOutput("selectCountries2"),actionButton("viewButton2", "View", icon = icon("eye"))),
                   box(plotOutput(outputId = "plotBarAvgYield2"),collapsible = T),
                   box(plotOutput(outputId = "plotBarArea2"),collapsible = T),
                   box(plotOutput(outputId = "plotBarProduction2"),collapsible = T)),
           
           tabItem(tabName = "acrossYears", box(menuItemOutput("selectCountries3A"),menuItemOutput("selectCrop3A"), menuItemOutput("selectParmA"),actionButton("viewButton3A", "View", icon = icon("eye"))),
                   box(menuItemOutput("selectCountries3B"),menuItemOutput("selectCrop3B"), menuItemOutput("selectParmB"),actionButton("viewButton3B", "View", icon = icon("eye"))),
                   box(plotOutput(outputId = "acrossYearsPlotA")),
                   box(plotOutput(outputId = "acrossYearsPlotB")))
    )
    ))
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(shiny.usecairo=T)
  mainTheme <- theme(legend.position="none", plot.title = element_text(hjust = 0.5,size=16,face="bold"),
                     axis.text=element_text(size=12), 
                     axis.title=element_text(size=14,face="bold"),
                     panel.background = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor.y = element_line(colour ="grey", linetype = "dashed"), 
                     panel.grid.minor.x = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     panel.border = element_rect(colour = "grey", fill=NA, size=0.5)) ###set ggplot theme
  lineTheme <- theme(plot.title = element_text(hjust = 0.5,size=16,face="bold"),
                     axis.text=element_text(size=12), 
                     axis.title=element_text(size=14,face="bold"),
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
  
  table_Europe <- read.table("Production_Crops_E_Europe.csv", sep=",", na.strings = "", header = TRUE)
  
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
  
  
  
  output$frontPlot <- renderPlot({ ggplot(plotData, aes(Year, Value)) + lineTheme + geom_smooth(na.rm = T, se = T, size = 1.2, method = "loess") + scale_x_continuous(breaks = seq(1970, 2010, 10), minor_breaks = seq(1961,2014,1))+ coord_cartesian(xlim = c(1961, 2014), expand = F)+ 
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
    
    return(table_filtered)
  })
  
  
  plotBarAvgYield1 <- eventReactive(input$viewButton1,{ 
    plotData <- getData1()
    
    plot <-ggplot(dplyr::filter(plotData,Element == "Yield"),aes_string("Item",paste("Y",input$selectYear1,sep = ""),fill="Item"))+
      geom_col(width = 0.5)+xlab("Crop") + ylab("Yield in t/ha") + ggtitle(paste("Average Yield in",input$selectCountry1,"in",input$selectYear1))+ 
      mainTheme +  scale_fill_brewer(palette = "Paired")
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
  
  
  output$plotBarAvgYield1 <- renderPlot(plotBarAvgYield1())
  output$plotBarArea1 <- renderPlot(plotBarArea1())
  output$plotBarProduction1 <- renderPlot(plotBarProduction1())
  
  
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
  
  
  output$plotBarAvgYield2 <- renderPlot(plotBarAvgYield2())
  output$plotBarArea2 <- renderPlot(plotBarArea2())
  output$plotBarProduction2 <- renderPlot(plotBarProduction2())
  
  ###############################
  ### Third tab, across years ###
  ###############################
  output$selectCountries3A <-  renderUI(selectInput("selectCountries3A", "Select Countries",selected =  "Albania", choices = countries, multiple = T))
  output$selectCrop3A      <-  renderUI(selectInput("selectCrop3A", "Select Crop" , choices = crops))
  output$selectParmA       <-  renderUI(selectInput("selectParmA", "Select Parameter" , choices = c("Yield", "Production", "Area harvested")))
  
  output$selectCountries3B <-  renderUI(selectInput("selectCountries3B", "Select Countries",selected =  "All", choices = countries, multiple = T))
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
    labelY <- "Yield (t/ha)" 
  } else if (input$selectParmA == "Production"){
    labelY <- "Production (t)"
  } else if (input$selectParmA == "Area harvested"){
    labelY <- "Area harvested (ha)"
  }
  
  
  plot <-ggplot(plotData,aes(Year, Value, colour = factor(Area))) + geom_line(size = 1.2, na.rm = T) + geom_point() + scale_x_continuous(breaks = seq(1960, 2010, 10))+ 
    ylab(labelY) + xlab("Year (y)") + ggtitle(paste("Historic trend in", input$selectParmA))+ lineTheme +scale_fill_brewer(palette = "Paired")
  return(plot)})
  
  output$acrossYearsPlotA <- renderPlot(plotAcrossYearsA())
  
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
    labelY <- "Yield (t/ha)" 
  } else if (input$selectParmB == "Production"){
    labelY <- "Production (t)"
  } else if (input$selectParmB == "Area harvested"){
    labelY <- "Area harvested (ha)"
  }
  #browser()
  plot <-ggplot(plotData,aes(Year, Value, colour = factor(Area))) + geom_line(size = 1.2, na.rm = T) + geom_point() + lineTheme + scale_x_continuous(breaks = seq(1960, 2010, 10))+
    ylab(labelY) + xlab("Year (y)") + ggtitle(paste("Historic trend in", input$selectParmB)) +scale_fill_brewer(palette = "Paired")
  return(plot)})
  
  output$acrossYearsPlotB <- renderPlot(plotAcrossYearsB())
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#rsconnect::setAccountInfo(name='buzzwinkel', token='D2B6A38F5FBE4C51B5BACEC30E2DA66F', secret='ggsXQnt15mHBUfGefD9F8vd701FBhjqdGAkzlDGn')
#rsconnect::deployApp("F:/GitHub/FAO.CropExplorer", appTitle = "FAO Crops Explorer")
