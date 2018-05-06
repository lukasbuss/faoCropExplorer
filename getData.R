require(ggplot2); require(dplyr);
table <- read.table("Production_Crops_E_Europe.csv", sep=",", na.strings = c("", "NA", "<NA>"), header = TRUE)

head(table)

selectCrops <- c("Barley","Maize","Wheat")

table_filtered <- filter(table,Area == "Germany" & Element == "Yield" & Item %in% selectCrops) %>% select(., c(Item,Y2014))
table_filtered$Y2014 <- table_filtered$Y2014/10000
                                                                                                          

ggplot(table_filtered, aes(Item, Y2014)) + geom_col(fill="darkblue") + xlab("Crop") + ylab("Yield in t/ha")

yearColumns <- NULL
for(i in 1961:2014){
  yearColumns[i-1960] <- paste("Y", i, sep="")
}

selectCols <- names(table)[(names(table) %in% c("Area", "Item", "Element", "Unit", yearColumns))]
acrossYears <- table[, selectCols]

acrossYearsYield <- dplyr::filter(acrossYears, Element == "Yield")%>% gather(., Year, Value,yearColumns[1]:yearColumns[length(yearColumns)], factor_key=TRUE)
acrossYearsYield[,"Year"] <- substr(acrossYearsYield$Year,2,5) %>% as.numeric

acrossYearsYieldfiltered <- filter(acrossYearsYield,Area == "Albania" & Item == "Maize")

ggplot(acrossYearsYieldfiltered, aes(Year, Value))+ geom_line()


############################

acrossYearsData <- dplyr::filter(table, Element == "Yield")%>% gather(., Year, Value,yearColumns[1]:yearColumns[length(yearColumns)], factor_key=TRUE)
acrossYearsData[,"Year"] <- substr(acrossYearsYield$Year,2,5) %>% as.numeric

acrossYearsData <- filter(acrossYearsYield, Area == "Albania" & Item == "Barley")

ifelse(input$selectParmA == "Yield", acrossYearsData$Value <- acrossYearsData$Value/10000)

ggplot(acrossYearsData, aes(Year, Value))+ geom_line()



##############
require(leaflet)
require(raster)
require(rgdal)
require(sp)

europeCountries <- shapefile("NUTS_RG_20M_2013_4326.shp")
europeCountries <- spTransform(europeCountries, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

CountryBorder <- europeCountries[europeCountries@data$LEVL_CODE == 0, ]
CountryBorder <- spTransform(CountryBorder, CRS("+init=epsg:4326"))

plot(CountryBorder)

leaflet(CountryBorder) %>% addTiles() %>% addPolygons(data = CountryBorder, stroke = F)
