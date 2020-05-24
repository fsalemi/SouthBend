#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggmap)
library(rgdal)
library(leaflet)
library(ggplot2)
library(DT)

################################### Global Declarations #########################################

############# Districts ##############
districts_spatial <- readOGR(dsn = "Data/City_Council_Districts/City_Council_Districts.shp", 
                             stringsAsFactors = FALSE)
district_spatial_point <- SpatialPointsDataFrame(coords = coordinates(districts_spatial), 
                                                 data = districts_spatial@data,
                                                 proj4string = CRS(proj4string(districts_spatial)))
names(districts_spatial@data)[3] <- "District"
districts_spatial$popup <- paste("<b>", "District: ", districts_spatial$District,"</b><br>",
                               "Representative: ", districts_spatial$Council_Me, "<br>", 
                               "Email: ",districts_spatial$Email, sep ="")

############# Parks ##############
parks <- read.csv("Data/Parks_Locations_and_Features.csv",stringsAsFactors = FALSE)
parks_spatial <- SpatialPointsDataFrame(coords = parks[,c("Lon","Lat")], data = parks,
                                        proj4string = CRS(proj4string(districts_spatial)))
parks_ov <- over(parks_spatial,districts_spatial)
parks_spatial@data$District <- parks_ov$District
parks_table <- parks_spatial@data %>% select("Park Name" = Park_Name, 
                                             "Park Type" = Park_Type, Address, District)

############# Facilities ##############
facilities <- read.csv("Data/Public_Facilities.csv")
facilities_spatial <- SpatialPointsDataFrame(coords = facilities[,c("Lon","Lat")], data = facilities,
                                        proj4string = CRS(proj4string(districts_spatial)))
facilities_ov <- over(facilities_spatial,districts_spatial)
facilities_spatial@data$District <- facilities_ov$District
facilities_table <- facilities_spatial@data %>% select("Facility Name" = POPL_NAME, 
                                                       "Facility Type" = POPL_TYPE,
                                                       Address = POPL_ADDR1, Phone = POPL_PHONE, District)
facilities_table$Address <- str_split(facilities_table$Address, "\\n", simplify = T)[,1]

############# Enforcment Cases ##############
EnfCases <- filter(read.csv("Data/Code_Enforcement_Cases.csv"), Case_Year == 14)
EnfCases_spatial <- SpatialPointsDataFrame(coords = EnfCases[,c("Lon","Lat")], data = EnfCases,
                                        proj4string = CRS(proj4string(districts_spatial)))
EnfCases_ov <- over(EnfCases_spatial,districts_spatial)
EnfCases_spatial@data$District <- EnfCases_ov$District
EnfCases_table <- EnfCases_spatial@data %>%
  group_by(District, "Case Type" = Case_Type_Code_Description) %>%
  summarize(Cases = n())
EnfCases_ov <- EnfCases_table %>%
  group_by(District) %>%
  summarize(Total_Cases = sum(Cases))
names(EnfCases_table)[3] <- "Number of Cases"

############# Schools ##############
schools_boundaries <- readOGR(dsn = "Data/School_Boundaries/School_Boundaries.shp", 
                             stringsAsFactors = FALSE)
schools_spatial <- SpatialPointsDataFrame(coords = coordinates(schools_boundaries), 
                                          data = schools_boundaries@data,
                                          proj4string = CRS(proj4string(districts_spatial)))
schools_ov <- over(schools_spatial,districts_spatial)
schools_spatial@data$District <- schools_ov$District
#SchoolAddress <- apply(coordinates(schools_boundaries), 1, revgeocode )
#saveRDS(SchoolAddress, file = "Data/SchoolAddress.Rds")
SchoolAddress <- readRDS(file = "Data/SchoolAddress.Rds")
schools_spatial$SchoolAddress <- SchoolAddress
schools_table <- schools_spatial@data %>% select(School, "School Type" = SchoolType, Address = SchoolAddress, District)

############# Abandoned Properties ##############
abandoned_property <- readOGR(dsn = "Data/Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp", 
                              stringsAsFactors = FALSE)
abandoned_spatial <- SpatialPointsDataFrame(coords = coordinates(abandoned_property), 
                                            data = abandoned_property@data,
                                            proj4string = CRS(proj4string(districts_spatial)))
abandoned_ov <- over(abandoned_spatial,districts_spatial)
abandoned_spatial@data$District <- abandoned_ov$District
abandoned_table <- abandoned_spatial@data %>% select(Street_Nam, Suffix, Address_Nu, 
                                                     Structures, Status = Outcome_St, District)
abandoned_table <-  abandoned_table %>% unite("Property Address", Address_Nu, 
                                              Street_Nam, Suffix, sep = " ")

############# Census ##############
census_boundaries <- readOGR(dsn = "Data/2010_CensusData/2010_CensusData.shp", 
                              stringsAsFactors = FALSE)
census_spatial <- SpatialPointsDataFrame(coords = coordinates(census_boundaries), 
                                          data = census_boundaries@data,
                                          proj4string = CRS(proj4string(districts_spatial)))
census_ov <- over(census_spatial,districts_spatial)
census_spatial@data$District <- census_ov$District
census_table <- census_spatial@data %>%
  drop_na() %>%
  mutate_all(as.numeric) %>% 
  group_by(District = as.character(District)) %>%
  summarize("Population" = sum(SE_T001_00),
            Male = sum(SE_T003_01),
            Female = sum(SE_T003_02),
            "Area Sq Miles" = sum(SE_T02A_00),
            "Number of Housing Unit" = sum(SE_T068_00))
            

###################################   UI   #########################################
shinyApp(
  ui <- tagList(
    navbarPage(
      theme = shinytheme("flatly"),
      div(
        includeCSS("styles.css")
      ),
      tabPanel("About", 
               div(img(src = "City.jpg", width="40%")),
               uiOutput("About")),
      tabPanel("District Analysis",
        h3("South Bend Districts"),
        fluidRow(
          column(3,
            wellPanel(
              selectInput("district", label = h3("District:"), 
                choices = list("ALL" = 0, 
                               "1301" = 1, 
                               "1302" = 2, 
                               "1303" = 3, 
                               "1304" = 4, 
                               "1305" = 5, 
                               "1306" = 6)), 
              imageOutput("image", height = "100%"),
              hr(),
              htmlOutput("info")
            )
           ),
          column(9,
            leafletOutput(outputId = "map")
          )
        ),
        h3("Districts Statistical Reports"),
        fluidRow(
          column(3,
            wellPanel(
              selectInput("parameter", label = h3("Parameter:"),
                choices = list("Census" = 1,
                               "Schools" = 2, 
                               "Parks" = 3, 
                               "Enforcement Case Summaries" = 4, 
                               "Abandoned Properties" = 5, 
                               "Public Facilities" = 6))
            )
          ),
          column(3,
                 plotOutput(outputId = "plot")
          ),
          column(6,
                 dataTableOutput(outputId = "table")
          )
        )
      )#,
      # tabPanel("Tab 2", "This panel is intentionally left blank"),
      # tabPanel("Tab 3", "This panel is intentionally left blank"),
      # tabPanel("Tab 4", "This panel is intentionally left blank")
    )
  ),


###################################   Server   #########################################
  server <- function(input, output) {
    output$About <- renderUI(includeHTML("www/About.htm"))
    output$info <- renderText({
     if(as.integer(input$district) != 0){
      paste(#"<b>", "District: ", "</b>", districts_spatial$District[as.integer(input$district)],"<br>",
                                       "<b>", "Representative: ", "</b>", districts_spatial$Council_Me[as.integer(input$district)], "<br>",
                                       "<b>", "Email: ", "</b>", districts_spatial$Email[as.integer(input$district)])
     }
    })

    output$image <- renderImage({
       return(list(
        src = paste0("www/", input$district, ".jpg"), height= "15%", width= "40%"#, align="buttom"
      ))
    }, deleteFile = FALSE)

    dataset <- eventReactive(input$parameter,{
      i <- as.integer(input$parameter) 
      if (i == 1) return(list(census_table, "Total Population", census_table, census_table$Population, "identity", district_spatial_point))
      if (i == 2) return(list(schools_ov, "Number of Schools", schools_table, NULL, "count", schools_spatial))
      if (i == 3) return(list(parks_ov, "Number of Parks", parks_table, NULL, "count", parks_spatial))
      if (i == 4) return(list(EnfCases_ov, "Number of Enforcement Cases", EnfCases_table, EnfCases_ov$Total_Cases, "identity", EnfCases_spatial))
      if (i == 5) return(list(abandoned_ov, "Number of Abandoned Properties", abandoned_table, NULL, "count", abandoned_spatial))
      if (i == 6) return(list(facilities_ov, "Number of Facilities", facilities_table, NULL, "count", facilities_spatial))
    })
    
    data <- eventReactive(input$district,{
      i <- as.integer(input$district) 
      if (i == 0) {
        District <- districts_spatial$District
        districtColor <- colorFactor("YlOrRd", District)(District)
        return(list(districts_spatial, districtColor))
      }
      districtColor <- "red"
      return(list(districts_spatial[i,], districtColor))
    })
  
    output$map <-  renderLeaflet({
      myMap <- leaflet()%>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addTiles(group = "Basic") %>%
        addPolygons(data = data()[[1]], popup = ~popup, color = "#444444", weight = 1, smoothFactor = 0.5,
                     opacity = 1.0, fillOpacity = 0.5,
                     fillColor = ~data()[[2]], 
                     highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                     group = "Districts")  
        if(as.integer(input$parameter) != 1)
          myMap <- myMap %>% addCircleMarkers(data = dataset()[[6]][data()[[1]],], stroke = 0, color = "navy",
                                fillOpacity = .6, radius = 6, group = "Markers") #%>%
        myMap %>% 
          addLayersControl(
            baseGroups = c("Basic", "Satellite"),
            overlayGroups= c("Districts", "Markers"),
            options = layersControlOptions(collapsed = FALSE),
            position = "bottomright") %>%
 #         hideGroup("Markers") %>% 
          setView(-86.25, 41.68, zoom = 11.4)
    })
    
    output$plot <- renderPlot({
     if(dataset()[[5]] == "identity"){  
      ggplot(drop_na(dataset()[[1]], District), aes(District, dataset()[[4]])) +
        geom_bar(fill = "navy", stat = dataset()[[5]]) +
        geom_text(aes(label=dataset()[[4]]), vjust=-.5) +
        labs(y=dataset()[[2]], 
             x="South Bend Districts", 
             title=paste0(dataset()[[2]], " per Districts")) +
        theme(plot.title = element_text(hjust = 0.5, size = 17))
     } 
     else{   
      ggplot(drop_na(dataset()[[1]], District), aes(District)) +
        geom_bar(fill = "navy", stat = dataset()[[5]]) +
        geom_text(stat='count', aes(label=..count..), vjust=-.5) +
        labs(y=dataset()[[2]], 
             x="South Bend Districts", 
             title=paste0(dataset()[[2]], " per Districts")) +
        theme(plot.title = element_text(hjust = 0.5, size = 17))
     }  
    })
    
    output$table <- renderDataTable({
      datatable(semi_join(dataset()[[3]], data()[[1]]@data), options = list(pageLength = 7))
    })
  }
)  
  
###################################   Application   #########################################
shinyApp(ui = ui, server = server)

