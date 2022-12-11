library(BiocManager)
options(repos = BiocManager::repositories())
library(leaflet)
library(leaflegend)
library(sf)
library(osmdata)
library(leaflet.extras)
library(leaflet.extras2)
library(htmlwidgets)
library(htmltools)
library(tidyverse)
library(shiny)
library(shinyTime)
library(DT)
library(plotly)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(shinyWidgets)
library(scales)
library(fontawesome)
library(shinyjs)
library(RColorBrewer)
library(showtext)
library(rhdf5)
library(raster)
library(bioRad)


ui <- dashboardPage(
  
  dashboardHeader(
    
    titleWidth='100%',
    title=span(
      column(2),
      column(10, class="title-box", 
             tags$h1(class="primary-title", 
                     style="margin-top:10px; font-family: 'Hammersmith One', sans-serif; color:white;", "BIRD MIGRATION IN ESTONIA"),
             tags$h3(class="primary-subtitle", 
                     style="margin-top:10px;; font-family: 'Rubik', sans-serif; color:white;", 'SHOWCASING THE SPECTACLE OF BIRD MIGRATION')),
      column(2),
      tags$img(src="birds_cropped.png", width='70%', align='right'))),
  footer = dashboardFooter(left=a(href="https://github.com/idarahu", 
                                  target="_blank", "@IdaRahu"),
                           right = "2022"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HOME", tabName="home", icon=icon("home")),
      menuItem("TRAFFIC RATES", tabName="graphs", icon=icon("line-chart")),
      menuItem("RADAR DATA", tabName="maps", icon=icon("map-marker")),
      tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Hammersmith+One|Rubik|Roboto');
                      .sidebar-menu li a { font-size: 14px; font-family: 'Rubik', sans-serif; color:white;}"))
    )
  ),
  
  dashboardBody(
    tags$style(type="text/css", "
/*    Move everything below the header */
    .content-wrapper {
        margin-top: 50px;
    }
    .content {
        padding-top: 60px;
    }
/*    Format the title/subtitle text */
    .title-box {
        position: absolute;
        text-align: center;
        top: 50%;
        left: 50%;
        transform:translate(-50%, -50%);
    }
    @media (max-width: 590px) {
        .title-box {
            position: absolute;
            text-align: center;
            top: 10%;
            left: 10%;
            transform:translate(-5%, -5%);
        }
    }
    @media (max-width: 767px) {
        .primary-title {
            font-size: 1.1em;
        }
        .primary-subtitle {
            font-size: 1em;
        }
    }
/*    Make the image taller */
    .main-header .logo {
        height: 125px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 175px;
    }"
    ),
    
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: black;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: black;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #343434;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: black;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #63056e;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #343434;
                                color: white;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #63056e;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #63056e;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: black;
                                }
                                
                                /* box */
                                .skin-blue .box {
                                background-color: #343434;
                                }
                                
                                /* slider */
                                .skin-blue .slider {
                                background-color: #403a60;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .nav-tabs {
                                background-color: #343434; font-size: 16px;
                                }
                                
                                '))),
    
    tabItems(
      tabItem(tabName="home",
              column(12, 
                     box(width=12, 
                         h3("INTRODUCTION", style="color:white; font-size:18px;"),
                         p("Migratory birds, who make up more than 90% of the Estonian bird population, spend around 25-33% of their annual cycle in transit between wintering and breeding areas. Survival challenges on these journeys play a significant role in adult specimen yearly mortal rate. Therefore, it is critical to study the habitat requirements of birds during their migration. Identifying important migration stopover areas is fundamental to develop comprehensive strategies to conserve their populations. For that aim, weather radars may be used.", 
                           align='justify', style="color:white; font-size:12px;"), 
                         p(),
                         p("Weather radars are special-purpose remote-sensing devices. Based on the measured radiation data, which has been reflected from the precipitation, it is possible to estimate the rainfall intensities on the ground, distinguish precipitation type and detect severe weather. However, this meteorological infrastructure also records aerial movements of many taxa, including birds, as noise and may thus be a useful remote-sensing tool for ornithological studies",
                           align='justify', style="color:white; font-size:12px;"),
                         p(),
                         p("This webpage illustrates how Estonian weather radars’ measurements could be employed to analyse birds' migration. For that, a toy dataset (measurements of Sürgavere radar from the 2nd of October at 6:00 UTC to the 3rd of October at 6:45 UTC in 2022) is used.",
                           align = 'justify',  style = "color:white; font-size:12px;"),
                         p(),
                         p("In the following figure, the overall analysis pipeline is described. As one can see, the radar cross-section (RCS) value of 200 square centimetres is chosen for the analysis because, during the studied time interval, the main species that travelled were geese.",
                           align = 'justify',  style = "color:white; font-size:12px;"),
                         tags$img(src="analysis_pipeline.png", width='75%', height='75%'),
                         tags$head(
                           tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Rubik:wght@300&display=swap');
                           p {font-family: 'Rubik', sans-serif; font-size: 10px;}"))),
                         setBackgroundImage(
                           src="bird.png",
                           shinydashboard=T)))),
      tabItem(tabName='graphs',
              fluidPage(
              column(4,
                     box(width=12,
                         h3("ESTONIAN WEATHER RADARS", style="color:white; font-size:18px;"),
                         p("Dual polarisation C-band (4-8 GHz, IEEE) Doppler weather radar Vaisala WRM200",
                           align='justify', style="color:white; font-size:12px;"), 
                         p(),
                         p("Harku (59.39767°N, 24.60210°E); Sürgavere (58.48231°N, 25.51866°E)", 
                           align='justify', style="color:white; font-size:12px;")),
                     box(width=12,
                         radioButtons("radar1", 
                                      label=tags$span(style="color: white; font-family: 'Rubik', sans-serif; font-size: 14px;", "SELECT RADAR"),
                                      choiceNames=list(
                                        tags$span(style="color: white; font-family: 'Rubik', sans-serif; font-size: 12px;", "Harku"), 
                                        tags$span(style="color: white; font-family: 'Rubik', sans-serif; font-size: 12px;", "Sürgavere")), 
                                      choiceValues=list("HAR",
                                                        "SUR"),
                                      selected="SUR"),
                         p(),
                         dateRangeInput("dateRange1",
                                        label=tags$span(style="color: white; font-family: 'Rubik', sans-serif; font-size: 14px;", "SELECT TIME INTERVAL"),
                                        start="2022-10-02", end="2022-10-03"
                         ),
                         timeInput("startTime1", 
                                   label=tags$span(style="color: white; font-family: 'Rubik', sans-serif; font-size: 12px;", "start (hh:mm, UTC)"), 
                                   value=strptime("06:00:00", "%T", tz='UTC'),
                                   minute.steps = 15),
                         timeInput("endTime1", 
                                   label=tags$span(style="color: white; font-family: 'Rubik', sans-serif; font-size: 12px;", "end (hh:mm, UTC)"), 
                                   value=strptime("06:45:00", "%T", tz='UTC'),
                                   minute.steps = 15))),
              column(8, 
                     box(width=12,
                         plotOutput("plot1", height = "300px")),
                     box(width=12,
                         plotOutput("plot2", height = "300px"))))),
      
      tabItem(tabName='maps',
              fluidPage(
                fluidRow(
                  column(6,
                         box(width=12,
                             leafletOutput("mapDBZH", width="100%", height="500px"))),
                  column(6, 
                        box(width=12,
                            leafletOutput("mapVID", width="100%", height="500px")))),
                p(),
                fluidRow(
                  box(width=12,
                      setSliderColor("PaleVioletRed", sliderId = 1),
                      chooseSliderSkin("Simple"),
                      sliderInput("measurementTimestamp", 
                                  label=tags$span(style="color: white; font-family: 'Rubik', sans-serif; font-size: 14px;", "TIMESTAMP OF THE MEASUREMENT"),
                                  min=as.POSIXct("2022-10-02 06:00:00", timezone='UTC'), 
                                  max=as.POSIXct("2022-10-03 06:45:00", timezone='UTC'),
                                  value=as.POSIXct("2022-10-02 18:30:00", timezone='UTC'),
                                  animate = animationOptions(interval=10000, loop=T))))))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  source('addLegend_decreasing.R')
  
  vpts_SUR <- readRDS('vpts_SUR.RData')
  
  # Colors for DBZH data
  pal_rev <- (brewer.pal(10, "Spectral"))
  colors <- colorBin(pal_rev, domain=NULL, bins=10, na.color="transparent")
  pal <- colorNumeric(colors, seq(-40, 40, by=5),
                      na.color="transparent", reverse=T)
  
  # Colors for VID data
  colorsVID <- colorBin("plasma", domain=NULL, bins=5000, na.color="transparent")
  palVID <- colorNumeric(colorsVID, seq(0, 5000, by=50), na.color="transparent")
  
  # Radar icon for the map
  anglerIcon2 <- makeIcon(
    iconUrl="https://cdn-icons-png.flaticon.com/512/1271/1271837.png",
    iconWidth=40, iconHeight=40)
  
  output$plot1 = renderPlot({
    
    start_date = input$dateRange1[1]
    start_timestamp = str_split(input$startTime1, ' ')[[1]][2]
    
    if (is.na(start_timestamp)) {
      start_timestamp = '00:00:00'
    }
    start_time = paste(start_date, start_timestamp)

    end_date = input$dateRange1[2]
    end_timestamp = str_split(input$endTime1, ' ')[[1]][2]
    if (is.na(end_timestamp)) {
      end_timestamp = '00:00:00'
    } 
    end_time = paste(end_date, end_timestamp)
    
    if (input$radar1 == "SUR") {
      plot(integrate_profile(vpts_SUR), xlim=c(as.POSIXct(start_time, tz='UTC'), as.POSIXct(end_time, tz='UTC')), 
           quantity="mtr", col="#001c3d") 
    }  
    if (input$radar1 == "HAR") {
        # Toy dataset does not contain Harku radar's data.
    }
  })
  
  output$plot2 = renderPlot({
    
    start_date = input$dateRange1[1]
    start_timestamp = str_split(input$startTime1, ' ')[[1]][2]
    if (is.na(start_timestamp)) {
      start_timestamp = '00:00:00'
    }
    start_time = paste(start_date, start_timestamp)
    
    end_date = input$dateRange1[2]
    end_timestamp = str_split(input$endTime1, ' ')[[1]][2]
    if (is.na(end_timestamp)) {
      end_timestamp = '00:00:00'
    } 
    end_time = paste(end_date, end_timestamp)
    
    if (input$radar1 == "SUR") {
      vpts_SUR %>%
        regularize_vpts() %>%
        plot(ylim=c(0, 2750), palette=(viridis::viridis(100, option="B")),
             xlim=c(as.POSIXct(start_time, tz='UTC'), as.POSIXct(end_time, tz='UTC')),
             zlim=c(1, 50), legend_ticks = seq(0, 50, 10))
    }  
    if (input$radar1 == "HAR") {
      # Toy dataset does not contain Harku radar's data.
    }
  })
  
  output$mapDBZH = renderLeaflet({
    start = as.POSIXct(input$measurementTimestamp) + 3*60*60
    ts = str_split(start, ' ')
    date = gsub('-', '', substr(ts[[1]][1], 3, nchar(ts[[1]][1])))
    time = gsub(':', '', ts[[1]][2])
    file_name1 = paste0('rasters/DBZH/SUR', date, time, '_DBZH.tif')

    leaflet() %>%
      setView(lng=25, lat=58.7, zoom=6) %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options=providerTileOptions(noWrap=T)) %>%
      addMarkers(lng=25.51886, lat=58.48231, icon=anglerIcon2) %>%
      addCircleMarkers(lng=25.51886, lat=58.48231, color='#a10559') %>%
      addCircleMarkers(lng=24.60210, lat=59.39767, color='#a10559') %>%
      addMarkers(lng=24.60210, lat=59.39767, icon=anglerIcon2) %>%
      addRasterImage(raster(file_name1), color=pal, opacity=0.9) %>%
      addLegend_decreasing(pal=pal, values=seq(-40, 40, by=5), title='DBZH', decreasing=T)
    
  })
  
  output$mapVID = renderLeaflet({
    start = as.POSIXct(input$measurementTimestamp) + 3*60*60
    ts = str_split(start, ' ')
    date = gsub('-', '', substr(ts[[1]][1], 3, nchar(ts[[1]][1])))
    time = gsub(':', '', ts[[1]][2])
    file_name2 = paste0('rasters/VID/SUR', date, time, '_VID.tif')
    
    leaflet() %>%
      setView(lng=25, lat=58.7, zoom=6) %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options=providerTileOptions(noWrap=T)) %>%
      addMarkers(lng=25.51886, lat=58.48231, icon=anglerIcon2) %>%
      addCircleMarkers(lng=25.51886, lat=58.48231, color='white') %>%
      addCircleMarkers(lng=24.60210, lat=59.39767, color='white') %>%
      addMarkers(lng=24.60210, lat=59.39767, icon=anglerIcon2) %>%
      addRasterImage(raster(file_name2), color=palVID, opacity=0.9) %>%
      addLegend_decreasing(pal=palVID, values=seq(0, 5000, by=50), 
                           title='birds/km<sup>2</sup>',
                           decreasing=T)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
