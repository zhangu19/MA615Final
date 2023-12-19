library(shiny)
library(bslib)
library(shinypanels)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(ggplot2)
library(rsconnect)


ui <- navbarPage(id = "tabs",
                 title = "Dominica",
                 
                 # general information
                 
                 navbarMenu(title = "General Information",
                            
                 tabPanel(title = "About Dominica", img(src="Flag_of_Dominica.png", width="200"),
                          p(style="font-size:12pt", "Dominica, officially known as the Commonwealth of Dominica, is an island country in the Caribbean.",
                            br(),
                            "Its area is about 750 square kilometers, making it the 174th largest country in the world.",
                            br(),
                            "Its population is about 72,000, with the majority residing in and around the capital city, Roseau.",
                            br(),
                            "The official language is English, though a large portion of the population also speaks Dominican Creole French.",
                            br(),
                            "The economy of Dominica is primarily dependent on agriculture, with bananas as the major crop, and tourism.",
                            br(),
                            "The island's tropical climate and natural beauty make it a destination for eco-tourism.")),
                 
                 tabPanel("World location",
                          leafletOutput("myOnemap", height = 700),
                          p()
                 ),
                 
                 tabPanel("Regional map",
                          img(src="Caribbean_general_map.png", height="600", width="800"),
                          p(style="font-size:12pt", "The Caribbean is a subregion of the Americas that includes the Caribbean Sea and its islands")
                 ),
                 
                 tabPanel("State map",
                          img(src="Dominica-Map.jpg", height="600", width="800"),
                          p(style="font-size:12pt", "The Caribbean is a subregion of the Americas that includes the Caribbean Sea and its islands")
                 )

                 ),
                 
                 # Demographics
                 
                 navbarMenu(
                   title = "Demographics",
                   
                   tabPanel(title = "Age Structure", img(src="DO_popgraph2023.jpg", height="600", width="800"),
                            p(style="font-size:12pt", "0-14 years: 20.82% (male 7,954/female 7,592)",
                              br(),
                              "15-64 years: 65.82% (male 25,085/female 24,053)",
                              br(),
                              "65 years and over: 13.36% (male 4,712/female 5,260)",
                              br(),
                              "(2023)")
                            ),
                   
                   tabPanel(title = "Ethnicity", plotOutput("ethnicityPlot"),
                            p(style="font-size:12pt", "African descent 84.5%, mixed 9%, Indigenous 3.8%, other 2.1%, unspecified 0.6%",
                              br(),
                              "(2011)")
                            ),
                   
                   tabPanel(title = "Land Use", plotOutput("landPlot"),
                            p(style="font-size:12pt", "agricultural land: 34.7% (arable land 8%, permanent crops 24%, permanent pasture 2.7%)",
                              br(),
                              "forest: 59.2%",
                              br(),
                              "other: 6.1%",
                              br(),
                              "(2018)")
                  ),
                 ),

                 nav_panel("Caribbean Region",
                           img(src="Region.png", height="300", width="800"),
                           p(style="font-size:12pt", "Compared with other Caribbean island states, Dominica has its uniqueness in terms of economies, resources, and environment."),    
                           tableOutput("comparisonTable")    
                           ),
                 
                 
                 nav_panel("SWOT", p("Analysis",
                                     
                                     panelsPage(
                                       panel(
                                         title = "Strengths",
                                         can_collapse = FALSE,
                                         body = div(
                                           style = "width: 300px;",
                                           p("Dominica is known for its stunning natural landscapes, including rainforests, mountains, and waterfalls, making it an attractive destination for eco-tourism. There is a growing global interest in eco-tourism, and Dominica can capitalize on this trend by promoting sustainable tourism practices.")
                                           
                                         )
                                       ),
                                       panel(
                                         title = "Weaknesses",
                                         can_collapse = FALSE,
                                         body = div(
                                           style = "width: 300px;",
                                           p("On the other hand, heavy reliance on agriculture and tourism makes Dominica's economy vulnerable to external shocks and natural disasters.")
                                           
                                         )
                                       ),
                                       panel(
                                         title = "Opportunities",
                                         can_collapse = FALSE,
                                         body = div(
                                           style = "width: 300px;",
                                           p("In 2022, The World Bank approved two loans for Dominica: the Dominica Second Covid-19 Response and Recovery Programmatic Development Policy Credit for US$30 million and the Dominica Disaster Risk Management Development Policy Financing for US$20 million. Both operations are expected to accelerate the recovery from the impacts of COVID-19, protect the lives and livelihoods of citizens, accelerate policy reforms and increase Dominica’s resilience to future shocks."),
                                           img(src="growth.png", height="200", width="300")
                                         )
                                       ),
                                       panel(
                                         title = "Threats",
                                         can_collapse = FALSE,
                                         body = div(
                                           style = "width: 300px;",
                                        p("In rating the island’s level of exposure and vulnerability to extreme events, Germanwatch’s 2010 Global Climate Risk Index ranks Dominica 25th out of 150 countries at risk and 55th with losses of 9.62% gross domestic product based on an analysis of extreme weather events between 1998 and 2007. Two factors were cited for Dominica: the impact of global warming on rising sea levels that increase the risk of storm surges, and the increase in the strength of hurricanes. Dominica is at risk to earthquakes and volcanic eruptions, floods and landslides, and hurricanes."),
                                         img(src="key-natural-hazard-stati.png", height="200", width="300")
                                         )
                                       )
                                     )
                                     
                 )
                 
                 
                 ),
                 
                 navbarMenu(
                   title = "Bibliogrphy",
                   align = "right",
                   tabPanel(tags$a(href="https://en.wikipedia.org/wiki/Dominica",
                                   "Wikipedia/Dominica")),
                   tabPanel(tags$a(href="https://data.un.org/",
                                   "UN Data")),
                   tabPanel(tags$a(href="https://data.worldbank.org/",
                                   "World Bank data")),
                   tabPanel(tags$a(href="https://sdgs.un.org/topics/small-island-developing-states/mvi",
                                   "Multidimensional Vulnerability Index")),
                   tabPanel(tags$a(href="https://www.imf.org/en/Data",
                                   "IMF Data")),
                   tabPanel(tags$a(href = "https://www.cia.gov/the-world-factbook/countries/dominica/#:~:text=African%20descent%2084.5%25%2C%20mixed%209,0.6%25%20(2011%20est.)",
                                   "CIA.gov")),
                   tabPanel(tags$a(href="https://www.shinyapps.io/",
                                   "shinyapps.io for publishing")),
                   tabPanel(tags$a(href="https://rstudio.github.io/leaflet/",
                                   "Leaflet doc")))
                 )
                



Roseau <- list(lat = 15.301389, lng = -61.388333)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$debug <- renderPrint({
    cat(paste(c("La", "casa", "en", "el", "árbol"), collapse = "\n"))
  })
  output$plot <- renderPlot(
    plot(cars)
  )
  
  
  output$myOnemap <- renderLeaflet({
    leaflet()  %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012) %>%
      setView(lng = Roseau$lng, lat = Roseau$lat, zoom = 2) %>%
      addMarkers(lng = Roseau$lng, lat = Roseau$lat, label = "Dominica")
  })

  
  ethnic_data <- data.frame(
    Ethnicity = c("African Descent", "Mixed", "Indigenous", "Other", "Unspecified"),
    Percentage = c(84.5, 9, 3.8, 2.1, 0.6)
  )
  output$ethnicityPlot <- renderPlot({
  ggplot(ethnic_data, aes(x = Ethnicity, y = Percentage, fill = Ethnicity)) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Ethnic Groups in Dominica (2011 est.)",
         x = "Ethnicity",
         y = "Percentage (%)") +
      scale_fill_brewer(palette = "Set3")
  })
  
  land_use_data <- data.frame(
    Land_Type = c("Arable Land", "Permanent Crops", "Permanent Pasture", "Forest", "Other"),
    Percentage = c(8, 24, 2.7, 59.2, 6.1)
  )
  output$landPlot <- renderPlot({
    ggplot(land_use_data, aes(x = "", y = Percentage, fill = Land_Type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Land Use in Dominica") +
      scale_fill_brewer(palette = "Set3")
  })
  
  
  comparison_data <- data.frame(
    Category = c("Main Economic Sectors", "Economic Reliance", "Notable Resources"),
    Dominica = c("Agriculture (especially bananas), eco-tourism, financial services", 
                 "Agriculture (20% GDP, 40% labor force)", 
                 "Ecological diversity, potential for renewable energy"),
    Barbados = c("Tourism, international business, services (primarily)", 
                 "Services (88.7% GDP)", 
                 "High-income economy based on tourism and offshore sector"),
    Trinidad_and_Tobago = c("Petroleum, petrochemicals, industrial focus", 
                            "Oil and Gas (40% GDP, 80% exports)", 
                            "Largest Caribbean producer of natural gas, significant oil reserves")
  )
  output$comparisonTable <- renderTable({
    comparison_data
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
