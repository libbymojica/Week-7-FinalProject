# Kite Migration Example for Shiny App Final Project
# by Libby Mojica
# March 2023



library(shiny)

#set up for the shiny app
library(tmap)
library(sf)
library(dplyr)
#library(tidyverse)
#library(tigris)
              

# read in data
load("kite_data.RData")

# set tmap mode to interactive
tmap_mode("view")


# Define UI
ui <- fluidPage(
    titlePanel("Timing of Kite Migration in the Southeast United States"),
    h5(
        "In the Southeastern US there are 3 species of kites. 
        The Snail Kite is a year-round resident of Florida whereas the 
        Swallow-tailed Kite and Mississippi Kite breed throughout the 
        Southeastern US and migrate to over-winter in Central and 
        South America. This app allows the user to 
        view the timing of kite observations (a proxy for kite presence/absence) 
        from 2020 - 2023 in selected southern states. Data is provided through 
        the Global Biodiversity Information Facility at https://www.gbif.org/.",

        img(src='kites_1000.png', align = "left"),
        
    ),
    h6("Photographers: Don Danko, Julio Mulero, Andy Morffew"
       
       ),
    
    # Sidebar layout
    sidebarLayout(
        sidebarPanel(
            # Input: select species shown on map
            checkboxGroupInput(
                inputId = "species",
                label = "Kite Species",
                # names should match that in the dataset
                choices = list("Swallow-tailed Kite", "Mississippi Kite", "Snail Kite"),
                # selected = sets which are selected by default
                selected = c("Swallow-tailed Kite", "Snail Kite")
            ),
            
           
    
    # Input: Filter by month
            sliderInput(
                inputId = "month",
                label = "Month Observed",
                min = 1,
                max = 12,
                value = c(4, 7)
            )
            
        ),
        
        # Main panel for map output
        mainPanel(# Output: interactive tmap object
            tmapOutput("map"))
        
    )
    
)


# Define server
server <- function(input, output){
    
    # Make a reactive object for the kite occurrence data by 
    # calling inputIDs to extract user chosen values
    occ_react <- reactive(
        multi_kite_sf %>%
            filter(Species %in% input$species) %>%
            filter(month >= input$month[1] &
                       month <= input$month[2])
    )
    
    # Render the map based on our reactive occurrence dataset
    output$map <- renderTmap({
        tm_shape(occ_react()) +
            tm_dots(
                col = "Species",
                size = 0.1,
                palette = "YlOrRd",
                title = "Species Occurrences",
                popup.vars = c(
                    "Species" = "Species",
                    "Month" = "month"
                )
            )
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


