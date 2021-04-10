#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Authors: 
#           Group A - Autumn Class
#               Oana Damian
#               Junylou Daniel
#               Robin Mathew
#               Torsten Mayer

rm(list = ls())
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(ggmap)
library(leaflet)
library(scales)
library(sp)
library(stringr)
library(strex)
library(readr)


leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))  # Set value for the minZoom and maxZoom settings.
load("./www/shinyenvdata.RData")
distchoices <- c("All Districts", sort(to_neigh$AREA_NAME))
register_google(key = "xxxxxxxxxxxx-xxxxxxxxxxxxxx")

# Define UI 
ui <- bootstrapPage( theme = "styles.css",
            
            useShinyjs(),
            tags$style(HTML(".tooltip > .tooltip-inner {
                    width: 200px;
                    color: black;
                    background-color: yellow;
                }")),
            div( class = "outer",
                 # map in the background
                 leafletOutput("map", width="100%", height="100%"),
                 absolutePanel( id = "controls", class = "sidedash-box", 
                    # Application title
                    tags$h1("Toronto Houses"),
                    radioButtons(inputId = "mytask", "What do you want to do today?",
                                 c("Sell/Bid a house"= "sellbid",
                                   "Find a house"= "findhouse"), inline=FALSE),
                    bsTooltip("mytask", "Sell/Bid a house, will return the property predicted price.\\nFind a house, will return listed properties for sale based on entered criteria","bottom"),
                    conditionalPanel(
                        condition = "input.mytask == 'sellbid'",
                        searchInput(inputId = "addr1", label = "Enter no. and Street Name", placeholder = "100 Queen St W", 
                                    btnSearch = icon("search")),
                        textInput(inputId = "distvar1", label = "District"),
                        bsTooltip("addr1", "Search the number and street name first.\\nLocation will be marked on the map and the District is determined automatically.\\nThe application assumes Toronto address and will indicate if the address is not found on any District.","right"),
                        selectInput(inputId = "typevar", label = "House Type", choices = c("Detached",
                                            "Semi-Detached", "Att/Row/Twnhouse", "Condo Apt", "Condo Townhouse",
                                            "Plex", "Comm Element Condo", "Link", "Co-Ownership Apt", "Co-Op Apt", "Store W/Apt/Offc")),
                        bsTooltip("typevar", "Specify the property type.","right"),
                    ),
                    conditionalPanel(
                        condition = "input.mytask == 'findhouse'",
                        numericInputIcon(inputId = "budget1", label = "Enter budget amount", value = "1000000",
                                icon = list(NULL, icon("dollar-sign"))),
                        selectInput(inputId = "distvar2", label = "District", choices = distchoices,
                                    selected = "All Districts"),
                        selectInput(inputId = "typevar2", label = "House Type", choices = c("All Types", "Detached",
                                            "Semi-Detached", "Att/Row/Twnhouse", "Condo Apt", "Condo Townhouse",
                                            "Plex", "Comm Element Condo", "Link", "Co-Ownership Apt", "Co-Op Apt", "Store W/Apt/Offc")),
                        bsTooltip("budget1", "This is the maximum amount you are willing to spend.","right"),
                        bsTooltip("distvar2", "You may choose a specific district to find listed properties.","right"),
                        bsTooltip("typevar2", "You may choose a specific property type or All Types","right"),
                    ),

                    tags$h4(HTML("</br>House Details:")),
                    
                    column(width=5,selectInput(inputId = "bedvar", label = "Bedrooms (Above Grade)", choices = 0:20)),
                    column(width=5,selectInput(inputId = "bedvarbg", label = "Bedrooms (Below Grade)", choices = 0:20)),
                    column(width=5,selectInput(inputId = "bathvar", label = "Bathrooms", choices = 1:15)),
                    column(width=5,selectInput(inputId = "parkvar", label = "Parking", choices = 0:15)),
                    column(width=5,numericInput(inputId = "sqftvar", label = "Area (sqft)", "1000")),
                    bsTooltip("bedvar", "Indicate the minimum number of above grade bedrooms (up to code) on the property.","right"),
                    bsTooltip("bedvarbg", "Indicate the minimum number of below grade bedrooms (NOT up to code) on the property.","right"),
                    bsTooltip("bathvar", "Indicate the minimum number of bathrooms on the property.","right"),
                    bsTooltip("parkvar", "Indicate the minimum number of parking space.","right"),
                    bsTooltip("sqftvar", "Indicate the minimum living space in Square Foot.","right"),
                    
            ),
            
            conditionalPanel(
                condition = "input.mytask == 'sellbid'",
                absolutePanel( class = "prediction-box", h4("Predicted price"), textOutput("houseprice")),
                bsTooltip("houseprice", "Predicted Price of the Property","bottom"),
            ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    pal <- colorNumeric("viridis", NULL)
    #set.seed(919)
    labels <- sprintf("<strong>%s</strong>", to_neigh$AREA_NAME) %>% lapply(htmltools::HTML)
    
    # Reactive expression to create data frame of all input values ----
    inputData <- reactive({
        if(input$mytask == "sellbid") {
            lat <- 43.6534
            lon <- -79.3841
            to_addr <- "Toronto City Hall"
            district_code <- 76
            city_district <- "Bay Street Corridor"
            geoAddress <- to_addr
            if(input$addr1 != "") {
                #  geocode lookup
                to_addr <- paste0(input$addr1,", Toronto, Ontario")
                addrcoord <- geocode(to_addr, output = "latlona", source = "google")
                lon <- as.numeric(addrcoord[1])
                lat <- as.numeric(addrcoord[2])
                geoAddress <- as.character(addrcoord[3])
                
                #  find the district
                j <- 1
                inpoly <- 0
                while (inpoly == 0 & j <= 140) {
                    neighnames <- file_js$features[[j]]$properties$AREA_NAME
                    inpoly <- point.in.polygon(lat, lon, file_js$features[[j]]$geometry$coordinates[,2],
                                               file_js$features[[j]]$geometry$coordinates[,1], mode.checked=FALSE)
                    j <- j + 1
                }
                if(inpoly > 0) { 
                    neighcodname <- parse_number(gsub("-", "", neighnames))
                    district_code <- neighcodname
                    city_district <- neigh_rank$area_name[neigh_rank$district_code==neighcodname]
                } else {
                    #Address cannot be found within Toronto    
                    city_district <- "Address NOT found in Toronto"
                }
            }
            
            col_header <- c("bathrooms", "sqft", "parking", "type", "long", "lat", "bedrooms_ag", "bedrooms_bg",
                            "address", "district_code", "city_district")
            value = list(as.numeric(input$bathvar), input$sqftvar, as.numeric(input$parkvar), input$typevar, lon, lat,
                         as.numeric(input$bedvar), as.numeric(input$bedvarbg), geoAddress, district_code, city_district)
            temp_df <- data.frame(matrix(ncol = length(col_header), nrow = 1))
            for(i in 1:length(col_header)) { temp_df[1, i] <- value[i] }
            colnames(temp_df) <- col_header
            
            temp_df <- merge(temp_df,neigh_rank, by="district_code")
            
            if(temp_df$type == "Detached") {
                temp_df$interaction_effect1 <- temp_df$sqft * temp_df$bathrooms
                temp_df$interaction_effect2 <- temp_df$bedrooms_ag * temp_df$bathrooms
                temp_df$interaction_effect3 <- temp_df$sqft * temp_df$mean_district_income
                temp_df$nonlinear_effect1 <- (temp_df$bathrooms)^2
                temp_df$nonlinear_effect2 <- (temp_df$mean_district_income)^0.5
            }
            
            return(temp_df)
            
        } else if(input$mytask == "findhouse") {
            
            houses_df1 <- subset(houses_df, houses_df$bedrooms_ag >= as.numeric(input$bedvar) &
                                            houses_df$bathrooms >= as.numeric(input$bathvar) &
                                            houses_df$bathrooms >= as.numeric(input$parkvar) &
                                            houses_df$list_price <= input$budget1 &
                                            houses_df$sqft >= input$sqftvar
            )
            if(input$distvar2 != "All Districts") {
                distcode <- parse_number(input$distvar2)
                houses_df1 <- subset(houses_df1, houses_df1$district_code == distcode[[1]][1])
            }
            if(input$typevar2 != "All Types") {
                houses_df1 <- subset(houses_df1, houses_df1$type == input$typevar2)
            }
            
            # new features from keywords in "description"
            v1 <- as.data.frame(grepl("Fitness",houses_df1$description))
            houses_df1$fitness <- v1
            colnames(houses_df1$fitness) <- "fitness"

            v2 <- as.data.frame(grepl("Swimming",houses_df1$description))
            houses_df1$swimming_pool <- v2
            colnames(houses_df1$swimming_pool) <- "swimming_pool"

            houses_df1$fitness <- ifelse(houses_df1$fitness == TRUE, "Yes", "No")
            houses_df1$swimming_pool <- ifelse(houses_df1$swimming_pool == TRUE, "Yes", "No")
            
            if(nrow(houses_df1)!=0) {
                houses_df1$labels <- paste("<strong>", "City","District:", houses_df1$city_district, "</strong>",
                                            "<p>", "Type:", houses_df1$type, "</p>",
                                            "<p>", "List", "Price:", "$", houses_df1$list_price, "</p>",
                                            "<p>", "Area:", houses_df1$sqft, "</p>",
                                            "<p>", "Number","of","bedrooms:", houses_df1$bedrooms, "</p>",
                                            "<p>", "Number","of","bathroooms:", houses_df1$bathrooms, "</p>",
                                            "<p>", "Number","of","parking:", houses_df1$parking, "</p>",
                                            "<p>", "Fitness", "center:", houses_df1$fitness, "</p>",
                                            "<p>", "Swimming", "pool:", houses_df1$swimming_pool, "</p>",
                                            "<p>", "Address:", houses_df1$full_address, "</p>"
                )
            }
            return(houses_df1)
        }
        
    })
    
    observeEvent(input$mytask, {
        inpvars <- c("bedvar", "bathvar", "parkvar", "sqftvar")
        if(input$mytask == "sellbid") {
            for(x in inpvars) { 
                disable(x)
            }
            disable("distvar1")
            disable("typevar")
        } else if(input$mytask == "findhouse") {
            for(x in inpvars) { 
                enable(x)
            }
            enable("typevar2")
        }
    })
    
    observeEvent(input$addr1, {
        mydata <- inputData()
        inpvars <- c("bedvar", "bathvar", "parkvar", "typevar", "sqftvar")
        if(input$addr1 == "") {
            updateTextInput(session, "distvar1", value = "Bay Street Corridor")
            for(x in inpvars) { 
                disable(x)
            }
        } else {
            mydistrict <- mydata$city_district
            updateTextInput(session, "distvar1", value = mydistrict)
            if (mydistrict != "Address NOT found in Toronto") {
                for (x in inpvars) {
                    enable(x)
                }
            } else {
                for (x in inpvars) {
                    disable(x)
                } 
            }
        }
    })
    
    output$houseprice <- reactive({
        if (input$mytask == "sellbid") {
            mydata <- inputData()
            if(mydata$type == "Detached") {
                model <- lm(final_price ~ ., data = traindetached_df)
                final_price_predicted <- predict(model,mydata)
            } else {
                model <- lm(final_price ~.+ sqft:mean_district_income + sqft:type + bedrooms_ag:bathrooms + sqft:bathrooms + I(mean_district_income^0.5), data = trainnondetached_df)
                final_price_predicted <- predict(model,mydata)
            }
            return(as.character(dollar(final_price_predicted)))
            
        }
    })
    
    output$map <- renderLeaflet({
        mydata <- inputData()
        
        if(input$mytask == "sellbid") {
            to_map <- leaflet(to_neigh) %>% setView(lng = mydata$long, mydata$lat, zoom = 13 ) %>% addTiles() %>%
                        addMarkers(lng = mydata$long, lat = mydata$lat, popup=mydata$address)
            to_map %>% addPolygons(fillColor = ~pal(AREA_SHORT_CODE), weight = 2, opacity = .1, color = "blue", 
                        dashArray = "3", fillOpacity = 0.2, highlight = highlightOptions(weight = 5, color = "red", dashArray = "",
                        fillOpacity = 0.7, bringToFront = TRUE), label = labels,
                        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px", direction = "auto")) 
        } else if(input$mytask == "findhouse") {
            if(nrow(mydata) != 0) {
                df_select1 <- subset(mydata, type == "Detached" | type == "Semi-Detached")
                df_select2 <- subset(mydata, type == "Condo Apt" | type == "Condo Townhouse")

                if(input$distvar2 == "All Districts") {
                    long <- -79.40603941
                    lat <- 43.7172117
                } else {
                    distndx <- match(mydata$district_code[1], to_neigh$AREA_SHORT_CODE)
                    long <- to_neigh$LONGITUDE[distndx]
                    lat <- to_neigh$LATITUDE[distndx]
                }
                to_map <- leaflet(to_neigh) %>% setView(lng = long, lat, zoom = 12 ) %>% addTiles()
            
                if(nrow(df_select1) != 0) {
                    to_map <- to_map %>% addCircleMarkers(lng = df_select1$long, lat=df_select1$lat, color = "red", radius = ~ 3,
                                 group = "Houses", label = lapply(df_select1$labels, HTML), labelOptions = (textsize = "25px"))
                }
                if(nrow(df_select2) != 0) {
                    to_map <- to_map %>% addCircleMarkers(lng = df_select2$long, lat = df_select2$lat, color = "blue", radius = ~ 3,
                                 group = "Condos", label = lapply(df_select2$labels, HTML), labelOptions = (textsize = "25px"))
                }
                to_map %>% addLayersControl(overlayGroups = c("Houses", "Condos"), options = layersControlOptions(collapsed = FALSE))
            } else {
                long <- -79.40603941
                lat <- 43.7172117
                to_map <- leaflet(to_neigh) %>% setView(lng = long, lat, zoom = 12 ) %>% addTiles()
                to_map
            }
            
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
