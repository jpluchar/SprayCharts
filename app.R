library(shiny)
library(baseballr)
library(dplyr)
library(tidyr)
library(DT)

#setwd("~/Documents/Trout/Trout")
#getwd()
#Load Master List of mlb_id
master = read.csv("master.csv", stringsAsFactors = FALSE)
#trout = read.csv("trout.csv", stringsAsFactors = FALSE)
#trout = trout[-c(1)]
#trout = drop_na(trout)
#trout$game_date = as.Date(trout$game_date, format = "%Y-%m-%d")
#trout$events[trout$events != "single" & trout$events != "double" & trout$events != "triple" & trout$events != "home_run"] <- 'out'

#Shiny UI
ui <- fluidPage(
  navbarPage(title = HTML("<font size=5>Spray Charts</font>")),
  theme = shinythemes::shinytheme("flatly"),
  sidebarLayout(
    sidebarPanel( "Select a valid date in 2019", br(),
    selectizeInput('playerInput', 'Player Name', c(Choose = " ", master$mlb_name), options = list(maxOptions = 10000) 
    ),
              #   textInput("playerInput", "Player Name",
             # value = "Mike Trout"
    #),
    dateRangeInput("dateRange", "Game Date",
                     start = as.Date("2019-03-28"),
                     end = as.Date("2019-09-30")
    ),
    selectInput("pitcherInput", "Pitcher Handedness",
                choices = c("R", "L"),
                multiple = TRUE,
                selected = c("R", "L")
    ),
    selectInput("typeInput", "Pitch Type",
                choices = c("FF", "FC", "FS", "FT", "SI", "SL", "CU", "CH"), 
                multiple = TRUE,
                selected = c("FF", "FC", "FS", "FT", "SI", "SL", "CU", "CH")
    ),
    selectInput("bbInput", "Batted Ball Type",
                choices = c("fly_ball", "ground_ball", "line_drive", "popup"),
                multiple = TRUE,
                selected = c("fly_ball", "ground_ball", "line_drive", "popup")
    ),
    sliderInput("distanceInput", "Hit Distance", min = 0, max = 500,
                value = c(0, 500)
    ),
    sliderInput("angleInput", "Launch Angle", min = -100, max = 100,
                value = c(-100, 100)
    ),
    sliderInput("veloInput", "Exit Velocity", min = 30, max = 120,
                value = c(30, 120))
    ),
    mainPanel(
      htmlOutput("name"),
      plotOutput("results"),
      br(), br(),
      DTOutput("table"),
      br(), br(),
      htmlOutput("text")
      )
  )
)
#Shiny Server
server <- function(input, output) {
  name = reactive({
    master %>%
      filter(
        mlb_name == input$playerInput
      )
  })
  id = reactive({
    id = name()[1,1]
  })
  player = reactive({
    name = name()[1,2]
  })
  trout = reactive({
    scrape = scrape_statcast_savant_batter(start_date = "2019-03-28", end_date = "2019-09-30", batterid = id()) %>% 
    filter(type == "X") %>% select(2, 6, 9, 19, 1, 38:39, 24, 53:55, 70, 71)
    sapply(scrape, class)
    scrape = drop_na(scrape)
    scrape$events[scrape$events != "single" & scrape$events != "double" & scrape$events != "triple" & scrape$events != "home_run"] <- 'out'
    scrape = as.data.frame(scrape)
   })
  filtered = reactive({
    trout()%>%
     filter(
        (between(game_date, input$dateRange[1], input$dateRange[2])),
        pitch_type %in% input$typeInput,
        p_throws %in% input$pitcherInput,
        bb_type %in% input$bbInput,
        (between(hit_distance_sc, input$distanceInput[1], input$distanceInput[2])),
        (between(launch_angle, input$angleInput[1], input$angleInput[2])),
        (between(launch_speed, input$veloInput[1], input$veloInput[2]))
     )
  })
  table = reactive({
    filtered = c("game_date", "events", "hit_distance_sc", "estimated_ba_using_speedangle", "estimated_woba_using_speedangle")
    table = filtered()[filtered]
  })
  output$name <- renderUI({
    if(!isTruthy(player())) {
      h3("Select a Valid Player Name")
    }
    else {
    x = paste(player(), "Batted Balls — 2019")
    h3(HTML(paste(x)))
    }
  })
  output$results <- renderPlot({
    if(!isTruthy(player())) {
      
    }
    else {
    ggspraychart(filtered(), point_alpha = .6, fill_legend_title = "Hit Type", fill_value = "events",
                 fill_palette = c("single"="#A2C8EC", "double"="#006BA4", "triple"="#FF940E", "out"="#595959", "home_run"="#C85200")
      )
    }
  })
  output$table <- renderDT({
    if(!isTruthy(player())) {
      
      }
      else{
        table()
      }
  })
  output$text = renderUI(HTML("<b>Created by Jordan Pluchar — UCLA Class of 2020.</b><br>
                            Built using RShiny and Bill Petti's baseballr — Data © MLB Advanced Media 
                           ")
  )
}
shinyApp(ui = ui, server = server)