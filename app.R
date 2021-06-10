
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(scales)

# Load Datasets
feat_play_prob_dat <- read.csv('feat_play_prob_dat.csv')

# Define UI
ui <- fluidPage(

    # use this in non shinydashboard app
    # setBackgroundColor(color = "ghostwhite"),
    useShinydashboard(),
    # -----------------
    
    # Application Title
    # titlePanel(title=div(img(src="nfl_logo.png", style="width:80px"), "Head Coach Play Calling Tendencies & Probabilities")),
    # titlePanel(title=div(img(src="nfl_logo.png", style="width:90px;"), "Head Coach Play Calling Tendencies", style="font-size:36px;")),
    br(),
    
    # Sidebar Input Panel
    sidebarLayout(
        sidebarPanel(
            h3("Head Coach"),
            hr(),
            selectInput("coachInput", label = NULL,
                        choices = feat_play_prob_dat %>%
                            dplyr::select(posteam_coach) %>%
                            distinct() %>%
                            arrange(posteam_coach) %>%
                            pull(),
                        selected = "Andy Reid"),
            br(),
            # numericInput("scorediffInput", "Score Differential", 0),
            h3("In-Game Situation"),
            hr(),
            selectInput("seastypeInput", "Regular Season or Playoffs", choices = c("Both", "Regular Season", "Playoffs")),
            sliderInput("qtrInput", "Quarter", min = 1, max = 5, value = c(1,5)),
            p(style = "font-size:12px; font-style:italic; text-align:right;", "*Q5 denotes all Overtime play"),
            sliderInput("scorediffInput", "Score Differential", min = -60, max = 60, value = c(-60,60)),
            selectInput("downInput", "Down", choices = c("Any", 1, 2, 3, 4)),
            # numericInput("ydstogoInput", "Yards to Go", 10, min=1),
            sliderInput("ydstogoInput", "Yards to Go", min = 0, max = 50, value = c(0,50)),
            sliderInput("yardsInput", "Play Yardline", min = 0, max = 100, value = c(0,100)),
            checkboxInput("twomindrillInput", "Two-Minute Drill", FALSE),
            # radioButtons("firstplays", "First 10 Play Calls of Game", choices = c("Yes", "No"), selected = "No"),
            # radioButtons("fgrngInput", "In Field-Goal Range", choices = c("Yes", "No"), selected = "No"),
            # radioButtons("redzoneInput", "In Red Zone", choices = c("Yes", "No"), selected = "No"),
            # selectInput("qbtype", "QB Type", c("Third String", "Second String", "Average", "Above Average", "Superstar"), selected = "Average"),
            # selectInput("rbtype", "RB Type", c("Third String", "Second String", "Average", "Above Average", "Superstar"), selected = "Average")
        ),

        # Main Panel
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Career Overview",
                                 br(),
                                 fluidRow(
                                     column(6,
                                            h2(strong(textOutput("coachname_career"))),
                                            tags$style(".bg-blue { background-color: #064583 !important; color: #fff !important; border:solid 1px #021b34;}"),
                                            tags$style(".bg-light-blue { background-color: #cdd9e6 !important; color: #000 !important; border:solid 1px #9bb4cd;}"),
                                            fluidRow(
                                                infoBox(
                                                    "PLAYS CALLED", value = uiOutput(style="font-size:32px;", "coach_plays"), icon = icon("headset"), color = "blue", width = 12
                                                ),
                                                valueBox(
                                                    uiOutput("coach_runpct"),  "RUN RATE", color = "light-blue", width = 6
                                                ),
                                                valueBox(
                                                    uiOutput("coach_passpct"),  "PASS RATE", color = "light-blue", width = 6
                                                ),
                                                valueBox(
                                                    uiOutput("coach_puntpct"),  "PUNT RATE", color = "light-blue", width = 6
                                                ),
                                                valueBox(
                                                    uiOutput("coach_fgapct"),  "FGA RATE", color = "light-blue", width = 6
                                                )      
                                            ),
                                     ),
                                     column(6,
                                            h2(strong("All Coaches")),
                                            tags$style(".bg-maroon { background-color: #BB0000 !important; color: #fff !important; border:solid 1px #4a0000;}"),
                                            tags$style(".bg-red { background-color: #f1cccc !important; color: #000 !important; border:solid 1px #e39999;}"),
                                            fluidRow(
                                                infoBox(
                                                    "PLAYS CALLED", value = uiOutput(style="font-size:32px;", "leag_plays"), icon = icon("headset"), color = "maroon", width = 12
                                                ),
                                                valueBox(
                                                    uiOutput("leag_runpct"),  "RUN RATE", color = "red", width = 6
                                                ),
                                                valueBox(
                                                    uiOutput("leag_passpct"),  "PASS RATE", color = "red", width = 6
                                                ),
                                                valueBox(
                                                    uiOutput("leag_puntpct"),  "PUNT RATE", color = "red", width = 6
                                                ),
                                                valueBox(
                                                    uiOutput("leag_fgapct"),  "FGA RATE", color = "red", width = 6
                                                )      
                                            ),
                                     )
                                 ),
                                br(),
                                br(),
                                # hr(),
                                # column(12, style = "background:#fff8db;border: 1px solid #ffd600; padding:10px 20px 10px 20px;",
                                #        h5(strong("NOTES")),
                                #        p("1. Data is all regular season & playoff play-by-play data from 1999-2020 leveraging the", tags$a(href="https://github.com/mrcaseb/nflfastR", "nflfastR"), "package."),
                                #        p("2. The number of Plays Called is based on selected inputs from side panel."),
                                #        p("3. The All Coaches section includes the play calls from the Head Coach selected (i.e. league-wide data).")
                                #        ),
                                # br()
                                 ),
                        tabPanel("Season-by-Season",
                                 br(),
                                 div(selectInput("seasInput", label = "Season", choices = c("All", 1999:2019), selected = "All"), style="float:right"),
                                 h2(strong(textOutput("coachname_season"))),
                                 DT::dataTableOutput("coach_sbs_stats"),
                                 br(),
                                 h2(strong("All Coaches")),
                                 DT::dataTableOutput("leag_sbs_stats"),
                                 br(),
                                 br()
                                 ),
                        tabPanel("Play Call Trend",
                                 br(),
                                 h2(strong(textOutput("coachname_trend"))),
                                 plotOutput("coachplot"),
                                 br(),
                                 h2(strong("All Coaches")),
                                 plotOutput("leagplot"),
                                 ),
                        tabPanel("Play Type Breakdown",
                                 br(),
                                 # div(selectInput("seasInput", label = "Season", choices = c("All", 1999:2019), selected = "All"), style="float:right"),
                                 h2(strong(textOutput("coachname_breakdown"))),
                                 br(),
                                 h4("Pass Plays"),
                                 DT::dataTableOutput("coach_pass_bd"),
                                 br(),
                                 h4("Run Plays"),
                                 DT::dataTableOutput("coach_run_bd"),
                                 br()
                        )
            )

        )
    )
)

# Define server logic
server <- function(input, output) {

    #---  HEAD COACH  ---#
    
        # Assign selected coach's name
        output$coachname_career <- renderText({input$coachInput})
        output$coachname_season <- renderText({input$coachInput})
        output$coachname_trend <- renderText({input$coachInput})
        output$coachname_breakdown <- renderText({input$coachInput})
        
        # Filter dataset based on user inputs (side panel)
        coach_dat <- reactive({
            
            if(input$seastypeInput == "Both"){
                
                if(input$downInput == "Any"){
                    
                    if(input$twomindrillInput == TRUE){
                        subset(feat_play_prob_dat, posteam_coach == input$coachInput & adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2] & half_seconds_remaining <= 120)
                    }
                    #else if twomindrill == FALSE
                    else{
                        subset(feat_play_prob_dat, posteam_coach == input$coachInput & adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2])
                    }
                    
                }
                #else if down == 1:4
                else{
                    
                    if(input$twomindrillInput == TRUE){
                        subset(feat_play_prob_dat, posteam_coach == input$coachInput & adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   down == input$downInput & yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2] & half_seconds_remaining <= 120)
                    }
                    #else if twomindrill == FALSE
                    else{
                        subset(feat_play_prob_dat, posteam_coach == input$coachInput & adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   down == input$downInput &  yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2])
                    }
                    
                }
                
            }
            #else if season type == REG or POST
            else{
                
                if(input$downInput == "Any"){
                    if(input$twomindrillInput == TRUE){
                        subset(feat_play_prob_dat, posteam_coach == input$coachInput & game_type == input$seastypeInput &
                                   adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2] & half_seconds_remaining <= 120)
                    }
                    #else if twomindrill == FALSE
                    else{
                        subset(feat_play_prob_dat, posteam_coach == input$coachInput & game_type == input$seastypeInput &
                                   adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2])
                    }
                    
                }
                #else if down == 1:4
                else{
                    
                    if(input$twomindrillInput == TRUE){
                        subset(feat_play_prob_dat, posteam_coach == input$coachInput & game_type == input$seastypeInput & down == input$downInput & 
                                   adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2] & half_seconds_remaining <= 120)
                    }
                    #else if twomindrill == FALSE
                    else{
                        subset(feat_play_prob_dat, posteam_coach == input$coachInput & game_type == input$seastypeInput & down == input$downInput &
                                   adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2])
                    }
                    
                }
                
            }
        })
        
        # Create Career table from filtered dataset for use with all coach's stat renders
        coach_dat_summ <- reactive({
            coach_dat() %>%
                summarise(
                    # games_coached = length(unique(game_id)),
                    plays_called = length(play_id),
                    run_plays = sum(run),
                    pass_plays = sum(pass),
                    punt_plays = sum(punt),
                    field_goal_att = sum(field_goal)
                ) %>%
                mutate(
                    run_pct = percent(run_plays/plays_called, accuracy = 1),
                    pass_pct = percent(pass_plays/plays_called, accuracy = 1),
                    punt_pct = percent(punt_plays/plays_called, accuracy = 1),
                    fga_pct = percent(field_goal_att/plays_called, accuracy = 1),
                    plays_called = dollar(plays_called, prefix = "", big.mark=","),
                    run_plays = dollar(run_plays, prefix = "", big.mark=","),
                    pass_plays = dollar(pass_plays, prefix = "", big.mark=","),
                    punt_plays = dollar(punt_plays, prefix = "", big.mark=","),
                    field_goal_att = dollar(field_goal_att, prefix = "", big.mark=",")
                ) %>%
                'colnames<-' (c("Play Calls", "Runs", "Passes", "Punts", "FGAs", "Run%", "Pass%", "Punt%", "FGA%"))
        })
        
        # Create Season table from filtered dataset for use with all coach's stat renders
        coach_sbs_dat_summ <- reactive({
            if(input$seasInput == "All"){
                coach_dat() %>%
                    group_by(
                        season
                    ) %>%
                    summarise(
                        # games_coached = length(unique(game_id)),
                        plays_called = length(play_id),
                        run_plays = sum(run),
                        pass_plays = sum(pass),
                        punt_plays = sum(punt),
                        field_goal_att = sum(field_goal)
                    ) %>%
                    mutate(
                        run_pct = percent(run_plays/plays_called, accuracy = 1),
                        pass_pct = percent(pass_plays/plays_called, accuracy = 1),
                        punt_pct = percent(punt_plays/plays_called, accuracy = 1),
                        fga_pct = percent(field_goal_att/plays_called, accuracy = 1),
                        plays_called = dollar(plays_called, prefix = "", big.mark=","),
                        run_plays = dollar(run_plays, prefix = "", big.mark=","),
                        pass_plays = dollar(pass_plays, prefix = "", big.mark=","),
                        punt_plays = dollar(punt_plays, prefix = "", big.mark=","),
                        field_goal_att = dollar(field_goal_att, prefix = "", big.mark=",")
                    ) %>%
                    'colnames<-' (c("Season", "Play Calls", "Runs", "Passes", "Punts", "FGAs", "Run%", "Pass%", "Punt%", "FGA%"))
            }
            else{
                coach_dat() %>%
                    group_by(
                        season
                    ) %>%
                    subset(
                        season == input$seasInput
                    ) %>%
                    summarise(
                        # games_coached = length(unique(game_id)),
                        plays_called = length(play_id),
                        run_plays = sum(run),
                        pass_plays = sum(pass),
                        punt_plays = sum(punt),
                        field_goal_att = sum(field_goal)
                    ) %>%
                    mutate(
                        run_pct = percent(run_plays/plays_called, accuracy = 1),
                        pass_pct = percent(pass_plays/plays_called, accuracy = 1),
                        punt_pct = percent(punt_plays/plays_called, accuracy = 1),
                        fga_pct = percent(field_goal_att/plays_called, accuracy = 1),
                        plays_called = dollar(plays_called, prefix = "", big.mark=","),
                        run_plays = dollar(run_plays, prefix = "", big.mark=","),
                        pass_plays = dollar(pass_plays, prefix = "", big.mark=","),
                        punt_plays = dollar(punt_plays, prefix = "", big.mark=","),
                        field_goal_att = dollar(field_goal_att, prefix = "", big.mark=",")
                    ) %>%
                    'colnames<-' (c("Season", "Play Calls", "Runs", "Passes", "Punts", "FGAs", "Run%", "Pass%", "Punt%", "FGA%"))
            }
            })
        
        # Coach num of plays called
        output$coach_plays <- renderText({
            coach_dat_summ()$'Play Calls'
        })
        
        # Coach run percentage
        output$coach_runpct <- renderText({
            coach_dat_summ()$'Run%'
        })
        
        # Coach pass percentage
        output$coach_passpct <- renderText({
            coach_dat_summ()$'Pass%'
        })
        
        # Coach punt percentage
        output$coach_puntpct <- renderText({
            coach_dat_summ()$'Punt%'
        })
        
        # Coach field goal attempt percentage
        output$coach_fgapct <- renderText({
            coach_dat_summ()$'FGA%'
        })
        
        # Coach Career count and rate table
        output$coach_career_stats <- renderTable({
            coach_dat_summ()
        })
        
        # Coach Season count and rate table
        output$coach_sbs_stats <- DT::renderDataTable(
            coach_sbs_dat_summ(), options = list(dom = 't', pageLength = 100), rownames = FALSE
        )
        
        # Coach Trend Plot
        coach_sbs_trend <- reactive({
            coach_dat() %>%
                group_by(
                    season
                ) %>%
                summarise(
                    # games_coached = length(unique(game_id)),
                    plays_called = as.numeric(length(play_id)),
                    run_plays = sum(run),
                    pass_plays = sum(pass),
                    punt_plays = sum(punt),
                    field_goal_att = sum(field_goal)
                ) %>%
                mutate(
                    run_pct = run_plays/plays_called,
                    pass_pct = pass_plays/plays_called,
                    punt_pct = punt_plays/plays_called,
                    fga_pct = field_goal_att/plays_called,
                )
        })
          
        output$coachplot <- renderPlot({
            
            coach_sbs_trend <- data.frame(coach_sbs_trend())
                
            ggplot(data = coach_sbs_trend, aes(x = season)) +
                geom_line(aes(y = run_pct, colour = "Run"), size=1.3) + 
                geom_line(aes(y = pass_pct, color="Pass"), size=1.3) +
                geom_line(aes(y = punt_pct, color="Punt"), size=1.3) +
                geom_line(aes(y = fga_pct, color="FGA"), size=1.3) +
                # scale_color_discrete(name = "Plays", labels = c("Run", "Pass", "Punt", "FGA")) +
                scale_colour_manual("Play Type", 
                                    breaks = c("Run", "Pass", "Punt", "FGA"),
                                    values = c("#4E84C4", "#D16103", "#52854C", "#FFDB6D")) +
                xlim(min(coach_sbs_trend$season)-1, max(coach_sbs_trend$season)+1) +
                xlab("Season") +
                scale_y_continuous(labels=scales::percent) +
                ylab("Percent of Play Calls") +
                theme(legend.key=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        })
        
        # Coach Pass Breakdown Table
        coach_pass_bd <- reactive({
            coach_dat() %>%
                subset(
                    play_type == 'pass'
                ) %>%
                group_by(
                    pass_length,
                    pass_location
                ) %>%
                mutate(
                    plays = sum(pass),
                    first_downs = sum(first_down_pass),
                    tot_yards_gained = sum(yards_gained),
                    avg_yards_gained = mean(yards_gained),
                    # pass_pct = percent(pass_plays/plays_called, accuracy = 1),
                    # pass_plays = dollar(pass_plays, prefix = "", big.mark=","),
                    first_down_pct = percent(first_downs/plays, accuracy = 1)
                ) %>%
                ungroup() %>%
                mutate(
                    tot_pass_plays = sum(pass),
                    play_pct = plays/tot_pass_plays
                ) %>%
                group_by(
                    pass_length,
                    pass_location
                ) %>%
                summarise(
                    plays = sum(pass),
                    play_pct = percent(mean(play_pct), accuracy = 1),
                    first_downs = sum(first_down_pass),
                    first_down_pct = percent(first_downs/plays, accuracy = 1),
                    tot_yards_gained = dollar(sum(yards_gained), prefix = "", big.mark=","),
                    avg_yards_gained = round(mean(yards_gained), digits = 1)
                ) %>%
                mutate(
                    plays = dollar(plays, prefix = "", big.mark=","),
                    first_downs = dollar(first_downs, prefix = "", big.mark=",")
                ) %>%
                'colnames<-' (c("Pass Length", "Pass Location", "Passes", "Play%", "First Downs", "First Down%", "Tot. Yards Gained", "Avg. Yards Gained"))
            })
        
        output$coach_pass_bd <- DT::renderDataTable(
            coach_pass_bd(), options = list(dom = 't', pageLength = 100), rownames = FALSE
        )
        
        # Coach Run Breakdown Table
        coach_run_bd <- reactive({
            coach_dat() %>%
                subset(
                    play_type == 'run'
                ) %>%
                group_by(
                    run_location,
                    run_gap,
                ) %>%
                mutate(
                    plays = sum(run),
                    first_downs = sum(first_down_rush),
                    tot_yards_gained = sum(yards_gained),
                    avg_yards_gained = mean(yards_gained),
                    # pass_pct = percent(pass_plays/plays_called, accuracy = 1),
                    # pass_plays = dollar(pass_plays, prefix = "", big.mark=","),
                    first_down_pct = percent(first_downs/plays, accuracy = 1)
                ) %>%
                ungroup() %>%
                mutate(
                    tot_run_plays = sum(run),
                    play_pct = plays/tot_run_plays
                ) %>%
                group_by(
                    run_location,
                    run_gap
                ) %>%
                summarise(
                    plays = sum(run),
                    play_pct = percent(mean(play_pct), accuracy = 1),
                    first_downs = sum(first_down_rush),
                    first_down_pct = percent(first_downs/plays, accuracy = 1),
                    tot_yards_gained = dollar(sum(yards_gained), prefix = "", big.mark=","),
                    avg_yards_gained = round(mean(yards_gained), digits = 1)
                ) %>%
                mutate(
                    plays = dollar(plays, prefix = "", big.mark=","),
                    first_downs = dollar(first_downs, prefix = "", big.mark=",")
                ) %>%
                'colnames<-' (c("Run Location", "Run Gap", "Runs", "Play%", "First Downs", "First Down%", "Tot. Yards Gained", "Avg. Yards Gained"))
        })
        
        output$coach_run_bd <- DT::renderDataTable(
            coach_run_bd(), options = list(dom = 't', pageLength = 100), rownames = FALSE
        )

    
    #---  LEAGUE  ---#
    
        # Filter dataset based on user inputs (side panel)
        leag_dat <- reactive({
            
            if(input$seastypeInput == "Both"){
                
                if(input$downInput == "Any"){
                    
                    if(input$twomindrillInput == TRUE){
                        subset(feat_play_prob_dat, adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2] & half_seconds_remaining <= 120)
                    }
                    #else if twomindrill == FALSE
                    else{
                        subset(feat_play_prob_dat, adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2])
                    }
                    
                }
                #else if down == 1:4
                else{
                    
                    if(input$twomindrillInput == TRUE){
                        subset(feat_play_prob_dat, adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   down == input$downInput & yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2] & half_seconds_remaining <= 120)
                    }
                    #else if twomindrill == FALSE
                    else{
                        subset(feat_play_prob_dat, adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   down == input$downInput &  yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2])
                    }
                    
                }
                
            }
            #else if season type == REG or POST
            else{
                
                if(input$downInput == "Any"){
                    if(input$twomindrillInput == TRUE){
                        subset(feat_play_prob_dat, game_type == input$seastypeInput &
                                   adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2] & half_seconds_remaining <= 120)
                    }
                    #else if twomindrill == FALSE
                    else{
                        subset(feat_play_prob_dat, game_type == input$seastypeInput &
                                   adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2])
                    }
                    
                }
                #else if down == 1:4
                else{
                    
                    if(input$twomindrillInput == TRUE){
                        subset(feat_play_prob_dat, game_type == input$seastypeInput & down == input$downInput & 
                                   adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2] & half_seconds_remaining <= 120)
                    }
                    #else if twomindrill == FALSE
                    else{
                        subset(feat_play_prob_dat, game_type == input$seastypeInput & down == input$downInput &
                                   adj_qtr >= input$qtrInput[1] & adj_qtr <= input$qtrInput[2] &
                                   yardline_100 >= input$yardsInput[1] & yardline_100 <= input$yardsInput[2] &
                                   score_differential >= input$scorediffInput[1] & score_differential <= input$scorediffInput[2] &
                                   ydstogo >= input$ydstogoInput[1] & ydstogo <= input$ydstogoInput[2])
                    }
                    
                }
                
            }
        })
        
        # Create table from filtered dataset for use with all league stat renders
        leag_dat_summ <- reactive({
            leag_dat() %>%
                summarise(
                    # games_coached = length(unique(game_id)),
                    plays_called = length(play_id),
                    run_plays = sum(run),
                    pass_plays = sum(pass),
                    punt_plays = sum(punt),
                    field_goal_att = sum(field_goal)
                ) %>%
                mutate(
                    run_pct = percent(run_plays/plays_called, accuracy = 1),
                    pass_pct = percent(pass_plays/plays_called, accuracy = 1),
                    punt_pct = percent(punt_plays/plays_called, accuracy = 1),
                    fga_pct = percent(field_goal_att/plays_called, accuracy = 1),
                    plays_called = dollar(plays_called, prefix = "", big.mark=","),
                    run_plays = dollar(run_plays, prefix = "", big.mark=","),
                    pass_plays = dollar(pass_plays, prefix = "", big.mark=","),
                    punt_plays = dollar(punt_plays, prefix = "", big.mark=","),
                    field_goal_att = dollar(field_goal_att, prefix = "", big.mark=",")
                ) %>%
                'colnames<-' (c("Play Calls", "Runs", "Passes", "Punts", "FGAs", "Run%", "Pass%", "Punt%", "FGA%"))
        })
        
        # Create table from filtered dataset for use with all league stat renders
        leag_sbs_dat_summ <- reactive({
            if(input$seasInput == "All"){
                leag_dat() %>%
                    group_by(
                        season
                    ) %>%
                    summarise(
                        # games_coached = length(unique(game_id)),
                        plays_called = length(play_id),
                        run_plays = sum(run),
                        pass_plays = sum(pass),
                        punt_plays = sum(punt),
                        field_goal_att = sum(field_goal)
                    ) %>%
                    mutate(
                        run_pct = percent(run_plays/plays_called, accuracy = 1),
                        pass_pct = percent(pass_plays/plays_called, accuracy = 1),
                        punt_pct = percent(punt_plays/plays_called, accuracy = 1),
                        fga_pct = percent(field_goal_att/plays_called, accuracy = 1),
                        plays_called = dollar(plays_called, prefix = "", big.mark=","),
                        run_plays = dollar(run_plays, prefix = "", big.mark=","),
                        pass_plays = dollar(pass_plays, prefix = "", big.mark=","),
                        punt_plays = dollar(punt_plays, prefix = "", big.mark=","),
                        field_goal_att = dollar(field_goal_att, prefix = "", big.mark=",")
                    ) %>%
                    'colnames<-' (c("Season", "Play Calls", "Runs", "Passes", "Punts", "FGAs", "Run%", "Pass%", "Punt%", "FGA%"))
            }
            else{
                leag_dat() %>%
                    group_by(
                        season
                    ) %>%
                    subset(
                        season == input$seasInput
                    ) %>%
                    summarise(
                        # games_coached = length(unique(game_id)),
                        plays_called = length(play_id),
                        run_plays = sum(run),
                        pass_plays = sum(pass),
                        punt_plays = sum(punt),
                        field_goal_att = sum(field_goal)
                    ) %>%
                    mutate(
                        run_pct = percent(run_plays/plays_called, accuracy = 1),
                        pass_pct = percent(pass_plays/plays_called, accuracy = 1),
                        punt_pct = percent(punt_plays/plays_called, accuracy = 1),
                        fga_pct = percent(field_goal_att/plays_called, accuracy = 1),
                        plays_called = dollar(plays_called, prefix = "", big.mark=","),
                        run_plays = dollar(run_plays, prefix = "", big.mark=","),
                        pass_plays = dollar(pass_plays, prefix = "", big.mark=","),
                        punt_plays = dollar(punt_plays, prefix = "", big.mark=","),
                        field_goal_att = dollar(field_goal_att, prefix = "", big.mark=",")
                    ) %>%
                    'colnames<-' (c("Season", "Play Calls", "Runs", "Passes", "Punts", "FGAs", "Run%", "Pass%", "Punt%", "FGA%")) 
            }
        })
        
        # League num of plays called
        output$leag_plays <- renderText({
            leag_dat_summ()$'Play Calls'
        })
        
        # League run percentage
        output$leag_runpct <- renderText({
            leag_dat_summ()$'Run%'
        })
        
        # League pass percentage
        output$leag_passpct <- renderText({
            leag_dat_summ()$'Pass%'
        })
        
        # League punt percentage
        output$leag_puntpct <- renderText({
            leag_dat_summ()$'Punt%'
        })
        
        # League field goal attempt percentage
        output$leag_fgapct <- renderText({
            leag_dat_summ()$'FGA%'
        })
        
        # League Total count and rate table
        output$leag_tot_stats <- renderTable({
            leag_dat_summ()
        })
        
        # League Season count and rate table
        output$leag_sbs_stats <- DT::renderDataTable(
            leag_sbs_dat_summ(), options = list(dom = 't', pageLength = 100), rownames = FALSE
        )
        
        # League Trend Plot
        leag_sbs_trend <- reactive({
            leag_dat() %>%
                group_by(
                    season
                ) %>%
                summarise(
                    # games_coached = length(unique(game_id)),
                    plays_called = as.numeric(length(play_id)),
                    run_plays = sum(run),
                    pass_plays = sum(pass),
                    punt_plays = sum(punt),
                    field_goal_att = sum(field_goal)
                ) %>%
                mutate(
                    run_pct = run_plays/plays_called,
                    pass_pct = pass_plays/plays_called,
                    punt_pct = punt_plays/plays_called,
                    fga_pct = field_goal_att/plays_called,
                )
        })
        
        output$leagplot <- renderPlot({
            
            leag_sbs_trend <- data.frame(leag_sbs_trend())
            
            ggplot(data = leag_sbs_trend, aes(x = season)) +
                geom_line(aes(y = run_pct, colour = "Run"), size=1.3) + 
                geom_line(aes(y = pass_pct, color="Pass"), size=1.3) +
                geom_line(aes(y = punt_pct, color="Punt"), size=1.3) +
                geom_line(aes(y = fga_pct, color="FGA"), size=1.3) +
                # scale_color_discrete(name = "Plays", labels = c("Run", "Pass", "Punt", "FGA")) +
                scale_colour_manual("Play Type", 
                                    breaks = c("Run", "Pass", "Punt", "FGA"),
                                    values = c("#4E84C4", "#D16103", "#52854C", "#FFDB6D")) +
                xlim(min(leag_sbs_trend$season)-1, max(leag_sbs_trend$season)+1) +
                xlab("Season") +
                scale_y_continuous(labels=scales::percent) +
                ylab("Percent of Play Calls") +
                theme(legend.key=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
