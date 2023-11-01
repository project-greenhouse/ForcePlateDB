library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(hawkinR)
library(tidyverse)
library(dplyr)
library(lubridate)
library(shinycustomloader)
library(shinycssloaders)
library(ggthemes)
#library(plotly)
library(DT)
library(echarts4r)

# Source files -----
source("/R/jumpFunctions.R")

# Template -----
## Header -----
header <- dashboardHeader(
    ### Title -----
    title = "hawkinR Dashboard", 
    #### Title width -----
    titleWidth = 300,
    ### User Icon -----
    userOutput("user"),
    ### Github Icon Link -----
    tags$li(
        class="dropdown", 
        tags$a(
            # link reference
            href="https://github.com/project-greenhouse/Load-Velocity-App",
            # icon
            icon("github"), 
            # display text
            "Source Code", 
            target="_blank"
        )
    )
)

## Sidebar -----
sidebar <- dashboardSidebar(
    width = 300,
    minified = FALSE,
    collapsed = FALSE,
    sidebarMenu(
        id="tabs",
        sidebarMenuOutput("menu")
    )
)

# UI -----
ui <- dashboardPage(
    skin = "black",
    header = header,
    sidebar = sidebar,
    ## Body -----
    dashboardBody(
        tabItems(
            ### Login Page -----
            tabItem(
                tabName = "loginPage",
                fluidPage(
                    titlePanel("Login"),
                    passwordInput("apiKey", "Access Key"),
                    actionButton("login", "Login"),
                    textOutput(outputId = "status")
                )
            ),
            ### Team Tab -----
            tabItem(
                tabName = "tab_team",
                h1("Team Dashboard"),
                dataTableOutput(outputId = "dfTeam") %>% withSpinner()
            ),
            ### Player Tab -----
            tabItem(
                tabName = "tab_player",
                fluidRow(
                    selectizeInput(
                        inputId = "athInput",
                        label = "Select Athlete:",
                        choices = c()
                    ),
                    textOutput(outputId = "playerText") %>% 
                        withSpinner(type = 7, color = '#4ec84b', proxy.height = "200px")
                ),
                fluidRow(
                    h1("Player Dashboard")
                )
            ),
            ### Jump Analysis Tab -----
            tabItem(
                tabName = "tab_ft",
                fluidPage(
                    column(
                        width = 3,
                        ##### Select  Athlete -----
                        selectizeInput(
                            inputId = "ftInputAth",
                            label = "Athlete:",
                            choices = c()
                        ),
                        uiOutput(outputId = "ftTableUI")
                    ),
                    column(
                        width = 9,
                        uiOutput(outputId = "ftPlotUI")
                    )
                ) 
            )
        )
    )
)


##--------------------##

# Server -----
server <- function(input, output, session) {
    
    ## Login Event -----
    
    ### Create reactive value -----
    valid <- reactiveVal(value = FALSE)
    
    ### Create reactive login -----
    access <- eventReactive(input$login, {
        # show loading spinner
        showPageSpinner()
        # Create object from password input
        key <- input$apiKey
        
        # Call for refresh token
        resp <- try(get_access(refreshToken = key), silent = TRUE)
        
        exp <- with_tz(as_datetime(as.numeric(Sys.getenv("accessToken_expiration"))), tzone = Sys.timezone())
        
        if (inherits(resp, "try-error")) {
            # Handle the error
            return(resp[[1]])
        } else {
            return(paste0("Success! Your token will expire at ", exp))
        }
    })
    
    ### Access Response Text -----
    output$status <- renderText({
        resp <- access()
        
        return(resp)
    })
    
    ### Login exposure -----
    observe({
        # get access reactive
        acc <- access()
        
        # Check the returned expiration token for valid access time
        if (grepl("Success!", acc)) {
            valid(TRUE)
        }
        
        # If login successful show tabs
        if(valid()) {
            # Show the main dashboard
            updateTabItems(
                session = session,
                inputId = "tabs", 
                selected = "tab_team"
            )
        }

    })
    
    #--------------------#
    
    ## Data -----
    
    ### Test Types -----
    tests <- data.frame(
        "name" = c("Countermovement Jump", "Squat Jump", "Isometric Test", "Drop Jump", "Free Run", "CMJ Rebound", "Multi Rebound", "Weigh In", "Drop Landing"),
        "abrv" = c("CMJ", "SJ", "ISO", "DJ", "FR", "CMJR", "MR", "WI", "DL"),
        "id" = c("7nNduHeM5zETPjHxvm7s", "QEG7m7DhYsD6BrcQ8pic", "2uS5XD5kXmWgIZ5HhQ3A","gyBETpRXpdr63Ab2E0V8", "5pRSUQVSJVnxijpPMck3", "pqgf2TPUOQOQs6r0HQWb", "r4fhrkPdYlLxYQxEeM78", "ubeWMPN1lJFbuQbAM97s", "rKgI4y3ItTAzUekTUpvR")
    )
    
    ### Get Teams -----
    teams <- eventReactive(input$login, {
        req(valid())
        
        df <- get_teams()
        
        return(df)
    })
    
    ### Get Groups -----
    groups <- eventReactive(input$login, {
        req(valid())
        
        df <- get_groups()
        
        return(df)
    })
    
    ### Get Athletes -----
    players <- eventReactive(input$login, {
        req(valid())
        
        df <- get_athletes()
        
        return(df)
    })
    
    ### Get Test Data -----
    # Creates separate data frames for each test type
    # Stored as objects with names "df"+abrv (dfCMJ, dfISO, dfSJ...)
    observeEvent(input$login, {
        req(valid())
        
        for( i in 1:9) {
            id <- tests$id[[i]]
            nm <- tests$abrv[[i]]
            
            x <- get_tests_type(typeId = id)
            
            assign(paste0("df",nm), x, envir = globalenv())
        }
        
        # hide loading spinner
        hidePageSpinner()
    })
    
    #--------------------#

    ## Header -----
    
    ### User Output -----
    output$user <- renderUser({
        
        dashboardUser(
            name = "Greenhouse Sports Performance", 
            image = "logo.png", 
            title = "Lauren Green",
            subtitle = "Author", 
            footer = p("Together We Grow", class = "text-center"),
            fluidRow(
                # Website
                dashboardUserItem(
                    width = 3,
                    socialButton(
                        href = "https://www.greenhousesp.com",
                        icon = icon("home")
                    )
                ),
                # Github
                dashboardUserItem(
                    width = 3,
                    socialButton(
                        href = "https://github.com/project-greenhouse",
                        icon = icon("square-github")
                    )
                ),
                # Instagram
                dashboardUserItem(
                    width = 3,
                    socialButton(
                        href = "https://www.instagram.com/greenhouse_sp/",
                        icon = icon("square-instagram")
                    )
                ),
                #YouTube
                dashboardUserItem(
                    width = 3,
                    socialButton(
                        href = "https://www.youtube.com/@greenhouseperformance",
                        icon = icon("square-youtube")
                    )
                )
            )
        )
    })
    
    #--------------------#
    
    ## Sidebar -----   
    
    ### Dynamic Menu -----
    output$menu <- renderMenu({
        # Check the entered username and password against database or hardcoded values
        if (valid()) {
            # Show the main sidebar
            sidebarMenu( id = "tabs",
                # Login Page
                menuItem("Login", tabName = "loginPage", icon = icon("home"), selected = TRUE),
                # Tab 1
                menuItem("Team", tabName = "tab_team", icon = icon("people-arrows")),
                # Tab 2
                menuItem("Player", tabName = "tab_player", icon = icon("person-walking")),
                # Tab 3
                menuItem("Test Analysis", tabName = "tab_ft", icon = icon("chart-line")),
                hr(),
                # Refresh Button
                actionButton(
                    inputId = "refresh",
                    label = "Refresh Data",
                    icon = icon("rotate")
                )
            )
        } else {
            # hide main sidebar
            sidebarMenu(id="tabs",
                        menuItem("Home", tabName="loginPage", icon = icon("home"), selected = TRUE))
        }
    })
    
    #--------------------#
    
    ## Body -----
    
    ### Team Page -----
    output$dfTeam <- renderDataTable({
        teams()
    })
    
    #-----#
    
    ### Player Page -----
    
    #### Player selection -----
    observe({
        req(valid())
        
        df <- players()
        
        lst <- unique(df$name)
        
        updateSelectizeInput(
            session = session,
            inputId = "athInput",
            label = "Select Athlete:",
            choices = lst
        )
    })
    
    #### Player Text -----
    output$playerText <- renderText({
        t <- input$athInput
        
        return(t)
    })
    
    #-----#
    
    ### Force-Time Page -----
    
    #### Athletes -----
    observe({
        req(players())
        # get roster
        df <- players()
        
        # get names from roster
        lst <- df$name
        
        lst <- c("", lst)
        
        updateSelectizeInput(
            session = session,
            inputId = "ftInputAth",
            label = "Athlete:",
            choices = lst,
            selected = ""
        )
    })
    
    #### Run Tests -----
    runFT <- reactiveVal(value = FALSE)
    runGetFT <- reactiveVal(value = FALSE)
    
    #### Get Athletes Test -----
    ##### Check Run Tests -----
    observe({
        req(input$ftInputAth)
        # player selected
        p <- input$ftInputAth
        
        if(is.character(p)){
            runFT(TRUE)
        } else {
            runFT(FALSE)
        }
    })
    
    ##### Check Get Tests -----
    observe({
        req(input$ftTable_rows_selected)
        
        x <- input$ftTable_rows_selected
        
        if(x > 0) {
            runGetFT(TRUE)
        } else {
            runGetFT(FALSE)
        }
    })
    
    #####  Tests DF -----
    ftTests <- eventReactive(input$ftInputAth,{
        req(runFT())
        
        # get roster
        df <- players()
        
        # player selected
        p <- input$ftInputAth
        
        # filter for athlete ID
        aID <- df$id[df$name == p] 
        
        # get tests by athlete
        atest <- try(get_tests_ath(athleteId = aID), silent = TRUE)
        
        # Check if an error occurred
        tests <- if (inherits(atest, "try-error")) {
            data.frame(
                "Tests" = c("No tests found")
            )
        } else { # format df
            atest  %>% mutate("date" = date(with_tz(as_datetime(timestamp),tzone = Sys.timezone()))) %>%
                arrange(desc(timestamp))
        }
        
        return(tests)
    })
    
    #### FT UI Outputs -----
    ##### Tests Table -----
    output$ftTable <- renderDataTable({
        req(runFT())
        
        df <- ftTests()
        
        df <- df %>%
            rename(
                "Date" = date, 
                "Trial" = segment
            ) %>%
            select(
                Date, Trial
            )
        
        datatable(
            df,
            extensions = c('RowGroup', 'Scroller'),
            options = list(
                rowGroup = list(dataSrc = 1),
                deferRender = TRUE,
                scrollY = 600,
                scroller = TRUE,
                dom = "ti"
            ),
            selection = 'single'
        )
    })
    
    output$rows <- renderText({
        # selected test
        s <- input$ftTable_rows_selected
        
        return(s)
    })
    ##### Table UI -----
    output$ftTableUI <- renderUI({
        
        return(
            if(isFALSE(runFT())){
                h3("Select Athelte")
            } else{
                withLoader(dataTableOutput(outputId = "ftTable"), type = "html", loader = "loader6")
            } 
        )
    })
    
    ##### FT Plot -----
    output$ftPlot <- renderEcharts4r({
        
        # selected test
        s <- input$ftTable_rows_selected
        
        # tests df
        tests <- ftTests()
        
        # make call
        id <- tests[s,1]
        
        df <- get_forcetime(testId = as.character(id))
        
        # arrange data frame
        df2 <- df %>% 
            transmute(
                "t" = time_s,
                "Fr" = force_right,
                "Fl" = force_left,
                "Fc" = force_combined,
                "v" = velocity_m_s,
                "d" = displacement_m,
                "p" = power_w
            )
        
        maxF <- ceiling(max(df2$Fc)/1000)*1000
        maxT <- max(df2$t)
        
        # create plot
        plot <- df2 |>
            e_chart(t, name = "Time (s)") |>
            e_area(Fc, name = "Combined") |>
            e_line(Fl, name = "Left") |>
            e_line(Fr, name = "Right") |>
            # Customize the x-axis and y-axis limits
            e_x_axis(
                min = 0,  # Minimum x-axis limit
                max = maxT, # Maximum x-axis limit
                name = "Time (s)"  # Customize the x-axis label
            ) |>
            e_y_axis(
                min = 0,  # Minimum y-axis limit
                max = maxF,   # Maximum y-axis limit
                name = "Force (N)" # Customize the y-axis label
            ) |>
            # Customize the colors
            e_color(
                c("#0bb4ff", "#9b19f5", "#ffa300") # Define your custom colors
            ) |>
            # Apply a theme (if needed)
            e_theme("chalk") |>
            e_legend(bottom = TRUE) |>
            # Customize the tooltip
            e_tooltip(
                axisPointer = list(
                    type = "cross"
                )
            ) |>
            e_toolbox_feature(feature = "dataZoom")
        
        return(plot)
    })
    ##### Plot UI -----
    output$ftPlotUI <- renderUI({
        if(isFALSE(runGetFT())){
            return(
                h3("No tests")
            )
        } else{
            return(withLoader(echarts4rOutput(outputId = "ftPlot"), type = "html", loader = "loader3"))
        }
    })
    
    
}

shinyApp(ui, server)
