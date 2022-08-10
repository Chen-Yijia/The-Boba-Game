#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DBI)
library(tidyverse)
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(summaryBox)
library(shinyalert)
library(plotly)
library(shinyjs)
source("components/mainPage/mainPageUI.R")
source("components/mainPage/mapUI.R")
source("components/popupPage/popupPageUI.R")
source("components/helpPage/helpPageUI.R")
source("components/loginPage/loginPageUI.R")
source("components/morePage/morePageUI.R")
source("serverFunctions/mainServer.R")
source("serverFunctions/mainFunctions.R")
source("components/leaderboardPage/leaderboardPageUI.R")

data <- read.csv("Historical Sale copy.csv")
day <- data$Day
demandmt <- round(data$Milktea, 2)
demandft <- round(data$Fruitstea, 2)
reviseddata <- data.frame(day, demandmt, demandft)

# Define UI for application that draws a histogram

ui <- navbarPage(
  id = "tabset",
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://unpkg.com/nes.css/css/nes.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
  ),
  "Boba shop",
  tabPanel(id = "logintab", "Sign in", value = "sign in", uiOutput("loginlogged")),
  tabPanel("Home",
    value = "home",
    uiOutput("homebackground")
  ),
  tabPanel("Help", value = "help", multiplePage),
  tabPanel(id = "moretab", "More", value = "more", quitgamePage),
  tabPanel(id = "mainpagetab", "", value = "simulator", uiOutput("mainPage")),
  tags$audio(src = "bgm.mp3", type = "audio/mp3", autoplay = NA, controls = NA)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  master <- reactiveValues(login = FALSE, accumEvent = c(0), historyDemand = reviseddata, rejectEvent = NULL)
  scoreBar <- reactiveValues(level = 1, day = 1, accumProfit = 0, accumWastage = 0, accumPoints = 0, dailyScore = NULL, accumQuantities = 0, accumLeftover = 0)
  clicked <- reactiveValues(neworderClick = FALSE, submitClick = FALSE, alertClick = FALSE)
  vals <- reactiveValues(password = NULL, playerid = NULL, playername = NULL, score = NULL, glory = NULL)
  dailyPurchase <- reactiveValues(total_possible = NULL, mt_possible = NULL, ft_possible = NULL, mt_demand = NULL, ft_demand = NULL, deficiency_user = NULL)
  eventDayList <- c(0, 0, 2, 0, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0)

  rv <- reactiveValues(page = 1)
  sliderValues <- reactive({
    data.frame(
      Name = c(
        "iceinteger",
        "milkinteger",
        "teainteger",
        "pearlinteger",
        "fruitsinteger",
        "jellyinteger",
        "mtpriceinteger",
        "ftpriceinteger"
      ),
      Value = as.character(c(
        input$iceinteger,
        input$milkinteger,
        input$teainteger,
        input$pearlinteger,
        input$fruitsinteger,
        input$jellyinteger,
        input$mtpriceinteger,
        input$ftpriceinteger
      )),
      stringsAsFactors = FALSE
    )
  })

  loginRelatedServer(input, output, session, vals, master)

  output$playername <- renderText({
    if (is.null(vals$playername)) {
      "Not logged in."
    } else {
      paste0("Welcome  ", as.character(vals$playername))
    }
  })


  # for auto updating deficiency
  output$ftuservalue <- renderText({
    if (!is.null(dailyPurchase$total_possible) && !is.null(dailyPurchase$mt_possible) && !is.null(dailyPurchase$ft_possible)) {
      dailyPurchase$deficiency_user <- calcPossibleFT(dailyPurchase$total_possible, dailyPurchase$mt_possible, dailyPurchase$ft_possible, dailyPurchase$mt_demand, dailyPurchase$ft_demand, input$mtuserdeficiency)
      print(dailyPurchase$deficiency_user)
      return(dailyPurchase$deficiency_user[2])
    }
  })

  observeEvent(input$restartalert, {
    if (input$restartalert) {
      # clear score bar
      scoreBar$level <- 1
      scoreBar$day <- 1
      scoreBar$accumProfit <- 0
      scoreBar$accumWastage <- 0
      scoreBar$accumPoints <- 0
      scoreBar$dailyScore <- NULL
      scoreBar$accumQuantities <- 0
      scoreBar$accumLeftover <- 0
      master$historyDemand <- reviseddata
      updateTabsetPanel(session, "tabset", selected = "home")
    }
  })

  observeEvent(input$quitalert, {
    if (input$quitalert) {
      # clear score bar
      scoreBar$level <- 1
      scoreBar$day <- 1
      scoreBar$accumProfit <- 0
      scoreBar$accumWastage <- 0
      scoreBar$accumPoints <- 0
      scoreBar$dailyScore <- NULL
      scoreBar$accumQuantities <- 0
      scoreBar$accumLeftover <- 0
      vals$password <- NULL
      vals$playerid <- NULL
      vals$playername <- NULL
      vals$score <- NULL
      vals$glory <- NULL
      master$login <- FALSE
      master$historyDemand <- reviseddata
      updateTabsetPanel(session, "tabset", selected = "sign in")
    }
  })



  # change alert defiiciency click status

  observeEvent(input$deficiencyalert, {
    clicked$alertClick <- input$deficiencyalert
    print(clicked$alertClick)
    print(input$deficiencyalert)
  })

  output$homebackground <- renderUI(
    renderHomeBackgroundPage(scoreBar$day)
  )

  output$mainPage <- renderUI(
    renderMainPage(scoreBar$day, eventDayList, master$accumEvent, master$rejectEvent)
  )

  output$loginlogged <- renderUI(
    renderloginLogged(master$login)
  )

  renderimagesActionDisplay(output)

  renderplotsHistoryPrice(input, output, master$historyDemand)


  observe({
    if (nrow(master$historyDemand) > 30) {
      reviseddata <- tail(master$historyDemand, -(nrow(master$historyDemand) - 30))
    } else {
      reviseddata <- master$historyDemand
    }
    # history plot
    output$previousdata <- renderPlotly({
      fig <- plot_ly(reviseddata, x = ~day, height = 230)
      fig <- fig %>% add_trace(y = ~demandmt, name = "Milk tea", type = "scatter", mode = "lines", line = list(color = "rgba(154, 220, 255, 1)", width = 3))
      fig <- fig %>% add_trace(y = ~demandft, name = "Fruits tea", mode = "lines", line = list(color = "rgba(255, 138, 174, 1)", width = 3))
      fig <- fig %>% layout(
        xaxis = list(title = "<b>Day</b>", showgrid = FALSE),
        yaxis = list(title = "<b>Demand</b>", showgrid = FALSE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        legend = list(x = 0.1, y = 1)
      )
      fig
    })
  })

  menuquitPopUp(input)

  displayPanelText(output, clicked, sliderValues)


  # disable submit if did not order
  observe({
    if (clicked$neworderClick) {
      shinyjs::enable("submit")
    } else {
      shinyjs::disable("submit")
    }
  })

  # handling deficiency click
  observe({
    if (clicked$alertClick) {

      # update display history demand
      newdaydemand <- data.frame(day = 30 + scoreBar$day, demandmt = dailyPurchase$mt_demand, demandft = dailyPurchase$ft_demand)
      master$historyDemand <- rbind(master$historyDemand, newdaydemand)

      # update day
      scoreBar$day <- scoreBar$day + 1

      # for updating the accum list for day 3
      if (scoreBar$day == 3) {
        master$accumEvent <- c(master$accumEvent, 2)
      }

      # switch back to home
      print("back to home!!!")
      updateTabsetPanel(session, "tabset", selected = "home")

      # prompt success
      shinyalert("Order submitted!", type = "success", html = T)

      scoreInfo <- calcScore(sliderValues, dailyPurchase$mt_demand, dailyPurchase$ft_demand, dailyPurchase$deficiency_user[1], master$accumEvent)

      print(scoreInfo)

      # update daily score dataframe
      if (is.null(scoreBar$dailyScore)) {
        scoreBar$dailyScore <- scoreInfo
      } else {
        scoreBar$dailyScore <- rbind(scoreBar$dailyScore, scoreInfo)
      }

      # update profit
      print(c(scoreBar$accumProfit, scoreInfo$Profit))
      scoreBar$accumProfit <- round(scoreBar$accumProfit + scoreInfo$Profit, 2)

      # update point
      scoreBar$accumPoints <- round(scoreBar$accumPoints + scoreInfo$Score, 2)

      # update wastage
      scoreBar$accumQuantities <- scoreBar$accumQuantities + scoreInfo$Total_Quantity
      scoreBar$accumLeftover <- scoreBar$accumLeftover + scoreInfo$Total_Leftover
      scoreBar$accumWastage <- round(scoreBar$accumLeftover / scoreBar$accumQuantities, 4)

      # recover states
      dailyPurchase$total_possible <- NULL
      dailyPurchase$t_possible <- NULL
      dailyPurchase$ft_possible <- NULL
      dailyPurchase$mt_demand <- NULL
      dailyPurchase$ft_demand <- NULL
      dailyPurchase$deficiency_user <- NULL
      clicked$alertClick <- FALSE

      if (scoreBar$day == 15) {
        print("test lb inside updating each day")
        vals$score <- as.numeric(scoreBar$accumPoints)
        print(vals$score)
        publishScore(vals$playerid, vals$score)
      }
    }
  })

  observeEvent(input$submit, {
    clicked$submitClick <- TRUE
    clicked$neworderClick <- FALSE
    shinyjs::enable("iceinteger")
    shinyjs::enable("milkinteger")
    shinyjs::enable("teainteger")
    shinyjs::enable("pearlinteger")
    shinyjs::enable("fruitsinteger")
    shinyjs::enable("jellyinteger")
    shinyjs::enable("mtpriceinteger")
    shinyjs::enable("ftpriceinteger")

    # check if need deficiency pop up
    popupInfo <- confirmStartPopUp(sliderValues, master$accumEvent)
    needPopup <- popupInfo[1]
    dailyPurchase$total_possible <- popupInfo[2]
    dailyPurchase$mt_possible <- popupInfo[3]
    dailyPurchase$ft_possible <- popupInfo[4]
    dailyPurchase$mt_demand <- popupInfo[5]
    dailyPurchase$ft_demand <- popupInfo[6]

    if (needPopup == 1) {
      print("deficiency")

      # deficiency alert
      shinyalert(
        class = "deficiency-alert", "Deficiency",
        inputId = "deficiencyalert",
        imageUrl = "https://i.ibb.co/TqyV8kr/Screenshot-2022-07-28-at-9-13-28-PM.png",
        type = "warning", html = T,
        text = tagList(renderDeficiencyPopUp(
          dailyPurchase$total_possible, dailyPurchase$mt_possible,
          dailyPurchase$ft_possible
        ))
      )
    } else {

      # update display history demand
      newdaydemand <- data.frame(day = 30 + scoreBar$day, demandmt = dailyPurchase$mt_demand, demandft = dailyPurchase$ft_demand)
      master$historyDemand <- rbind(master$historyDemand, newdaydemand)

      # update day
      scoreBar$day <- scoreBar$day + 1

      # for updating the accum list for day 3
      if (scoreBar$day == 3) {
        master$accumEvent <- c(master$accumEvent, 2)
      }

      # switch back to home
      print("back to home!!!")
      updateTabsetPanel(session, "tabset", selected = "home")

      # prompt success
      shinyalert("Order submitted!", type = "success", html = T)

      # no deficiency case
      scoreInfo <- calcScore(sliderValues, dailyPurchase$mt_demand, dailyPurchase$ft_demand, mt_user = "Indian", master$accumEvent)

      # update daily score dataframe
      if (is.null(scoreBar$dailyScore)) {
        scoreBar$dailyScore <- scoreInfo
      } else {
        scoreBar$dailyScore <- rbind(scoreBar$dailyScore, scoreInfo)
      }

      # update profit
      print(c(scoreBar$accumProfit, scoreInfo$Profit))
      scoreBar$accumProfit <- round(scoreBar$accumProfit + scoreInfo$Profit, 2)

      # update point
      scoreBar$accumPoints <- round(scoreBar$accumPoints + scoreInfo$Score, 2)

      # update wastage
      scoreBar$accumQuantities <- scoreBar$accumQuantities + scoreInfo$Total_Quantity
      scoreBar$accumLeftover <- scoreBar$accumLeftover + scoreInfo$Total_Leftover
      scoreBar$accumWastage <- round(scoreBar$accumLeftover / scoreBar$accumQuantities, 4)

      if (scoreBar$day == 15) {
        print("test lb inside updating each day")
        vals$score <- as.numeric(scoreBar$accumPoints)
        print(vals$score)
        publishScore(vals$playerid, vals$score)
      }
    }
  })


  observeEvent(input$submitneworder, {
    print(sliderValues())
    clicked$neworderClick <- TRUE
    clicked$submitClick <- FALSE
    shinyjs::disable("iceinteger")
    shinyjs::disable("milkinteger")
    shinyjs::disable("teainteger")
    shinyjs::disable("pearlinteger")
    shinyjs::disable("fruitsinteger")
    shinyjs::disable("jellyinteger")
    shinyjs::disable("mtpriceinteger")
    shinyjs::disable("ftpriceinteger")

    # enable confirm start button
    # shinyjs::enable("submit")
  })


  # help page
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
    hide(selector = ".page")
    show(paste0("step", rv$page))
  })

  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }

  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))

  renderimagesHelpPage(output)

  # start game
  switchToSimulator(input, session)
  switchToHome(input, session)

  # display score bar text
  diaplyScoreBarText(output, scoreBar)

  # notification button
  observeEvent(input$event5yes, {
    master$accumEvent <- c(master$accumEvent, 5)
  })

  observeEvent(input$event5no, {
    if (is.null(master$rejectEvent)) {
      master$rejectEvent <- c(5)
    } else {
      master$rejectEvent <- c(master$rejectEvent, 5)
    }
  })

  observeEvent(input$event6yes, {
    master$accumEvent <- c(master$accumEvent, 6)
    scoreBar$accumProfit <- scoreBar$accumProfit - 1200
  })

  observeEvent(input$event6no, {
    if (is.null(master$rejectEvent)) {
      master$rejectEvent <- c(6)
    } else {
      master$rejectEvent <- c(master$rejectEvent, 6)
    }
  })


  # React to completion of game
  output$score <- renderUI({
    if (is.null(vals$score)) {
      "No score yet."
    } else {
      as.character(vals$score)
    }
  })


  output$leaderboardview <- renderUI({
    tableOutput("leaderboard")
  })

  output$leaderboard <- renderTable({
    numclicks <- input$publishscore + input$playgame # to force a refresh whenever one of these buttons is clicked
    leaderboard <- getLeaderBoard(vals$playerid, vals$score)
    leaderboard
  })
}
# Run the application
shinyApp(ui = ui, server = server)
