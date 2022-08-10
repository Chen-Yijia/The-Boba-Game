


source("components/mainPage/actionPanel.R")
source("components/mainPage/displayPanel.R")
source("components/mainPage/notification.R")



data2 <- read.csv("PriceDemand.csv")

renderMainPage <- function(day, eventDayList, accumEvent, rejectEvent) {
  mainPage <- fluidPage(
    shinyjs::useShinyjs(),
    header = tags$head(
      tags$link(rel = "stylesheet", href = "https://unpkg.com/nes.css/css/nes.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
    ),
    fluidRow(
      div(
        class = "day-number-and-matrix",
        div(class = "day", h3(textOutput("leveldayoutput"))),
        div(class = "accum-profit-word", h4(textOutput("accumprofitoutput"))),
        # div(class="accum-profit-bar", progressBar(id = "profit-prog-bar", value = 60, display_pct = T)),
        div(class = "accum-waste", h4(textOutput("accumwastageoutput"))),
        div(class = "accum-points", h4(textOutput("accumscoreoutput"))),
      ),
    ),
    fluidRow(
      class = "plot-and-notification",
      column(
        7,
        div(class = "panel-header", h5("Previous data")),
        wellPanel(
          class = "previous-data-plot",
          plotlyOutput("previousdata"),
        )
      ),
      column(5,
        offset = 0.5,
        renderNotificationBox(day, eventDayList, accumEvent, rejectEvent)
      )
    ),
    br(),

    # below is the display panel and action panel
    fluidRow(
      class = "current-and-next-day",
      actionPanel,
      displayPanel
    )
  )
  mainPage
}
