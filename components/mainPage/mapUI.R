
source("components/leaderboardPage/leaderboardPageUI.R")


renderHomeBackgroundPage <- function(day=1) {
  if (day == 1) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background1",
            actionButton("mapstart", class="map-start-btn",tags$img(src="https://i.ibb.co/4YvwqrG/image.png")),
      )))
  } else if (day ==2) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background2",
        actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==3) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background3",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==4) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background4",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==5) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background5",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==6) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background6",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==7) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background7",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==8) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background8",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==9) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background9",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==10) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background10",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==11) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background11",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==12) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background12",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==13) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background13",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day ==14) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background14",
            actionButton("mapstart", class="map-continue-btn", tags$img(src="https://i.ibb.co/tDx1P9F/continue-btn.png")))
      ))
  } else if (day > 14) {
    mapPage <- fluidPage(
      fluidRow(
        div(class="map-background-leaderboard",
            leaderboardui
      )))
  }
  mapPage
}



mapPage <- fluidPage(
  fluidRow(
    div(class="map-background",
        actionButton("mapstart", class="map-start-btn",tags$img(src="https://i.ibb.co/4YvwqrG/image.png")))
  )
  
)
