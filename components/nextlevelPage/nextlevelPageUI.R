nextlevelPage <- fluidPage(
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://unpkg.com/nes.css/css/nes.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
  
  
  # below is my code: for displaying the performance measures
  fluidRow(
    div(
      class = "day-number-and-matrix", 
      div(class="day", h3("Level 1 Day 4")),
      div(class="accum-profit-word", h4("Accumulated profit: +$99")),
      div(class="accum-profit-bar", progressBar(id = "profit-prog-bar", value = 60, display_pct = T)),
      div(class="accum-waste", h4("Accumulated waste: -$45")),
      div(class="accum-points", h4("Accumulated points: 0")),
    ),
  )
)