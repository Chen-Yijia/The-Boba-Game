quitgamePage <- fluidPage(
  fluidRow(
    div(class="quitgame-background",
        div(class="login-grid-container",
            div(useShinyalert(),actionButton("quitgame", class="quitgame-btn",tags$img(src="https://i.ibb.co/VTnQdvH/image.png"))),
            div(useShinyalert(),actionButton("restart", class="restart-btn",tags$img(src="https://i.ibb.co/C1njbMD/image.png")))))
    
  )
)