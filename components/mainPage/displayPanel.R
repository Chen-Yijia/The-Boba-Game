
displayPanel <- column(
  3,
  offset=1,
  class="current-day-display-panel",
  div(class = "panel-header", h5("Current input for quantity and price")),
  wellPanel(
    class="current-data-body",
    
    div(
      class= "current-price",
      div(class="current-data-header",h4("Current beverage prices")),
      
      # current beverage price
      div(
        class="current-price-two",
        div(
          class = "current-data-element-wrap",
          div(class = "img-container",
              uiOutput(class = "currentpriceimg", outputId = "milkteaimg1")),
          # h5("$ 5")
          h5(textOutput("mtpricedisplay"))
          ),
        
        
        div(
          class = "current-data-element-wrap",
          div(class = "img-container",
              uiOutput(class = "currentpriceimg", outputId = "fruitsteaimg1")),
          h5(textOutput("ftpricedisplay"))),
      ),
    ),
    
    
    
    
    # current ingredient quantity
    
    div(
      
      class="current-quantity",
      
      div(class="current-data-header",h4("Current ingredient quantities")),
      
      div(
        class="current-quantity-three",
        div(
          class = "current-data-element-wrap",
          div(class = "img-container",
              uiOutput(class = "currentquantityimg", outputId = "iceimg1"),),
          h5(textOutput("icequantitydisplay"))),
        
        div(
          class = "current-data-element-wrap",
          div(class = "img-container",
              uiOutput(class = "currentquantityimg", outputId = "milkimg1"),),
          h5(textOutput("milkquantitydisplay"))),
        
        div(
          class = "current-data-element-wrap",
          div(class = "img-container",
              uiOutput(class = "currentquantityimg", outputId = "teaimg1"),),
          h5(textOutput("teaquantitydisplay"))),
      ),
      
      
      div(
        class="current-quantity-three",
        div(
          class = "current-data-element-wrap",
          div(class = "img-container",
              uiOutput(class = "currentquantityimg", outputId = "pearlimg1"),),
          h5(textOutput("pearlquantitydisplay"))),
        
        div(
          class = "current-data-element-wrap",
          div(class = "img-container",
              uiOutput(class = "currentquantityimg", outputId = "fruitsimg1"),),
          h5(textOutput("fruitsquantitydisplay"))),
        
        div(
          class = "current-data-element-wrap",
          div(class = "img-container",
              uiOutput(class = "currentquantityimg", outputId = "jellyimg1"),),
          h5(textOutput("jellyquantitydisplay"))),
      ),
    ),
    
    
  ),

  div(class = "extra-line-display"),
  popup <- fluidPage(useShinyalert(), 
  disabled(actionBttn(
    inputId = "submit",
    label = "Confirm start",
    style = "pill",
    icon = icon("fa-solid fa-play"),
    block = TRUE
  ))),
)

