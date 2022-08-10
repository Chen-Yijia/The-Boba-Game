


library(shiny)
library(summaryBox)
library(shinyWidgets)

# for the popup menus
mtmenu <- fluidPage(
  useShinyalert(), 
  div(actionButton("mtmenu", "Pricing menu for milk tea"), style = "margin: auto; width: 150px;")
)
ftmenu <- fluidPage(
  useShinyalert(), 
  div(actionButton("ftmenu", "Pricing menu for fruit tea"), style = "margin: auto; width: 150px;")
)

actionPanel <- column(8,
  # offset=1,
    class = "action-panel-body",

  div(
    class = "panel-header",
    h5("Select next day's data: ingredient quantity & beverage price"),
  ),
  
  
  # combine the quantity & price
  
  
  
  column(4,
         class = "quantity-selection-column",
         wellPanel(
           div(class = "quantity-select",
               tagList(
                 div(class = "img-container",
                     uiOutput(class = "quantityimg", outputId = "iceimg"),),
                 
                 sliderInput(
                   "iceinteger",
                   label = "",
                   ticks = FALSE,
                   min = 0,
                   max = 1500,
                   value = 750
                 )
               )),
           
           div(class = "quantity-select",
               tagList(
                 div(class = "img-container",
                     uiOutput(class = "quantityimg", outputId = "milkimg")),
                 
                 sliderInput(
                   "milkinteger",
                   label = "",
                   ticks = FALSE,
                   min = 0,
                   max = 1500,
                   value = 750
                 )
               )),
           div(class = "quantity-select",
               tagList(
                 div(class = "img-container",
                     uiOutput(class = "quantityimg", outputId = "teaimg")),
                 
                 sliderInput(
                   "teainteger",
                   label = "",
                   ticks = FALSE,
                   min = 0,
                   max = 1500,
                   value = 750
                 )
               )),
           div(class = "quantity-select",
               tagList(
                 div(class = "img-container",
                     uiOutput(class = "quantityimg", outputId = "pearlimg")),
                 
                 sliderInput(
                   "pearlinteger",
                   label = "",
                   ticks = FALSE,
                   min = 0,
                   max = 1500,
                   value = 750
                 )
               )),
           div(class = "quantity-select",
               tagList(
                 div(class = "img-container",
                     uiOutput(class = "quantityimg", outputId = "fruitsimg")),
                 
                 sliderInput(
                   "fruitsinteger",
                   label = "",
                   ticks = FALSE,
                   min = 0,
                   max = 1500,
                   value = 750
                 )
               )),
           div(class = "quantity-select",
               tagList(
                 div(class = "img-container",
                     uiOutput(class = "quantityimg", outputId = "jellyimg")),
                 
                 sliderInput(
                   "jellyinteger",
                   label = "",
                   ticks = FALSE,
                   min = 0,
                   max = 1500,
                   value = 750
                 )
               )),
         )),
  column(8,
         offset = 0.5,
         class = "price-selection-column",
         #milk tea column
         div(
           column(6,
                  class = "milktea-price-plot",
                  wellPanel(
                    div(class = "price-select",
                        tagList(
                          div(class = "img-container",
                              uiOutput(class = "priceimg", outputId = "milkteaimg"),),
                          
                          sliderInput(
                            "mtpriceinteger",
                            label = "",
                            ticks = FALSE,
                            min = 2,
                            max = 6,
                            value = 4
                          )
                        )),
                    mtmenu,
                    plotOutput("currentdata"),
                  )),
           
           #fruits tea column
           column(6,
                  class = "fruitstea-price-plot",
                  wellPanel(
                    div(class = "price-select",
                        tagList(
                          div(class = "img-container",
                              uiOutput(class = "priceimg", outputId = "fruitsteaimg"),),
                          
                          sliderInput(
                            "ftpriceinteger",
                            label = "",
                            ticks = FALSE,
                            min = 3,
                            max = 8,
                            value = 6
                          )
                        )),
                    ftmenu,
                    plotOutput("currentdata2"),
                  ))
         ), ),
  
  div(class = "extra-line"),
  actionBttn(
    inputId = "submitneworder",
    label = "Send order for the next day",
    style = "pill",
    icon = icon("fa-solid fa-paper-plane"),
    block = TRUE
  ),
  
  

  
)