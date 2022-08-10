library(shiny)
library(shinyalert)

popup <- fluidPage(
       useShinyalert(),
       actionButton("start", "Start")
)

# slider
testcontent <- fluidPage(
       fluidRow(
              column(
                     4,
                     prettyCheckbox("milk", "Milk", FALSE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("tea", "Tea", FALSE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("ice", "Ice", FALSE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("fruits", "Fruits", FALSE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("pearl", "Pearl", FALSE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("jelly", "Jelly", FALSE, shape = "round"),
              ),
              column(
                     6,
                     # verbatimTextOutput("value"),
                     sliderInput("obs", "Number of units allocated for milk tea:",
                            min = 0, max = 20, value = 10
                     ),
              ),
              column(
                     6,
                     # verbatimTextOutput("value"),
                     sliderInput("obs", "Number of units allocated for milk tea:",
                            min = 0, max = 20, value = 10
                     )
              )
       )
)

# text box
testcontent2 <- fluidPage(
       fluidRow(
              column(
                     4,
                     prettyCheckbox("milk", "Milk", FALSE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("tea", "Tea", FALSE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("ice", "Ice", TRUE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("fruits", "Fruits", FALSE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("pearl", "Pearl", FALSE, shape = "round"),
              ),
              column(
                     4,
                     prettyCheckbox("jelly", "Jelly", FALSE, shape = "round"),
              ),
              column(
                     6,
                     textInput("testboxmt", "Number of units allocated for milk tea:"),
                     verbatimTextOutput("value"),
              ),
              column(
                     6,
                     textInput("testboxmt", "Number of units allocated for fruit tea:"),
                     verbatimTextOutput("value1")
              )
       )
)


mt_ing_price <- div(
       h2(
              "The ingredients required are as follows:",
              column(12, "Milk: $0.6"),
              column(12, "Tea: $0.3"),
              column(12, "Pearl: $0.5"),
              column(12, "Ice: $0.2")
       ),
       p("Note: There will be discount for ingredients when buying in bulk.")
)


ft_ing_price <- div(
       h2(
              "The ingredients required are as follows:",
              column(12, "Tea: $0.3"),
              column(12, "Fruits: $1.5"),
              column(12, "Jelly: $0.6"),
              column(12, "Ice: $0.2")
       ),
       p("Note: There will be discount for ingredients when buying in bulk.")
)


# the final deficiency pop up



renderDeficiencyPopUp <- function(total_possible, mt_possible, ft_possible) {
       deficiency_updated_mt <- fluidPage(
              shinyjs::useShinyjs(),
              h4(class = "deficiency-popup-text", paste0("Maximum cups of milk tea: ", mt_possible, ". Please enter your choice of cups of milk tea to make in the below field")),
              hr(class = "deficiency-divider"),
              fluidRow(
                     column(
                            6,
                            numericInput("mtuserdeficiency", label = "Number of cups of milk tea to make:", mt_possible, min = 0, max = mt_possible),
                            verbatimTextOutput("mtuservalue"),
                     ),
                     column(
                            6,
                            # textInput("ftuserdeficiency", label = "Number of cups of fruits tea to make:") %>% disabled(),
                            # numericInput("mtuserdeficiency","Number of cups of fruits tea to make:",calcPossibleFT(total_possible, mt_possible, ft_possible, mt_demand, ft_demand, mt_user = 123)),
                            h3(class = "ft-control-label", "Number of cups of fruits tea to make:"),
                            verbatimTextOutput("ftuservalue")
                     )
              )
       )
       deficiency_updated_mt
}
