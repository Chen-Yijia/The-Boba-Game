# remotes::install_github("deepanshu88/summaryBox")
library(shiny)
library(summaryBox)

helpPage <- fluidPage(
  "hello",
class="first", textInput("txtq", "Enter the text to display below:"),
      textOutput("a")
) 


NUM_PAGES <- 6

pg1 <- div(fluidPage(
  
  column(3,div(uiOutput(outputId = "logoimg"),
               div(h1("Team members:")),
               div(class="member-names",h3("Tan Shu Yi 1005150")),
               div(class="member-names",h3("Chen Yijia 1004869")),
               div(class="member-names",h3("Feng Zhuoer 1004871")),
               div(class="member-names",h3("Ethan Pang 1005197")),
               
  )),
  
  column(9, offset = 0.5, 
         div(class = "panel-header", h1("Introduction",align="center")),
         div(class="help-page-element",
             div(
               class = "help-page-content",
               "Our team’s game aims to provide players with a chance to explore pricing and inventory management strategies to get themselves a competitive edge in the industry. This gives players a risk-free opportunity to estimate and react to the stochastic nature of demand. Apart from learning the different inventory management strategies, players can gain a deeper insight into business operations and development.",
               div(uiOutput(outputId = "labelimg"), align="center"
               ))
             
         ),
  )
  
))

pg2 <- div(fluidPage(
  div(class = "panel-header", h1("Our system",align="center")),
  div(
    class="help-page-element",
    div(
      column(6,
             div(uiOutput(outputId = "portionimg")
             ))),
    column(6,
           div(uiOutput(outputId = "ingpriceimg"), align="center"
           ))),
),
)

pg3 <- div(fluidPage(
  div(class = "panel-header", h1("Previous data",align="center")),
  div(
    class="help-page-element",
    div(
      fluidRow(
        column(6,
               div(uiOutput(outputId = "demanddayimg")
               )),
        column(6,
               class="page3-content",
               div(class="align-top-para","The demand data from the past 30 days follows a normal distribution. The demand for milk tea (indicated in blue) has a mean of 500 cups per day whereas the demand for the fruit tea (indicated in pink) has a mean of 350 cups per day", align="justify"
               )))),
    div(
      fluidRow(
        column(6,
               class="page3-content",
               div(class="align-para","As for economic considerations, we will be applying the law of demand. Observing from the graphs on the right, the demand of drinks varies inversely with price. For milk tea, the demand equation is given by demand=8000/price squared. For fruit tea, the demand equation is given by demand=12600/price squared.", align="justify"
               )),
        column(6,
               div(uiOutput(outputId = "demandpriceimg")
               ))))
  ))
)

pg4 <- div(fluidPage(
  div(class = "panel-header", h1("Game functions",align="center")),
  div(
    class="help-page-element-extra1",
    div(
      column(12,
             div(class="page6-content", 
                 div(uiOutput(outputId = "faqimg"), align="center"
                 )))),
    column(12,
           div(class="page6-content", 
               div(uiOutput(outputId = "functionimg"), align="center"
               ))))
)
)

pg5 <- div(fluidPage(
  div(class = "panel-header", h1("Game instructions",align="center")),
  div(
    class="help-page-element-extra",
    div(class = "pic-align",
        column(6, h3("Step 1", align="center"),
               div(uiOutput(outputId = "checkdemand")
               )),
        column(6, h3("Step 2", align="center"),
               div(uiOutput(outputId = "pricedemand")
               ))),
    div(class = "pic-align",
        column(6, h3("Step 3a", align="center"),
               div(uiOutput(outputId = "mtmenu")
               )),
        column(6, h3("Step 3b", align="center"),
               div(uiOutput(outputId = "ftmenu")
               ))),
    div(class = "pic-align",
        column(6, h3("Step 4", align="center"),
               div(uiOutput(outputId = "actionpanel")
               )),
        column(6, h3("Step 5", align="center"),
               div(uiOutput(outputId = "confirmorder")
               ))),
    div(class = "pic-align",
        column(6, h3("Step 6", align="center"),
               div(uiOutput(outputId = "deficiency")
               )),
        column(6, h3("Step 7", align="center"),
               div(uiOutput(outputId = "ordersuccess")
               ))),
    div(class = "pic-align",
        column(6, h3("Step 8", align="center"),
               div(uiOutput(outputId = "notifications")
               )),
        column(6, h3("Step 9", align="center"),
               div(uiOutput(outputId = "scorebar")
               )))
  )
),
)

pg6 <- div(fluidPage(
  div(class = "panel-header", h1("Point system",align="center")),
  div(
    class="help-page-element",
    div(
      column(12,
             div(class="page6-content", 
                 div(uiOutput(outputId = "scoreimg"), align="center"
                 )))),
    column(12,
           div(class="page3-content", 
               div(class = "align-top-para","The score is calculated based on the formula indicated in the above table, with a fair weightage for each metric taken into consideration. The aim of this is so that players can strike a balance between earning profit, fulfilling demands and reducing wastage. The score will determine your ranking in the leaderboard.", align="justify"
               ))),)
),
)
  
  
pagelist <- c(pg1,pg2,pg3, pg4, pg5, pg6)

multiplePage <- fluidPage(
  useShinyjs(),
  hidden(
    lapply(seq(NUM_PAGES), function(i) {
      div(
        class = "page",
        id = paste0("step", i),
        pagelist[names(pagelist) == "children"][i],
      )
    })
  ),
  br(),
  div(class="prev-next-btns",circleButton("prevBtn", "<"),
      circleButton("nextBtn", ">"))
)




