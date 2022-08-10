
renderimagesActionDisplay <- function(output) {
  output$iceimg <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/2458/2458132.png", height = "100%", width = "100%")
  })
  output$milkimg <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/372/372973.png", height = "100%", width = "100%")
  })
  output$teaimg <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/714/714113.png", height = "100%", width = "100%")
  })
  output$pearlimg <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/7519/7519289.png", height = "100%", width = "100%")
  })
  output$fruitsimg <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/1625/1625048.png", height = "100%", width = "100%")
  })
  output$jellyimg <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/184/184579.png", height = "100%", width = "100%")
  })
  output$milkteaimg <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/3081/3081162.png", height = "100%", width = "100%")
  })
  output$fruitsteaimg <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/5304/5304705.png", height = "100%", width = "100%")
  })


  output$iceimg1 <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/2458/2458132.png", height = "70%", width = "70%")
  })
  output$milkimg1 <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/372/372973.png", height = "70%", width = "70%")
  })
  output$teaimg1 <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/714/714113.png", height = "70%", width = "70%")
  })
  output$pearlimg1 <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/7519/7519289.png", height = "70%", width = "70%")
  })
  output$fruitsimg1 <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/1625/1625048.png", height = "70%", width = "70%")
  })
  output$jellyimg1 <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/184/184579.png", height = "70%", width = "70%")
  })
  output$milkteaimg1 <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/3081/3081162.png", height = "70%", width = "70%")
  })
  output$fruitsteaimg1 <- renderUI({
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/5304/5304705.png", height = "70%", width = "70%")
  })
}

renderplotsHistoryPrice <- function(input, output, historyDemand) {
  Demand_FT <- data2$Demand_FT
  Price_FT <- data2$Price_FT
  Demand_MT <- data2$Demand_MT
  Price_MT <- data2$Price_MT

  output$currentdata <- renderPlot(
    {
      ggplot(data2, aes(x = Price_MT, y = Demand_MT)) +
        geom_line() +
        labs(x = "Price for milk tea", y = "Demand for milk tea") +
        theme(plot.background = element_rect(fill = "#FFF89A"), axis.title = element_text(size = 14, face = "bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ) +
        geom_vline(xintercept = input$mtpriceinteger, color = "#FF8AAE", size = 2) +
        geom_point(x = input$mtpriceinteger, y = Demand_MT[which(Price_MT == input$mtpriceinteger)], shape = 1, size = 8) +
        annotate("text", x = 8, y = 13000, label = paste0("price: ", input$mtpriceinteger)) +
        annotate("text", x = 8, y = 12000, label = paste0("demand: ", Demand_MT[which(Price_MT == input$mtpriceinteger)])) +
        ylim(0, 1500)
    },
    height = 266
  )

  output$currentdata2 <- renderPlot(
    {
      ggplot(data2, aes(x = Price_FT, y = Demand_FT)) +
        geom_line() +
        labs(x = "Price for fruit tea", y = "Demand for fruit tea") +
        theme(plot.background = element_rect(fill = "#FFF89A"), axis.title = element_text(size = 14, face = "bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ) +
        geom_vline(xintercept = input$ftpriceinteger, color = "#FF8AAE", size = 2) +
        geom_point(x = input$ftpriceinteger, y = Demand_FT[which(Price_FT == input$ftpriceinteger)], shape = 1, size = 8) +
        annotate("text", x = 10, y = 13000, label = paste0("price: ", input$ftpriceinteger)) +
        annotate("text", x = 10, y = 12000, label = paste0("demand: ", Demand_FT[which(Price_FT == input$ftpriceinteger)])) +
        ylim(0, 1500)
    },
    height = 266
  )
}

diaplyScoreBarText <- function(output, scoreBar) {
  output$leveldayoutput <- renderText({
    paste0("Level ", scoreBar$level, " Day ", scoreBar$day)
  })

  output$accumprofitoutput <- renderText({
    if (scoreBar$accumProfit >= 0) {
      paste0("Accumulated profit: +$", scoreBar$accumProfit)
    } else {
      paste0("Accumulated profit: -$", abs(scoreBar$accumProfit))
    }
  })

  output$accumwastageoutput <- renderText({
    paste0("Accumulated waste: ", scoreBar$accumWastage * 100, "%")
  })

  output$accumscoreoutput <- renderText({
    paste0("Accumulated points: ", scoreBar$accumPoints, " pt")
  })
}


menuquitPopUp <- function(input) {
  # for the menu
  observeEvent(input$mtmenu, {
    shinyalert("Pearl milk tea", imageUrl = "https://cdn-icons-png.flaticon.com/512/3081/3081162.png", type = "info", html = T, text = tagList(mt_ing_price))
  })
  observeEvent(input$ftmenu, {
    shinyalert("Fruit tea", imageUrl = "https://cdn-icons-png.flaticon.com/512/5304/5304705.png", type = "info", html = T, text = tagList(ft_ing_price))
  })

  # for quit game
  observeEvent(input$quitgame, {
    shinyalert(
      class = "quit-alert", "Quit game", inputId = "quitalert", confirmButtonText = "OK", cancelButtonText = "Cancel", showCancelButton = TRUE,
      showConfirmButton = TRUE, type = "error", html = T, text = "Progress in this level will be lost. Are you sure you want to quit?"
    )
  })

  observeEvent(input$restart, {
    shinyalert(
      class = "restart-alert", "Restart game", inputId = "restartalert", confirmButtonText = "OK", cancelButtonText = "Cancel", showCancelButton = TRUE,
      showConfirmButton = TRUE, type = "error", html = T, text = "Progress in this level will be lost. Are you sure you want to restart?"
    )
  })
}

switchToSimulator <- function(input, session) {
  observeEvent(input$mapstart, {
    print("start game!!!")
    updateTabsetPanel(session, "tabset", selected = "simulator")
  })
}

switchToHome <- function(input, session) {
  observeEvent(input$startgame, {
    updateTabsetPanel(session, "tabset", selected = "home")
  })
}

displayPanelText <- function(output, clicked, sliderValues) {
  # for display slider input in the display panel
  output$icequantitydisplay <- renderText({
    if (clicked$neworderClick && !clicked$submitClick) {
      sliderValues()$Value[1]
    } else {
      ""
    }
  })

  output$milkquantitydisplay <- renderText({
    if (clicked$neworderClick && !clicked$submitClick) {
      sliderValues()$Value[2]
    } else {
      ""
    }
  })
  output$teaquantitydisplay <- renderText({
    if (clicked$neworderClick && !clicked$submitClick) {
      sliderValues()$Value[3]
    } else {
      ""
    }
  })
  output$pearlquantitydisplay <- renderText({
    if (clicked$neworderClick && !clicked$submitClick) {
      sliderValues()$Value[4]
    } else {
      ""
    }
  })
  output$fruitsquantitydisplay <- renderText({
    if (clicked$neworderClick && !clicked$submitClick) {
      sliderValues()$Value[5]
    } else {
      ""
    }
  })
  output$jellyquantitydisplay <- renderText({
    if (clicked$neworderClick && !clicked$submitClick) {
      sliderValues()$Value[6]
    } else {
      ""
    }
  })
  output$mtpricedisplay <- renderText({
    if (clicked$neworderClick && !clicked$submitClick) {
      paste0("$ ", sliderValues()$Value[7])
    } else {
      ""
    }
  })
  output$ftpricedisplay <- renderText({
    if (clicked$neworderClick && !clicked$submitClick) {
      paste0("$ ", sliderValues()$Value[8])
    } else {
      ""
    }
  })
}

renderimagesHelpPage <- function(output) {
  output$logoimg <- renderUI({
    tags$img(src = "https://i.ibb.co/gdqnLSs/image.png", height = "60%", width = "80%")
  })

  output$labelimg <- renderUI({
    tags$img(src = "https://i.ibb.co/fvvn1GV/Screenshot-2022-06-29-at-9-20-22-PM.png")
  })

  output$portionimg <- renderUI({
    tags$img(src = "https://i.ibb.co/CHxNzqF/image.png", height = "60%", width = "80%")
  })

  output$ingpriceimg <- renderUI({
    tags$img(src = "https://i.ibb.co/7vkTFhc/Screenshot-2022-06-29-at-9-38-28-PM.png", height = "60%", width = "80%")
  })

  output$demanddayimg <- renderUI({
    tags$img(src = "https://i.ibb.co/M9wLT48/plot.png")
  })

  output$demandpriceimg <- renderUI({
    tags$img(src = "https://i.ibb.co/n00FcRf/plot2.png", height = "55%", width = "80%")
  })

  output$gameinputimg <- renderUI({
    tags$img(src = "https://i.ibb.co/5KPKb1L/Screenshot-2022-06-30-at-12-26-30-AM.png", height = "80%", width = "100%")
  })

  output$procedureimg <- renderUI({
    tags$img(src = "https://i.ibb.co/rtwJHKD/Screenshot-2022-06-30-at-12-56-20-AM.png", height = "100%", width = "100%")
  })

  output$procedureimg <- renderUI({
    tags$img(src = "https://i.ibb.co/8Bkqs6C/esa-game-layout.png", height = "100%", width = "100%")
  })

  output$checkdemand <- renderUI({
    tags$img(src = "https://i.ibb.co/FW5YZjf/Check-Demand.png")
  })
  output$pricedemand <- renderUI({
    tags$img(src = "https://i.ibb.co/5Rr47Y7/Price-Demand.png")
  })
  output$mtmenu <- renderUI({
    tags$img(src = "https://i.ibb.co/JkFGTVM/Milk-Tea-menu.png")
  })
  output$ftmenu <- renderUI({
    tags$img(src = "https://i.ibb.co/6yQJ9s8/Fruit-Tea-menu.png")
  })
  output$actionpanel <- renderUI({
    tags$img(src = "https://i.ibb.co/4pdcFjM/Action-Panel.png")
  })
  output$confirmorder <- renderUI({
    tags$img(src = "https://i.ibb.co/nQNQRK8/Confirm-Order.png")
  })
  output$deficiency <- renderUI({
    tags$img(src = "https://i.ibb.co/bgXKKVV/Deficiency-Pop-up.png")
  })
  output$ordersuccess <- renderUI({
    tags$img(src = "https://i.ibb.co/HDr50Sj/Order-Success-page.png")
  })
  output$notifications <- renderUI({
    tags$img(src = "https://i.ibb.co/p1wWYvq/Notifications.png")
  })
  output$scorebar <- renderUI({
    tags$img(src = "https://i.ibb.co/HDrC24h/profit-waste-points.png")
  })

  output$functionimg <- renderUI({
    tags$img(src = "https://i.ibb.co/NSz6NML/Screenshot-2022-08-10-at-10-38-44-AM.png", height = "60%", width = "60%")
  })

  output$scoreimg <- renderUI({
    tags$img(src = "https://i.ibb.co/kJy1SfW/image.png")
  })

  output$faqimg <- renderUI({
    tags$img(src = "https://i.ibb.co/nDV8zWH/image.png", height = "60%", width = "60%")
  })
}

loginRelatedServer <- function(input, output, session, vals, master) {
  # Fire some code if the user clicks the Register button
  observeEvent(input$register, {
    showModal(passwordModal(failed = FALSE))
  })
  # Fire some code if the user clicks the passwordok button
  observeEvent(input$passwordok, {
    # Check that password1 exists and it matches password2
    if (str_length(input$password1) > 0 && (input$password1 == input$password2)) {
      # store the password and close the dialog
      vals$playername <- input$playername
      vals$password <- input$password1
      print(vals$playername) # for debugging

      if (!is.null(vals$playername)) {
        success <- registerPlayer(vals$playername, vals$password)
        if (success) {
          vals$playerid <- getPlayerID(vals$playername, vals$password)
          print(vals$playerid)
          master$login <- TRUE
          removeModal()
        } else {
          showModal(passwordModal(failed = TRUE))
        }
      }
    } else {
      showModal(passwordModal(failed = TRUE))
    }
  })
  # Fire some code if the user clicks the Login button
  observeEvent(input$login, {
    showModal(loginModal(failed = FALSE))
  })
  # Fire some code if the user clicks the loginok button
  observeEvent(input$loginok, {
    # Get the playerID and check if it is valid
    playerid <- getPlayerID(input$playername, input$password3)
    if (playerid > 0) {
      # store the playerid and playername and close the dialog
      vals$playerid <- playerid
      # print(vals$playerid) # for debugging
      vals$playername <- input$playername
      removeModal()
      master$login <- TRUE
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })
  output$loggedInAs <- renderUI({
    if (is.null(vals$playername)) {
      "Not logged in yet."
    } else {
      vals$playername
    }
  })
}
