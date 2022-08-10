contentTagsEvent0 <- tagList(div("No event happens today! Just focus on your current business!"))

contentTagsEvent2 <- tagList(div("Your competitor brand KOI starts a promotion compaign, and many of your customers now switch to buy from KOI instead :( Your shop's demand has dropped by 20% because of this."))

contentTagsEvent5 <- tagList(div("The government has launched a health promotion comapign, which will result in 25% drop in milk tea demand, but at the same time, boost fruits tea sales by 25%. Do you want to align with this compaign?"),
                             div(
                               class="notification-btn",
                               actionBttn(
                                 inputId = "event5yes",
                                 label = "Yes",
                                 style = "simple", 
                                 color = "primary",
                               ),
                               actionBttn(
                                 inputId = "event5no",
                                 label = "No",
                                 style = "simple", 
                                 color = "primary",
                               ),
                             ))

contentTagsEvent5yes <- tagList(div("You have accepted the health compaign!"))
contentTagsEvent5no <- tagList(div("You have declined the health compaign!"))

contentTagsEvent6 <- tagList(div("There is a new supplier availble which offers cheaper ingredients, but it will incur you a penalty charge of $1200 to the previous supplier. Do you want to switch to this new cheaper supplier?"),
                             div(
                               class="notification-btn",
                               actionBttn(
                                 inputId = "event6yes",
                                 label = "Yes",
                                 style = "simple", 
                                 color = "primary",
                               ),
                               actionBttn(
                                 inputId = "event6no",
                                 label = "No",
                                 style = "simple", 
                                 color = "primary",
                               ),
                             ))

contentTagsEvent6yes <- tagList(div("You have accepted the new supplier offer!"))
contentTagsEvent6no <- tagList(div("You have declined the new supplier offer!"))

notificationBoxEvent0 <- summaryBox2(contentTagsEvent0, "A peaceful day!", width = 10, icon = "fas fa-bell", style = "info")                     
notificationBoxEvent2 <- summaryBox2(contentTagsEvent2, "Drop in sales!", width = 10, icon = "fas fa-bell", style = "info")
notificationBoxEvent5 <- summaryBox2(contentTagsEvent5, "Health promotion compaign!", width = 10, icon = "fas fa-bell", style = "info")
notificationBoxEvent5yes <- summaryBox2(contentTagsEvent5yes, "Health promotion compaign!", width = 10, icon = "fas fa-bell", style = "info")
notificationBoxEvent5no <- summaryBox2(contentTagsEvent5no, "Health promotion compaign!", width = 10, icon = "fas fa-bell", style = "info")
notificationBoxEvent6 <- summaryBox2(contentTagsEvent6, "Cheaper supplier available!", width = 10, icon = "fas fa-bell", style = "info")
notificationBoxEvent6yes <- summaryBox2(contentTagsEvent6yes, "Cheaper supplier available!", width = 10, icon = "fas fa-bell", style = "info")
notificationBoxEvent6no <- summaryBox2(contentTagsEvent6no, "Cheaper supplier available!", width = 10, icon = "fas fa-bell", style = "info")

renderNotificationBox <- function(day, eventDayList, accumEvent, rejectEvent) {
  todayEvent <- eventDayList[day]
  if (todayEvent == 0) {
    displayPage <- notificationBoxEvent0
  } else if (todayEvent == 2) {
    displayPage <- notificationBoxEvent2
  } else if (todayEvent == 5) {
    if (5 %in% accumEvent) {
      displayPage <- notificationBoxEvent5yes
    } else if (5 %in% rejectEvent){
      displayPage <- notificationBoxEvent5no
    } else {
      displayPage <- notificationBoxEvent5
    }
    
  } else if (todayEvent == 6) {
    if (6 %in% accumEvent) {
      displayPage <- notificationBoxEvent6yes
    } else if (6 %in% rejectEvent){
      displayPage <- notificationBoxEvent6no
    } else {
      displayPage <- notificationBoxEvent6
    }
  }
  
  displayPage
}
