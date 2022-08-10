loginui <- fluidPage(
  # actionButton("register", "Register"),
  # actionButton("login", "Login"),
  fluidRow(
    div(class="login-background",
        div(class="login-grid-container",
            div(actionButton("login", class="register-btn",tags$img(src="https://i.ibb.co/pnHQF24/image.png"))),
            div(actionButton("register", class="register-btn",tags$img(src="https://i.ibb.co/X2vyBYN/image.png")))))
    
  )
)

renderloginLogged <- function(loginStatus) {
  if (loginStatus) {
    displayPage <- fluidPage(
      fluidRow(
        div(class="logged-in-background",
            div(class="welcome-player",h1(textOutput("playername"))),
            div(class="start-btn-container", actionButton("startgame", class="start-game-btn",tags$img(src="https://i.ibb.co/WV1vRdj/image.png"))))))
  }
  else {
    displayPage <- loginui
  }
  
  displayPage
}

passwordModal <- function(failed = FALSE) {
  modalDialog(
    title = "Create a new password",
    textInput("playername","Please input your username"),
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    if (failed)
      div(tags$b("Username is taken or the passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("passwordok", "OK")
    )
  )
}

loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("playername", "Enter your assigned Player Name", ""),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "project004",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "project004",
    password = "V5KxyVLU")
  # on.exit(dbDisconnect(conn), add = TRUE)
  conn
}


getPlayerID <- function(playername,password){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM PlayerNames WHERE PlayerName=?id1 AND Password=?id2;"
  print(conn)
  query<- DBI::sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  result <- dbGetQuery(conn,query)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    playerid <- result$PlayerID[1]
  } else {
    playerid <- 0
  }
  dbDisconnect(conn)
  playerid
}


createNewPlayerQuery <- function(conn,playername,password){
  querytemplate <- "INSERT INTO PlayerNames (playername,password) VALUES (?id1,?id2);"
  query<- DBI::sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
}
?sqlInterpolate

registerPlayer <- function(playername,password){
  conn <- getAWSConnection()
  query <- createNewPlayerQuery(conn,playername,password)
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  query2 <- "SELECT PlayerName FROM PlayerNames"
  result2 <- dbGetQuery(conn,query2)
  size2 <- length(result2)
  if (any(result2 == playername)==FALSE){
    while(!success & iter < MAXITER){
      iter <- iter+1
      tryCatch(
        
        {  # This is not a SELECT query so we use dbExecute
          result <- dbExecute(conn,query)
          print(result)
          success <- TRUE
        }, error=function(cond){print("registerPlayer: ERROR")
          print(cond)
          # The query failed, likely because of a duplicate playername
          playername <- getRandomPlayerName(conn)
          query <- createNewPlayerQuery(conn,playername,password) }, 
        warning=function(cond){print("registerPlayer: WARNING")
          print(cond)},
        finally = {print(paste0("Iteration ",iter," done."))
        }
      )
    } # end while loop
  }
  else {
    print("FAILED")
  }
  # This may not have been successful
  if (!success) playername = NULL
  #Close the connection
  on.exit(dbDisconnect(conn), add=TRUE)
  return(success)
}

