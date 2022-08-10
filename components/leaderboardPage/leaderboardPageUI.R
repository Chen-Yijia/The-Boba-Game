

source("components/loginPage/loginPageUI.R")

leaderboardui <- fluidPage(
  fluidRow(div(class = "scoreboard-body",
               wellPanel(
                 uiOutput("leaderboardview"))
               )),
  # actionButton("register", "Register"),
  # actionButton("login", "Login"),
  # htmlOutput("loggedInAs"),
)

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "project004",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "project004",
    password = "V5KxyVLU")
  conn
}

publishScore <- function(playerid, score){
  conn <- getAWSConnection()
  if (is.null(playerid)) {
    querytemplate <- "INSERT INTO Leaderboard (PlayerID,Highscore) VALUES (?id1,?id2) ON DUPLICATE KEY UPDATE Highscore=?id2"
    query <- sqlInterpolate(conn, querytemplate,id1=as.integer(10),id2=score) # guest id
    print(query) #for debug
  } else {
    querytemplate <- "INSERT INTO Leaderboard (PlayerID,Highscore) VALUES (?id1,?id2)"
    query <- sqlInterpolate(conn, querytemplate,id1=playerid,id2=score)
    print(query) #for debug
  }
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      success <- TRUE
    }, error=function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  # dbDisconnect(conn)
  on.exit(dbDisconnect(conn), add=TRUE)
}

getLeaderBoard <- function(playerid,score){
  conn <- getAWSConnection()
  query <- "SELECT * FROM Leaderboardview"
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  result[,1] <- as.integer(result[,1])
  #print(c(playerid, score))
  if (is.null(playerid)) {
    print("inside null id")
    special_rank <- subset(result, as.numeric(result$PlayerID) == as.numeric(10) & as.numeric(result$Highscore) == as.numeric(score))$Ranking
  }
  else {
    special_rank <- subset(result, as.numeric(result$PlayerID) == as.numeric(playerid) & as.numeric(result$Highscore) == as.numeric(score))$Ranking
  }
  
  print("sepecial rank")
  print(special_rank)
  print("after rank")
  size <- nrow(result)
  if ((special_rank <= 5)) {
    result <- result[1:9,]
  } else if ((special_rank >= size-4) ){
    result <- result[(size-8):size,]
  } else {
    result <- result[(special_rank-4):(special_rank+4),]
  }
  result <- subset(result,select=-PlayerID)
  # dbDisconnect(conn)
  print(result)
  on.exit(dbDisconnect(conn), add=TRUE)
  return(result)
}





 
