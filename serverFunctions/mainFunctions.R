
confirmStartPopUp <- function(sliderValues, event_raw = c(0)){
  iceinteger <- as.integer(sliderValues()[1,2])
  milkinteger <- as.integer(sliderValues()[2,2])
  teainteger <- as.integer(sliderValues()[3,2])
  pearlinteger <- as.integer(sliderValues()[4,2])
  fruitsinteger <- as.integer(sliderValues()[5,2])
  jellyinteger <- as.integer(sliderValues()[6,2])
  mt_price <- as.integer(sliderValues()[7,2])
  ft_price <- as.integer(sliderValues()[8,2])
  
  if (sum(event_raw) == 0){
    event <- c(FALSE, FALSE, FALSE)
  } else if (sum(event_raw) == 2) {
    event <- c(TRUE, FALSE, FALSE)
  } else if (sum(event_raw) == 5) {
    event <- c(FALSE, TRUE, FALSE)
  } else if (sum(event_raw) == 6) {
    event <- c(FALSE, FALSE, TRUE)
  } else if (sum(event_raw) == 7) {
    event <- c(TRUE, TRUE, FALSE)
  } else if (sum(event_raw) == 8) {
    event <- c(TRUE, FALSE, TRUE)
  } else if (sum(event_raw) == 11) {
    event <- c(FALSE, TRUE, TRUE)
  } else if (sum(event_raw) == 13) {
    event <- c(TRUE, TRUE, TRUE)
  }
  
  milk_tea_demand <- function(mt_price){
    mt_mean <- 8000/mt_price^2
    temp <- as.integer(rnorm(1, mean = mt_mean, sd = 182))
    while (temp < 0)
    {
      temp <- as.integer(rnorm(1, mean = mt_mean, sd = 182))
    }
    return(temp)
  }
  
  fruit_tea_demand <- function(ft_price){
    ft_mean <- 12600/ft_price^2
    temp <- as.integer(rnorm(1, mean = ft_mean, sd = 152))
    while (temp < 0)
    {
      temp <- as.integer(rnorm(1, mean = ft_mean, sd = 152))
    }
    return(temp)
  }
  
  if (event[1] == FALSE & event[3] == FALSE) {
    mt_demand <- milk_tea_demand(mt_price)
    ft_demand <- fruit_tea_demand(ft_price)
  } else if (event[1] == TRUE & event[3] == FALSE) {
    mt_demand <- as.integer(milk_tea_demand(mt_price)*0.8)
    ft_demand <- as.integer(fruit_tea_demand(ft_price)*0.8)
  } else if (event[1] == FALSE & event[3] == TRUE) {
    mt_demand <- as.integer(milk_tea_demand(mt_price)*0.75)
    ft_demand <- as.integer(fruit_tea_demand(ft_price)*1.25)
  } else if (event[1] == TRUE & event[3] == TRUE) {
    mt_demand <- as.integer(milk_tea_demand(mt_price)*0.6)
    ft_demand <- as.integer(fruit_tea_demand(ft_price))
  }
  
  
  # Deficiency List
  deficiency_list <- function(mt_demand, 
                              ft_demand, 
                              teainteger, 
                              iceinteger, 
                              milkinteger,
                              pearlinteger,
                              fruitsinteger,
                              jellyinteger){
    judge <- TRUE
    
    judge_tea <- (teainteger >= mt_demand + ft_demand)
    judge_ice <- (iceinteger >= mt_demand + ft_demand)
    judge_milk <- (milkinteger >= mt_demand)
    judge_pearl <- (pearlinteger >= mt_demand)
    judge_fruit <- (fruitsinteger >= ft_demand)
    judge_jelly <- (jellyinteger >= ft_demand)
    
    judge <- 
      judge_tea & 
      judge_ice & 
      judge_milk & 
      judge_pearl & 
      judge_fruit & 
      judge_jelly
    
    judge_list = data.frame(deficiency_ingredients = c(judge_tea,
                                                       judge_ice,
                                                       judge_milk,
                                                       judge_pearl,
                                                       judge_fruit,
                                                       judge_jelly),
                            row.names = c('tea',
                                          'ice',
                                          'milk',
                                          'pearl',
                                          'fruit',
                                          'jelly')
    )
    deficiency_list <- row.names(subset(judge_list, deficiency_ingredients == FALSE))
    
    return(deficiency_list)
  }
  
  deficiency_ingre <- deficiency_list(mt_demand, ft_demand, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)
  
  require_pop <- "ice" %in% deficiency_ingre | "tea" %in% deficiency_ingre
  
  possible_amount <- function(mt_demand, 
                              ft_demand, 
                              teainteger, 
                              iceinteger, 
                              milkinteger,
                              pearlinteger,
                              fruitsinteger,
                              jellyinteger){
    # Tea and Ice restrict the total number of beverage possible:
    total_possible <- min(c(teainteger, iceinteger, mt_demand + ft_demand))
    # Milk and Pearl restrict the number of milk tea possible:
    mt_possible <- min(c(milkinteger, pearlinteger, teainteger, iceinteger, mt_demand))
    # Fruit and Jelly restrict the number of fruit tea possible:
    ft_possible <- min(c(fruitsinteger, jellyinteger, teainteger, iceinteger, ft_demand))
    return(c(total_possible, mt_possible, ft_possible))
  }
  
  possible_values <- possible_amount(mt_demand, ft_demand, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)
  
  c(require_pop, possible_values, mt_demand, ft_demand)
}

calcScore <- function(sliderValues, mt_demand, ft_demand, mt_user="Indian", event_raw = c(0)){
  iceinteger <- as.integer(sliderValues()[1,2])
  milkinteger <- as.integer(sliderValues()[2,2])
  teainteger <- as.integer(sliderValues()[3,2])
  pearlinteger <- as.integer(sliderValues()[4,2])
  fruitsinteger <- as.integer(sliderValues()[5,2])
  jellyinteger <- as.integer(sliderValues()[6,2])
  mt_price <- as.integer(sliderValues()[7,2])
  ft_price <- as.integer(sliderValues()[8,2])
  
  if (sum(event_raw) == 0){
    event <- c(FALSE, FALSE, FALSE)
  } else if (sum(event_raw) == 2) {
    event <- c(TRUE, FALSE, FALSE)
  } else if (sum(event_raw) == 5) {
    event <- c(FALSE, TRUE, FALSE)
  } else if (sum(event_raw) == 6) {
    event <- c(FALSE, FALSE, TRUE)
  } else if (sum(event_raw) == 7) {
    event <- c(TRUE, TRUE, FALSE)
  } else if (sum(event_raw) == 8) {
    event <- c(TRUE, FALSE, TRUE)
  } else if (sum(event_raw) == 11) {
    event <- c(FALSE, TRUE, TRUE)
  } else if (sum(event_raw) == 13) {
    event <- c(TRUE, TRUE, TRUE)
  }
  
  deficiency_list <- function(mt_demand, 
                              ft_demand, 
                              teainteger, 
                              iceinteger, 
                              milkinteger,
                              pearlinteger,
                              fruitsinteger,
                              jellyinteger){
    judge <- TRUE
    
    judge_tea <- (teainteger >= mt_demand + ft_demand)
    judge_ice <- (iceinteger >= mt_demand + ft_demand)
    judge_milk <- (milkinteger >= mt_demand)
    judge_pearl <- (pearlinteger >= mt_demand)
    judge_fruit <- (fruitsinteger >= ft_demand)
    judge_jelly <- (jellyinteger >= ft_demand)
    
    judge <- 
      judge_tea & 
      judge_ice & 
      judge_milk & 
      judge_pearl & 
      judge_fruit & 
      judge_jelly
    
    judge_list = data.frame(deficiency_ingredients = c(judge_tea,
                                                       judge_ice,
                                                       judge_milk,
                                                       judge_pearl,
                                                       judge_fruit,
                                                       judge_jelly),
                            row.names = c('tea',
                                          'ice',
                                          'milk',
                                          'pearl',
                                          'fruit',
                                          'jelly')
    )
    deficiency_list <- row.names(subset(judge_list, deficiency_ingredients == FALSE))
    
    return(deficiency_list)
  }
  
  possible_amount <- function(mt_demand, 
                              ft_demand, 
                              teainteger, 
                              iceinteger, 
                              milkinteger,
                              pearlinteger,
                              fruitsinteger,
                              jellyinteger){
    # Tea and Ice restrict the total number of beverage possible:
    total_possible <- min(c(teainteger, iceinteger, mt_demand + ft_demand))
    # Milk and Pearl restrict the number of milk tea possible:
    mt_possible <- min(c(milkinteger, pearlinteger, teainteger, iceinteger, mt_demand))
    # Fruit and Jelly restrict the number of fruit tea possible:
    ft_possible <- min(c(fruitsinteger, jellyinteger, teainteger, iceinteger, ft_demand))
    return(c(total_possible, mt_possible, ft_possible))
  }
  
  sales <- function(mt_demand, 
                    ft_demand, 
                    teainteger, 
                    iceinteger, 
                    milkinteger,
                    pearlinteger,
                    fruitsinteger,
                    jellyinteger,
                    mt_user) #only let them control milk tea
  {
    if (length(deficiency_list(mt_demand, 
                               ft_demand, 
                               teainteger, 
                               iceinteger, 
                               milkinteger,
                               pearlinteger,
                               fruitsinteger,
                               jellyinteger)) == 0)  {
      return(c(mt_demand, ft_demand))
    } else if (mt_user != "Indian"){
      return(c(mt_user, min(ft_demand, 
                            possible_amount(mt_demand, 
                                            ft_demand, 
                                            teainteger, 
                                            iceinteger, 
                                            milkinteger,
                                            pearlinteger,
                                            fruitsinteger,
                                            jellyinteger)[3],
                            possible_amount(mt_demand, 
                                            ft_demand, 
                                            teainteger, 
                                            iceinteger, 
                                            milkinteger,
                                            pearlinteger,
                                            fruitsinteger,
                                            jellyinteger)[1] - mt_user)))
    } else if (mt_user == "Indian"){
      return(c(possible_amount(mt_demand, 
                               ft_demand, 
                               teainteger, 
                               iceinteger, 
                               milkinteger,
                               pearlinteger,
                               fruitsinteger,
                               jellyinteger)[2],
               possible_amount(mt_demand, 
                               ft_demand, 
                               teainteger, 
                               iceinteger, 
                               milkinteger,
                               pearlinteger,
                               fruitsinteger,
                               jellyinteger)[3]))
    }
  }
  
  mt_sales <- sales(mt_demand, ft_demand, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger, mt_user)[1]
  ft_sales <- sales(mt_demand, ft_demand, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger, mt_user)[2]
  
  score <- function(mt_sales,
                    ft_sales,
                    teainteger, 
                    iceinteger, 
                    milkinteger,
                    pearlinteger,
                    fruitsinteger,
                    jellyinteger){
    tea_leftover <- teainteger - mt_sales - ft_sales
    ice_leftover <- iceinteger - mt_sales - ft_sales
    milk_leftover <- milkinteger - mt_sales
    pearl_leftover <- pearlinteger - mt_sales
    fruit_leftover <- fruitsinteger - ft_sales
    jelly_leftover <- jellyinteger - ft_sales
    
    if (event[2] == FALSE) {
      tea_p <- -0.00002*teainteger + 0.3
      ice_p <- 0.2
      milk_p <- 0.6
      pearl_p <- -0.00005*pearlinteger + 0.5
      fruit_p <- -0.0001*fruitsinteger + 1.5
      jelly_p <- -0.00005*pearlinteger + 0.6
    } else if (event[2] == TRUE){
      tea_p <- (-0.00002*teainteger + 0.3)*0.9
      ice_p <- 0.2*0.9
      milk_p <- 0.6*0.9
      pearl_p <- (-0.00005*pearlinteger + 0.5)*0.9
      fruit_p <- (-0.0001*fruitsinteger + 1.5)*0.9
      jelly_p <- (-0.00005*pearlinteger + 0.6)*0.9
    }
    
    mt_revenue = mt_price * mt_sales
    ft_revenue = ft_price * ft_sales
    
    cost <- 
      ice_p*iceinteger +
      milk_p*milkinteger +
      tea_p*teainteger +
      pearl_p*pearlinteger + 
      fruit_p*fruitsinteger +
      jelly_p*jellyinteger
    
    if (event[2] == TRUE){
      cost <- cost + 1200
    }
    
    profit <- mt_revenue + ft_revenue - cost
    
    ROI <- profit/cost
    FR <- (mt_sales + ft_sales)/(mt_demand + ft_demand)
    Percentage_Waste <- (tea_leftover + 
                           ice_leftover + 
                           milk_leftover +
                           pearl_leftover +
                           jelly_leftover + 
                           fruit_leftover)/(teainteger +
                                              iceinteger +
                                              milkinteger +
                                              pearlinteger + 
                                              jellyinteger +
                                              fruitsinteger)
    Score <- ROI + FR - Percentage_Waste
    return(c(tea_leftover, ice_leftover, milk_leftover, pearl_leftover, fruit_leftover, jelly_leftover, profit, ROI, FR, Percentage_Waste, Score, cost))
  }
  
  tea_leftover <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[1]
  ice_leftover <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[2]
  milk_leftover <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[3]
  pearl_leftover <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[4]
  fruit_leftover <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[5]
  jelly_leftover <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[6]
  profit1 <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[7]
  ROI1 <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[8]
  FR1 <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[9]
  Percentage_Waste1 <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[10]
  Score1 <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[11]
  cost <- score(mt_sales, ft_sales, teainteger, iceinteger, milkinteger, pearlinteger, fruitsinteger, jellyinteger)[12]
  
  final_df <- data.frame(Tea_Leftover = as.integer(tea_leftover),
                         Ice_Leftover = as.integer(ice_leftover),
                         Milk_Leftover = as.integer(milk_leftover),
                         Pearl_Leftover = as.integer(pearl_leftover),
                         Fruit_Leftover = as.integer(fruit_leftover),
                         Jelly_Leftover = as.integer(jelly_leftover),
                         Total_Leftover = as.integer((tea_leftover + 
                                                        ice_leftover + 
                                                        milk_leftover +
                                                        pearl_leftover +
                                                        jelly_leftover + 
                                                        fruit_leftover)),
                         Total_Quantity = as.integer(iceinteger+teainteger+milkinteger+pearlinteger+fruitsinteger+jellyinteger),
                         Cost = cost,
                         Profit = profit1,
                         ROI = ROI1,
                         Fulfillment_Rate = FR1,
                         Waste_Percentage = Percentage_Waste1,
                         Score = Score1,
                         mt_Demand = mt_demand,
                         ft_Demand = ft_demand)
  return(final_df)
  
}

calcPossibleFT <- function(total_possible, mt_possible, ft_possible, mt_demand, ft_demand, mt_user = "Indian") #only let them control milk tea
{
  if (mt_user == "Indian") {
    return(c(mt_possible, ft_possible))
  } else if (mt_user != "Indian") {
    #return(c(mt_user, min(c(ft_demand, ft_possible, total_possible - mt_user))))
    return(c(mt_user, min(ft_demand, ft_possible, total_possible - mt_user)))
  }
}

