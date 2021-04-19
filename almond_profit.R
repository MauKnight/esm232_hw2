# Function to calculate profits from almonds
# ESM 232
# Function developed by Kat Leigh, David Segan, Alex Milward and Mauricio Collado
######################################

#' @param  price ($/lb)
#' @param  almond (ton/acre)
#' @param  year 
#' @param  cost (cost/acre)
#' @param discount rate (default 0.12)
#' @return data frame with estimate of profit
almond_profit = function(almond, year, price=2.5, cost=3800, acre=1, discount=0.12) {
  
  price_ton=price*2000
  
  # make sure values are reasonable
  if (length(almond) < 1)
    return(NA)
  
  # yield cannot be negative
  #if (min(almond) < 0)
   # return(NA)
  
  # generate a unique identifier or scenario number
  scen = seq(from=1, to=length(almond))
  yearprofit = data.frame(scen=scen, almond=almond, year=year)
  yearprofit$net =  yearprofit$almond*(price_ton)*acre-cost*acre
  
  # note how discount is passed through to this function
  # remember to normalize the year to start year e.g the first year
  yearprofit= yearprofit %>% 
    mutate(netpre = compute_NPV(value=net, time=year-year[1], discount=discount ))
  
  return(yearprofit)
}