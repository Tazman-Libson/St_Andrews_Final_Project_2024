
library(tidyverse)

library(readr)

#Data downloaded on Feb 1st 2024 from https://www.nasdaq.com/market-activity/stocks/


Apple <- read_csv("Apple.csv")
Microsoft <- read_csv("Microsoft.csv")
Intel <- read_csv("Intel.csv")
Meta <- read_csv("META.csv")

price_to_returns<- function(Stock){
  Stock$last <- as.numeric(sub('.', '', Stock$`Close/Last`))
  returns_name <- paste(deparse(substitute(Stock)), ' Returns')
  output <- Stock %>% 
    #Get rid of earliest date
    filter(Date != Stock$Date[length(Stock$Date)]) %>%
    #Add the offset list of prices
    mutate(day_before = Stock$last[-1])%>%
    #take log of ratio of price and price of previous day
    mutate(Returns = 100*(log(last) - log(day_before)))%>%
    select(Date, Returns)
  return(output)
}


apl <- price_to_returns(Apple)
mic <- price_to_returns(Microsoft)
tel <- price_to_returns(Intel)
met <- price_to_returns(Meta)

example_returns <- apl %>%
  left_join(mic, join_by(Date)) %>%
  left_join(tel, join_by(Date)) %>%
  left_join(met, join_by(Date)) %>%
  rename(Apple = Returns.x, Microsoft = Returns.y, Intel = Returns.x.x, Meta = Returns.y.y )

saveRDS(example_returns, file = 'ExampleReturns.RData')



