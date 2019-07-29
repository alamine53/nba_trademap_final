setwd("~/Documents/04_Projects/2019/02_NBA_TradeAnimation")

library(dplyr)
library(rvest)
library(stringr)
library(readr)

url_csv2 <- 'https://raw.githubusercontent.com/alamine53/nbatrademap/master/raw_NbaCities.csv'
nbacities <- read_csv(url(url_csv2))
saveRDS(nbacities, file = "nbacities")

link <- paste0("https://www.basketball-reference.com/friv/free_agents.fcgi")
table <-  html_table(html(url(link) ), fill = TRUE)
raw_transactions <-table[[1]] %>% 
  select(Rk, Player, WS, OTm, NTm, Terms) %>%
  filter(Rk != "Rk", NTm != "", as.numeric(Rk) < 30) 
saveRDS(raw_transactions, file = "raw_transactions")
