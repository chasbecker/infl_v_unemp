# inflation vs unemployment
# is there a correlation?
#
# all data from the St Louis FED via FRED
# retrieved and Infl*10 column calculated in Excel

rm(list=ls())
library(tidyverse)
library(lubridate)

# start with the raw file downloaded in Excel from FRED
u_tbl <- read.table( "./data/UNRATE.csv", sep=",", header=FALSE, skip=0 )
i_tbl <- read.table( "./data/CPILFESL.csv", sep=",", header=FALSE, skip=0 )

i <- read_csv( "./data/infl.csv" )
i["Infl*10"] <- round( i["Infl*10"], 1 )
u <- read_csv( "./data/uempl.csv")

iu <- inner_join( i, u, by="date" )
iu$date <- mdy( iu$date ) 
iu <- iu %>% rename(  CPI = value.x, Unempl = value.y, Inflx10 = "Infl*10"  )

explPlot1 <- ggplot( data = iu )+
                geom_line( aes(y=Inflx10, x= date, colour="Infl"),size=1 )+
                geom_line( aes(y=Unempl, x= date, colour="Unempl"),size=1) +
                scale_color_manual(name = "Infl_v_Unempl", values = c("Infl" = "chartreuse", "Unempl" = "deeppink1"))
  
explPlot1

