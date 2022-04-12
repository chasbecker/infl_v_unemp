# inflation vs unemployment
# is there a correlation?
#
# all data from the St Louis FED via the FRED MS-Excel extension
# then saved as csv format.

rm(list=ls())
library(tidyverse)
library(lubridate)

# start with the raw file downloaded in Excel from FRED
# their might be some pointless and inefficient flailing here
# but I'm a "belt and suspenders" type of guy

u_tbl <- read.table( "./data/UNRATE.csv", sep=",", header=FALSE, skip=0 )
toskip <- which( u_tbl$V1 == "date" ) - 1
u_tbl <- read.table( "./data/UNRATE.csv", sep=",", header=TRUE, skip=toskip )
u_tbl$date <- mdy( u_tbl$date )
write.csv( u_tbl, "./data/uempl.csv", sep=",", row.names=FALSE )

i_tbl <- read.table( "./data/CPILFESL.csv", sep=",", header=FALSE, skip=0 )
toskip <- which( i_tbl$V1 == "date" ) - 1
i_tbl <- read.table( "./data/CPILFESL.csv", sep=",", header=TRUE, skip=toskip )
i_tbl$date <- mdy( i_tbl$date )
write.csv( i_tbl, "./data/infl.csv", sep=",", row.names=FALSE)






i <- read.csv( "./data/infl.csv" )
# here need to calculate "inflation", then inflation * 10"
i["Infl*10"] <- round( i["Infl*10"], 1 )
u <- read_csv( "./data/uempl.csv")

#### on pause
iu <- inner_join( i, u, by="date" )
iu$date <- mdy( iu$date ) 
iu <- iu %>% rename(  CPI = value.x, Unempl = value.y, Inflx10 = "Infl*10"  )

explPlot1 <- ggplot( data = iu )+
                geom_line( aes(y=Inflx10, x= date, colour="Infl"),size=1 )+
                geom_line( aes(y=Unempl, x= date, colour="Unempl"),size=1) +
                scale_color_manual(name = "Infl_v_Unempl", values = c("Infl" = "chartreuse", "Unempl" = "deeppink1"))
  
explPlot1

