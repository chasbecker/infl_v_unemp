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
# but I'm a "belt and suspenders" type of guy.  And this separates
# the transforms from the analysis

u_tbl <- read.csv( "./data/UNRATE.csv", sep=",", header=FALSE, skip=0 )
toskip <- which( u_tbl$V1 == "date" ) - 1
u_tbl <- read.csv( "./data/UNRATE.csv", header=TRUE, skip=toskip )
u_tbl$YMDate <- mdy( u_tbl$date )
u_tbl$Unempl <- u_tbl$value
u_tbl <- subset( u_tbl, select = c(YMDate, Unempl))
write.csv( u_tbl, "./data/unempl.csv", row.names=FALSE )

#
# this is overall, higher than just 'core' inflation
#
i_tbl <- read.csv( "./data/CPIAUCSL.csv", sep=",", header=FALSE, skip=0 )
toskip <- which( i_tbl$V1 == "date" ) - 1
i_tbl <- read.csv( "./data/CPIAUCSL.csv", sep=",", header=TRUE, skip=toskip )
i_tbl$YMDate <- mdy( i_tbl$date )
i_tbl$CPI <- i_tbl$value
# calculate month-to-month inflation from previous and current CPI
i_tbl <- (mutate( i_tbl, Inflx1000 = ( (CPI - lag(CPI) ) / lag(CPI) )*1000 ))
i_tbl$Inflx1000 <- round( i_tbl$Inflx1000, digits = 1 )
i_tbl <- subset( i_tbl, select = c(YMDate, Inflx1000))
write.csv( i_tbl, "./data/infl.csv", row.names=FALSE)

#### on pause
iu_tbl <- inner_join( i_tbl, u_tbl , by="YMDate" )


explPlot1 <- ggplot( data = iu_tbl )+
                geom_line( aes(y=Inflx1000, x= YMDate, colour="Inflx1000"),size=1 )+
                geom_line( aes(y=Unempl, x= YMDate, colour="Unempl"),size=1) +
                scale_color_manual(name = "Infl_v_Unempl", values = c("Inflx1000" = "chartreuse", "Unempl" = "deeppink1"))
  
explPlot1

