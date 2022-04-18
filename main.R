# inflation vs unemployment
# is there a correlation?
#
# all FRED data from the St Louis FED

#  Need these two packages for this project
#install.packages("fredr")
#install.packages("usethis")

rm(list=ls())
library(fredr)
library(usethis)  # to edit .Renviron file <***>
library(tidyverse)

# get FRED API key, save to environment variable

# 1) registered for FRED account
# 2) open a .Renviron file (uncomment line below)
# edit_r_environ() <***>
# 3) add API key like so
# 3.1) FRED_API_KEY=abcdefghijklmnopqrstuvwxyz123456
# 4) save and close the .Renviron file
# 5) restart R & RStudio

# read, transform, save unemployment data from FRED
u_tbl <- fredr(series_id = "UNRATE")
u_tbl$unempl <- u_tbl$value
u_tbl <- subset( u_tbl, select = c(date, unempl))
write.csv( u_tbl, "./data/unempl.csv", row.names=FALSE )

# read, transform, save CPI/inflation data from FRED
i_tbl <- fredr(series_id = "CPIAUCSL")     
# rename the 'value' column to 'cpi' to make the 'mutate' statement more readable
i_tbl <- rename(i_tbl, CPI = value )
# calculate monthly inflation,
# multiply by 10 to put in similar range to unemployment
# and round to one decimal
i_tbl <- (mutate( i_tbl, inflx10 = ( (CPI - lag(CPI) ) / lag(CPI) )*1000 ))
i_tbl$inflx10 <- round( i_tbl$inflx10, digits = 1 )
i_tbl <- subset( i_tbl, select = c(date, inflx10))
write.csv( i_tbl, "./data/infl.csv", row.names=FALSE)

iu_tbl <- inner_join( i_tbl, u_tbl , by="date" )

explPlot1 <- ggplot( data = iu_tbl )+
                geom_line( aes(y=inflx10, x= date, colour="inflx10"),size=1 )+
                geom_line( aes(y=unempl, x= date, colour="unempl"),size=1) +
                scale_color_manual(name = "Infl_v_Unempl", values = c("inflx10" = "steelblue1", "unempl" = "deeppink1"))
  
explPlot1

