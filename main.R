# inflation vs unemployment
# is there a correlation?
#
# all FRED data from the St Louis FED

#  Needed for this project
#install.packages("fredr")
#install.packages("usethis")

rm(list=ls())
library(fredr)
# library(usethis)  # to edit .Renviron file one time <***>>>
library(tidyverse)

# The variable 'infladj' will be used to normalize the inflation value so it's:
# a) always positive, and b) zero at some arbitrary selected unemployment rate.
# This is to allow testing various hypothesis about natural inflation and unemployment.

infladj <- as.numeric( readline( "How much do you want to normalize inflation for (pls enter a number): ") )

# get FRED API key, save to environment variable

# 1) registered for FRED account
# 2) open a .Renviron file (uncomment line below)
# edit_r_environ() <<<***>
# 3) add API key to .Renviron file like so
# 3.1) FRED_API_KEY=abcdefghijklmnopqrstuvwxyz123456
# 4) save and close the .Renviron file
# 5) restart R & RStudio

# read, transform, save unemployment data from FRED
u_tbl <- fredr(series_id = "UNRATE")
u_tbl <- rename( u_tbl, unempl = value )
u_tbl <- subset( u_tbl, select = c(date, unempl))
write.csv( u_tbl, "./data/unempl.csv", row.names=FALSE )

# read, transform, save CPI/inflation data from FRED
i_tbl <- fredr(series_id = "CPIAUCSL")     
# rename the 'value' column to 'cpi' to make the 'mutate' statement more readable
i_tbl <- rename(i_tbl, CPI = value )
# calculate monthly inflation,
# multiply by 10 to normalize to unemployment, and round to one decimal
i_tbl <- (mutate( i_tbl, inflx10 = ( (CPI - lag(CPI) ) / lag(CPI) )*1000 ))
i_tbl$inflx10 <- round( i_tbl$inflx10, digits = 1 )
i_tbl <- subset( i_tbl, select = c(date, inflx10))

# calculate 'infdev2', being the absolute difference between actual inflation
# and the user specified hypothetical natural inflation
i_tbl <- mutate( i_tbl, infdev2 = abs( inflx10 - infladj ) )

write.csv( i_tbl, "./data/infl.csv", row.names=FALSE)


iu_tbl <- inner_join( i_tbl, u_tbl , by="date" )

explPlot1 <- ggplot( data = iu_tbl, aes( x = date ) )+
                geom_line( aes(y=inflx10, colour="inflx10"),size=1 )+
                geom_line( aes(y=unempl, colour="unempl"),size=1) +
                geom_line( aes( y = infdev2, colour="infdev2")) +
                scale_color_manual(name = "Infl_v_Unempl", values = c("inflx10" = "bisque", "unempl" = "deeppink1", "infdev2" = "deepskyblue2"))
  
explPlot1

