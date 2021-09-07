
install.packages("tseries")
install.packages("fGarch")
install.packages("zoo")
library(tseries)
library(fGarch)
library(zoo)

con=url("https://finance.yahoo.com")

##Zoom
zoom= get.hist.quote(instrument = "ZM", start = "2010-06-01",quote = "Close")
plot(zoom)
