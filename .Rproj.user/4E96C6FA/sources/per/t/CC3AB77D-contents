
library(Rblpapi)
library(data.table)
library(plyr)
library(reshape2)
library(quantmod)
library(PerformanceAnalytics)

ticker <- read.csv("tickers.csv", sep=",", header = FALSE)
ticker <- as.character(ticker[,1])


con <- blpConnect()

# ovr <- c("Eqy_Fund_Crncy" = "EUR")
opt <- c("periodicitySelection"="DAILY",
         "adjustmentFollowDPDF"="FALSE",
         "adjustmentAbnormal"="TRUE",
         "adjustmentSplit"="TRUE",
         "adjustmentNormal"="TRUE")

histoPx <- bdh(ticker,
               c("Px_Last"),
               start.date = as.Date("1999-12-31"),
               end.date = Sys.Date(),
                options= opt)

Fx <- cbind(ticker, 
            bdp(ticker, c("Crncy")))
          
histoPx <- ldply(histoPx, data.frame)
colnames(histoPx)[1] <- "ticker"

histoPx <- merge(histoPx, Fx,  by=  "ticker")
histoPx <- data.table(histoPx, key= "date")

histoPx[Crncy == "GBp", Crncy:= toupper(Crncy)]

Fx <- unique(histoPx[Crncy != "EUR", Crncy])

FxTicker <- paste0(Fx, "EUR Curncy")
Fx <- bdh(FxTicker,
          c("Px_Last"),
          start.date = as.Date("1999-12-31"),
          end.date = Sys.Date(),
          options= opt)

Fx <- ldply(Fx, data.frame)
Fx <- data.table(Fx, key= "date")

Fx[, .id:= sub("EUR Curncy","",.id)]
colnames(Fx)[1] <- "Crncy"

blpDisconnect(con)

setkey(histoPx, date, Crncy)
setkey(Fx, date, Crncy)

histoPx <- Fx[histoPx]
histoPx[is.na(Px_Last), Px_Last:= 1]

histoPx[, Px_Last:= Px_Last * i.Px_Last]
histoPx[Crncy == "GBP", Px_Last:= Px_Last / 100]

serie <- histoPx[, .(date, ticker, Px_Last)]
serie <- dcast(serie, date ~ticker)

setDT(serie)
serie <- serie[, lapply(.SD, function(x) na.locf(x, na.rm= FALSE))]
serie <- serie[complete.cases(serie)]

serie <- xts(serie[,-1, drop=FALSE], order.by = serie$date)


return <- ROC(serie, type="discrete")[-1]

a <- VaR(return, p=0.99, weights= NULL, 
    portfolio_method = "component", method = "modified",
    na.rm= TRUE)


