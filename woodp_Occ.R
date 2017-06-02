library(unmarked)
library(AICcmodavg)
library(RCurl)

#Choose a directory where the data are downloaded to and load the data
x <- getURL("https://raw.githubusercontent.com/chrissuthy/Occupancy_Chapter/master/woodp_Occ.csv")
woodpecker <- read.csv(text = x)

head(woodpecker)

woodp.y <- woodpecker[,c("y.1","y.2","y.3")]
woodp.sitecovs <- woodpecker[,c("snags"),drop=F]
woodp.obsCovs <- woodpecker[,c("date.1","date.2","date.3")]

# create unmarked frame with data
umf <- unmarkedFrameOccu(y = woodp.y,
                         siteCovs = woodp.sitecovs,
                         obsCovs = list(date=woodp.obsCovs))
umf
summary(umf)

# fit a collection of models
modList <- list(
  #~p ~psi 
  "p(.)psi(.)"          = occu(~1           ~1,     umf),  
  "p(date)psi(.)"       = occu(~scale(date) ~1,     umf),  
  "p(.)psi(snags)"      = occu(~1           ~log(snags), umf),  
  "p(date)psi(snags)"   = occu(~scale(date) ~log(snags), umf)
)
# AIC model selection
aictab(modList)

#------------------
# model predictions
#------------------

# predict/plot date effect on detection
date.dat <- data.frame(date=150:190)
date.fit <- predict(modList[[4]],type="det",newdata=date.dat,appendData=T)

with(date.fit, {
  plot(date, Predicted, type="l",xlim=c(150,190),ylim=c(0,1),las=1,bty="n",lwd=2,
       xlab="Ordinal date", ylab="Detection probability")
  lines(date, Predicted+1.96*SE, lty=2)
  lines(date, Predicted-1.96*SE, lty=2)
})

# predict/plot snag density effect on occupancy
snags.dat <- data.frame(snags=seq(0.25,6,by=0.25))
snags.fit <- predict(modList[[4]],type="state",newdata=snags.dat,appendData=T)

with(snags.fit, {
  plot(snags, Predicted, type="l",xlim=c(0,6),ylim=c(0,1),las=1,bty="n",lwd=2,
       xlab="Snag density (#/ha)", ylab="Nest occupancy probability")
  lines(snags, Predicted+1.96*SE, lty=2)
  lines(snags, Predicted-1.96*SE, lty=2)
})
