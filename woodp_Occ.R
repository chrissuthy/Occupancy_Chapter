library(unmarked)
library(AICcmodavg)
library(RCurl)

#Choose a directory where the data are downloaded to and load the data
x <- getURL("https://raw.githubusercontent.com/chrissuthy/Occupancy_Chapter/master/woodp_Occ.csv")
woodpecker <- read.csv(text = x)

head(woodpecker)

n.sites <- nrow(woodpecker)
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
modList <- fitList(
  #~p ~psi 
  "p(.)psi(.)"          = occu(~1           ~1,                 umf),  
  "p(date)psi(.)"       = occu(~scale(date) ~1,                 umf),  
  "p(.)psi(snags)"      = occu(~1           ~scale(log(snags)), umf),  
  "p(date)psi(snags)"   = occu(~scale(date) ~scale(log(snags)), umf)
)
# AIC model selection
aictab(modList,second.ord = F)
modSel(modList)

#mean detection (95% CI)
plogis(coef(modList@fits[[4]])[3])
plogis(confint(modList@fits[[4]],type="det")[1,])

#mean occupancy (95% CI)
plogis(coef(modList@fits[[4]])[1])
plogis(confint(modList@fits[[4]],type="state")[1,])

#finite sample occupancy
sum(bup(ranef(modList@fits[[4]]),stat="mode"))/n.sites
apply(confint(ranef(modList@fits[[4]]),level=.95),2,sum)/n.sites

#------------------
# model predictions
#------------------
png("covariate_effects.png",width = 6.5,height=3,units="in",res=600)
par(mfrow=c(1,2),cex=.8)

# predict/plot date effect on detection
date.dat <- data.frame(date=150:190)
date.fit <- predict(modList@fits[[4]],type="det",newdata=date.dat,appendData=T)

with(date.fit, {
  plot(date, Predicted, type="l",xlim=c(150,190),ylim=c(0,1),las=1,bty="n",lwd=2,
       xlab="Ordinal date", ylab="Detection probability")
  lines(date, Predicted+1.96*SE, lty=2, col="darkgray")
  lines(date, Predicted-1.96*SE, lty=2, col="darkgray")
})

# predict/plot snag density effect on occupancy
snags.dat <- data.frame(snags=seq(0.25,6,by=0.25))
snags.fit <- predict(modList@fits[[4]],type="state",newdata=snags.dat,appendData=T)

with(snags.fit, {
  plot(snags, Predicted, type="l",xlim=c(0,6),ylim=c(0,1),las=1,bty="n",lwd=2,
       xlab="Snag density (#/ha)", ylab="Nest occupancy probability")
  lines(snags, Predicted+1.96*SE, lty=2, col="darkgray")
  lines(snags, Predicted-1.96*SE, lty=2, col="darkgray")
})

dev.off()
