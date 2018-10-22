library(unmarked)
library(AICcmodavg)
library(RCurl)

#Choose a directory where the data are downloaded to and load the data
x <- getURL("https://raw.githubusercontent.com/chrissuthy/Occupancy_Chapter/master/vole_DynOcc.csv")
water.vole <- read.csv(text = x)

head(water.vole)

wv.y <- water.vole[,c("y11","y12","y13",
                      "y21","y22","y23",
                      "y31","y32","y33",
                      "y41","y42","y43")]
wv.sitecovs <- water.vole[,c("Length","Connectivity","Block")]

wv.yearly.covs <- list(yr = matrix(c('2009', '2010','2011', '2012'), 
                                   nrow=nrow(water.vole), ncol=4, byrow=TRUE))
  
umf <- unmarkedMultFrame(y = wv.y,
                         siteCovs = wv.sitecovs,
                         yearlySiteCovs = wv.yearly.covs,
                         numPrimary = 4)

umf
summary(umf)

modList <- fitList(
  #~psi, ~col, ~ext, ~p 
  "psi(.)gam(.)   eps(.)   p(.)"  = colext(~1, ~1,            ~1,      ~1,  umf),
  "psi(.)gam(Conx)eps(.)   p(.)"  = colext(~1, ~Connectivity, ~1,      ~1,  umf),
  "psi(.)gam(.)   eps(Size)p(.)"  = colext(~1, ~1,            ~Length, ~1,  umf),
  "psi(.)gam(Conx)eps(Size)p(.)"  = colext(~1, ~Connectivity, ~Length, ~1,  umf),
  "psi(.)gam(.)   eps(.)   p(yr)" = colext(~1, ~1,            ~1,      ~yr, umf),
  "psi(.)gam(Conx)eps(.)   p(yr)" = colext(~1, ~Connectivity, ~1,      ~yr, umf),
  "psi(.)gam(.)   eps(Size)p(yr)" = colext(~1, ~1,            ~Length, ~yr, umf),
  "psi(.)gam(Conx)eps(Size)p(yr)" = colext(~1, ~Connectivity, ~Length, ~yr, umf)
)
# AIC model selection
modSel(modList)

#mean extinction (AIC-best: ~1)
plogis(coef(modList@fits[[6]])["ext(Int)"])
plogis(confint(modList@fits[[6]],type="ext"))

#colonization regression parameters (AIC-best: ~connectivity)
coef(modList@fits[[6]])["col(Int)"]
coef(modList@fits[[6]])["col(Connectivity)"]
confint(modList@fits[[6]],type="col")

pred.vals <- quantile(water.vole$Connectivity,c(0,0.5,1))
plogis(coef(modList@fits[[6]])["col(Int)"] +
       coef(modList@fits[[6]])["col(Connectivity)"]*pred.vals)

#yearly detection probs (AIC-best: ~yr)
year.df <- data.frame(yr = c("2009","2010","2011","2012"))
predicted <- predict(modList@fits[[6]],type="det",newdata=year.df,appendData=T)
predicted

#predict colonization~connectivty
conx.dat <- data.frame(Connectivity=seq(min(water.vole$Connectivity),
                                        max(water.vole$Connectivity),
                                        0.1))
conx.fit <- predict(modList@fits[[6]],type="col",newdata=conx.dat,appendData=T)

#yearly occupancy rates (finite sample)
mu  <- apply(bup(ranef(modList@fits[[6]]),stat="mean"),2,sum)/114
cis <- apply(confint(ranef(modList@fits[[6]]),level=.95),c(2,3),sum)/114
obs <- c(sum(apply(umf@y[, 1:3] >0,1,sum)>0,na.rm=T),
         sum(apply(umf@y[, 4:6] >0,1,sum)>0,na.rm=T),
         sum(apply(umf@y[, 7:9] >0,1,sum)>0,na.rm=T),
         sum(apply(umf@y[,10:12]>0,1,sum)>0,na.rm=T))/114


#make plots

# for(i in 1:2){
# if(i ==1 ) png("vole_DynOcc.png",width = 6,height=3.5,units="in",res=600)
# if(i == 2) postscript("vole_DynOcc.eps",width = 6,height=3.5)
# par(mfrow=c(1,2),cex=.8)
postscript("Figure 7.3a.eps",width = 3,height=3.5)
par(cex=0.8)

with(conx.fit, {
  plot(Connectivity, Predicted, type="l",xlim=c(0,40),ylim=c(0,1),las=1,bty="n",lwd=2,
       xlab="Connectivity", ylab="Colonization probability")
  lines(Connectivity, lower, lty=2, col="darkgray")
  lines(Connectivity, upper, lty=2, col="darkgray")
})
dev.off()

postscript("Figure 7.3b.eps",width = 3,height=3.5)
par(cex=0.8)

plot(c(2009,2010,2011,2012),obs, type="n",ylim=c(0,1), las=1,bty="n",
     xlab="Year", ylab="Occupancy",axes=F, xlim=c(2008.8,2012.2))
axis(2,las=1)
axis(1,at=(2009:2012),labels=paste(2009:2012))
segments((2009:2012),cis[1,],(2009:2012),cis[2,], col="grey")
points(2009:2012,        mu, cex=1.5, pch=21, bg=1)
points((2009:2012)+0.1, obs, cex=1.5, pch=21, bg="grey90")
dev.off()

# }