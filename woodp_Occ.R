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

umf <- unmarkedFrameOccu(y = woodp.y,
                         siteCovs = woodp.sitecovs,
                         obsCovs = list(date=woodp.obsCovs))
umf
summary(umf)

modList <- list(
  #~p ~psi 
  Null     = occu(~1    ~1,     umf),  # fit a model
  Date     = occu(~date ~1,     umf),  # fit a model
  Snags    = occu(~1    ~snags, umf),  # fit a model
  GLOBAL   = occu(~date ~snags, umf)  # fit a model
)
# AIC model selection
aictab(modList)

