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

umf <- unmarkedMultFrame(y = wv.y,
                         siteCovs = wv.sitecovs,
                         numPrimary = 4)

umf
summary(umf)

modList <- list(
  #~psi, ~col, ~ext, ~p 
  "psi(.)gam(.)eps(.)p(.)"       = colext(~1, ~1,            ~1,      ~1, umf),  # fit a model
  "psi(.)gam(Conx)eps(.)p(.)"    = colext(~1, ~Connectivity, ~1,      ~1, umf),  # fit a model
  "psi(.)gam(.)eps(Size)p(.)"    = colext(~1, ~1,            ~Length, ~1, umf),  # fit a model
  "psi(.)gam(Conx)eps(Size)p(.)" = colext(~1, ~Connectivity, ~Length, ~1, umf)  # fit a model
)
# AIC model selection
aictab(modList)
