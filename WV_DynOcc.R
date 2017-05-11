library(unmarked)
library(AICcmodavg)

#Choose a directory where the data are downloaded to and load the data
setwd(choose.dir())
water.vole <- read.table("WV_DynOcc.txt",h=T)

head(water.vole)

wv.y <- water.vole[,c("y11","y12","y13",
                      "y21","y22","y23",
                      "y31","y32","y33",
                      "y41","y42","y43")]
wv.sitecovs <- water.vole[,c("Length","Connectivity","Block")]

umf <- unmarkedMultFrame(y = wv.y,
                         siteCovs = wv.sitecovs,
                         numPrimary = 4)

umf                                # look at data
summary(umf)                       # summarize

modList <- list(
  #~psi, ~col, ~ext, ~p 
  fm1 <- colext(~1, ~1,            ~1,      ~1, umf),  # fit a model
  fm2 <- colext(~1, ~Connectivity, ~1,      ~1, umf),  # fit a model
  fm3 <- colext(~1, ~1,            ~Length, ~1, umf),  # fit a model
  fm4 <- colext(~1, ~Connectivity, ~Length, ~1, umf)  # fit a model
)


