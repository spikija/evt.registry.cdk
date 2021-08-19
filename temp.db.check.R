# 16.08.2021
# on GIT Hub since 16.8.2021

# 31.07.2021
# checking Hecker C. new entries for consistency/quality
usual.libs()

setwd("C:/Sci/Stroke/EVT_Registry/Db")

myd <- as_tibble(read_excel("C:/Sci/Stroke/EVT_Registry/Db/EVT_Reg_v6.xlsx"))

# first fetch proc.compl from 02_THROMBEKTOMIE.xlsx
db.orig <- myd[!is.na(myd$orbis.id),c("orbis.id", "proc.compl")]
db.orig$orbis.id <- as.character(db.orig$orbis.id)

db.temp <- as_tibble(read_xlsx("C:/Sci/Stroke/EVT_Registry/Db/proc.compl.temp.xlsx"))

db.comp <- left_join(db.orig, db.temp, by = "orbis.id")


# RESULT
# taking over procedural complications from 02-THROMBEKTOMIE
# will be used mainly as yes/no in the database

#myd <- as_tibble(read_excel("C:/Sci/Stroke/EVT_Registry/Db/temp.rb.xlsx"))


# fields of interest
foi <- c(
  #"age", 
  "dob", "gender", "adm.nihss",  
  "disc.nihss",
  "wakeup.stroke",
  "toast", #"toast.ce",
  "dissection",
  "lysis",
  
  "anest.type",
  
  "stroke", "pavk", "af", "dm", "ah", "ica.stenosis", "heart.dis", "antithromb",
  "anticoag", "doak", "smoker", "kidney.failure", "heart.failure","statin.hist",
  
  "aspects",
  "vessel.side",
  "vessel.type",
  "vessel.subtype",
  #"ica.with.mca",
  #"mca",
  "ica.occl.height",
  #"g.coll",
  
  #"onset.to.img",
  "onset.to.lysis",
  #"onset.to.punct", 
  "ang.punct.to.target",
  "ang.target.to.open", 
  "proc.time",
  #"onset.to.end",
  "ang.punct.to.end", 
  "ang.steps", #"g.tici", "evt.type2", "evt.type3",
  "ica.stenting",
  
  "vessel.perforation",
  "proc.compl",
  #"g.proc.compl",
  "bleed.class1",
  "bleed.class2",
  "bleed.class3",
  #"bleed.3a",
  #"bleed.3b",
  #"bleed.3d",
  #"g.bleed2", "g.bleed3", "bleed.any",
  #"inf.vol",
  
  "hosp.death", #"g.mrs",
  
  
  #"crp", 
  "glucosis", "hba1c", "creatinin",
  "chol", "tg", "hdl", "ldl", "leuco", "neu", "lym", 
  #"neu.lym.rat", 
  "ery", "hct", 
  "tr", "fibrinogen",
  
  #"inr2", "g.wmh", 
  
  #"time.onset.year", #"bmi", 
  #"lysis.mt.overlap",
  #"first.pass",
  
  #"hosp.days",
  #"ang.end.to.img.ctrl",
  #"lysis.to.puncture"
  "ica.ipsi.diam", "mca.m1.diam",
  "mca.m2.diam",
  "mca.length.straight",
  "mca.length.curve" 
  )

myd <- as_tibble(read_excel("C:/Sci/Stroke/EVT_Registry/Db/EVT_Reg_v7.xlsx"))

# exclude basilaris occlusions

excl.v <- table(myd$vessel.type)
# exclude basilaris, vertebralis
excl.v <- attributes(excl.v[-c(4, 5, 6, 7, 13, 15)])$dimnames[[1]]

myd <- myd %>% 
  filter(myd$vessel.type %in% excl.v)
# reduced from 694 to 654!
# check "numeric" columns for non-numeric values
non.numerics <- c("adm.nihss", "disc.nihss", "onset.to.lysis", "proc.time",
                  "ang.punct.to.end", "ang.steps",
                  "glucosis", "hba1c", "creatinin",
                  "chol", "tg", "hdl", "ldl", "leuco", "neu", "lym",
                  "ery", "hct", 
                  "tr", "fibrinogen",
                  "ica.ipsi.diam", "mca.m1.diam",
                  "mca.m2.diam",
                  "mca.length.straight",
                  "mca.length.curve"
)

# convert to numerics
numerics <- non.numerics
myd[numerics] <- lapply(myd[numerics], function(x) as.numeric(x))

# convert to factors
to.factor <- c("gender",
               "wakeup.stroke",
               "toast", 
               "dissection",
               "lysis",
               "anest.type",
               "stroke", "pavk", "af", "dm", "ah", "ica.stenosis", "heart.dis", "antithromb",
               "anticoag", "doak", "smoker", "kidney.failure", "heart.failure","statin.hist",
               
               "vessel.side",
               "vessel.type",
               "vessel.subtype",
               "ica.occl.height",
               "ica.stenting",
               
               "vessel.perforation",
               "proc.compl",
                "bleed.class1",
               "bleed.class2",
               "bleed.class3",
               
               "hosp.death")

myd[to.factor] <- lapply(myd[to.factor], function(x) factor(x))

myd <- myd[, foi]

dfSummary(myd)
render(paste0(getwd(),"/evt.reg.rmd"))

# various times
# load through cdk import function
rb <- cdk.evt.db.import("C:/Sci/Stroke/EVT_Registry/Db/EVT_Reg_v7.xlsx")

# Onset-to-image, min
cb(na.omit(rb[rb$onset.to.img < 0, c("orbis.id", "date.time.onset", "img.date.nativ", "onset.to.img")]))
cb(na.omit(rb[rb$onset.to.img > 480, c("orbis.id", "date.time.onset", "img.date.nativ", "onset.to.img")]))

# Onset-to-IVT, min
cb(na.omit(rb[rb$onset.to.lysis < 0, c("orbis.id", "date.time.onset", "lysis.date.time", "onset.to.lysis")]))
cb(na.omit(rb[rb$onset.to.lysis > 270, c("orbis.id", "date.time.onset", "lysis.date.time", "onset.to.lysis")]))

# Onset-to-arterial puncture, min
cb(na.omit(rb[rb$onset.to.punct < 0, c("orbis.id", "date.time.onset", "ang.punct.start", "onset.to.punct")]))
cb(na.omit(rb[rb$onset.to.punct > 480, c("orbis.id", "date.time.onset", "ang.punct.start", "onset.to.punct")]))

# Arterial puncture-to-target, min
cb(na.omit(rb[rb$ang.punct.to.target < 0, c("orbis.id", "ang.punct.start","ang.target.img", "ang.punct.to.target")]))
cb(na.omit(rb[rb$ang.punct.to.target > 120, c("orbis.id", "ang.punct.start","ang.target.img", "ang.punct.to.target")]))

# Target to recanalization, min
cb(na.omit(rb[rb$ang.target.to.open < 0, c("orbis.id", "ang.target.img","ang.time.target.open", "ang.target.to.open")]))
cb(na.omit(rb[rb$ang.target.to.open > 120, c("orbis.id", "ang.target.img","ang.time.target.open", "ang.target.to.open")]))

# Arterial puncture to end of MT, min
cb(na.omit(rb[rb$ang.punct.to.end < 0, c("orbis.id", "ang.punct.start","ang.proc.end", "ang.punct.to.end")]))
cb(na.omit(rb[rb$ang.punct.to.end > 240, c("orbis.id", "ang.punct.start","ang.proc.end", "ang.punct.to.end")]))

# Procedure time, min
cb(na.omit(rb[rb$proc.time < 0, c("orbis.id", "ang.punct.start","ang.proc.end", "ang.punct.to.end")]))
cb(na.omit(rb[rb$proc.time > 240, c("orbis.id", "ang.punct.start","ang.proc.end", "ang.punct.to.end")]))

# Onset-to-procedure termination
cb(na.omit(rb[rb$onset.to.end < 0, c("orbis.id", "date.time.onset","ang.proc.end", "onset.to.end")]))
cb(na.omit(rb[rb$onset.to.end > 600, c("orbis.id", "date.time.onset","ang.proc.end", "onset.to.end")]))


# remarks
# 1. what to do with ang.steps = 0? 
# 

# auxillary functions

non.num <- lapply(myd2[, non.numerics], function(x) which.nonnum(x))
# filter out with 0 values
non.num <- non.num[lapply(non.num, length)>0]
# display spoiled values
myv <- list()
for (l in 1:length(non.num))
{
  myv <- c(myv, print(myd2[non.num[[l]],c("orbis.id", names(non.num)[l])]))
}

# function to convert data frame to copyable text
cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}
