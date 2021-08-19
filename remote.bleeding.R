# ****************************************************************************************
# Remote bleeding after mechanical interventions project
# 03.05.2019
# new paradigma (without patients that have no catheter - i.e. no access, spontan recanalsation)
# Machegger, -- Pikija.
# Database: 1.1.2012 - 1.1.2019
# all anterior strokes
# new review after Clinical Neuroradiology - 18.03.2021, statistical changes, paradigma from distance to thrombus!
# ****************************************************************************************

usual.libs()

# DBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBD
# import database
# DBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBD

setwd("C:/Sci/Stroke/EVT_Registry/RemoteBleeding")
rb <- cdk.evt.db.import("C:/Sci/Stroke/EVT_Registry/EVT_Reg_v5.xlsx")
#rb <- import.rb.db()


# RESULTS Section
# number of total rows prior to exlusions/restrictions
nrow(rb)

# RESTRICTIONS for subarachnoidal hemorrhage paper!
# Do not analyse patients without control imaging (N=2)
rb <- rb[!is.na(rb$ct.outc),]
nrow(rb)

# 18.3.2021 - N = 415 patients, 417 procedures where contact with artery was made
# restrict on !is.na(evt.type2) - only patients with catheter employed
rb <- rb[!is.na(rb$evt.type2), ]
# RESULTS Section
# number of patients
length(unique(rb$pat.id))

# RESULTS Section
# number of procedures
length(unique(rb$id))

# 18.03.2021
# number of missing data
sum(is.na(rb))


# TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
# make table based on bleeding group
indep <- c("age", "gender", "adm.nihss",  
           "disc.nihss",
           "wakeup.stroke",
           "toast", "toast.ce",
           "dissection",
           "lysis",
           
           "anest.type",
           
           "stroke", "pavk", "af", "dm", "ah", "ica.stenosis", "heart.dis", "antithromb",
           "anticoag", "doak", "smoker", "kidney.failure", "heart.failure","statin.hist",
           
           "aspects",
           "vessel.side",
           "vessel.type",
           "vessel.subtype",
           "ica.with.mca",
           "mca",
           "ica.occl.height",
           "g.coll",
           
           "onset.to.img",
           "onset.to.lysis",
           "onset.to.punct", 
           "ang.punct.to.target",
           "ang.target.to.open", 
           "proc.time",
           "onset.to.end",
           "ang.punct.to.end", 
           "ang.steps", "g.tici", "evt.type2", "evt.type3",
           "ica.stenting",
           
           "vessel.perforation",
           "g.proc.compl",
           "bleed.class1",
           "bleed.3a",
           "bleed.3b",
           "bleed.3d",
           "g.bleed2", "g.bleed3", "bleed.any",
           "inf.vol",
           
           "hosp.death", "g.mrs",
              
              
           "crp", "glucosis", "hba1c", "creatinin",
           "chol", "tg", "hdl", "ldl", "leuco", "neu", "lym", 
           "neu.lym.rat", "ery", "hct", 
           "tr", "fibrinogen", "inr2", "g.wmh", 
           
           "time.onset.year", "bmi", 
           "lysis.mt.overlap",
           "first.pass",
           
           "hosp.days",
           "ang.end.to.img.ctrl",
           "lysis.to.puncture")

rb$g.bleed4 <- relevel(rb$g.bleed4, "yes")
rb$first.pass <- relevel(rb$first.pass, "yes")
rb$bleed.3a <- relevel(rb$bleed.3a, "yes")
rb$bleed.3b <- relevel(rb$bleed.3b, "yes")
rb$bleed.3d <- relevel(rb$bleed.3d, "yes")
rb$ica.with.mca <- relevel(rb$ica.with.mca, "ica+mca")
rb$mca <- relevel(rb$mca, "mca m1")
rb$sah.only <- relevel(rb$sah.only, "no")

rb$bleed.any <- relevel(rb$bleed.any, "yes")
myt <- f.dem.table(c("sah.only"), indep, 2, 2, 99, rb)


# RESULTS
# Table 1 - Demographic, split on two categories - whether Remote Bleeding
# existed or not
myt

myt %>%
  kable() %>%
  save_kable(file = "Table Demographics.html", self_contained = T)

# TABLE - End
# TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT


# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# Figure 4 - START
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# make graph with percentage of patients over time (bin to 10 minutes)
mys <- seq(min(rb$proc.time, na.rm = T), 
           max(rb$proc.time, na.rm = T), 
           by = 10)

rb$proc.time.cut <- cut(rb$proc.time, breaks = mys, label = FALSE)

# make graph with percentage of patients experiencing remote bleeding
# g.bleed1
# build percentages accross time points
t.rb <- rb[!is.na(rb$proc.time.cut),]
t.rb$proc.time.cut <- factor(t.rb$proc.time.cut)

t.rb <- rb[c("proc.time.cut", "sah.only")] %>% 
  group_by(proc.time.cut, sah.only) %>% 
  summarise(n())

t.rb
# sum accrross proc.time.cut factor
t.temp <- NULL
t.temp <- data.frame(tapply(t.rb$`n()`, t.rb$proc.time.cut, FUN = sum))
t.temp$id <- c(1:nrow(t.temp))
colnames(t.temp) <- c("sum", "id")

# transfer sums to t.rb
t.rb$sum <- NA
for (i in 1:nrow(t.rb))
{
  t.rb$sum[i] <- t.temp$sum[t.temp$id == t.rb$proc.time.cut[i]]
}
t.rb <- t.rb[!is.na(t.rb$proc.time.cut),]
t.rb$perc <- round(t.rb$`n()` / t.rb$sum * 100,2)

# show only sah.only == "yes"
t.rb <- t.rb[t.rb$sah.only == "yes",]
# RESULTS!
corr.test(t.rb$proc.time.cut, t.rb$perc)
print(corr.test(t.rb$proc.time.cut, t.rb$perc),short=FALSE)

# add 10 to procedural time
t.rb$proc.time.cut <- t.rb$proc.time.cut * 10

# make smooth graphs
# FIGURE 4
number_ticks <- function(n) {function(limits) pretty(limits, n)}

p <- NULL
p <- ggplot(t.rb, aes(proc.time.cut, perc)) +
  geom_point() + 
  geom_smooth(method = "lm")

p <- p  + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p <- p + scale_x_continuous(breaks=number_ticks(10)) +
  scale_y_continuous(breaks=number_ticks(10))

p + labs(x = "Procedure time in minutes",
         y = "Rate of SAH in (%)")

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# Figure 4 - END
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

 


# TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
# Table 3
# multivariate logistics
# TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

rb$g.bleed4 <- relevel(rb$g.bleed4, "no")
rb$lysis <- relevel(rb$lysis, "no")
rb$wakeup.stroke <- relevel(rb$wakeup.stroke, "no")
rb$toast.ce <- relevel(rb$toast.ce, "ce+unk")
rb$ica.stenting <- relevel(rb$ica.stenting, "yes")
rb$vessel.perforation <- relevel(rb$vessel.perforation, "yes")
rb$evt.type2 <- relevel(rb$evt.type2, "stent.retr only")
rb$first.pass <- relevel(rb$first.pass, "no")
rb$sah.only <- relevel(rb$sah.only, "no")

# ******************************************************************************
# 18.03.2021
# check which variables have p<0.2
mt <- f.dem.table(c("sah.only"), indep, 2, 2,"0.2",rb)
mt


# p<0.2 analysis
testv <- c("sah.only", "adm.nihss", "af", "proc.time", "toast",
           "ica.stenting", "vessel.perforation", "ang.steps")

rb.test <- rb[, testv]

rb.test$af <- relevel(rb.test$af, "no")
rb.test$vessel.perforation <- relevel(rb.test$vessel.perforation, "no")

# NA summary
sapply(rb.test, function(x) sum(is.na(x)))

# make imputation
par(mfrow = c(1,1))
md.pattern(rb.test)
imputed_Data <- mice(rb.test, m=5, maxit = 10, method = 'pmm', seed = 500)
#summary(imputed_Data)

# chose one model, for example 2 (could be another!)
rb.imp <- as_tibble(complete(imputed_Data, 2))

# Fit the full model 
frml <- as.formula(paste(testv[1], paste("~"), paste(testv[2:6], collapse = "+")))
full.model <- glm(frml, data = rb.imp, family = binomial)
summ(full.model, confint = TRUE, digits = 3, ci.width = .5)

tab_model(full.model, show.ci = 0.50)

# model 2 with vessel perforation
frml <- as.formula(paste(testv[1], paste("~"), paste(testv[2:7], collapse = "+")))
full.model <- glm(frml, data = rb.imp, family = binomial)
summ(full.model, confint = TRUE, digits = 3, ci.width = .5)

tab_model(full.model, show.ci = 0.50)

# model 3 with number of attempts, without proc time
frml <- as.formula(paste(testv[1], paste("~"), paste(testv[c(2:3,4:6,8)], collapse = "+")))
full.model <- glm(frml, data = rb.imp, family = binomial)
summ(full.model, confint = TRUE, digits = 3, ci.width = .5)

tab_model(full.model, show.ci = 0.50)


# END 18.03.2021
# ******************************************************************************

# logistic analysis as in towarddatascience.com/model-selection-101-using-r
# prepare time variable by 10 minutes increment
mys <- seq(min(rb$proc.time, na.rm = T), 
           max(rb$proc.time, na.rm = T), 
           by = 10)
rb$cut.ang <- cut(rb$proc.time, breaks = mys, label = FALSE)

# make dummy variable for MT device type
rb$mt.dt.1 <- ifelse(rb$evt.type2 == "aspir only", 1, 0)



# new analysis from 26.09.2019
# original variables p<0.2
# toast.ce +lysis +  mca.m1 +  proc.time +  ang.steps +  g.tici +  ica.stenting +  ang.target.to.open
# droped: ang.steps, g.tici, ica.stenting, toast.ce 
# Model 1
fit.1 <- glm(sah.only ~ 
               ang.steps + vessel.perforation +
               adm.nihss + proc.time
             , data = rb, family = binomial())

summary(fit.1)
logistic.display(fit.1)

# model with device type as dummy variable for "aspiration ?nly" = 1!
# 30.09.2019
fit.1 <- glm(sah.only ~ 
               ang.steps + vessel.perforation +
               adm.nihss + proc.time + mt.dt.1
             , data = rb, family = binomial())

summary(fit.1)
logistic.display(fit.1)


fit.2 <- fit.1

add1.test <- add1(fit.2, scope = .~. + .^2, test = "Chisq")
add1.test[order(add1.test$'Pr(>Chi)'),]

fit.3 <- update(fit.2, .~. + proc.time:mt.dt.1 )
summary(fit.3)
#fit.3 <- fit.2

# which variable to drop
drop1.test <- drop1(fit.3, test="Chisq")
drop1.test[rev(order(drop1.test$'Pr(>Chi)')),]

# droped: ica.stenting, g.tici
fit.4 <- update(fit.3, .~. - ang.steps)
#fit.4 <- fit.3

summary(fit.4)

#more interactions?
add1.test <- add1(fit.4, scope = .~. + .^2, test = "Chisq")
add1.test[order(add1.test$'Pr(>Chi)'),]

#fit.4 <- update(fit.4, .~. + lysis:first.pass + vessel.perforation:first.pass )
#summary(fit.4)


drop1.test <- drop1(fit.4, test="Chisq")
drop1.test[rev(order(drop1.test$'Pr(>Chi)')),]

#fit.4 <- update(fit.4, .~. - toast.ce)
#fit.4 <- fit.3

# outliers
influencePlot(fit.4)
outlierTest(fit.4)

final.fit <- update(fit.4, data = rb[-c(361),])

# final fit for paper 26.09.2019 (lysis, mca.m1, proc.time)
summary(final.fit)
logistic.display(final.fit)




# making model graphs
plot_model(final.fit, type = "pred", terms="ang.punct.to.end")
plot_model(final.fit, transform = NULL, show.values = TRUE, axis.labels = "", value.offset = .4)


# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# FIGURE 3
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
plot_model(full.model, type = "pred", terms=c("proc.time", "vessel.perforation"), 
           title = "",
           axis.title = c("Procedure time", "Subarachnoid hemorrhage probability"),
           legend.title = c("rt-PA"))


effect_plot(final.fit, pred = proc.time, data = rb, interval = TRUE)
# END making model graphs



# ROC calculations
myroc <- roc(rb$sah.only, rb$proc.time)
myroc$auc
ggroc(myroc)


# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# FIGURE 2
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
roc_i <- roc(rb$sah.only,
             rb$proc.time,
             legacy.axes=TRUE,
             ci=TRUE, 
             boot.n=2000, 
             ci.alpha=0.9, 
             stratified=TRUE,
             plot=TRUE, 
             auc.polygon=TRUE, 
             max.auc.polygon=TRUE, 
             grid=FALSE,
             print.auc=TRUE, 
             print.thres='best', 
             print.thres.best.method='y',
             print.thres.adj=c(-0.05, 1.25),#,c(-0.05, 2.0),c(-0.05, 5.0)),
             print.thres.pattern="Cut-off: %.1f \n\nSp: %.3f \nSe: %.3f",
             print.thres.pattern.cex = 1.5,
             print.auc.cex = 1.5,
             print.auc.y=0.2,
             print.auc.x=0.8,
             cex.axis=1.5,
             cex.lab=1.5,
             print.thres.pch=16,
             print.thres.cex=2.0,
             cex.main=1.5)










# cleaning database
na.check <- c("dob", "gender", "pre.mrs", "adm.nihss", "mrs.90d", "wakeup.stroke",
              "toast", "time.onset", "toast", "dissection", "lysis", "stroke",
              "pavk", "af", "dm", "ah", "ica.stenosis", "heart.dis", "antithromb",
              "anticoag", "doak", "smoker", "kidney.failure", "heart.failure",
              "statin.hist", "statin.disch", "crp", "glucosis", "hba1c", "creatinin",
              "chol", "tg", "hdl", "ldl", "leuco", "neu", "lym", "ery", "hct", 
              "tr", "fibrinogen", "inr", "img.date", "coll", "ang.punct.start",
              "ang.target.to.end", "ang.steps", "tici")


# quality check
ret.id <- function(x) {
  ifelse(nrow(rb[which(is.na(rb[x])), c("id")]) != 0, 
         sprintf ("for: %s na id s: %s ... n/N(perc): %1.0f/%1.0f (%1.1f)", 
                  x, 
                  myd[which(is.na(myd[x])), c("id")],
                  nrow(myd[which(is.na(myd[x])), c("id")]), nrow(myd),
                  nrow(myd[which(is.na(myd[x])), c("id")])/nrow(myd)*100),
         sprintf(""))
}

lapply(na.check, ret.id)
