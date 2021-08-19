# *********************************************************************
# *********************************************************************
# function for importing CDK endovascular thrombectomy database
# history of changes: ...
# last change: 16.03.2021
# *********************************************************************
# *********************************************************************

cdk.evt.db.import <- function(db.name, db.label.name)
{
  
  # after incorporating entire 2012-2019 endovascular intervention dataset, at 21.4.2019...
  
  myd <- as_tibble(read_excel(db.name))
  
  # nr of rows
  nrow(myd)
  
  # factoring
  # list of data fields
  df.toconvert <- c("stroke", "ah", "dm",
                    "af",
                    "ica.stenosis",
                    "heart.dis",
                    "antithromb",
                    "anticoag", "doak", "smoker", "kidney.failure", "heart.failure",
                    "statin.hist", "statin.disch",
                    "hosp.death", "lysis", "pavk", "dissection",
                    "wakeup.stroke", "no.access"
  )
  
  
  
  myd[df.toconvert] <- lapply(myd[df.toconvert], conv.f)
  # calculate hospitalisation days from disc.date and death.date
  data.format <- c("%Y-%m-%d")
  myd$disc.date <- as_date(myd$disc.date)
  myd$death.date <- as_date(myd$death.date)
  myd$hosp.days <- ifelse(!is.na(myd$disc.date),
                          round(as.numeric(difftime(strptime(myd$disc.date, data.format), strptime(myd$date.time.onset, data.format), units="days")), 0),
                          ifelse(!is.na(myd$death.date),
                                 round(as.numeric(difftime(strptime(myd$death.date, data.format), strptime(myd$date.time.onset, data.format), units="days")), 0), NA
                          ))
  
  
  myd$CT_HUAvg <- as.numeric(myd$CT_HUAvg)
  # calculate tortuosity index ti
  myd$ti <- myd$mca.length.curve / myd$mca.length.straight
  myd$atartcombgr <- factor(myd$atartcombgr)
  
  myd$under16mm <- factor(ifelse(myd$dist.to.clot < 16, "yes", "no"))
  
  # first pass variable
  myd$first.pass <- NA
  myd$first.pass <- ifelse(myd$ang.steps == 1, "yes", "no")
  # set NA when no.access is "yes"
  myd$first.pass <- ifelse(myd$no.access == "yes", NA, myd$first.pass)
  myd$first.pass <- factor(myd$first.pass)
  
  # lyse - EVT overlapping? 
  # Calculated as onset.to.lyse + 60 min  < onset.to.punct => no overlaping
  #                                       >= onset.to.punct => overlaping
  # will be calculated after time calculations, see under
  
  # ica only with mca m1/m2
  myd$ica.with.mca <- ifelse(myd$vessel.type == "ica" & 
                               (myd$vessel.subtype == "m1" | 
                                  myd$vessel.subtype == "m2"), "ica+mca",
                             
                             ifelse(myd$vessel.type == "ica" & 
                                      myd$vessel.subtype == "ica", "ica", NA))
  
  myd$ica.with.mca <- factor(myd$ica.with.mca)
  
  myd$mca <- ifelse(myd$vessel.type == "mca" & 
                      (myd$vessel.subtype == "m1"), "mca m1", 
                    ifelse(myd$vessel.type == "mca" &
                             myd$vessel.subtype == "m2", "mca m2", NA))
  
  myd$mca <- factor(myd$mca)
  
  # ica stenting
  myd$ica.stenting <- factor(myd$ica.stenting)
  myd$ica.stenting <- relevel(myd$ica.stenting, "yes")
  
  # gender
  myd$gender <- factor(myd$gender)
  
  # BMI (weight / height^height (m))
  myd$bmi <- myd$weight / ((myd$height/100)^2)
  # TOAST classifications
  myd$toast <- factor(myd$toast)
  myd$toast.ce <- NA
  myd$toast.ce[myd$toast == "ce"] <- "ce+unk"
  myd$toast.ce[myd$toast == "unkn"] <- "ce+unk"
  myd$toast.ce[myd$toast == "laa" | myd$toast == "und" | myd$toast == "other"] <- "other"
  myd$toast.ce <- factor(myd$toast.ce)
  
  myd$toast.laa <- factor(ifelse(myd$toast == "laa", "laa", "other"))
  
  
  
  # procedural complications
  # 1=Pseudoaneurysma
  # 2=gro?es Leistenh?matom
  # 3= thromb Verschluss der A. femoralis
  # 4=ICA Dissection post thrombectomy
  # 5=splitting of the distal tip of Sofia5F - it remained in MCA M1 part (not removabel)
  # 6=Verschlu? der A. poplitea rechts (auf Punktionsseite)
  # 7=Perforation mit schwerer SAB
  myd$g.proc.compl <- NA
  myd$g.proc.compl[myd$proc.compl == 1 | myd$proc.compl == 2 
                   | myd$proc.compl == 3 | myd$proc.compl == 6] <- "distal compl." 
  
  myd$g.proc.compl[myd$proc.compl == 4 | myd$proc.compl == 5 
                   | myd$proc.compl == 7] <- "downstr. compl." 
  
  myd$g.proc.compl <- factor(myd$g.proc.compl)
  myd$g.proc.compl <- relevel(myd$g.proc.compl, "downstr. compl.")
  
  # vessel perforation
  myd$vessel.perforation <- factor(myd$vessel.perforation)
  myd$vessel.perforation <- relevel(myd$vessel.perforation, "yes")
  
  # anestesia type
  myd$anest.type <- factor(ifelse(myd$anest.type == 1, "GA", "Consc."))
  
  # ica occlusion height
  myd$ica.occl.height <- factor(myd$ica.occl.height)
  
  # collaterals
  myd$g.coll <- NA
  myd$g.coll[myd$coll == "1"] <- "absent"
  myd$g.coll[myd$coll == "2"] <- "good"
  myd$g.coll[myd$coll == "3"] <- "equal"
  myd$g.coll <- factor(myd$g.coll)
  
  myd$g.coll2 <- NA
  myd$g.coll2[myd$g.coll == "absent"] <- "absent"
  myd$g.coll2[myd$g.coll == "good" | myd$g.coll == "equal"] <- "good/equal"
  myd$g.coll2 <- factor(myd$g.coll2)
  
  # vessels
  myd$vessel.type <- factor(myd$vessel.type)
  myd$vessel.subtype <- factor(myd$vessel.subtype)
  myd$vessel.side <- factor(myd$vessel.side)
  
  # neutrophil/lymphocyte ratio
  myd$neu.lym.rat <- myd$neu / myd$lym
  
  # TICI
  myd$tici <- factor(myd$tici)
  myd$g.tici <- factor(ifelse(myd$tici == "0" | myd$tici == "1" | myd$tici == "2a", "0-2a", "2b-3"))
  myd$g.tici <- relevel(myd$g.tici, "2b-3")
  
  # TICI 2b, 3
  myd$g.tici2 <- factor(ifelse(myd$tici == "2b", "2b", "3"))
  
  # TICI 2b, 3
  myd$compl.reperfusion <- factor(ifelse(myd$tici == "3", "complete", "not complete"))
  myd$succ.reperfusion <- factor(ifelse(myd$tici == "2b" | myd$tici == "3", "successful", "not successful"))
  
  # pre-mRS
  myd$pre.mrs <- factor(myd$pre.mrs)
  myd$g.pre.mrs <- factor(ifelse(myd$pre.mrs == "0" | myd$pre.mrs == "1" | myd$pre.mrs == "2", 
                                 "0-2", "3-5"))
  
  # inr of 1.2 is normal
  myd$inr2 <- ifelse(myd$inr == 1.2, NA, myd$inr)
  
  # Clot density
  myd$CT_Ratio <- ifelse(is.na(myd$CT_Ratio), myd$CT_HUAvg / myd$CT_HUKontra, myd$CT_Ratio)
  
  # Type of EVT (stent retriever, aspiration...)
  # 1 Aspiration (ACE, Sofia, Catalyst)
  # 2 Aspiration first then Stentretriever (ACE, Sofia, Catalyst)
  # 3 Stentretriever and aspiration catheter together (over ACE, Sofia, newer catheters..)
  # 4 only stentretriever
  # 5 no access to target artery
  # 6 no neurointervention - other reasons (see comment)
  # 7 Penumbra Aspirationskatheter (old)
  # 8 Stentretriever and Aspiration together over DAC
  
  myd$evt.type <- factor(myd$evt.type)
  myd$evt.type2 <- NA
  myd$evt.type2[myd$evt.type == "1" | myd$evt.type == "7"] <- "aspir only"
  myd$evt.type2[myd$evt.type == "2" | myd$evt.type == "3" | myd$evt.type == "8"] <- "aspir + stent.retr"
  myd$evt.type2[myd$evt.type == "4"] <- "stent.retr only"
  myd$evt.type2 <- factor(myd$evt.type2)
  
  # stent retriever used
  myd$evt.type3 <- NA
  myd$evt.type3[myd$evt.type2 == "aspir + stent.retr" | myd$evt.type2 == "stent.retr only"] <- "stent retr. used"
  myd$evt.type3[myd$evt.type2 == "aspir only"] <- "aspir only"
  myd$evt.type3 <- factor(myd$evt.type3)
  
  
  # time analysis
  data.format <- c("%Y-%m-%d")
  myd$time.long <- factor(round(as.numeric(difftime(strptime("2019-01-01", data.format), 
                                                    strptime(myd$date.time.onset, data.format), units="days") / 365.242), 0))
  myd$date.time.onset.year <- factor(as.numeric(format(as.Date(myd$date.time.onset, format = "%Y-%m-%d"), "%Y")))
  
  myd$mrs6 <- ifelse(myd$mrs.90d == 6, 1, 0)
  #myd$mrs.90d[myd$mrs.90d == "folgt"] <- NA
  
  # order mrs.90d
  #myd$mrs.90d <- factor(myd$mrs.90d, ordered = TRUE)
  myd$mrs.90d <- factor(myd$mrs.90d)
  
  myd$g.mrs <- NA
  myd$g.mrs[myd$mrs.90d == "0" | myd$mrs.90d == "1" | myd$mrs.90d == "2"] <- "0-2"
  myd$g.mrs[myd$mrs.90d == "3" | myd$mrs.90d == "4" | myd$mrs.90d == "5" | myd$mrs.90d == "6"] <- "3-6"
  myd$g.mrs <- factor(myd$g.mrs)
  myd$g.mrs <- relevel(myd$g.mrs, "0-2")
  
  
  myd$wmh <- NA
  myd$wmh <- myd$front.l + myd$front.r + myd$par.occ.l + myd$par.occ.r + myd$temp.l + 
    myd$temp.r + myd$infrat.l + myd$infrat.r
  
  myd$g.wmh <- NA
  myd$g.wmh[myd$wmh == 0] <- "no wmh"
  myd$g.wmh[myd$wmh > 0] <- "wmh"
  myd$g.wmh <- factor(myd$g.wmh)
  myd$g.wmh <- relevel(myd$g.wmh, "wmh")
  
  # separate into none to mild vs. severe based in 75% percentile (not included!)
  myd$g.wmh2 <- NA
  perc75 <- quantile(myd$wmh, na.rm = T)[4]
  myd$g.wmh2[myd$wmh <= perc75] <- "absent to mild"
  myd$g.wmh2[myd$wmh > perc75] <- "severe"
  myd$g.wmh2 <- factor(myd$g.wmh2)
  
  # combination of g.wmh and g.coll2
  myd$g.coll.wmh <- NA
  myd$g.coll.wmh <- interaction(myd$g.wmh, myd$g.coll2)
  myd$g.coll.wmh <- factor(myd$g.coll.wmh)
  
  # collapse no wmh/good with wmh.good
  myd$g.coll.wmh2 <- NA
  myd$g.coll.wmh2[myd$g.coll.wmh == "wmh.absent"] <- "wmh.absent"
  myd$g.coll.wmh2[myd$g.coll.wmh == "no wmh.absent"] <- "no wmh.absent"
  myd$g.coll.wmh2[myd$g.coll.wmh == "wmh.good/equal" | 
                    myd$g.coll.wmh == "no wmh.good/equal"] <- "good coll"
  myd$g.coll.wmh2 <- factor(myd$g.coll.wmh2)
  
  # calculate age (dob - time onset)
  myd$age <- round(as.numeric(difftime(strptime(myd$date.time.onset, data.format), 
                                       strptime(myd$dob, data.format), units = "days")/365.242), 0)
  
  
  myd$wkday <- NA
  myd$wkday[myd$wakeup.stroke == 0 & !is.na(myd$wakeup.stroke)] <- 
    weekdays(myd$date.time.onset[myd$wakeup.stroke == "no" & !is.na(myd$wakeup.stroke)], abbr = TRUE)
  
  # agreggate Fr + Sa + So
  myd$wknd <- 0
  myd$wknd[myd$wkday == "Fr." | myd$wkday == "Sa." | myd$wkday == "So."] <- 1
  
  # symptomaci bleeding
  myd$sympt.bleed <- factor(myd$sympt.bleed)
  
  # sah only
  myd$sah.only <- "no"
  myd$sah.only[myd$bleed.class2 == "3c" & myd$bleed.class3 == "0" & myd$bleed.class1 == "0"] <- "yes"
  myd$sah.only <- factor(myd$sah.only)
  
  #nrow(myd[which(!is.na(myd$wakeup.stroke) & myd$wakeup.stroke == 0), c("id")])
  myd$bleed.class1[myd$bleed.class1 == "0"] <- NA
  myd$bleed.class1 <- factor(myd$bleed.class1)
  
  # whether remote bleeding is there
  myd$g.bleed1 <- "no"
  myd$g.bleed1[!is.na(myd$bleed.class2) | !is.na(myd$bleed.class3)] <- "yes"
  myd$g.bleed1 <- factor(myd$g.bleed1)
  
  # whether remote bleeding is accompanied with intraparenchymal bleeding
  myd$g.bleed2 <- "no"
  myd$g.bleed2[myd$g.bleed1 == "yes" & myd$bleed.class1 != "0"] <- "yes"
  myd$g.bleed2 <- factor(myd$g.bleed2)
  myd$g.bleed2 <- relevel(myd$g.bleed2, "yes")
  
  # whether there is more than one remote bleeding type
  myd$g.bleed3 <- "no"
  myd$g.bleed3[myd$bleed.class2 != "0" & myd$bleed.class3 != "0"] <- "yes"
  myd$g.bleed3 <- factor(myd$g.bleed3)
  myd$g.bleed3 <- relevel(myd$g.bleed3, "yes")
  
  # how many 3c were there (subarachnoidal bleeding)
  myd$g.bleed4 <- "no"
  myd$g.bleed4[myd$bleed.class2 == "3c" | myd$bleed.class3 == "3c"] <- "yes"
  myd$g.bleed4 <- factor(myd$g.bleed4)
  myd$g.bleed4 <- relevel(myd$g.bleed4, "yes")
  
  # SAH + other bleeding types
  myd$g.bleed5 <- "no"
  myd$g.bleed5[myd$g.bleed4 == "yes" & 
                 (myd$bleed.class1 != "0")] <- "yes"
  myd$g.bleed5[myd$g.bleed4 == "yes" & 
                 (myd$bleed.class2 != "0" & myd$bleed.class2 != "3c")] <- "yes"
  myd$g.bleed5[myd$g.bleed4 == "yes" & 
                 (myd$bleed.class3 != "0" & myd$bleed.class3 != "3c")] <- "yes"
  
  # bleeding without SAH
  myd$g.bleed6 <- "no"
  myd$g.bleed6[myd$g.bleed4 == "no" & (myd$bleed.class1 != "0" | myd$bleed.class2 != "0" |
                                         myd$bleed.class3 != "0")] <- "yes"
  
  
  
  # change bleed.class2 and 3 to NA when =="0" to have better consistency
  # (above is not made due to previous code versions compatibility)
  myd$bleed.class2[myd$bleed.class2 == "0"] <- NA
  myd$bleed.class3[myd$bleed.class3 == "0"] <- NA
  
  # any bleeding
  myd$bleed.any <- NA
  myd$bleed.any[is.na(myd$bleed.class1)] <- "no"
  myd$bleed.any[!is.na(myd$bleed.class1) | 
                  !is.na(myd$bleed.class2) | 
                  !is.na(myd$bleed.class3)] <- "yes"
  myd$bleed.any <- factor(myd$bleed.any)
  
  # only parenchymal bleedings
  myd$bleed.parenchym <- NA
  myd$bleed.parenchym[is.na(myd$bleed.class1)] <- "no"
  myd$bleed.parenchym[!is.na(myd$bleed.class1) &
                        is.na(myd$bleed.class2) &
                        is.na(myd$bleed.class3)] <- "yes"
  myd$bleed.parenchym <- factor(myd$bleed.parenchym)
  
  # number of Type 3a, 3b and 3d bleeding (3c == SAH!)
  myd$bleed.3a <- "no"
  myd$bleed.3a[myd$bleed.class2 == "3a" | myd$bleed.class3 == "3a"] <- "yes"
  myd$bleed.3b <- "no"
  myd$bleed.3b[myd$bleed.class2 == "3b" | myd$bleed.class3 == "3b"] <- "yes"
  myd$bleed.3d <- "no"
  myd$bleed.3d[myd$bleed.class2 == "3d" | myd$bleed.class3 == "3d"] <- "yes"
  
  myd$bleed.3a <- factor(myd$bleed.3a)
  myd$bleed.3b <- factor(myd$bleed.3b)
  myd$bleed.3d <- factor(myd$bleed.3d)
  
  # onset calculations
  time.format <- c("%Y-%m-%d %H:%M:%S")
  
  # onset to lysis
  myd$onset.to.lysis[which(!is.na(myd$date.time.onset) & !is.na(myd$lysis.date.time))] <- 
    as.numeric(
      difftime(
        strptime(myd$lysis.date.time[which(!is.na(myd$date.time.onset) & !is.na(myd$lysis.date.time))], time.format),
        strptime(myd$date.time.onset[which(!is.na(myd$date.time.onset) & !is.na(myd$lysis.date.time))], time.format), 
        units = "mins"))
  
  
  # onset to first image
  myd$onset.to.img <- NA
  
  myd$onset.to.img[which(!is.na(myd$img.date.nativ) & !is.na(myd$date.time.onset))] <- 
    as.numeric(
      difftime(
        strptime(myd$img.date.nativ[which(!is.na(myd$img.date.nativ) & !is.na(myd$date.time.onset))], time.format),
        strptime(myd$date.time.onset[which(!is.na(myd$img.date.nativ) & !is.na(myd$date.time.onset))], time.format), 
        units = "mins"))
  
  # onset to first angio puncture 
  myd$onset.to.punct <- NA
  
  myd$onset.to.punct[which(!is.na(myd$ang.punct.start) & !is.na(myd$date.time.onset))] <- 
    as.numeric(
      difftime(
        strptime(myd$ang.punct.start[which(!is.na(myd$ang.punct.start) & !is.na(myd$date.time.onset))], time.format),
        strptime(myd$date.time.onset[which(!is.na(myd$ang.punct.start) & !is.na(myd$date.time.onset))], time.format), 
        units = "mins"))
  
  myd$mca.m1 <- ifelse(myd$mca == "mca m1", "yes", "no")
  myd$mca.m1 <- ifelse(is.na(myd$mca.m1), "no", myd$mca.m1)
  myd$mca.m1 <- factor(myd$mca.m1)
  
  # onset to target (for thrombus migration paper)
  myd$onset.to.target <- myd$onset.to.punct + myd$ang.punct.to.target
  
  # lyse.mt overlapping
  myd$lysis.mt.overlap <- NA
  myd$lysis.mt.overlap[myd$lysis == "yes"] <-
    ifelse(myd$onset.to.lysis[myd$lysis == "yes"] + 60 < myd$onset.to.punct[myd$lysis == "yes"],
           "no overlap", "overlap")
  myd$lysis.mt.overlap <- factor(myd$lysis.mt.overlap)
  
  #lysis to puncture
  myd$lysis.to.puncture <- as.numeric(
    difftime(
      strptime(myd$ang.punct.start, time.format),
      strptime(myd$lysis.date.time, time.format), 
      units = "mins"))
  
  # onset to end of procedure
  myd$onset.to.end <- NA
  myd$onset.to.end <- myd$onset.to.punct + myd$proc.time
  
  # distance to clot 22.09.2019
  myd$dsa.open <- factor(myd$dsa.open)
  myd$at.art.present <- factor(myd$at.art.present)
  myd$cta.a1.ipsi.open <- factor(myd$cta.a1.ipsi.open)
  myd$acoa.open <- factor(myd$acoa.open)
  myd$acop.r.open <- factor(myd$acop.r.open)
  myd$acop.l.open <- factor(myd$acop.l.open)
  myd$fetal.r <- factor(myd$fetal.r)
  myd$fetal.l <- factor(myd$fetal.l)
  myd$dsa.at.artery.visible <- factor(myd$dsa.at.artery.visible)
  
  myd$ipsi.fetal <- "no"
  myd$ipsi.fetal[myd$fetal.r == "yes" & myd$vessel.side == "right"] <- "yes"
  myd$ipsi.fetal[myd$fetal.l == "yes" & myd$vessel.side == "left"] <- "yes"
  myd$ipsi.fetal <- factor(myd$ipsi.fetal)
  
  myd$ipsi.acop <- "no"
  myd$ipsi.acop[myd$acop.r.open == "yes" & myd$vessel.side == "right"] <- "yes"
  myd$ipsi.acop[myd$acop.l.open == "yes" & myd$vessel.side == "left"] <- "yes"
  myd$ipsi.acop <- factor(myd$ipsi.acop)
  
  
  myd$composit.type <- factor(myd$composit.type)
  myd$surface.type <- factor(myd$surface.type)
  
  
  # cow complete?
  myd$cow <- NA
  myd$cow <- ifelse(myd$acop.l.open == "yes" & myd$acop.r.open == "yes" 
                    & myd$acoa.open == "yes", "yes", "no")
  myd$cow <- factor(myd$cow) 
  
  # ct to angio time
  myd$ct.to.angio <- NA
  myd$ct.to.angio <- myd$onset.to.punct - myd$onset.to.img
  
  # ct to target
  myd$ct.to.target <- NA
  myd$ct.to.target <- myd$onset.to.target - myd$onset.to.img
  
  
  # distance to clot
  
  per25 <- quantile(myd$dist.to.clot, na.rm = T)[2]
  myd$dtc.grp1 <- NA
  myd$dtc.grp1 <- ifelse(myd$dist.to.clot <= per25, "yes", "no")
  myd$dtc.grp1 <- factor(myd$dtc.grp1)
  

  # !!! new methodology
  # 12.06.2020 
  # measure in DSA dsa.dist.to.clot as "last known position on CTA"
  # + first branching im M2
  #myd$dsa.dist.to.clot <- ifelse(is.na(myd$dsa.dist.to.clot) &
  #                                 myd$dsa.open == "yes",
  #                               myd$mca.length.curve, myd$dsa.dist.to.clot)
  # re-calculate delta for dsa.open == yes
  #myd$dtc.delta <- ifelse(is.na(myd$dtc.delta) &
  #                          myd$dsa.open == "yes",
  #                        myd$dsa.dist.to.clot - myd$dist.to.clot,
  #                        myd$dtc.delta)
  
  # when dsa.open is true, calculate dtc.delta with dsa.dist.to.clot = mca.length.curve
  myd$dsa.dist.to.clot <- ifelse(myd$dsa.open == "yes", myd$mca.length.curve, myd$dsa.dist.to.clot)
  # calculate dtc.delta 
  myd$dtc.delta <- myd$dsa.dist.to.clot - myd$dist.to.clot
  
  # 22.03.2021
  # divide into three groups
  # 1 - proximal migration (< 25 perc)
  # 2 - stable (within 25-75 percentile)
  # 3 - distal migration (> 75 percentile)
  # 4 - resolution
  sm <- summary(myd$dtc.delta)
  myd$migr.grp <- ifelse(myd$dtc.delta <= sm[2], "growth", 
    ifelse(myd$dtc.delta > sm[2] & myd$dtc.delta < sm[5], "stable",
      ifelse(myd$dtc.delta >= sm[5], "migration", NA))) 
  
  myd$migr.grp <- ifelse(myd$dsa.open == "yes", "resolution", myd$migr.grp)
  # Kaesmacher Definition of basal ganglia involvement
  # in spite of stable appearance of thrombus
  # so it should be reclassified to migration (when not growth or resolution)
  myd$migr.grp <- ifelse(myd$early.mov == "yes" & 
                           myd$migr.grp =="stable", "migration", myd$migr.grp)
  
  myd$migr.grp <- factor(myd$migr.grp)
  # make it ordinal
  myd$migr.grp <- factor(myd$migr.grp, levels = c("growth", "stable", "migration", "resolution" ))
  
  # second group
  # proximal, distal and resolution to thrombus migration
  # stable alone
  myd$migr.grp2 <- ifelse(myd$migr.grp == "growth", "growth", 
                          "other")
  
  myd$migr.grp2 <- factor(myd$migr.grp2)
  
  # third group
  myd$migr.grp3 <- ifelse(myd$migr.grp == "growth" | 
                            myd$migr.grp == "stable", "stable2", 
                          ifelse(myd$migr.grp == "migration" | 
                                   myd$migr.grp == "resolution", "distal+", NA))
  
  myd$migr.grp3 <- factor(myd$migr.grp3)
  
  # calculate velocity
  
  myd$vel.tot <- (myd$dtc.delta / myd$ct.to.target) * 100
  
  myd$vel.plus <- ifelse(myd$dtc.delta > 0, myd$dtc.delta/myd$ct.to.target, NA)
  myd$vel.plus <- ifelse(myd$vel.plus < 0, NA, myd$vel.plus) # N = 65
  
  myd$vel.minus <- ifelse(myd$dtc.delta <= 0, abs(myd$dtc.delta)/myd$ct.to.target, NA)
  myd$vel.minus <- ifelse(myd$vel.minus < 0, NA, myd$vel.minus) # N = 89
  
  # calculate angio end to control imaging in minutes
  # since we do not have always ang.proc.end but we have ang.punct.start and proc.time we will
  # 1. calculate difference in minutes between ang.punct.start and img.ctrl.date and then 
  # 2. substract proc time to this difference to become end of procedure time to image control time
  myd$img.ctrl.date <- as_datetime(myd$img.ctrl.date)
  myd$ang.end.to.img.ctrl <- 
    as.numeric(
      difftime(
        strptime(myd$img.ctrl.date, time.format),
        strptime(myd$ang.punct.start, time.format), 
        units = "mins"))
  
  myd$ang.end.to.img.ctrl <- myd$ang.end.to.img.ctrl - myd$proc.time
  
  # 23.03.2021
  # MCA tortuosity index
  # method: 10.1161/HYPERTENSIONAHA.118.11647
  myd$tort <- myd$mca.length.straight / myd$mca.length.curve
  
  myd$CT_Hyperdense <- factor(myd$CT_Hyperdense)
  
  # clinical recovery: adm.nihss - disc.nihss >= 2
  # significant recovery: >= 10
  myd$delta.nihss <- myd$adm.nihss - myd$disc.nihss
  
  myd$rec.nihss <- ifelse(myd$delta.nihss >= 10, "significant", 
                                 ifelse(myd$delta.nihss >= 2, "clinical",
                                        "none"))
  myd$rec.nihss <- ifelse(myd$hosp.death == "yes", "none", myd$rec.nihss)
  
  myd$rec.nihss <- factor(myd$rec.nihss)
  # reorder
  myd$rec.nihss <- factor(myd$rec.nihss, levels = c("none", "clinical", "significant"))
  
  # all diameters together
  myd$tot.diam <- myd$ica.ipsi.diam + myd$mca.m1.diam + myd$mca.m2.diam
  
  
  # again viscosity
  # viscosity
  # g/dl to g/L
  myd$tot.protein2 <- myd$tot.protein * 10
  myd$hct.perc <- myd$hct / 100
  # viscosity low shear stress
  myd$wbv.low <- myd$hct.perc * 1.89 + 3.76*(myd$tot.protein2 / 78.42)
  # in high shear stress
  myd$wbv.high <- myd$hct.perc * 0.12 + 0.17*(myd$tot.protein2 / 2.07)
  
  # Poiseuille's law (Q = (pi * pressure * diameter^4)/(8 * viscosity * length))
  myd$map <- (myd$sys.before + 2 * (myd$dys.before)) / 3
  # first version - whole length of MCA on DSA (where pressure is theoretically possible)
  #myd$pois.law <- (3.141569 * myd$sys.before *  myd$mca.m1.diam^4) / (8 * myd$wbv.high * myd$mca.length.curve)
  # second version - dist.to.clot - the tube where pressure exists (at start)
  myd$pois.law <- (3.141569 * myd$sys.before *  myd$mca.m1.diam^4) / (8 * myd$wbv.high * myd$dist.to.clot)
  
  # make it log
  myd$pois.law.log <- log(myd$pois.law)
  myd$pois.law.map <- (pi * myd$map *  myd$mca.m1.diam^4) / (8 * myd$wbv.high * myd$mca.length.curve)
  
  # blood viscosity approximation (Guyton 2000 in Seul-Ki Jeong 2013 BMC Neurol)
  myd$bv <- 1.4715 + 5.878 * (myd$hct/100) - 12.98*(myd$hct/100)^2 + 31.964*(myd$hct/100)^3
  
  # resistance to flow since difference between pressure is essentialy not known
  myd$resist.flow <- (8 * myd$bv * myd$dist.to.clot) / (pi *  myd$mca.m1.diam^4)
  myd$resist.flow.log <- log(myd$resist.flow)
  
  
  # exclude non-ICA and non-MCA patients
  excl.v <- table(myd$vessel.type)
  # exclude basilaris, vertebralis
  excl.v <- attributes(excl.v[-c(4, 5, 6, 7, 13, 15)])$dimnames[[1]]
  
  myd <- myd %>% 
    filter(myd$vessel.type %in% excl.v)
  
  name.list <- c()
  
  return (myd)
}

# ******************************************************************************
# imports database and make pre-specified restrictions
# used by projects:
#   distance to clot (2020-2021, Millesi, Pikija)
#   remote bleeding (SAH in EVT) (2020-2021, Pikija, Grisennauer)
#   thrombus migration (2021, Pikija...)
# ******************************************************************************
import.db <- function(db.name, db.labels.name)
{
  
  dtc <- cdk.evt.db.import(db.name, db.labels.name)
  myd.labels <- as_tibble(read_excel(db.labels.name))
  
  print(sprintf("Total Intervetions ACI/ACM: %0.0f", nrow(dtc)))
  
  #dtc <- dtc[dtc$tici == "2a" | dtc$tici == "2b" | dtc$tici == "3",]
  #sprintf("TICI 2a, 2b, 3: %0.0f", nrow(dtc))
  
  # take only M1 occlusions
  dtc <- dtc[dtc$vessel.type == "mca" & dtc$vessel.subtype == "m1",]
  sprintf("MCA M1 occlusions %0.0f", nrow(dtc))
  
  # take only with collateral score
  dtc <- dtc[!is.na(dtc$g.coll),]
  sprintf("With collateral score: %0.0f", nrow(dtc))
  
  # only with 3 month mRS available
  #dtc <- dtc[!is.na(dtc$g.mrs),] 
  #sprintf("With 3 months score: %0.0f", nrow(dtc))
  
  # remove NAs
  dtc <- dtc[!is.na(dtc$dsa.open), ]
  dtc <- dtc[!is.na(dtc$migr.grp), ]
  
  print(sprintf("Patient number: %0.0f", nrow(dtc)))
  
  #dtc <- dtc[!is.na(dtc$dsa.open),]
  
  # dtc$dtc.norm <- NA
  #dtc <- dtc[!is.na(dtc$atartcombgr),]
  #dtc <- dtc[!is.na(dtc$dtc.delta),]
  
  print(sprintf("Target Population: %0.0f", nrow(dtc)))
  
  # make variable labels
  var_lab(dtc$g.mrs) <- c("mRS at 3 months")
  
  
  # finaly, attach labels to data
  for (i in 1:nrow(myd.labels))
  {
    if (attributes(dtc[i])$names == myd.labels$name[i])
    {var_lab(dtc[i]) <- myd.labels$label[i]}
  }
  
  
  return (dtc)
  
} # end import.db

# DBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBD
# import database
# DBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBD

# make it clean - 21.04.2020
init.data <- function()
{
  setwd("C:/Sci/Stroke/CirculusWillisi/DistanceToClot")
  dtc <- import.db("C:/Sci/Stroke/EVT_Registry/EVT_Reg_v5.xlsx", 
                   "C:/Sci/Stroke/EVT_Registry/FieldLabels_EVT_Reg_v4.xlsx")
  
  # remove rows without dtc.delta (?)
  # possible loosing info on "resolution" thrombus - where no more thrombus is visible on DSA?
  
  # cohort I - only with dtc.delta
  #dtc <- dtc[!is.na(dtc$vel.tot), ]
  #dtc <- dtc[-c(3,4)]
  # remove outliers (!)
  #dtc <- dtc[dtc$dtc.delta > -10 & dtc$dtc.delta < 10,]
  sprintf("Number of Patients in Cohort I (only velocity): %0.0f", nrow(dtc))
  skim(dtc$vel.tot)
  # cohort II - with possible resolution patients?
  
  return (dtc)
}

# 30.03.2021
# Import remote bleeding database
import.rb.db <- function()
{
  dtc <- cdk.evt.db.import(db.name, db.labels.name)
  myd.labels <- as_tibble(read_excel(db.labels.name))
  
  print(sprintf("Total Intervetions ACI/ACM: %0.0f", nrow(dtc)))
  
}

# ***************************************************************************
# yes - no relevel with yes on first position
# ***************************************************************************
yesno.relevel <- function(db, to.relevel)
{
  
  for (i in to.relevel)
  {
    db[[i]] <- factor(db[[i]], c("yes", "no")) 
  }
  
  return(db)
}

# function to check which values in vector (column) are non-numeric
# credit to: https://stackoverflow.com/questions/21196106/finding-non-numeric-data-in-a-data-frame-or-vector
which.nonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}
