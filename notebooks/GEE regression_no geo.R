library(readxl)
ch3 <- read_excel("C:/Users/sychen11/Box/Anaplasma projects/Shih-Yu_Dissertation/Dissertation/Chapter 3/Ch3_Analysis/data set/ch3_anaplasma.xlsx")
View(ch3)


library(plyr)
ch3$ELISA <- revalue(ch3$ELISA, c("Negative"=0, "Positive"=1))
ch3$ELISA <- as.numeric(as.character(ch3$ELISA))

ch3$ticks_on_cattle <- as.factor(ch3$ticks_on_cattle)
ch3$ticks_on_cattle <- relevel (ch3$ticks_on_cattle, ref = "None")

ch3$change_ndls <- as.factor(ch3$change_ndls)
ch3$change_ndls <- relevel (ch3$change_ndls, ref = "everyone")

ch3$closed_herd <- as.factor(ch3$closed_herd)
ch3$closed_herd <- relevel (ch3$closed_herd, ref = "Yes")





library(geepack)


ch31 <- ch3[!(is.na(ch3$Sex)),]
Sex <- geeglm(ELISA ~ Sex, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(Sex)

ch31 <- ch3[!(is.na(ch3$Breed)),]
Breed <- geeglm(ELISA ~ Breed, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(Breed)

ch31 <- ch3[!(is.na(ch3$Region)),]
Region <- geeglm(ELISA ~ Region, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(Region)

ch31 <- ch3[!(is.na(ch3$no_measures)),]
no_measures <- geeglm(ELISA ~ no_measures, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(no_measures)

ch31 <- ch3[!(is.na(ch3$closed_herd)),]
closed_herd <- geeglm(ELISA ~ closed_herd, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(closed_herd)

ch31 <- ch3[!(is.na(ch3$habitat)),]
habitat <- geeglm(ELISA ~ habitat, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(habitat)

ch31 <- ch3[!(is.na(ch3$wildfires)),]
wildfires <- geeglm(ELISA ~ wildfires, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(wildfires)

ch31 <- ch3[!(is.na(ch3$domestic_ruminants)),]
domestic_ruminants <- geeglm(ELISA ~ domestic_ruminants, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(domestic_ruminants)

ch31 <- ch3[!(is.na(ch3$ticks_on_cattle)),]
ticks_on_cattle <- geeglm(ELISA ~ ticks_on_cattle, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(ticks_on_cattle)

ch31 <- ch3[!(is.na(ch3$tick_control)),]
tick_control <- geeglm(ELISA ~ tick_control, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(tick_control)

ch31 <- ch3[!(is.na(ch3$change_ndls)),]
change_ndls <- geeglm(ELISA ~ change_ndls, family=binomial, id = LocationID, data=ch31, corstr = "exchangeable")
summary(change_ndls)






