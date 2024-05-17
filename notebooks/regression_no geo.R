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


library(lme4)

Sex <- glmer(ELISA ~ Sex + (1|LocationID), family=binomial, data=ch3)
summary(Sex)

Breed <- glmer(ELISA ~ Breed + (1|LocationID), family=binomial, data=ch3)
summary(Breed)

region <- glmer(ELISA ~ Region + (1|LocationID), family=binomial, data=ch3)
summary(region)

nomeasure <- glmer(ELISA ~ no_measures + (1|LocationID), family=binomial, data=ch3)
summary(nomeasure)

closed_herd <- glmer(ELISA ~ closed_herd + (1|LocationID), family=binomial, data=ch3)
summary(closed_herd)

habitat <- glmer(ELISA ~ habitat + (1|LocationID), family=binomial, data=ch3)
summary(habitat)

wildfires <- glmer(ELISA ~ wildfires + (1|LocationID), family=binomial, data=ch3)
summary(wildfires)

domestic_ruminants <- glmer(ELISA ~ domestic_ruminants + (1|LocationID), family=binomial, data=ch3)
summary(domestic_ruminants)

ticks_on_cattle <- glmer(ELISA ~ ticks_on_cattle + (1|LocationID), family=binomial, data=ch3)
summary(ticks_on_cattle)

tick_control <- glmer(ELISA ~ tick_control + (1|LocationID), family=binomial, data=ch3)
summary(tick_control)

change_ndls <- glmer(ELISA ~ change_ndls + (1|LocationID), family=binomial, data=ch3)
summary(change_ndls)










not_change_ndl <- glmer(ELISA ~ not_change_ndl + (1|LocationID), family=binomial, data=ch3)
summary(not_change_ndl)














