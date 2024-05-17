library(readxl)
chap3_herd <- read_excel("C:/Users/sychen11/Box/Anaplasma projects/Shih-Yu_Dissertation/Dissertation/Chapter 3/Ch3_Analysis/data set/ch3_anaplasma_herd-level.xlsx")
View(chap3_herd)

library(plyr)

chap3_herd$ticks_on_cattle <- as.factor(chap3_herd$ticks_on_cattle)
chap3_herd$ticks_on_cattle <- relevel (chap3_herd$ticks_on_cattle, ref = "None")

chap3_herd$change_ndls <- as.factor(chap3_herd$change_ndls)
chap3_herd$change_ndls <- relevel (chap3_herd$change_ndls, ref = "everyone")


chap3_herd$closed_herd <- as.factor(chap3_herd$closed_herd)
chap3_herd$closed_herd <- relevel (chap3_herd$closed_herd, ref = "Yes")


Region <- lm(prevalence ~ Region, data=chap3_herd)
summary(Region)

herd_size <- lm(prevalence ~ herd_size, data=chap3_herd)
summary(herd_size)

closed_herd <- lm(prevalence ~ closed_herd, data=chap3_herd)
summary(closed_herd)

habitat <- lm(prevalence ~ habitat, data=chap3_herd)
summary(habitat)

wildfires <- lm(prevalence ~ wildfires, data=chap3_herd)
summary(wildfires)

domestic_ruminants <- lm(prevalence ~ domestic_ruminants, data=chap3_herd)
summary(domestic_ruminants)

ticks_on_cattle <- lm(prevalence ~ ticks_on_cattle, data=chap3_herd)
summary(ticks_on_cattle)

tick_control <- lm(prevalence ~ tick_control, data=chap3_herd)
summary(tick_control)

change_ndls <- lm(prevalence ~ change_ndls, data=chap3_herd)
summary(change_ndls)












