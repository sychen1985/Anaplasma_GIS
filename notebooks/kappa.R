library(readxl)
kappa <- read_excel("C:/Users/sychen11/Box/Anaplasma projects/Shih-Yu_Dissertation/Dissertation/Chapter 3/Ch3_Analysis/data set/kappa.xlsx")
View(kappa)



library(vcd)
library(irr)

kappa <- kappa[!(is.na(kappa$perceived_inf)),]

table <- table(kappa$Infection_herd, kappa$perceived_inf)
print(table)

library(psych)
cohen.kappa(table)