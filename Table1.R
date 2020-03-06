library(knitr)
library(table1)
library(dplyr)
library(tidyr)
library(mosaic)

crowd <- read.csv("C:/Users/olsonkai/Desktop/MSA clinic data.csv")

### combining *Unspecified, Patient Refused and Unknown in race category
levels(race) <- c("Unknown","American Indian and Alaska Native","Asian","Black or African American",
                 "More Than One Race","Native Hawaiian and Other Pacific Islander","Other","Unknown","Unknown",
                 "White or Caucasian")

### combining *Unspecified, Patient Refused and Unknown in ethnicity category
levels(eth) <- c("Unknown","Hispanic","Non-Hispanic","Unknown","Unknown")


### lables for table1

label(age) = "Age (years)"
label(gender) = "Gender"
label(arr) = "Arrival mode grouper"
label(ed) = "ED clinical disposition"
label(cduflag) = "CDU patient flag"
label(mar) = "Marital status"
label(sexor) = "Sexual orientation"
label(lang) = "Primary Language"
label(boardtime) = "Boarding time interval (min)"
label(toprovtime) = "ED door-to-provider time interval (min)"
label(wrminutes) = "Total time spent in any ED internal waiting area (min)"
label(LOS) = "Total ED length of stay (min)"
label(decisionmin) = "ED door-to-disposition decision time interval (min)"
label(race) = "Race"
label(eth) = "Ethnicity"
label(insurance) = "Insurance"
label(primcare) = "Primary Care Provider"
label(primlang) = "Primary Language"

cduflag <-
  factor(cduflag,
         labels=c("No","Yes"))

primlang <- 
  factor(primlang,
         labels=c("English",
                  "Non-English"))

primcare <-
  factor(primcare,
         labels=c("No","Yes"))

insurance <-
  factor(insurance,
         labels=c("No","Yes"))


### ADD P VALUES 


### RACE table1 with lines
table1(~ age + gender + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime| race*eth, data=crowd, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F)

### RACE table1 without lines
### without PCP, language
### assumptions: null = no insurance, null/asked and no pcp = no pcp, lang is english or not
table1(~age + gender + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime| race*eth, data=crowd, 
        overall="Overall",rowlabelhead = "Demographics & Medical History",
        topclass="Table1", render.continuous=c(.="Mean (SD)"))


### age (as categories) table1 with lines

cr = data.frame(myd, age, gender, race, eth, ben, arr, ed, cduflag, pcp, mar, sexor, boardtime, toprovtime, wrminutes, LOS, decisionmin,lang)

cr.age = cr %>%
  mutate(
    age = ifelse(age >0 & age <20, "0-19", 
                 ifelse(age >19 & age <30, "20-29",
                        ifelse(age >29 & age <40, "30-39",
                               ifelse(age >39 & age <50, "40-49",
                                      ifelse(age >49 & age <60, "50-59",
                                             ifelse(age >59 & age <70, "60-69",
                                                    ifelse(age >69 & age <80,"70-79",
                                                           ifelse(age >79 & age <90, "80-89", "90+")))))))))
table1(~ race + eth + gender + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime| cr.age$age, data=crowd, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F)


### gender table1 with lines
table1(~ race + eth + age + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime| gender, data=crowd, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F)

