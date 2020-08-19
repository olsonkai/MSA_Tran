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

##aov(gender ~ ethrace)

### RACE table1 with lines
### assumptions: null = no insurance, null/asked and no pcp = no pcp, lang is english or not
table1(~ age + gender + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime| race*eth, data=crowd, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F)

table1(~ age + gender + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime + race+ eth, data=crowd, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F)


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


###################################################
# Combining Race and Ethnicity
###################################################

### assume unknown to be non-hispanic

ethrace = numeric()

for(i in 1:length(race)){
  ifelse(race[i] == "American Indian and Alaska Native" & eth[i] == "Hispanic", 
         ethrace[i] <- "Hispanic American Indian and Alaska Native",
         ifelse(race[i] == "American Indian and Alaska Native" & eth[i] == "Non-Hispanic"|
                  race[i]=="American Indian and Alaska Native" & eth[i]=="Unknown", ethrace[i] <- "Non-Hispanic American Indian and Alaska Native",
                ifelse(race[i] == "Asian" & eth[i] == "Hispanic", 
                       ethrace[i] <- "Hispanic Asian",
                       ifelse(race[i] == "Asian" & eth[i] == "Non-Hispanic" | race[i]=="Asian" & eth[i] == "Unknown", ethrace[i] <- "Non-Hispanic Asain",
                              ifelse(race[i] == "Black or African American" & eth[i]== "Hispanic", 
                                     ethrace[i]<- "Hispanic Black or African American",
                                     ifelse(race[i] == "Black or African American" & eth[i]== "Non-Hispanic" | race[i] == "Black or African American" & eth[i]== "Unknown", 
                                            ethrace[i]<- "Non-Hispanic Black or African American",
                                            ifelse(race[i] == "More Than One Race" & eth[i]== "Hispanic", 
                                                   ethrace[i]<- "Hispanic and More Than One Race",
                                                   ifelse(race[i] == "More Than One Race" & eth[i]== "Non-Hispanic" |  race[i] == "More Than One Race" & eth[i]== "Unknown",
                                                          ethrace[i]<- "Non-Hispanic and More Than One Race",
                                                          ifelse(race[i] == "Native Hawaiian and Other Pacific Islander" & eth[i]== "Hispanic", 
                                                                 ethrace[i]<- "Hispanic Native Hawaiian or Other Pacific Islander",
                                                                 ifelse(race[i] == "Native Hawaiian and Other Pacific Islander" & eth[i]== "Non-Hispanic" |race[i] == "Native Hawaiian and Other Pacific Islander" & eth[i]== "Unknown" , 
                                                                        ethrace[i]<- "Non-Hispanic Native Hawaiian or Other Pacific Islander",
                                                                        ifelse(race[i] == "Other" & eth[i]== "Hispanic", 
                                                                               ethrace[i]<- "Hispanic and Other Race",
                                                                               ifelse(race[i] == "Other" & eth[i]== "Non-Hispanic" | race[i]=="Other" & eth[i]=="Unknown", 
                                                                                      ethrace[i]<- "Non-Hispanic and Other Race",
                                                                                      ifelse(race[i] == "White or Caucasian" & eth[i]== "Hispanic", 
                                                                                             ethrace[i]<- "Hispanic White or Caucasian",
                                                                                             ifelse(race[i] == "White or Caucasian" & eth[i]== "Non-Hispanic" | race[i]=="White or Caucasian" & eth[i]=="Unknown", 
                                                                                                    ethrace[i]<- "Non-Hispanic White or Caucasian",
                                                                                                    ifelse(race[i] == "Unknown" & eth[i]== "Hispanic", 
                                                                                                           ethrace[i]<- "Hispanic with Unknown Race",
                                                                                                                  ethrace[i]<- "Non-Hispanic with Unknown Race")))))))))))))))
}

table(ethrace)

###################################################
# table 1 for new combined race/eth variable
###################################################

cr1 = data.frame(myd, age, gender, ethrace, ben, arr, ed, cduflag, pcp, mar, sexor, boardtime, toprovtime, wrminutes, LOS, decisionmin,lang)

### ethrace
label(ethrace) = "Race/Ethnicity"
table1(~ age + gender + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime| ethrace, data=crowd, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F)

### age
table1(~ ethrace + gender + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime| cr.age$age, data=crowd, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F)

### gender
table1(~ ethrace + age + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime| gender, data=crowd, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F)

table1(~ ethrace + age + gender +arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime , data=crowd, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F)

### add p-value 
### cannot get to work

rndr3 <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- cr1[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- summary(aov(y ~ cr1$ethrace))[[1]][["Pr(>F)"]][[1]]
    } else {
      p <- chisq.test(y ~ cr2$ethrace)$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

cr2$ethrace <- factor(cr2$ethrace, levels=c("Hispanic American Indian and Alaska Native", "Hispanic and More Than One Race", "Hispanic and Other Race","Hispanic Asian", "Hispanic Black or African American",
"Hispanic Native Hawaiian or Other Pacific Islander","Hispanic White or Caucasian", "Hispanic with Unknown Race", "Non-Hispanic American Indian and Alaska Native", "Non-Hispanic and More Than One Race", "Non-Hispanic and Other Race","Non-Hispanic Asian", "Non-Hispanic Black or African American",
"Non-Hispanic Native Hawaiian or Other Pacific Islander","Non-Hispanic White or Caucasian", "Non-Hispanic with Unknown Race", "333333"), labels=c("Hispanic American Indian and Alaska Native", "Hispanic and More Than One Race", "Hispanic and Other Race","Hispanic Asian", "Hispanic Black or African American",
                                                                                                                                                  "Hispanic Native Hawaiian or Other Pacific Islander","Hispanic White or Caucasian", "Hispanic with Unknown Race", "Non-Hispanic American Indian and Alaska Native", 
                                                                                                                                                  "Non-Hispanic and More Than One Race", "Non-Hispanic and Other Race","Non-Hispanic Asian", "Non-Hispanic Black or African American",
                                                                                                                                                  "Non-Hispanic Native Hawaiian or Other Pacific Islander","Non-Hispanic White or Caucasian", "Non-Hispanic with Unknown Race","ANOVA P-value"))
table1(~ age + gender + arr + ed + cduflag + mar + sexor + primlang + primcare + insurance +
         toprovtime+wrminutes+LOS +decisionmin + boardtime| cr2$ethrace, data=cr2, overall="Overall",
       render.continuous=c(.="Mean (SD)"), 
       topclass="Rtable1-shade Rtable1-grid Rtable1-zebra", 
       droplevels = F,
       render=rndr3)
