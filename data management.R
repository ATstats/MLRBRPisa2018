library(haven)
library(dplyr)
library(writexl)
setwd("C:/Users/albert go/Desktop/Schoolworks/BAYESTA/Project FIles")
schqq <- read_sas("sch_quest.sas7bdat")
stuqq <- read_sas("stu_quest.sas7bdat")

schqq_phl <- schqq %>% filter(CNT=="PHL")
stuqq_phl <- stuqq %>% filter(CNT=="PHL")

schqq_phl_var <- schqq_phl[,c("CNTSCHID","STRATUM","SC061Q01TA","SC061Q02TA","SC061Q03TA"
                              ,"SC061Q04TA", "SC061Q05TA", "SC061Q11HA","SC061Q06TA"
                              ,"SC061Q07TA","SC061Q08TA","SC061Q09TA","SC061Q10TA")]

stuqq_phl_var <- stuqq_phl[,c("CNTSCHID","ST004D01T","ST123Q02NA","ST123Q03NA","ST123Q04NA","ESCS","PV1MATH",
                              "PV2MATH","PV3MATH","PV4MATH","PV5MATH","PV6MATH","PV7MATH","PV8MATH"
                              ,"PV9MATH","PV10MATH")]
#Note: ST004D01T - Gender with 1 - Female 2 - Male
#
View(stuqq_phl_var)

library(DataExplorer)
get_all_vars(stuqq_phl_var)
create_report(schqq_phl_var)
create_report(stuqq_phl_var)

#Merge Data Sets
qq_phl_merged <- merge(schqq_phl_var, stuqq_phl_var, by=c("CNTSCHID"))
qq_phl_merged <- qq_phl_merged %>% mutate(student_hindrance =(SC061Q01TA+SC061Q02TA+SC061Q03TA+SC061Q04TA+SC061Q05TA+SC061Q11HA)/6
                                          , teacher_hindrance = (SC061Q06TA+SC061Q07TA+SC061Q08TA+SC061Q09TA+SC061Q10TA)/5
                                          , PERFMATH = (PV1MATH+PV2MATH+PV3MATH+PV4MATH+PV5MATH+PV6MATH+PV7MATH+
                                                               PV8MATH+PV9MATH+PV10MATH)/10
                                          , parent_support = (ST123Q02NA+ST123Q03NA+ST123Q04NA)/3
                                          , gender = recode(ST004D01T, '1'=1,'2'=0))
View(qq_phl_merged)
write_xlsx(qq_phl_merged, "Bayestat Data.xlsx")

library(readxl)
qq_phl_merged <- read_excel("Bayestat Data.xlsx")
qq_phl_merged_complete <- na.omit(qq_phl_merged)
View(qq_phl_merged_complete)

#Traditional Multilevel Model
#Step 1 - Examine the Intraclass Correlation Coefficient in mixed-effect models (ICC(1)) of the outcome

library(nlme)

#ICC(1)
Null.Model <- lme(PERFMATH ~ 1, random = ~1|CNTSCHID, data = qq_phl_merged_complete, control=list(opt="optim"))

VarCorr(Null.Model)
1806.535/(1806.535+3377.601)

Null.gls <- gls(PERFMATH ~1, data = qq_phl_merged_complete)
anova(Null.gls,Null.Model)

#There is significant intercept variation. 34.85% of the variation in PV1MATH scores is a function of the school
#where they belong. Thus, a model that allows for random variation in PV1MATH among schools is a better fit than a model that does not allow random variation

#Step 2 - Explain Level 1 and Level 2 Intercept Variance
#Level 1 - ESCS, parent_support, gender
#Level 2 - student_hindrance, teacher_hindrance

Model.1a<-lme(PERFMATH~ESCS+parent_support+gender+,random=~student_hindrance+teacher_hindrance|CNTSCHID,
              data=qq_phl_merged_complete,control=list(opt="optim"))

Model.1b<-lme(PERFMATH~ESCS+parent_support+gender+student_hindrance*ESCS+teacher_hindrance*ESCS ,random=~student_hindrance+teacher_hindrance|CNTSCHID,
             data=qq_phl_merged_complete,control=list(opt="optim"))

summary(Model.1a)
coef(Model.1a)

summary(Model.1b)

#Bayesian Multilevel Model
library(rstanarm)
library(rstan)
library(brms)
options(contrasts = c("contr.sum","contr.poly"))

#Step 1 - Intercept only Model
brms_interceptonly <- brm(PERFMATH ~ 1 + (1|CNTSCHID),
                          data = qq_phl_merged_complete,
                          warmup = 100,
                          iter = 200,
                          chains = 2,
                          cores = 2)

get_prior(PERFMATH~ESCS+gender+parent_support+(student_hindrance+teacher_hindrance|CNTSCHID),
          data=qq_phl_merged_complete)

prior1<- c(prior(normal(0,10), class = Intercept),
           prior(normal(0, 10), class = b, coef = ESCS),
           prior(normal(0, 10), class = b, coef = gender),
           prior(normal(0, 10), class = b, coef = parent_support),
           prior(cauchy(0,10), class = sigma),
           prior(lkj(2), class=cor))

mod1<-brm(PERFMATH~ESCS+gender+parent_support+(student_hindrance+teacher_hindrance|CNTSCHID),
          data=qq_phl_merged_complete,
          family = gaussian(),
          prior=prior1,
          warmup=2000,
          iter = 5000,
          chains=2)
posterior_summary(mod1, pars = c("^b_", "^sd_", "sigma"), probs = c(0.025, 0.975) )
