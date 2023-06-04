# Related to the youtube video: 

# Install the following packages if you didn't so already -----------------
install.packages("ggplot2")
install.packages("survminer")
install.packages("gtsummary")
install.packages("readr")
install.packages("readxl")
install.packages("haven")


# Remove all objects from the R session 
# providing a clean slate to start fresh.  --------------------------------
rm(list=ls(all=TRUE))


# Run the packages -------------------------------
library(ggplot2)
library(survminer)
library(gtsummary) # tbl_regression()
library(survival)  # survfit()
library(readr)     # read_csv()
library(readr)     # read_csv()
library(readxl)    # read_excel()
library(haven)     # read_sav()  # for SPSS files 


# Import file -----------------------------------------------------------
dat <- read_sav("Overall Survival SPSS.sav")


# Groups must be factors --------------------------------------------------
#If your database has categorical variables with number (e.g, histologic grade 1, 2, 3), 
# R will understand them as numercic variables. Therefore, you should change them to factors

dat$Group <- factor(dat$GRUPO_Factor)



# Event status and time must be numeric ---------------------------------------------------
## The status must be numeric and must have "1" for event and "0" for censoring.

dat$Status <- as.numeric(dat$Evento_Status)

dat$Time <- as.numeric(dat$Tempo_Time)



fit <- survfit(Surv(Time, Status) ~ Group, data = dat) #we will have here onle one curve. Therfore, there will be no log-rank p value
fit # to see the medians and 95% CI
ggsurvplot(fit,
           pval = TRUE, #as I told you, even I ask for a p-value, in this example there will not be any.
           pval.coord = c(1, 0.2), # p-value and position (x,y)
           conf.int = FALSE, # state "TRUE" if you wanna see the shadows of 95%CI along the curve. 
           legend = c(0.5, 0.5), # group legend location
           legend.title = "Groups",
           legend.labs=c("Grupo1", "Grupo2"), 
           surv.median.line = "hv", # the options are: "none", "hv", "h", "v". v: vertical, h:horizontal
           risk.table = "absolute", # Add risk table: "absolute", "percentage", "abs_pct", "nrisk_cumcensor",  "nrisk_cumStatuss" 
           #risk.table.col = "strata", # Change risk table color by groups, if you want.
           risk.table.y.text = TRUE,  # Tirar nomes dos grupos do risk table: FALSE
           #linetype = "strata", # Change line type by groups, if you wish
           #break.time.by = 5, # break time axis by 5.
           xlab = "Time (months)", ylab = "Progression-Free Survival", #label the axis as you wish
           palette="jco", # "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty"
           title="Progression-Free Survival 1", font.title = c(16, "bold", "darkblue"), #lable the title as you wish.
)
#Information you might need:
fit
summary(fit ,time=36) #3 anos
summary(fit ,time=40) 
summary(fit ,time=60) # 5 anos
summary(fit ,time=120) # 10 anos

summary(fit ,time=36)$surv  #3 anos
summary(fit ,time=40)$surv 
summary(fit ,time=60)$surv # 5 anos
summary(fit ,time=120)$surv # 10 anos


# Cox_regression - Hazard Ratio -------------------------------------------
fit_cox <- coxph(Surv(Time, Status) ~ Group, data = dat)
summary(fit_cox)

fit_cox %>%
  tbl_regression(exp = TRUE)





########
########
## SPSS - Life Table
https://www.sv-europe.com/blog/survival-analysis-part-2/
  

Number entering interval -> the number of surviving cases at the beginning of the interval
Number Withdrawing during interval -> the n of censored cases. 
Number of exposed to risk = The number of surviving cases - 1/2 censored cases


- Number Entering Interval: This refers to the number of individuals who enter a specific time interval or interval of observation. 
It represents the individuals who are at risk of experiencing the event of interest (such as death or failure) at the beginning of the interval. 
In other words, it is the number of individuals who contribute to the analysis during that particular time interval.


