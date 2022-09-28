# analysis_SM.R
# Replication code for the Supplementary Materials for
# "Survey Attention and Self-Reported Political Behavior"
# by R. Michael Alvarez and Yimeng Li
# September 28, 2022.

# Set directory
setwd("C:/OneDrive - California Institute of Technology/Methodology - Attention Filter/Code/POQ Replication Package")

# Load libraries
library(dplyr)
# library(ggplot2)
library(margins)
# library(stringr)
# library(tidyr)

# Load dataset
# Note: We store turnout and demographic information in OCsurvey_turnout and OCsurvey_demo separately so that individual voters cannot be identified.

load("./OCsurvey_turnout.rda")
OCsurvey <- OCsurvey_turnout
rm(OCsurvey_turnout)

load("./OCsurvey_demo.rda")

options(digits = 3)

#*************************************************************
# Figure SM1
#*************************************************************

# Note: Figure SM1 is taken from Figure 8 of the book
# Alvarez, R. Michael, Nicholas Adams-Cohen, Seo-young Silvia Kim, and Yimeng Li.
# Securing American elections: How data-driven election monitoring can improve our democracy.
# Cambridge University Press, 2020.

#*************************************************************
# Table SM1
#*************************************************************

summary(margins(glm(AF_IRI1 ~ age_SR_factor + gender_SR_factor + educ_SR_factor + race_SR_factor, data = OCsurvey_demo, family = "binomial")))
summary(margins(glm(AF_IMC1 ~ age_SR_factor + gender_SR_factor + educ_SR_factor + race_SR_factor, data = OCsurvey_demo, family = "binomial")))

#*************************************************************
# Table SM2
#*************************************************************

# Birth year:
table(OCsurvey$AF_IRI1[(OCsurvey$Asked_BirthYear == TRUE)], useNA = "always")
table(OCsurvey$AF_IMC1[(OCsurvey$Asked_BirthYear == TRUE)], useNA = "always")
table(OCsurvey$AF_comb[(OCsurvey$Asked_BirthYear == TRUE)], useNA = "always")
prop.table(table(OCsurvey$AF_IRI1[(OCsurvey$Asked_BirthYear == TRUE)], OCsurvey$BirthYear_Flag[(OCsurvey$Asked_BirthYear == TRUE)], useNA = "always"), margin = 1)
prop.table(table(OCsurvey$AF_IMC1[(OCsurvey$Asked_BirthYear == TRUE)], OCsurvey$BirthYear_Flag[(OCsurvey$Asked_BirthYear == TRUE)], useNA = "always"), margin = 1)
prop.table(table(OCsurvey$AF_comb[(OCsurvey$Asked_BirthYear == TRUE)], OCsurvey$BirthYear_Flag[(OCsurvey$Asked_BirthYear == TRUE)], useNA = "always"), margin = 1)

# City of Residence:
table(OCsurvey$AF_IRI1[(OCsurvey$Asked_CityResidence == TRUE)], useNA = "always")
table(OCsurvey$AF_IMC1[(OCsurvey$Asked_CityResidence == TRUE)], useNA = "always")
table(OCsurvey$AF_comb[(OCsurvey$Asked_CityResidence == TRUE)], useNA = "always")
prop.table(table(OCsurvey$AF_IRI1[(OCsurvey$Asked_CityResidence == TRUE)], OCsurvey$CityResidence_Flag[(OCsurvey$Asked_CityResidence == TRUE)], useNA = "always"), margin = 1)
prop.table(table(OCsurvey$AF_IMC1[(OCsurvey$Asked_CityResidence == TRUE)], OCsurvey$CityResidence_Flag[(OCsurvey$Asked_CityResidence == TRUE)], useNA = "always"), margin = 1)
prop.table(table(OCsurvey$AF_comb[(OCsurvey$Asked_CityResidence == TRUE)], OCsurvey$CityResidence_Flag[(OCsurvey$Asked_CityResidence == TRUE)], useNA = "always"), margin = 1)

# Registration - Before or After Jan 1, 2017
table(OCsurvey$AF_IRI1[(OCsurvey$Asked_RegTiming == TRUE)], useNA = "always")
table(OCsurvey$AF_IMC1[(OCsurvey$Asked_RegTiming == TRUE)], useNA = "always")
table(OCsurvey$AF_comb[(OCsurvey$Asked_RegTiming == TRUE)], useNA = "always")
prop.table(table(OCsurvey$AF_IRI1[(OCsurvey$Asked_RegTiming == TRUE)], OCsurvey$RegTiming_Flag[(OCsurvey$Asked_RegTiming == TRUE)], useNA = "always"), margin = 1)
prop.table(table(OCsurvey$AF_IMC1[(OCsurvey$Asked_RegTiming == TRUE)], OCsurvey$RegTiming_Flag[(OCsurvey$Asked_RegTiming == TRUE)], useNA = "always"), margin = 1)
prop.table(table(OCsurvey$AF_comb[(OCsurvey$Asked_RegTiming == TRUE)], OCsurvey$RegTiming_Flag[(OCsurvey$Asked_RegTiming == TRUE)], useNA = "always"), margin = 1)

#*************************************************************
# Table SM3-8
#*************************************************************

# Note: Table SM3-8 are replicated in analysis.R

#*************************************************************
# Table SM9
#*************************************************************

Inacc_Turnout_SM <- list()

Inacc_Turnout_SM$Gen2018_fBoth <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_Turnout_SM$Gen2018_mixed <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_comb == 1))
Inacc_Turnout_SM$Gen2018_pBoth <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_Turnout_SM$Gen2018_fAll <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_Turnout_SM$Gen2018_pAll <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_Turnout_SM$Gen2016_fBoth <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_Turnout_SM$Gen2016_mixed <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_comb == 1))
Inacc_Turnout_SM$Gen2016_pBoth <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_Turnout_SM$Gen2016_fAll <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_Turnout_SM$Gen2016_pAll <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_Turnout_SM$Gen2014_fBoth <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_Turnout_SM$Gen2014_mixed <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_comb == 1))
Inacc_Turnout_SM$Gen2014_pBoth <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_Turnout_SM$Gen2014_fAll <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_Turnout_SM$Gen2014_pAll <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_Turnout_SM$Gen2012_fBoth <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_Turnout_SM$Gen2012_mixed <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_comb == 1))
Inacc_Turnout_SM$Gen2012_pBoth <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_Turnout_SM$Gen2012_fAll <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_Turnout_SM$Gen2012_pAll <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_Turnout_SM$PRI2018_fBoth <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_Turnout_SM$PRI2018_mixed <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_comb == 1))
Inacc_Turnout_SM$PRI2018_pBoth <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_Turnout_SM$PRI2018_fAll <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_Turnout_SM$PRI2018_pAll <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_Turnout_SM$PRI2016_fBoth <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_Turnout_SM$PRI2016_mixed <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_comb == 1))
Inacc_Turnout_SM$PRI2016_pBoth <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_Turnout_SM$PRI2016_fAll <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_Turnout_SM$PRI2016_pAll <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Table_Inacc_Turnout_SM <- as.data.frame(
  rbind(
    c("2018 General", "Fail Both", Inacc_Turnout_SM$Gen2018_fBoth$coefficients, summary(Inacc_Turnout_SM$Gen2018_fBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2018_fBoth)),
    c("2018 General", "Fail One", Inacc_Turnout_SM$Gen2018_mixed$coefficients, summary(Inacc_Turnout_SM$Gen2018_mixed)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2018_mixed)),
    c("2018 General", "Pass Both", Inacc_Turnout_SM$Gen2018_pBoth$coefficients, summary(Inacc_Turnout_SM$Gen2018_pBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2018_pBoth)),
    c("2018 General", "Fail All", Inacc_Turnout_SM$Gen2018_fAll$coefficients, summary(Inacc_Turnout_SM$Gen2018_fAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2018_fAll)),
    c("2018 General", "Pass All", Inacc_Turnout_SM$Gen2018_pAll$coefficients, summary(Inacc_Turnout_SM$Gen2018_pAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2018_pAll)),
    c("2018 Primary", "Fail Both", Inacc_Turnout_SM$PRI2018_fBoth$coefficients, summary(Inacc_Turnout_SM$PRI2018_fBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2018_fBoth)),
    c("2018 Primary", "Fail One", Inacc_Turnout_SM$PRI2018_mixed$coefficients, summary(Inacc_Turnout_SM$PRI2018_mixed)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2018_mixed)),
    c("2018 Primary", "Pass Both", Inacc_Turnout_SM$PRI2018_pBoth$coefficients, summary(Inacc_Turnout_SM$PRI2018_pBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2018_pBoth)),
    c("2018 Primary", "Fail All", Inacc_Turnout_SM$PRI2018_fAll$coefficients, summary(Inacc_Turnout_SM$PRI2018_fAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2018_fAll)),
    c("2018 Primary", "Pass All", Inacc_Turnout_SM$PRI2018_pAll$coefficients, summary(Inacc_Turnout_SM$PRI2018_pAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2018_pAll)),
    c("2016 General", "Fail Both", Inacc_Turnout_SM$Gen2016_fBoth$coefficients, summary(Inacc_Turnout_SM$Gen2016_fBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2016_fBoth)),
    c("2016 General", "Fail One", Inacc_Turnout_SM$Gen2016_mixed$coefficients, summary(Inacc_Turnout_SM$Gen2016_mixed)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2016_mixed)),
    c("2016 General", "Pass Both", Inacc_Turnout_SM$Gen2016_pBoth$coefficients, summary(Inacc_Turnout_SM$Gen2016_pBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2016_pBoth)),
    c("2016 General", "Fail All", Inacc_Turnout_SM$Gen2016_fAll$coefficients, summary(Inacc_Turnout_SM$Gen2016_fAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2016_fAll)),
    c("2016 General", "Pass All", Inacc_Turnout_SM$Gen2016_pAll$coefficients, summary(Inacc_Turnout_SM$Gen2016_pAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2016_pAll)),
    c("2016 Primary", "Fail Both", Inacc_Turnout_SM$PRI2016_fBoth$coefficients, summary(Inacc_Turnout_SM$PRI2016_fBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2016_fBoth)),
    c("2016 Primary", "Fail One", Inacc_Turnout_SM$PRI2016_mixed$coefficients, summary(Inacc_Turnout_SM$PRI2016_mixed)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2016_mixed)),
    c("2016 Primary", "Pass Both", Inacc_Turnout_SM$PRI2016_pBoth$coefficients, summary(Inacc_Turnout_SM$PRI2016_pBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2016_pBoth)),
    c("2016 Primary", "Fail All", Inacc_Turnout_SM$PRI2016_fAll$coefficients, summary(Inacc_Turnout_SM$PRI2016_fAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2016_fAll)),
    c("2016 Primary", "Pass All", Inacc_Turnout_SM$PRI2016_pAll$coefficients, summary(Inacc_Turnout_SM$PRI2016_pAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$PRI2016_pAll)),
    c("2014 General", "Fail Both", Inacc_Turnout_SM$Gen2014_fBoth$coefficients, summary(Inacc_Turnout_SM$Gen2014_fBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2014_fBoth)),
    c("2014 General", "Fail One", Inacc_Turnout_SM$Gen2014_mixed$coefficients, summary(Inacc_Turnout_SM$Gen2014_mixed)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2014_mixed)),
    c("2014 General", "Pass Both", Inacc_Turnout_SM$Gen2014_pBoth$coefficients, summary(Inacc_Turnout_SM$Gen2014_pBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2014_pBoth)),
    c("2014 General", "Fail All", Inacc_Turnout_SM$Gen2014_fAll$coefficients, summary(Inacc_Turnout_SM$Gen2014_fAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2014_fAll)),
    c("2014 General", "Pass All", Inacc_Turnout_SM$Gen2014_pAll$coefficients, summary(Inacc_Turnout_SM$Gen2014_pAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2014_pAll)),        
    c("2012 General", "Fail Both", Inacc_Turnout_SM$Gen2012_fBoth$coefficients, summary(Inacc_Turnout_SM$Gen2012_fBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2012_fBoth)),
    c("2012 General", "Fail One", Inacc_Turnout_SM$Gen2012_mixed$coefficients, summary(Inacc_Turnout_SM$Gen2012_mixed)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2012_mixed)),
    c("2012 General", "Pass Both", Inacc_Turnout_SM$Gen2012_pBoth$coefficients, summary(Inacc_Turnout_SM$Gen2012_pBoth)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2012_pBoth)),
    c("2012 General", "Fail All", Inacc_Turnout_SM$Gen2012_fAll$coefficients, summary(Inacc_Turnout_SM$Gen2012_fAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2012_fAll)),
    c("2012 General", "Pass All", Inacc_Turnout_SM$Gen2012_pAll$coefficients, summary(Inacc_Turnout_SM$Gen2012_pAll)$coef[,"Std. Error"], confint(Inacc_Turnout_SM$Gen2012_pAll))
  ), stringsAsFactors = FALSE
)

colnames(Table_Inacc_Turnout_SM) <- c("Election", "Group", "Estimate", "SE", "Lower", "Upper")

#*************************************************************
# Table SM10
#*************************************************************

Inacc_ByMail_SM <- list()

Inacc_ByMail_SM$Gen2018_fBoth <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_ByMail_SM$Gen2018_mixed <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_comb == 1))
Inacc_ByMail_SM$Gen2018_pBoth <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_ByMail_SM$Gen2018_fAll <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_ByMail_SM$Gen2018_pAll <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_ByMail_SM$Gen2016_fBoth <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_ByMail_SM$Gen2016_mixed <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_comb == 1))
Inacc_ByMail_SM$Gen2016_pBoth <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_ByMail_SM$Gen2016_fAll <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_ByMail_SM$Gen2016_pAll <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_ByMail_SM$Gen2014_fBoth <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_ByMail_SM$Gen2014_mixed <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_comb == 1))
Inacc_ByMail_SM$Gen2014_pBoth <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_ByMail_SM$Gen2014_fAll <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_ByMail_SM$Gen2014_pAll <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_ByMail_SM$Gen2012_fBoth <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_ByMail_SM$Gen2012_mixed <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_comb == 1))
Inacc_ByMail_SM$Gen2012_pBoth <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_ByMail_SM$Gen2012_fAll <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_ByMail_SM$Gen2012_pAll <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_ByMail_SM$PRI2018_fBoth <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_ByMail_SM$PRI2018_mixed <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_comb == 1))
Inacc_ByMail_SM$PRI2018_pBoth <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_ByMail_SM$PRI2018_fAll <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_ByMail_SM$PRI2018_pAll <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Inacc_ByMail_SM$PRI2016_fBoth <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE))
Inacc_ByMail_SM$PRI2016_mixed <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_comb == 1))
Inacc_ByMail_SM$PRI2016_pBoth <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE))
Inacc_ByMail_SM$PRI2016_fAll <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == FALSE, AF_IMC1 == FALSE, AF_IRI2 == FALSE | AF_IMC2 == FALSE))
Inacc_ByMail_SM$PRI2016_pAll <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == TRUE, AF_IMC1 == TRUE, AF_IRI2 == TRUE | AF_IMC2 == TRUE))

Table_Inacc_ByMail_SM <- as.data.frame(
  rbind(
    c("2018 General", "Fail Both", Inacc_ByMail_SM$Gen2018_fBoth$coefficients, summary(Inacc_ByMail_SM$Gen2018_fBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2018_fBoth)),
    c("2018 General", "Fail One", Inacc_ByMail_SM$Gen2018_mixed$coefficients, summary(Inacc_ByMail_SM$Gen2018_mixed)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2018_mixed)),
    c("2018 General", "Pass Both", Inacc_ByMail_SM$Gen2018_pBoth$coefficients, summary(Inacc_ByMail_SM$Gen2018_pBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2018_pBoth)),
    c("2018 General", "Fail All", Inacc_ByMail_SM$Gen2018_fAll$coefficients, summary(Inacc_ByMail_SM$Gen2018_fAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2018_fAll)),
    c("2018 General", "Pass All", Inacc_ByMail_SM$Gen2018_pAll$coefficients, summary(Inacc_ByMail_SM$Gen2018_pAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2018_pAll)),
    c("2018 Primary", "Fail Both", Inacc_ByMail_SM$PRI2018_fBoth$coefficients, summary(Inacc_ByMail_SM$PRI2018_fBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2018_fBoth)),
    c("2018 Primary", "Fail One", Inacc_ByMail_SM$PRI2018_mixed$coefficients, summary(Inacc_ByMail_SM$PRI2018_mixed)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2018_mixed)),
    c("2018 Primary", "Pass Both", Inacc_ByMail_SM$PRI2018_pBoth$coefficients, summary(Inacc_ByMail_SM$PRI2018_pBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2018_pBoth)),
    c("2018 Primary", "Fail All", Inacc_ByMail_SM$PRI2018_fAll$coefficients, summary(Inacc_ByMail_SM$PRI2018_fAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2018_fAll)),
    c("2018 Primary", "Pass All", Inacc_ByMail_SM$PRI2018_pAll$coefficients, summary(Inacc_ByMail_SM$PRI2018_pAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2018_pAll)),
    c("2016 General", "Fail Both", Inacc_ByMail_SM$Gen2016_fBoth$coefficients, summary(Inacc_ByMail_SM$Gen2016_fBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2016_fBoth)),
    c("2016 General", "Fail One", Inacc_ByMail_SM$Gen2016_mixed$coefficients, summary(Inacc_ByMail_SM$Gen2016_mixed)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2016_mixed)),
    c("2016 General", "Pass Both", Inacc_ByMail_SM$Gen2016_pBoth$coefficients, summary(Inacc_ByMail_SM$Gen2016_pBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2016_pBoth)),
    c("2016 General", "Fail All", Inacc_ByMail_SM$Gen2016_fAll$coefficients, summary(Inacc_ByMail_SM$Gen2016_fAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2016_fAll)),
    c("2016 General", "Pass All", Inacc_ByMail_SM$Gen2016_pAll$coefficients, summary(Inacc_ByMail_SM$Gen2016_pAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2016_pAll)),
    c("2016 Primary", "Fail Both", Inacc_ByMail_SM$PRI2016_fBoth$coefficients, summary(Inacc_ByMail_SM$PRI2016_fBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2016_fBoth)),
    c("2016 Primary", "Fail One", Inacc_ByMail_SM$PRI2016_mixed$coefficients, summary(Inacc_ByMail_SM$PRI2016_mixed)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2016_mixed)),
    c("2016 Primary", "Pass Both", Inacc_ByMail_SM$PRI2016_pBoth$coefficients, summary(Inacc_ByMail_SM$PRI2016_pBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2016_pBoth)),
    c("2016 Primary", "Fail All", Inacc_ByMail_SM$PRI2016_fAll$coefficients, summary(Inacc_ByMail_SM$PRI2016_fAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2016_fAll)),
    c("2016 Primary", "Pass All", Inacc_ByMail_SM$PRI2016_pAll$coefficients, summary(Inacc_ByMail_SM$PRI2016_pAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$PRI2016_pAll)),
    c("2014 General", "Fail Both", Inacc_ByMail_SM$Gen2014_fBoth$coefficients, summary(Inacc_ByMail_SM$Gen2014_fBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2014_fBoth)),
    c("2014 General", "Fail One", Inacc_ByMail_SM$Gen2014_mixed$coefficients, summary(Inacc_ByMail_SM$Gen2014_mixed)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2014_mixed)),
    c("2014 General", "Pass Both", Inacc_ByMail_SM$Gen2014_pBoth$coefficients, summary(Inacc_ByMail_SM$Gen2014_pBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2014_pBoth)),
    c("2014 General", "Fail All", Inacc_ByMail_SM$Gen2014_fAll$coefficients, summary(Inacc_ByMail_SM$Gen2014_fAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2014_fAll)),
    c("2014 General", "Pass All", Inacc_ByMail_SM$Gen2014_pAll$coefficients, summary(Inacc_ByMail_SM$Gen2014_pAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2014_pAll)),        
    c("2012 General", "Fail Both", Inacc_ByMail_SM$Gen2012_fBoth$coefficients, summary(Inacc_ByMail_SM$Gen2012_fBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2012_fBoth)),
    c("2012 General", "Fail One", Inacc_ByMail_SM$Gen2012_mixed$coefficients, summary(Inacc_ByMail_SM$Gen2012_mixed)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2012_mixed)),
    c("2012 General", "Pass Both", Inacc_ByMail_SM$Gen2012_pBoth$coefficients, summary(Inacc_ByMail_SM$Gen2012_pBoth)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2012_pBoth)),
    c("2012 General", "Fail All", Inacc_ByMail_SM$Gen2012_fAll$coefficients, summary(Inacc_ByMail_SM$Gen2012_fAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2012_fAll)),
    c("2012 General", "Pass All", Inacc_ByMail_SM$Gen2012_pAll$coefficients, summary(Inacc_ByMail_SM$Gen2012_pAll)$coef[,"Std. Error"], confint(Inacc_ByMail_SM$Gen2012_pAll))
  ), stringsAsFactors = FALSE
)

colnames(Table_Inacc_ByMail_SM) <- c("Election", "Group", "Estimate", "SE", "Lower", "Upper")
