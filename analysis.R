# analysis.R
# Replication code for paper
# "Survey Attention and Self-Reported Political Behavior"
# by R. Michael Alvarez and Yimeng Li
# September 21, 2022.

# Set directory
setwd("C:/OneDrive - California Institute of Technology/Methodology - Attention Filter/Code/POQ Replication Package")

# Load libraries
library(boot)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Load dataset
load("./OCsurvey_turnout.rda")
OCsurvey <- OCsurvey_turnout
rm(OCsurvey_turnout)

options(digits = 3)

#*************************************************************
# Table 1
#*************************************************************

prop.table(table(OCsurvey$AF_IRI1, useNA = "always"))
prop.table(table(OCsurvey$AF_IRI1[(OCsurvey$AF_order == "IRI-IMC")], useNA = "always"))
prop.table(table(OCsurvey$AF_IRI1[(OCsurvey$AF_order == "IMC-IRI")], useNA = "always"))

prop.table(table(OCsurvey$AF_IRI2[(OCsurvey$Asked_IRI2 == TRUE)], useNA = "always"))

prop.table(table(OCsurvey$AF_IMC1, useNA = "always"))
prop.table(table(OCsurvey$AF_IMC1[(OCsurvey$AF_order == "IMC-IRI")], useNA = "always"))
prop.table(table(OCsurvey$AF_IMC1[(OCsurvey$AF_order == "IRI-IMC")], useNA = "always"))

prop.table(table(OCsurvey$AF_IMC2[(OCsurvey$Asked_IMC2 == TRUE)], useNA = "always"))

#*************************************************************
# Figure 1/Table SM3
#*************************************************************

# Percentage of respondents misreporting turnout in six recent elections among those who
# - failed the IRI (fIRI)
# - passed the IRI (pIRI)
# - failed the IMC (fIMC)
# - passed the IMC (pIMC)

Inacc_Turnout <- list()

Inacc_Turnout$Gen2018_All <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE))
Inacc_Turnout$Gen2018_fIRI <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == FALSE))
Inacc_Turnout$Gen2018_pIRI <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == TRUE))
Inacc_Turnout$Gen2018_fIMC <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IMC1 == FALSE))
Inacc_Turnout$Gen2018_pIMC <- lm(1 - Gen2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IMC1 == TRUE))

Inacc_Turnout$Gen2016_All <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE))
Inacc_Turnout$Gen2016_fIRI <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == FALSE))
Inacc_Turnout$Gen2016_pIRI <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1== TRUE))
Inacc_Turnout$Gen2016_fIMC <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IMC1 == FALSE))
Inacc_Turnout$Gen2016_pIMC <- lm(1 - Gen2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IMC1== TRUE))

Inacc_Turnout$Gen2014_All <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE))
Inacc_Turnout$Gen2014_fIRI <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == FALSE))
Inacc_Turnout$Gen2014_pIRI <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1== TRUE))
Inacc_Turnout$Gen2014_fIMC <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IMC1 == FALSE))
Inacc_Turnout$Gen2014_pIMC <- lm(1 - Gen2014_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IMC1== TRUE))

Inacc_Turnout$Gen2012_All <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE))
Inacc_Turnout$Gen2012_fIRI <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == FALSE))
Inacc_Turnout$Gen2012_pIRI <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1== TRUE))
Inacc_Turnout$Gen2012_fIMC <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IMC1 == FALSE))
Inacc_Turnout$Gen2012_pIMC <- lm(1 - Gen2012_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IMC1== TRUE))

Inacc_Turnout$PRI2018_All <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE))
Inacc_Turnout$PRI2018_fIRI <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == FALSE))
Inacc_Turnout$PRI2018_pIRI <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1== TRUE))
Inacc_Turnout$PRI2018_fIMC <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IMC1 == FALSE))
Inacc_Turnout$PRI2018_pIMC <- lm(1 - PRI2018_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IMC1== TRUE))

Inacc_Turnout$PRI2016_All <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE))
Inacc_Turnout$PRI2016_fIRI <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == FALSE))
Inacc_Turnout$PRI2016_pIRI <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1== TRUE))
Inacc_Turnout$PRI2016_fIMC <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IMC1 == FALSE))
Inacc_Turnout$PRI2016_pIMC <- lm(1 - PRI2016_Turnout_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IMC1== TRUE))

Table_Inacc_Turnout <- as.data.frame(
  rbind(
    c("2018 General", "All Respondents", Inacc_Turnout$Gen2018_All$coefficients, summary(Inacc_Turnout$Gen2018_All)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2018_All)),
    c("2018 General", "Fail IRI", Inacc_Turnout$Gen2018_fIRI$coefficients, summary(Inacc_Turnout$Gen2018_fIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2018_fIRI)),
    c("2018 General", "Pass IRI", Inacc_Turnout$Gen2018_pIRI$coefficients, summary(Inacc_Turnout$Gen2018_pIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2018_pIRI)),
    c("2018 General", "Fail IMC", Inacc_Turnout$Gen2018_fIMC$coefficients, summary(Inacc_Turnout$Gen2018_fIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2018_fIMC)),
    c("2018 General", "Pass IMC", Inacc_Turnout$Gen2018_pIMC$coefficients, summary(Inacc_Turnout$Gen2018_pIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2018_pIMC)),
    c("2018 Primary", "All Respondents", Inacc_Turnout$PRI2018_All$coefficients, summary(Inacc_Turnout$PRI2018_All)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2018_All)),
    c("2018 Primary", "Fail IRI", Inacc_Turnout$PRI2018_fIRI$coefficients, summary(Inacc_Turnout$PRI2018_fIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2018_fIRI)),
    c("2018 Primary", "Pass IRI", Inacc_Turnout$PRI2018_pIRI$coefficients, summary(Inacc_Turnout$PRI2018_pIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2018_pIRI)),
    c("2018 Primary", "Fail IMC", Inacc_Turnout$PRI2018_fIMC$coefficients, summary(Inacc_Turnout$PRI2018_fIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2018_fIMC)),
    c("2018 Primary", "Pass IMC", Inacc_Turnout$PRI2018_pIMC$coefficients, summary(Inacc_Turnout$PRI2018_pIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2018_pIMC)),
    c("2016 General", "All Respondents", Inacc_Turnout$Gen2016_All$coefficients, summary(Inacc_Turnout$Gen2016_All)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2016_All)),
    c("2016 General", "Fail IRI", Inacc_Turnout$Gen2016_fIRI$coefficients, summary(Inacc_Turnout$Gen2016_fIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2016_fIRI)),
    c("2016 General", "Pass IRI", Inacc_Turnout$Gen2016_pIRI$coefficients, summary(Inacc_Turnout$Gen2016_pIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2016_pIRI)),
    c("2016 General", "Fail IMC", Inacc_Turnout$Gen2016_fIMC$coefficients, summary(Inacc_Turnout$Gen2016_fIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2016_fIMC)),
    c("2016 General", "Pass IMC", Inacc_Turnout$Gen2016_pIMC$coefficients, summary(Inacc_Turnout$Gen2016_pIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2016_pIMC)),
    c("2016 Primary", "All Respondents", Inacc_Turnout$PRI2016_All$coefficients, summary(Inacc_Turnout$PRI2016_All)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2016_All)),
    c("2016 Primary", "Fail IRI", Inacc_Turnout$PRI2016_fIRI$coefficients, summary(Inacc_Turnout$PRI2016_fIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2016_fIRI)),
    c("2016 Primary", "Pass IRI", Inacc_Turnout$PRI2016_pIRI$coefficients, summary(Inacc_Turnout$PRI2016_pIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2016_pIRI)),
    c("2016 Primary", "Fail IMC", Inacc_Turnout$PRI2016_fIMC$coefficients, summary(Inacc_Turnout$PRI2016_fIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2016_fIMC)),
    c("2016 Primary", "Pass IMC", Inacc_Turnout$PRI2016_pIMC$coefficients, summary(Inacc_Turnout$PRI2016_pIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$PRI2016_pIMC)),
    c("2014 General", "All Respondents", Inacc_Turnout$Gen2014_All$coefficients, summary(Inacc_Turnout$Gen2014_All)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2014_All)),
    c("2014 General", "Fail IRI", Inacc_Turnout$Gen2014_fIRI$coefficients, summary(Inacc_Turnout$Gen2014_fIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2014_fIRI)),
    c("2014 General", "Pass IRI", Inacc_Turnout$Gen2014_pIRI$coefficients, summary(Inacc_Turnout$Gen2014_pIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2014_pIRI)),
    c("2014 General", "Fail IMC", Inacc_Turnout$Gen2014_fIMC$coefficients, summary(Inacc_Turnout$Gen2014_fIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2014_fIMC)),
    c("2014 General", "Pass IMC", Inacc_Turnout$Gen2014_pIMC$coefficients, summary(Inacc_Turnout$Gen2014_pIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2014_pIMC)),
    c("2012 General", "All Respondents", Inacc_Turnout$Gen2012_All$coefficients, summary(Inacc_Turnout$Gen2012_All)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2012_All)),
    c("2012 General", "Fail IRI", Inacc_Turnout$Gen2012_fIRI$coefficients, summary(Inacc_Turnout$Gen2012_fIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2012_fIRI)),
    c("2012 General", "Pass IRI", Inacc_Turnout$Gen2012_pIRI$coefficients, summary(Inacc_Turnout$Gen2012_pIRI)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2012_pIRI)),
    c("2012 General", "Fail IMC", Inacc_Turnout$Gen2012_fIMC$coefficients, summary(Inacc_Turnout$Gen2012_fIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2012_fIMC)),
    c("2012 General", "Pass IMC", Inacc_Turnout$Gen2012_pIMC$coefficients, summary(Inacc_Turnout$Gen2012_pIMC)$coef[,"Std. Error"], confint(Inacc_Turnout$Gen2012_pIMC))
  ), stringsAsFactors = FALSE
)

colnames(Table_Inacc_Turnout) <- c("Election", "Group", "Estimate", "SE", "Lower", "Upper")

Table_Inacc_Turnout <- Table_Inacc_Turnout %>%
  mutate(Election = factor(Election, levels = rev(c("2018 General", "2018 Primary", "2016 General", "2016 Primary", "2014 General", "2012 General"))),
         Group = factor(Group, levels = rev(c("All Respondents", "Fail IRI", "Pass IRI", "Fail IMC", "Pass IMC"))),
         Estimate = as.double(Estimate),
         SE = as.double(SE),
         Lower = as.double(Lower),
         Upper = as.double(Upper))

Table_Inacc_Turnout_Formatted <-
  bind_rows(
    Table_Inacc_Turnout %>% select(Election, Group, Estimate) %>%
      mutate(Group = factor(Group, levels = rev(levels(Group))), Estimate = as.character(round(Estimate*100, 1))) %>%
      spread(key = Group, value = Estimate) %>%
      mutate(Stat = "Estimate"),
    Table_Inacc_Turnout %>% select(Election, Group, SE) %>% arrange(Election, Group) %>%
      mutate(Group = factor(Group, levels = rev(levels(Group))), SE = str_c("(", round(SE*100, 1), ")")) %>%
      spread(key = Group, value = SE) %>%
      mutate(Stat = "Standard Error")
  ) %>%
  arrange(desc(Election), Stat)

Figure_Inacc_Turnout_IRI <- ggplot(Table_Inacc_Turnout %>% filter(Group %in% c("Pass IRI", "Fail IRI")), aes(x = Election, y = Estimate*100, colour = Group)) +
  geom_point(size = 2, position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = Lower*100, ymax = Upper*100), width = 0.2, position = position_dodge(width=0.9)) +
  geom_linerange(aes(ymin = Estimate*100 - SE*100, ymax = Estimate*100 + SE*100), size = 1.5, position = position_dodge(width=0.9)) +
  coord_flip() +
  labs(x = "Election", y = "", colour = "") +
  scale_colour_manual(values = rev(c("grey20", "grey80")), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 32), breaks = c(0, 10, 20, 30)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_bw() +
  theme(panel.border = element_rect(colour="black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

Figure_Inacc_Turnout_IMC <- ggplot(Table_Inacc_Turnout %>% filter(Group %in% c("Pass IMC", "Fail IMC")), aes(x = Election, y = Estimate*100, colour = Group)) +
  geom_point(size = 2, position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = Lower*100, ymax = Upper*100), width = 0.2, position = position_dodge(width=0.9)) +
  geom_linerange(aes(ymin = Estimate*100 - SE*100, ymax = Estimate*100 + SE*100), size = 1.5, position = position_dodge(width=0.9)) +
  coord_flip() +
  labs(x = "Election", y = "Percentage of Respondents Misreporting Turnout", colour = "") +
  scale_colour_manual(values = rev(c("grey20", "grey80")), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 32), breaks = c(0, 10, 20, 30)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_bw() +
  theme(panel.border = element_rect(colour="black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

ggsave("./Result/Figure_Inacc_Turnout_IRI.tiff", Figure_Inacc_Turnout_IRI, width = 8, height = 2, units = "in", dpi = 600)
ggsave("./Result/Figure_Inacc_Turnout_IMC.tiff", Figure_Inacc_Turnout_IMC, width = 8, height = 2, units = "in", dpi = 600)

#*************************************************************
# Figure 2/Table SM4
#*************************************************************

# Percentage of respondents misreporting mode of voting in six recent elections among those who
# - failed the IRI (fIRI)
# - passed the IRI (pIRI)
# - failed the IMC (fIMC)
# - passed the IMC (pIMC)

Inacc_ByMail <- list()

Inacc_ByMail$Gen2018_All <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE))
Inacc_ByMail$Gen2018_fIRI <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == FALSE))
Inacc_ByMail$Gen2018_pIRI <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == TRUE))
Inacc_ByMail$Gen2018_fIMC <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IMC1 == FALSE))
Inacc_ByMail$Gen2018_pIMC <- lm(1 - Gen2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IMC1 == TRUE))

Inacc_ByMail$Gen2016_All <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE))
Inacc_ByMail$Gen2016_fIRI <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == FALSE))
Inacc_ByMail$Gen2016_pIRI <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1== TRUE))
Inacc_ByMail$Gen2016_fIMC <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IMC1 == FALSE))
Inacc_ByMail$Gen2016_pIMC <- lm(1 - Gen2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IMC1== TRUE))

Inacc_ByMail$Gen2014_All <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE))
Inacc_ByMail$Gen2014_fIRI <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == FALSE))
Inacc_ByMail$Gen2014_pIRI <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1== TRUE))
Inacc_ByMail$Gen2014_fIMC <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IMC1 == FALSE))
Inacc_ByMail$Gen2014_pIMC <- lm(1 - Gen2014_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IMC1== TRUE))

Inacc_ByMail$Gen2012_All <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE))
Inacc_ByMail$Gen2012_fIRI <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == FALSE))
Inacc_ByMail$Gen2012_pIRI <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1== TRUE))
Inacc_ByMail$Gen2012_fIMC <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IMC1 == FALSE))
Inacc_ByMail$Gen2012_pIMC <- lm(1 - Gen2012_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IMC1== TRUE))

Inacc_ByMail$PRI2018_All <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE))
Inacc_ByMail$PRI2018_fIRI <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == FALSE))
Inacc_ByMail$PRI2018_pIRI <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1== TRUE))
Inacc_ByMail$PRI2018_fIMC <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IMC1 == FALSE))
Inacc_ByMail$PRI2018_pIMC <- lm(1 - PRI2018_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IMC1== TRUE))

Inacc_ByMail$PRI2016_All <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE))
Inacc_ByMail$PRI2016_fIRI <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == FALSE))
Inacc_ByMail$PRI2016_pIRI <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1== TRUE))
Inacc_ByMail$PRI2016_fIMC <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IMC1 == FALSE))
Inacc_ByMail$PRI2016_pIMC <- lm(1 - PRI2016_ByMail_Flag ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IMC1== TRUE))

Table_Inacc_ByMail <- as.data.frame(
  rbind(
    c("2018 General", "All Respondents", Inacc_ByMail$Gen2018_All$coefficients, summary(Inacc_ByMail$Gen2018_All)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2018_All)),
    c("2018 General", "Fail IRI", Inacc_ByMail$Gen2018_fIRI$coefficients, summary(Inacc_ByMail$Gen2018_fIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2018_fIRI)),
    c("2018 General", "Pass IRI", Inacc_ByMail$Gen2018_pIRI$coefficients, summary(Inacc_ByMail$Gen2018_pIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2018_pIRI)),
    c("2018 General", "Fail IMC", Inacc_ByMail$Gen2018_fIMC$coefficients, summary(Inacc_ByMail$Gen2018_fIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2018_fIMC)),
    c("2018 General", "Pass IMC", Inacc_ByMail$Gen2018_pIMC$coefficients, summary(Inacc_ByMail$Gen2018_pIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2018_pIMC)),
    c("2018 Primary", "All Respondents", Inacc_ByMail$PRI2018_All$coefficients, summary(Inacc_ByMail$PRI2018_All)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2018_All)),
    c("2018 Primary", "Fail IRI", Inacc_ByMail$PRI2018_fIRI$coefficients, summary(Inacc_ByMail$PRI2018_fIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2018_fIRI)),
    c("2018 Primary", "Pass IRI", Inacc_ByMail$PRI2018_pIRI$coefficients, summary(Inacc_ByMail$PRI2018_pIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2018_pIRI)),
    c("2018 Primary", "Fail IMC", Inacc_ByMail$PRI2018_fIMC$coefficients, summary(Inacc_ByMail$PRI2018_fIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2018_fIMC)),
    c("2018 Primary", "Pass IMC", Inacc_ByMail$PRI2018_pIMC$coefficients, summary(Inacc_ByMail$PRI2018_pIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2018_pIMC)),
    c("2016 General", "All Respondents", Inacc_ByMail$Gen2016_All$coefficients, summary(Inacc_ByMail$Gen2016_All)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2016_All)),
    c("2016 General", "Fail IRI", Inacc_ByMail$Gen2016_fIRI$coefficients, summary(Inacc_ByMail$Gen2016_fIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2016_fIRI)),
    c("2016 General", "Pass IRI", Inacc_ByMail$Gen2016_pIRI$coefficients, summary(Inacc_ByMail$Gen2016_pIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2016_pIRI)),
    c("2016 General", "Fail IMC", Inacc_ByMail$Gen2016_fIMC$coefficients, summary(Inacc_ByMail$Gen2016_fIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2016_fIMC)),
    c("2016 General", "Pass IMC", Inacc_ByMail$Gen2016_pIMC$coefficients, summary(Inacc_ByMail$Gen2016_pIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2016_pIMC)),
    c("2016 Primary", "All Respondents", Inacc_ByMail$PRI2016_All$coefficients, summary(Inacc_ByMail$PRI2016_All)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2016_All)),
    c("2016 Primary", "Fail IRI", Inacc_ByMail$PRI2016_fIRI$coefficients, summary(Inacc_ByMail$PRI2016_fIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2016_fIRI)),
    c("2016 Primary", "Pass IRI", Inacc_ByMail$PRI2016_pIRI$coefficients, summary(Inacc_ByMail$PRI2016_pIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2016_pIRI)),
    c("2016 Primary", "Fail IMC", Inacc_ByMail$PRI2016_fIMC$coefficients, summary(Inacc_ByMail$PRI2016_fIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2016_fIMC)),
    c("2016 Primary", "Pass IMC", Inacc_ByMail$PRI2016_pIMC$coefficients, summary(Inacc_ByMail$PRI2016_pIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$PRI2016_pIMC)),
    c("2014 General", "All Respondents", Inacc_ByMail$Gen2014_All$coefficients, summary(Inacc_ByMail$Gen2014_All)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2014_All)),
    c("2014 General", "Fail IRI", Inacc_ByMail$Gen2014_fIRI$coefficients, summary(Inacc_ByMail$Gen2014_fIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2014_fIRI)),
    c("2014 General", "Pass IRI", Inacc_ByMail$Gen2014_pIRI$coefficients, summary(Inacc_ByMail$Gen2014_pIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2014_pIRI)),
    c("2014 General", "Fail IMC", Inacc_ByMail$Gen2014_fIMC$coefficients, summary(Inacc_ByMail$Gen2014_fIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2014_fIMC)),
    c("2014 General", "Pass IMC", Inacc_ByMail$Gen2014_pIMC$coefficients, summary(Inacc_ByMail$Gen2014_pIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2014_pIMC)),
    c("2012 General", "All Respondents", Inacc_ByMail$Gen2012_All$coefficients, summary(Inacc_ByMail$Gen2012_All)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2012_All)),
    c("2012 General", "Fail IRI", Inacc_ByMail$Gen2012_fIRI$coefficients, summary(Inacc_ByMail$Gen2012_fIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2012_fIRI)),
    c("2012 General", "Pass IRI", Inacc_ByMail$Gen2012_pIRI$coefficients, summary(Inacc_ByMail$Gen2012_pIRI)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2012_pIRI)),
    c("2012 General", "Fail IMC", Inacc_ByMail$Gen2012_fIMC$coefficients, summary(Inacc_ByMail$Gen2012_fIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2012_fIMC)),
    c("2012 General", "Pass IMC", Inacc_ByMail$Gen2012_pIMC$coefficients, summary(Inacc_ByMail$Gen2012_pIMC)$coef[,"Std. Error"], confint(Inacc_ByMail$Gen2012_pIMC))
  ), stringsAsFactors = FALSE
)

colnames(Table_Inacc_ByMail) <- c("Election", "Group", "Estimate", "SE", "Lower", "Upper")

Table_Inacc_ByMail <- Table_Inacc_ByMail %>%
  mutate(Election = factor(Election, levels = rev(c("2018 General", "2018 Primary", "2016 General", "2016 Primary", "2014 General", "2012 General"))),
         Group = factor(Group, levels = rev(c("All Respondents", "Fail IRI", "Pass IRI", "Fail IMC", "Pass IMC"))),
         Estimate = as.double(Estimate),
         SE = as.double(SE),
         Lower = as.double(Lower),
         Upper = as.double(Upper))

Table_Inacc_ByMail_Formatted <-
  bind_rows(
    Table_Inacc_ByMail %>% select(Election, Group, Estimate) %>%
      mutate(Group = factor(Group, levels = rev(levels(Group))), Estimate = as.character(round(Estimate*100, 1))) %>%
      spread(key = Group, value = Estimate) %>%
      mutate(Stat = "Estimate"),
    Table_Inacc_ByMail %>% select(Election, Group, SE) %>% arrange(Election, Group) %>%
      mutate(Group = factor(Group, levels = rev(levels(Group))), SE = str_c("(", round(SE*100, 1), ")")) %>%
      spread(key = Group, value = SE) %>%
      mutate(Stat = "Standard Error")
  ) %>%
  arrange(desc(Election), Stat)

Figure_Inacc_ByMail_IRI <- ggplot(Table_Inacc_ByMail %>% filter(Group %in% c("Pass IRI", "Fail IRI")), aes(x = Election, y = Estimate*100, colour = Group)) +
  geom_point(size = 2, position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = Lower*100, ymax = Upper*100), width = 0.2, position = position_dodge(width=0.9)) +
  geom_linerange(aes(ymin = Estimate*100 - SE*100, ymax = Estimate*100 + SE*100), size = 1.5, position = position_dodge(width=0.9)) +
  coord_flip() +
  labs(x = "Election", y = "", colour = "") +
  scale_colour_manual(values = rev(c("grey20", "grey80")), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 32), breaks = c(0, 10, 20, 30)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_bw() +
  theme(panel.border = element_rect(colour="black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

Figure_Inacc_ByMail_IMC <- ggplot(Table_Inacc_ByMail %>% filter(Group %in% c("Pass IMC", "Fail IMC")), aes(x = Election, y = Estimate*100, colour = Group)) +
  geom_point(size = 2, position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = Lower*100, ymax = Upper*100), width = 0.2, position = position_dodge(width=0.9)) +
  geom_linerange(aes(ymin = Estimate*100 - SE*100, ymax = Estimate*100 + SE*100), size = 1.5, position = position_dodge(width=0.9)) +
  coord_flip() +
  labs(x = "Election", y = "Percentage of Respondents Misreporting Mode of Voting", colour = "") +
  scale_colour_manual(values = rev(c("grey20", "grey80")), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 32), breaks = c(0, 10, 20, 30)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_bw() +
  theme(panel.border = element_rect(colour="black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

ggsave("./Result/Figure_Inacc_ByMail_IRI.tiff", Figure_Inacc_ByMail_IRI, width = 8, height = 2, units = "in", dpi = 600)
ggsave("./Result/Figure_Inacc_ByMail_IMC.tiff", Figure_Inacc_ByMail_IMC, width = 8, height = 2, units = "in", dpi = 600)

#*************************************************************
# Figure 3/Table SM5
#*************************************************************

# Percentage of respondents confirmed to have participated in six recent elections among those who
# - failed the IRI (fIRI)
# - passed the IRI (pIRI)
# - failed the IMC (fIMC)
# - passed the IMC (pIMC)

Val_Turnout <- list()

Val_Turnout$Gen2018_All <- lm(Gen2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE))
Val_Turnout$Gen2018_fIRI <- lm(Gen2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == FALSE))
Val_Turnout$Gen2018_pIRI <- lm(Gen2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == TRUE))
Val_Turnout$Gen2018_fIMC <- lm(Gen2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IMC1 == FALSE))
Val_Turnout$Gen2018_pIMC <- lm(Gen2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE, AF_IMC1 == TRUE))

Val_Turnout$Gen2016_All <- lm(Gen2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE))
Val_Turnout$Gen2016_fIRI <- lm(Gen2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == FALSE))
Val_Turnout$Gen2016_pIRI <- lm(Gen2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == TRUE))
Val_Turnout$Gen2016_fIMC <- lm(Gen2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IMC1 == FALSE))
Val_Turnout$Gen2016_pIMC <- lm(Gen2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE, AF_IMC1 == TRUE))

Val_Turnout$Gen2014_All <- lm(Gen2014_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE))
Val_Turnout$Gen2014_fIRI <- lm(Gen2014_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == FALSE))
Val_Turnout$Gen2014_pIRI <- lm(Gen2014_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == TRUE))
Val_Turnout$Gen2014_fIMC <- lm(Gen2014_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IMC1 == FALSE))
Val_Turnout$Gen2014_pIMC <- lm(Gen2014_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE, AF_IMC1 == TRUE))

Val_Turnout$Gen2012_All <- lm(Gen2012_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE))
Val_Turnout$Gen2012_fIRI <- lm(Gen2012_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == FALSE))
Val_Turnout$Gen2012_pIRI <- lm(Gen2012_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == TRUE))
Val_Turnout$Gen2012_fIMC <- lm(Gen2012_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IMC1 == FALSE))
Val_Turnout$Gen2012_pIMC <- lm(Gen2012_Turnout ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE, AF_IMC1 == TRUE))

Val_Turnout$PRI2018_All <- lm(PRI2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE))
Val_Turnout$PRI2018_fIRI <- lm(PRI2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == FALSE))
Val_Turnout$PRI2018_pIRI <- lm(PRI2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == TRUE))
Val_Turnout$PRI2018_fIMC <- lm(PRI2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IMC1 == FALSE))
Val_Turnout$PRI2018_pIMC <- lm(PRI2018_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE, AF_IMC1 == TRUE))

Val_Turnout$PRI2016_All <- lm(PRI2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE))
Val_Turnout$PRI2016_fIRI <- lm(PRI2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == FALSE))
Val_Turnout$PRI2016_pIRI <- lm(PRI2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == TRUE))
Val_Turnout$PRI2016_fIMC <- lm(PRI2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IMC1 == FALSE))
Val_Turnout$PRI2016_pIMC <- lm(PRI2016_Turnout ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE, AF_IMC1 == TRUE))

Table_Val_Turnout <- as.data.frame(
  rbind(
    c("2018 General", "All Respondents", Val_Turnout$Gen2018_All$coefficients, summary(Val_Turnout$Gen2018_All)$coef[,"Std. Error"], confint(Val_Turnout$Gen2018_All)),
    c("2018 General", "Fail IRI", Val_Turnout$Gen2018_fIRI$coefficients, summary(Val_Turnout$Gen2018_fIRI)$coef[,"Std. Error"], confint(Val_Turnout$Gen2018_fIRI)),
    c("2018 General", "Pass IRI", Val_Turnout$Gen2018_pIRI$coefficients, summary(Val_Turnout$Gen2018_pIRI)$coef[,"Std. Error"], confint(Val_Turnout$Gen2018_pIRI)),
    c("2018 General", "Fail IMC", Val_Turnout$Gen2018_fIMC$coefficients, summary(Val_Turnout$Gen2018_fIMC)$coef[,"Std. Error"], confint(Val_Turnout$Gen2018_fIMC)),
    c("2018 General", "Pass IMC", Val_Turnout$Gen2018_pIMC$coefficients, summary(Val_Turnout$Gen2018_pIMC)$coef[,"Std. Error"], confint(Val_Turnout$Gen2018_pIMC)),
    c("2018 Primary", "All Respondents", Val_Turnout$PRI2018_All$coefficients, summary(Val_Turnout$PRI2018_All)$coef[,"Std. Error"], confint(Val_Turnout$PRI2018_All)),
    c("2018 Primary", "Fail IRI", Val_Turnout$PRI2018_fIRI$coefficients, summary(Val_Turnout$PRI2018_fIRI)$coef[,"Std. Error"], confint(Val_Turnout$PRI2018_fIRI)),
    c("2018 Primary", "Pass IRI", Val_Turnout$PRI2018_pIRI$coefficients, summary(Val_Turnout$PRI2018_pIRI)$coef[,"Std. Error"], confint(Val_Turnout$PRI2018_pIRI)),
    c("2018 Primary", "Fail IMC", Val_Turnout$PRI2018_fIMC$coefficients, summary(Val_Turnout$PRI2018_fIMC)$coef[,"Std. Error"], confint(Val_Turnout$PRI2018_fIMC)),
    c("2018 Primary", "Pass IMC", Val_Turnout$PRI2018_pIMC$coefficients, summary(Val_Turnout$PRI2018_pIMC)$coef[,"Std. Error"], confint(Val_Turnout$PRI2018_pIMC)),
    c("2016 General", "All Respondents", Val_Turnout$Gen2016_All$coefficients, summary(Val_Turnout$Gen2016_All)$coef[,"Std. Error"], confint(Val_Turnout$Gen2016_All)),
    c("2016 General", "Fail IRI", Val_Turnout$Gen2016_fIRI$coefficients, summary(Val_Turnout$Gen2016_fIRI)$coef[,"Std. Error"], confint(Val_Turnout$Gen2016_fIRI)),
    c("2016 General", "Pass IRI", Val_Turnout$Gen2016_pIRI$coefficients, summary(Val_Turnout$Gen2016_pIRI)$coef[,"Std. Error"], confint(Val_Turnout$Gen2016_pIRI)),
    c("2016 General", "Fail IMC", Val_Turnout$Gen2016_fIMC$coefficients, summary(Val_Turnout$Gen2016_fIMC)$coef[,"Std. Error"], confint(Val_Turnout$Gen2016_fIMC)),
    c("2016 General", "Pass IMC", Val_Turnout$Gen2016_pIMC$coefficients, summary(Val_Turnout$Gen2016_pIMC)$coef[,"Std. Error"], confint(Val_Turnout$Gen2016_pIMC)),
    c("2016 Primary", "All Respondents", Val_Turnout$PRI2016_All$coefficients, summary(Val_Turnout$PRI2016_All)$coef[,"Std. Error"], confint(Val_Turnout$PRI2016_All)),
    c("2016 Primary", "Fail IRI", Val_Turnout$PRI2016_fIRI$coefficients, summary(Val_Turnout$PRI2016_fIRI)$coef[,"Std. Error"], confint(Val_Turnout$PRI2016_fIRI)),
    c("2016 Primary", "Pass IRI", Val_Turnout$PRI2016_pIRI$coefficients, summary(Val_Turnout$PRI2016_pIRI)$coef[,"Std. Error"], confint(Val_Turnout$PRI2016_pIRI)),
    c("2016 Primary", "Fail IMC", Val_Turnout$PRI2016_fIMC$coefficients, summary(Val_Turnout$PRI2016_fIMC)$coef[,"Std. Error"], confint(Val_Turnout$PRI2016_fIMC)),
    c("2016 Primary", "Pass IMC", Val_Turnout$PRI2016_pIMC$coefficients, summary(Val_Turnout$PRI2016_pIMC)$coef[,"Std. Error"], confint(Val_Turnout$PRI2016_pIMC)),
    c("2014 General", "All Respondents", Val_Turnout$Gen2014_All$coefficients, summary(Val_Turnout$Gen2014_All)$coef[,"Std. Error"], confint(Val_Turnout$Gen2014_All)),
    c("2014 General", "Fail IRI", Val_Turnout$Gen2014_fIRI$coefficients, summary(Val_Turnout$Gen2014_fIRI)$coef[,"Std. Error"], confint(Val_Turnout$Gen2014_fIRI)),
    c("2014 General", "Pass IRI", Val_Turnout$Gen2014_pIRI$coefficients, summary(Val_Turnout$Gen2014_pIRI)$coef[,"Std. Error"], confint(Val_Turnout$Gen2014_pIRI)),
    c("2014 General", "Fail IMC", Val_Turnout$Gen2014_fIMC$coefficients, summary(Val_Turnout$Gen2014_fIMC)$coef[,"Std. Error"], confint(Val_Turnout$Gen2014_fIMC)),
    c("2014 General", "Pass IMC", Val_Turnout$Gen2014_pIMC$coefficients, summary(Val_Turnout$Gen2014_pIMC)$coef[,"Std. Error"], confint(Val_Turnout$Gen2014_pIMC)),
    c("2012 General", "All Respondents", Val_Turnout$Gen2012_All$coefficients, summary(Val_Turnout$Gen2012_All)$coef[,"Std. Error"], confint(Val_Turnout$Gen2012_All)),
    c("2012 General", "Fail IRI", Val_Turnout$Gen2012_fIRI$coefficients, summary(Val_Turnout$Gen2012_fIRI)$coef[,"Std. Error"], confint(Val_Turnout$Gen2012_fIRI)),
    c("2012 General", "Pass IRI", Val_Turnout$Gen2012_pIRI$coefficients, summary(Val_Turnout$Gen2012_pIRI)$coef[,"Std. Error"], confint(Val_Turnout$Gen2012_pIRI)),
    c("2012 General", "Fail IMC", Val_Turnout$Gen2012_fIMC$coefficients, summary(Val_Turnout$Gen2012_fIMC)$coef[,"Std. Error"], confint(Val_Turnout$Gen2012_fIMC)),
    c("2012 General", "Pass IMC", Val_Turnout$Gen2012_pIMC$coefficients, summary(Val_Turnout$Gen2012_pIMC)$coef[,"Std. Error"], confint(Val_Turnout$Gen2012_pIMC))
  ), stringsAsFactors = FALSE
)

colnames(Table_Val_Turnout) <- c("Election", "Group", "Estimate", "SE", "Lower", "Upper")

Table_Val_Turnout <- Table_Val_Turnout %>%
  mutate(Election = factor(Election, levels = rev(c("2018 General", "2018 Primary", "2016 General", "2016 Primary", "2014 General", "2012 General"))),
         Group = factor(Group, levels = rev(c("All Respondents", "Fail IRI", "Pass IRI", "Fail IMC", "Pass IMC"))),
         Estimate = as.double(Estimate),
         SE = as.double(SE),
         Lower = as.double(Lower),
         Upper = as.double(Upper))

Table_Val_Turnout_Formatted <-
  bind_rows(
    Table_Val_Turnout %>% select(Election, Group, Estimate) %>%
      mutate(Group = factor(Group, levels = rev(levels(Group))), Estimate = as.character(round(Estimate*100, 1))) %>%
      spread(key = Group, value = Estimate) %>%
      mutate(Stat = "Estimate"),
    Table_Val_Turnout %>% select(Election, Group, SE) %>% arrange(Election, Group) %>%
      mutate(Group = factor(Group, levels = rev(levels(Group))), SE = str_c("(", round(SE*100, 1), ")")) %>%
      spread(key = Group, value = SE) %>%
      mutate(Stat = "Standard Error")
  ) %>%
  arrange(desc(Election), Stat)

Figure_Val_Turnout_IRI <- ggplot(Table_Val_Turnout %>% filter(Group %in% c("Pass IRI", "Fail IRI")), aes(x = Election, y = Estimate*100, colour = Group)) +
  geom_point(size = 2, position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = Lower*100, ymax = Upper*100), width = 0.2, position = position_dodge(width=0.9)) +
  geom_linerange(aes(ymin = Estimate*100 - SE*100, ymax = Estimate*100 + SE*100), size = 1.5, position = position_dodge(width=0.9)) +
  coord_flip() +
  labs(x = "Election", y = "", colour = "") +
  scale_colour_manual(values = rev(c("grey20", "grey80")), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(50, 100), breaks = c(50, 60, 70, 80, 90, 100)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_bw() +
  theme(panel.border = element_rect(colour="black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

Figure_Val_Turnout_IMC <- ggplot(Table_Val_Turnout %>% filter(Group %in% c("Pass IMC", "Fail IMC")), aes(x = Election, y = Estimate*100, colour = Group)) +
  geom_point(size = 2, position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = Lower*100, ymax = Upper*100), width = 0.2, position = position_dodge(width=0.9)) +
  geom_linerange(aes(ymin = Estimate*100 - SE*100, ymax = Estimate*100 + SE*100), size = 1.5, position = position_dodge(width=0.9)) +
  coord_flip() +
  labs(x = "Election", y = "Percentage of Validated Voters", colour = "") +
  scale_colour_manual(values = rev(c("grey20", "grey80")), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(50, 100), breaks = c(50, 60, 70, 80, 90, 100)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_bw() +
  theme(panel.border = element_rect(colour="black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

ggsave("./Result/Figure_Val_Turnout_IRI.tiff", Figure_Val_Turnout_IRI, width = 8, height = 2, units = "in", dpi = 600)
ggsave("./Result/Figure_Val_Turnout_IMC.tiff", Figure_Val_Turnout_IMC, width = 8, height = 2, units = "in", dpi = 600)

#*************************************************************
# Figure 4/Table SM6
#*************************************************************

# Percentage of  respondents confirmed to have voted by mail in six recent elections among those who
# - failed the IRI (fIRI)
# - passed the IRI (pIRI)
# - failed the IMC (fIMC)
# - passed the IMC (pIMC)

Val_ByMail <- list()

Val_ByMail$Gen2018_All <- lm(Gen2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE))
Val_ByMail$Gen2018_fIRI <- lm(Gen2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == FALSE))
Val_ByMail$Gen2018_pIRI <- lm(Gen2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IRI1 == TRUE))
Val_ByMail$Gen2018_fIMC <- lm(Gen2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IMC1 == FALSE))
Val_ByMail$Gen2018_pIMC <- lm(Gen2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE, AF_IMC1 == TRUE))

Val_ByMail$Gen2016_All <- lm(Gen2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE))
Val_ByMail$Gen2016_fIRI <- lm(Gen2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == FALSE))
Val_ByMail$Gen2016_pIRI <- lm(Gen2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IRI1 == TRUE))
Val_ByMail$Gen2016_fIMC <- lm(Gen2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IMC1 == FALSE))
Val_ByMail$Gen2016_pIMC <- lm(Gen2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE, AF_IMC1 == TRUE))

Val_ByMail$Gen2014_All <- lm(Gen2014_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE))
Val_ByMail$Gen2014_fIRI <- lm(Gen2014_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == FALSE))
Val_ByMail$Gen2014_pIRI <- lm(Gen2014_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IRI1 == TRUE))
Val_ByMail$Gen2014_fIMC <- lm(Gen2014_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IMC1 == FALSE))
Val_ByMail$Gen2014_pIMC <- lm(Gen2014_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE, AF_IMC1 == TRUE))

Val_ByMail$Gen2012_All <- lm(Gen2012_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE))
Val_ByMail$Gen2012_fIRI <- lm(Gen2012_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == FALSE))
Val_ByMail$Gen2012_pIRI <- lm(Gen2012_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IRI1 == TRUE))
Val_ByMail$Gen2012_fIMC <- lm(Gen2012_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IMC1 == FALSE))
Val_ByMail$Gen2012_pIMC <- lm(Gen2012_ByMail ~ 1, data = OCsurvey %>% filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE, AF_IMC1 == TRUE))

Val_ByMail$PRI2018_All <- lm(PRI2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE))
Val_ByMail$PRI2018_fIRI <- lm(PRI2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == FALSE))
Val_ByMail$PRI2018_pIRI <- lm(PRI2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IRI1 == TRUE))
Val_ByMail$PRI2018_fIMC <- lm(PRI2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IMC1 == FALSE))
Val_ByMail$PRI2018_pIMC <- lm(PRI2018_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE, AF_IMC1 == TRUE))

Val_ByMail$PRI2016_All <- lm(PRI2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE))
Val_ByMail$PRI2016_fIRI <- lm(PRI2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == FALSE))
Val_ByMail$PRI2016_pIRI <- lm(PRI2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IRI1 == TRUE))
Val_ByMail$PRI2016_fIMC <- lm(PRI2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IMC1 == FALSE))
Val_ByMail$PRI2016_pIMC <- lm(PRI2016_ByMail ~ 1, data = OCsurvey %>% filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE, AF_IMC1 == TRUE))

Table_Val_ByMail <- as.data.frame(
  rbind(
    c("2018 General", "All Respondents", Val_ByMail$Gen2018_All$coefficients, summary(Val_ByMail$Gen2018_All)$coef[,"Std. Error"], confint(Val_ByMail$Gen2018_All)),
    c("2018 General", "Fail IRI", Val_ByMail$Gen2018_fIRI$coefficients, summary(Val_ByMail$Gen2018_fIRI)$coef[,"Std. Error"], confint(Val_ByMail$Gen2018_fIRI)),
    c("2018 General", "Pass IRI", Val_ByMail$Gen2018_pIRI$coefficients, summary(Val_ByMail$Gen2018_pIRI)$coef[,"Std. Error"], confint(Val_ByMail$Gen2018_pIRI)),
    c("2018 General", "Fail IMC", Val_ByMail$Gen2018_fIMC$coefficients, summary(Val_ByMail$Gen2018_fIMC)$coef[,"Std. Error"], confint(Val_ByMail$Gen2018_fIMC)),
    c("2018 General", "Pass IMC", Val_ByMail$Gen2018_pIMC$coefficients, summary(Val_ByMail$Gen2018_pIMC)$coef[,"Std. Error"], confint(Val_ByMail$Gen2018_pIMC)),
    c("2018 Primary", "All Respondents", Val_ByMail$PRI2018_All$coefficients, summary(Val_ByMail$PRI2018_All)$coef[,"Std. Error"], confint(Val_ByMail$PRI2018_All)),
    c("2018 Primary", "Fail IRI", Val_ByMail$PRI2018_fIRI$coefficients, summary(Val_ByMail$PRI2018_fIRI)$coef[,"Std. Error"], confint(Val_ByMail$PRI2018_fIRI)),
    c("2018 Primary", "Pass IRI", Val_ByMail$PRI2018_pIRI$coefficients, summary(Val_ByMail$PRI2018_pIRI)$coef[,"Std. Error"], confint(Val_ByMail$PRI2018_pIRI)),
    c("2018 Primary", "Fail IMC", Val_ByMail$PRI2018_fIMC$coefficients, summary(Val_ByMail$PRI2018_fIMC)$coef[,"Std. Error"], confint(Val_ByMail$PRI2018_fIMC)),
    c("2018 Primary", "Pass IMC", Val_ByMail$PRI2018_pIMC$coefficients, summary(Val_ByMail$PRI2018_pIMC)$coef[,"Std. Error"], confint(Val_ByMail$PRI2018_pIMC)),
    c("2016 General", "All Respondents", Val_ByMail$Gen2016_All$coefficients, summary(Val_ByMail$Gen2016_All)$coef[,"Std. Error"], confint(Val_ByMail$Gen2016_All)),
    c("2016 General", "Fail IRI", Val_ByMail$Gen2016_fIRI$coefficients, summary(Val_ByMail$Gen2016_fIRI)$coef[,"Std. Error"], confint(Val_ByMail$Gen2016_fIRI)),
    c("2016 General", "Pass IRI", Val_ByMail$Gen2016_pIRI$coefficients, summary(Val_ByMail$Gen2016_pIRI)$coef[,"Std. Error"], confint(Val_ByMail$Gen2016_pIRI)),
    c("2016 General", "Fail IMC", Val_ByMail$Gen2016_fIMC$coefficients, summary(Val_ByMail$Gen2016_fIMC)$coef[,"Std. Error"], confint(Val_ByMail$Gen2016_fIMC)),
    c("2016 General", "Pass IMC", Val_ByMail$Gen2016_pIMC$coefficients, summary(Val_ByMail$Gen2016_pIMC)$coef[,"Std. Error"], confint(Val_ByMail$Gen2016_pIMC)),
    c("2016 Primary", "All Respondents", Val_ByMail$PRI2016_All$coefficients, summary(Val_ByMail$PRI2016_All)$coef[,"Std. Error"], confint(Val_ByMail$PRI2016_All)),
    c("2016 Primary", "Fail IRI", Val_ByMail$PRI2016_fIRI$coefficients, summary(Val_ByMail$PRI2016_fIRI)$coef[,"Std. Error"], confint(Val_ByMail$PRI2016_fIRI)),
    c("2016 Primary", "Pass IRI", Val_ByMail$PRI2016_pIRI$coefficients, summary(Val_ByMail$PRI2016_pIRI)$coef[,"Std. Error"], confint(Val_ByMail$PRI2016_pIRI)),
    c("2016 Primary", "Fail IMC", Val_ByMail$PRI2016_fIMC$coefficients, summary(Val_ByMail$PRI2016_fIMC)$coef[,"Std. Error"], confint(Val_ByMail$PRI2016_fIMC)),
    c("2016 Primary", "Pass IMC", Val_ByMail$PRI2016_pIMC$coefficients, summary(Val_ByMail$PRI2016_pIMC)$coef[,"Std. Error"], confint(Val_ByMail$PRI2016_pIMC)),
    c("2014 General", "All Respondents", Val_ByMail$Gen2014_All$coefficients, summary(Val_ByMail$Gen2014_All)$coef[,"Std. Error"], confint(Val_ByMail$Gen2014_All)),
    c("2014 General", "Fail IRI", Val_ByMail$Gen2014_fIRI$coefficients, summary(Val_ByMail$Gen2014_fIRI)$coef[,"Std. Error"], confint(Val_ByMail$Gen2014_fIRI)),
    c("2014 General", "Pass IRI", Val_ByMail$Gen2014_pIRI$coefficients, summary(Val_ByMail$Gen2014_pIRI)$coef[,"Std. Error"], confint(Val_ByMail$Gen2014_pIRI)),
    c("2014 General", "Fail IMC", Val_ByMail$Gen2014_fIMC$coefficients, summary(Val_ByMail$Gen2014_fIMC)$coef[,"Std. Error"], confint(Val_ByMail$Gen2014_fIMC)),
    c("2014 General", "Pass IMC", Val_ByMail$Gen2014_pIMC$coefficients, summary(Val_ByMail$Gen2014_pIMC)$coef[,"Std. Error"], confint(Val_ByMail$Gen2014_pIMC)),
    c("2012 General", "All Respondents", Val_ByMail$Gen2012_All$coefficients, summary(Val_ByMail$Gen2012_All)$coef[,"Std. Error"], confint(Val_ByMail$Gen2012_All)),
    c("2012 General", "Fail IRI", Val_ByMail$Gen2012_fIRI$coefficients, summary(Val_ByMail$Gen2012_fIRI)$coef[,"Std. Error"], confint(Val_ByMail$Gen2012_fIRI)),
    c("2012 General", "Pass IRI", Val_ByMail$Gen2012_pIRI$coefficients, summary(Val_ByMail$Gen2012_pIRI)$coef[,"Std. Error"], confint(Val_ByMail$Gen2012_pIRI)),
    c("2012 General", "Fail IMC", Val_ByMail$Gen2012_fIMC$coefficients, summary(Val_ByMail$Gen2012_fIMC)$coef[,"Std. Error"], confint(Val_ByMail$Gen2012_fIMC)),
    c("2012 General", "Pass IMC", Val_ByMail$Gen2012_pIMC$coefficients, summary(Val_ByMail$Gen2012_pIMC)$coef[,"Std. Error"], confint(Val_ByMail$Gen2012_pIMC))
  ), stringsAsFactors = FALSE
)

colnames(Table_Val_ByMail) <- c("Election", "Group", "Estimate", "SE", "Lower", "Upper")

Table_Val_ByMail <- Table_Val_ByMail %>%
  mutate(Election = factor(Election, levels = rev(c("2018 General", "2018 Primary", "2016 General", "2016 Primary", "2014 General", "2012 General"))),
         Group = factor(Group, levels = rev(c("All Respondents", "Fail IRI", "Pass IRI", "Fail IMC", "Pass IMC"))),
         Estimate = as.double(Estimate),
         SE = as.double(SE),
         Lower = as.double(Lower),
         Upper = as.double(Upper))

Table_Val_ByMail_Formatted <-
  bind_rows(
    Table_Val_ByMail %>% select(Election, Group, Estimate) %>%
      mutate(Group = factor(Group, levels = rev(levels(Group))), Estimate = as.character(round(Estimate*100, 1))) %>%
      spread(key = Group, value = Estimate) %>%
      mutate(Stat = "Estimate"),
    Table_Val_ByMail %>% select(Election, Group, SE) %>% arrange(Election, Group) %>%
      mutate(Group = factor(Group, levels = rev(levels(Group))), SE = str_c("(", round(SE*100, 1), ")")) %>%
      spread(key = Group, value = SE) %>%
      mutate(Stat = "Standard Error")
  ) %>%
  arrange(desc(Election), Stat)

Figure_Val_ByMail_IRI <- ggplot(Table_Val_ByMail %>% filter(Group %in% c("Pass IRI", "Fail IRI")), aes(x = Election, y = Estimate*100, colour = Group)) +
  geom_point(size = 2, position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = Lower*100, ymax = Upper*100), width = 0.2, position = position_dodge(width=0.9)) +
  geom_linerange(aes(ymin = Estimate*100 - SE*100, ymax = Estimate*100 + SE*100), size = 1.5, position = position_dodge(width=0.9)) +
  coord_flip() +
  labs(x = "Election", y = "", colour = "") +
  scale_colour_manual(values = rev(c("grey20", "grey80")), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(50, 100), breaks = c(50, 60, 70, 80, 90, 100)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_bw() +
  theme(panel.border = element_rect(colour="black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

Figure_Val_ByMail_IMC <- ggplot(Table_Val_ByMail %>% filter(Group %in% c("Pass IMC", "Fail IMC")), aes(x = Election, y = Estimate*100, colour = Group)) +
  geom_point(size = 2, position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = Lower*100, ymax = Upper*100), width = 0.2, position = position_dodge(width=0.9)) +
  geom_linerange(aes(ymin = Estimate*100 - SE*100, ymax = Estimate*100 + SE*100), size = 1.5, position = position_dodge(width=0.9)) +
  coord_flip() +
  labs(x = "Election", y = "Percentage of Validated Voters Who Voted by Mail", colour = "") +
  scale_colour_manual(values = rev(c("grey20", "grey80")), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(50, 100), breaks = c(50, 60, 70, 80, 90, 100)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_bw() +
  theme(panel.border = element_rect(colour="black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

ggsave("./Result/Figure_Val_ByMail_IRI.tiff", Figure_Val_ByMail_IRI, width = 8, height = 2, units = "in", dpi = 600)
ggsave("./Result/Figure_Val_ByMail_IMC.tiff", Figure_Val_ByMail_IMC, width = 8, height = 2, units = "in", dpi = 600)

#*************************************************************
# Figure 5/Table SM7
#*************************************************************

# The bias, standard error, and root mean squared error of turnout estimates (percentage) based on
# - all respondents,
# - respondents passing the IRI, and
# - respondents passing the IMC
# for six recent elections

# Bias is calculated as the percentage of voters according to the self-reports minus the percentage of voters according to administrative records.
# Std. Error is obtained from the standard error associated with the intercept by running an linear regression of turnout containing only the intercept.
# RMSE is calculated using 1,000 bootstrap samples with replacements.

Data_Turnout <- list()

Data_Turnout$Gen2018 <- OCsurvey %>%
  filter(Asked_Gen2018_Turnout == TRUE, Registered_OC_Gen2018 == TRUE) %>%
  transmute(Turnout = Gen2018_Turnout, Turnout_SR = Gen2018_Turnout_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_Turnout$Gen2016 <- OCsurvey %>%
  filter(Asked_Gen2016_Turnout == TRUE, Registered_OC_Gen2016 == TRUE) %>%
  transmute(Turnout = Gen2016_Turnout, Turnout_SR = Gen2016_Turnout_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_Turnout$Gen2014 <- OCsurvey %>%
  filter(Asked_Gen2014_Turnout == TRUE, Registered_OC_Gen2014 == TRUE) %>%
  transmute(Turnout = Gen2014_Turnout, Turnout_SR = Gen2014_Turnout_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_Turnout$Gen2012 <- OCsurvey %>%
  filter(Asked_Gen2012_Turnout == TRUE, Registered_OC_Gen2012 == TRUE) %>%
  transmute(Turnout = Gen2012_Turnout, Turnout_SR = Gen2012_Turnout_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_Turnout$PRI2018 <- OCsurvey %>%
  filter(Asked_PRI2018_Turnout == TRUE, Registered_OC_PRI2018 == TRUE) %>%
  transmute(Turnout = PRI2018_Turnout, Turnout_SR = PRI2018_Turnout_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_Turnout$PRI2016 <- OCsurvey %>%
  filter(Asked_PRI2016_Turnout == TRUE, Registered_OC_PRI2016 == TRUE) %>%
  transmute(Turnout = PRI2016_Turnout, Turnout_SR = PRI2016_Turnout_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

get_bias_se_turnout <- function(data) {
  Election_actual <- lm(Turnout ~ 1, data = data)
  Election_All <- lm(Turnout_SR ~ 1, data = data)
  Election_IRI <- lm(Turnout_SR ~ 1, data = data %>% filter(AF_IRI1 == 1))
  Election_IMC <- lm(Turnout_SR ~ 1, data = data %>% filter(AF_IMC1 == 1))
  bias_turnout <- c(Election_All$coefficients, Election_IRI$coefficients, Election_IMC$coefficients) - Election_actual$coefficients
  se_turnout <- c(summary(Election_All)$coef[,"Std. Error"], summary(Election_IRI)$coef[,"Std. Error"], summary(Election_IMC)$coef[,"Std. Error"])
  return(list(bias = bias_turnout, se = se_turnout))
}

stats_turnout <- function(data, indices) {
  b.sample <- data[indices,]
  Election_actual <- lm(Turnout ~ 1, data = data)
  b.Election_All <- lm(Turnout_SR ~ 1, data = b.sample)
  b.Election_IRI <- lm(Turnout_SR ~ 1, data = b.sample %>% filter(AF_IRI1 == 1))
  b.Election_IMC <- lm(Turnout_SR ~ 1, data = b.sample %>% filter(AF_IMC1 == 1))
  turnout_actual <- Election_actual$coefficients
  error_turnout_all <- b.Election_All$coefficients - turnout_actual
  error_turnout_IRI <- b.Election_IRI$coefficients - turnout_actual
  error_turnout_IMC <- b.Election_IMC$coefficients - turnout_actual
  CI_turnout_all <- confint(b.Election_All, level = 0.9)
  CI_turnout_IRI <- confint(b.Election_IRI, level = 0.9)
  CI_turnout_IMC <- confint(b.Election_IMC, level = 0.9)
  covered_turnout_all <- (CI_turnout_all[1] <= turnout_actual & turnout_actual <= CI_turnout_all[2])
  covered_turnout_IRI <- (CI_turnout_IRI[1] <= turnout_actual & turnout_actual <= CI_turnout_IRI[2])
  covered_turnout_IMC <- (CI_turnout_IMC[1] <= turnout_actual & turnout_actual <= CI_turnout_IMC[2])
  return(c(error_turnout_all, error_turnout_IRI, error_turnout_IMC, covered_turnout_all, covered_turnout_IRI, covered_turnout_IMC))
}

get_rmse_coverage_turnout <- function(data, statistic, R){
  b.outputs <- boot(data = data, statistic = statistic, R = R)$t
  rmse_turnout_all <- sqrt(mean(b.outputs[,1]^2))
  rmse_turnout_IRI <- sqrt(mean(b.outputs[,2]^2))
  rmse_turnout_IMC <- sqrt(mean(b.outputs[,3]^2))
  coverage_turnout_all <- mean(b.outputs[,4]^2)
  coverage_turnout_IRI <- mean(b.outputs[,5]^2)
  coverage_turnout_IMC <- mean(b.outputs[,6]^2)
  return(list(rmse = c(rmse_turnout_all, rmse_turnout_IRI, rmse_turnout_IMC),
              coverage = c(coverage_turnout_all, coverage_turnout_IRI, coverage_turnout_IMC)))
}

set.seed(2020)
bias_se_turnout <- lapply(Data_Turnout, get_bias_se_turnout)
rmse_coverage_turnout <- lapply(Data_Turnout, get_rmse_coverage_turnout, statistic = stats_turnout, R = 1000)

Table_Est_Turnout <- data.frame(
  Election = c(rep("2018 General", 3), rep("2016 General", 3), rep("2014 General", 3), rep("2012 General", 3), rep("2018 Primary", 3), rep("2016 Primary", 3)),
  Group = rep(c("All", "Pass IRI", "Pass IMC"), 6),
  Bias = unlist(lapply(bias_se_turnout, `[[`, "bias")),
  SE = unlist(lapply(bias_se_turnout, `[[`, "se")),
  RMSE = unlist(lapply(rmse_coverage_turnout, `[[`, "rmse")),
  Coverage = unlist(lapply(rmse_coverage_turnout, `[[`, "coverage"))
) %>%
  mutate(Election = factor(Election, levels = c("2018 General", "2018 Primary", "2016 General", "2016 Primary", "2014 General", "2012 General")),
         Group = factor(Group, levels = c("All", "Pass IRI", "Pass IMC")))

Table_Est_Turnout_Formatted <-
  bind_rows(
    Table_Est_Turnout %>% select(Election, Group, Bias) %>%
      mutate(Bias = round(Bias*100, 1)) %>%
      spread(key = Group, value = Bias) %>%
      mutate(Stat = "Bias"),
    Table_Est_Turnout %>% select(Election, Group, SE) %>%
      mutate(SE = round(SE*100, 1)) %>%
      spread(key = Group, value = SE) %>%
      mutate(Stat = "Standard Error"),
    Table_Est_Turnout %>% select(Election, Group, RMSE) %>%
      mutate(RMSE = round(RMSE*100, 1)) %>%
      spread(key = Group, value = RMSE) %>%
      mutate(Stat = "RMSE"),
  )

Figure_Est_Turnout_Bias <- ggplot(Table_Est_Turnout, aes(x = Group, y = Bias*100, group = Election)) +
  geom_line(aes(linetype = Election)) +
  geom_point(aes(shape = Election)) +
  labs(x = "", y = "Bias", group = "Election") +
  guides(group = FALSE, linetype = FALSE, shape = FALSE) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

Figure_Est_Turnout_SE <- ggplot(Table_Est_Turnout, aes(x = Group, y = SE*100, group = Election)) +
  geom_line(aes(linetype = Election)) +
  geom_point(aes(shape = Election)) +
  labs(x = "", y = "Standard Error", group = "Election") +
  guides(group = FALSE, linetype = FALSE, shape = FALSE) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

Figure_Est_Turnout_RMSE <- ggplot(Table_Est_Turnout, aes(x = Group, y = RMSE*100, group = Election)) +
  geom_line(aes(linetype = Election)) +
  geom_point(aes(shape = Election)) +
  labs(x = "", y = "Root Mean Squared Error", group = "Election") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

ggsave("./Result/Figure_Est_Turnout_Bias.tiff", Figure_Est_Turnout_Bias, width = 2.25, height = 4, units = "in", dpi = 600)
ggsave("./Result/Figure_Est_Turnout_SE.tiff", Figure_Est_Turnout_SE, width = 2.25, height = 4, units = "in", dpi = 600)
ggsave("./Result/Figure_Est_Turnout_RMSE.tiff", Figure_Est_Turnout_RMSE, width = 3.5, height = 4, units = "in", dpi = 600)

#*************************************************************
# Figure 6/Table SM8
#*************************************************************

# The bias, standard error, and root mean squared error of estimates of the percentage of by-mail voters based on
# - all respondents,
# - respondents passing the IRI, and
# - respondents passing the IMC
# for six recent elections

# Bias is calculated as the percentage of by-mail voters according to the self-reports minus the percentage of voters according to administrative records.
# Std. Error is obtained from the standard error associated with the intercept by running an linear regression of voting-by-mail containing only the intercept.
# RMSE is calculated using 1,000 bootstrap samples with replacements.

Data_ByMail <- list()

Data_ByMail$Gen2018 <- OCsurvey %>%
  filter(Asked_Gen2018_ByMail == TRUE, Registered_OC_Gen2018 == TRUE) %>%
  transmute(ByMail = Gen2018_ByMail, ByMail_SR = Gen2018_ByMail_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_ByMail$Gen2016 <- OCsurvey %>%
  filter(Asked_Gen2016_ByMail == TRUE, Registered_OC_Gen2016 == TRUE) %>%
  transmute(ByMail = Gen2016_ByMail, ByMail_SR = Gen2016_ByMail_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_ByMail$Gen2014 <- OCsurvey %>%
  filter(Asked_Gen2014_ByMail == TRUE, Registered_OC_Gen2014 == TRUE) %>%
  transmute(ByMail = Gen2014_ByMail, ByMail_SR = Gen2014_ByMail_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_ByMail$Gen2012 <- OCsurvey %>%
  filter(Asked_Gen2012_ByMail == TRUE, Registered_OC_Gen2012 == TRUE) %>%
  transmute(ByMail = Gen2012_ByMail, ByMail_SR = Gen2012_ByMail_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_ByMail$PRI2018 <- OCsurvey %>%
  filter(Asked_PRI2018_ByMail == TRUE, Registered_OC_PRI2018 == TRUE) %>%
  transmute(ByMail = PRI2018_ByMail, ByMail_SR = PRI2018_ByMail_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

Data_ByMail$PRI2016 <- OCsurvey %>%
  filter(Asked_PRI2016_ByMail == TRUE, Registered_OC_PRI2016 == TRUE) %>%
  transmute(ByMail = PRI2016_ByMail, ByMail_SR = PRI2016_ByMail_SR, AF_IRI1 = AF_IRI1, AF_IMC1 = AF_IMC1)

get_bias_se_bymail <- function(data) {
  Election_actual <- lm(ByMail ~ 1, data = data)
  Election_All <- lm(ByMail_SR ~ 1, data = data)
  Election_IRI <- lm(ByMail_SR ~ 1, data = data %>% filter(AF_IRI1 == 1))
  Election_IMC <- lm(ByMail_SR ~ 1, data = data %>% filter(AF_IMC1 == 1))
  bias_bymail <- c(Election_All$coefficients, Election_IRI$coefficients, Election_IMC$coefficients) - Election_actual$coefficients
  se_bymail <- c(summary(Election_All)$coef[,"Std. Error"], summary(Election_IRI)$coef[,"Std. Error"], summary(Election_IMC)$coef[,"Std. Error"])
  return(list(bias = bias_bymail, se = se_bymail))
}

stats_bymail <- function(data, indices) {
  b.sample <- data[indices,]
  Election_actual <- lm(ByMail ~ 1, data = data)
  b.Election_All <- lm(ByMail_SR ~ 1, data = b.sample)
  b.Election_IRI <- lm(ByMail_SR ~ 1, data = b.sample %>% filter(AF_IRI1 == 1))
  b.Election_IMC <- lm(ByMail_SR ~ 1, data = b.sample %>% filter(AF_IMC1 == 1))
  bymail_actual <- Election_actual$coefficients
  error_bymail_all <- b.Election_All$coefficients - bymail_actual
  error_bymail_IRI <- b.Election_IRI$coefficients - bymail_actual
  error_bymail_IMC <- b.Election_IMC$coefficients - bymail_actual
  CI_bymail_all <- confint(b.Election_All, level = 0.9)
  CI_bymail_IRI <- confint(b.Election_IRI, level = 0.9)
  CI_bymail_IMC <- confint(b.Election_IMC, level = 0.9)
  covered_bymail_all <- (CI_bymail_all[1] <= bymail_actual & bymail_actual <= CI_bymail_all[2])
  covered_bymail_IRI <- (CI_bymail_IRI[1] <= bymail_actual & bymail_actual <= CI_bymail_IRI[2])
  covered_bymail_IMC <- (CI_bymail_IMC[1] <= bymail_actual & bymail_actual <= CI_bymail_IMC[2])
  return(c(error_bymail_all, error_bymail_IRI, error_bymail_IMC, covered_bymail_all, covered_bymail_IRI, covered_bymail_IMC))
}

get_rmse_coverage_bymail <- function(data, statistic, R){
  b.outputs <- boot(data = data, statistic = statistic, R = R)$t
  rmse_bymail_all <- sqrt(mean(b.outputs[,1]^2))
  rmse_bymail_IRI <- sqrt(mean(b.outputs[,2]^2))
  rmse_bymail_IMC <- sqrt(mean(b.outputs[,3]^2))
  coverage_bymail_all <- mean(b.outputs[,4]^2)
  coverage_bymail_IRI <- mean(b.outputs[,5]^2)
  coverage_bymail_IMC <- mean(b.outputs[,6]^2)
  return(list(rmse = c(rmse_bymail_all, rmse_bymail_IRI, rmse_bymail_IMC),
              coverage = c(coverage_bymail_all, coverage_bymail_IRI, coverage_bymail_IMC)))
}

set.seed(2020)
bias_se_bymail <- lapply(Data_ByMail, get_bias_se_bymail)
rmse_coverage_bymail <- lapply(Data_ByMail, get_rmse_coverage_bymail, statistic = stats_bymail, R = 1000)

Table_Est_ByMail <- data.frame(
  Election = c(rep("2018 General", 3), rep("2016 General", 3), rep("2014 General", 3), rep("2012 General", 3), rep("2018 Primary", 3), rep("2016 Primary", 3)),
  Group = rep(c("All", "Pass IRI", "Pass IMC"), 6),
  Bias = unlist(lapply(bias_se_bymail, `[[`, "bias")),
  SE = unlist(lapply(bias_se_bymail, `[[`, "se")),
  RMSE = unlist(lapply(rmse_coverage_bymail, `[[`, "rmse")),
  Coverage = unlist(lapply(rmse_coverage_bymail, `[[`, "coverage"))
) %>%
  mutate(Election = factor(Election, levels = c("2018 General", "2018 Primary", "2016 General", "2016 Primary", "2014 General", "2012 General")),
         Group = factor(Group, levels = c("All", "Pass IRI", "Pass IMC")),
         Abs_Bias = abs(Bias))

Table_Est_ByMail_Formatted <-
  bind_rows(
    Table_Est_ByMail %>% select(Election, Group, Bias) %>%
      mutate(Bias = round(Bias*100, 1)) %>%
      spread(key = Group, value = Bias) %>%
      mutate(Stat = "Bias"),
    Table_Est_ByMail %>% select(Election, Group, SE) %>%
      mutate(SE = round(SE*100, 1)) %>%
      spread(key = Group, value = SE) %>%
      mutate(Stat = "Standard Error"),
    Table_Est_ByMail %>% select(Election, Group, RMSE) %>%
      mutate(RMSE = round(RMSE*100, 1)) %>%
      spread(key = Group, value = RMSE) %>%
      mutate(Stat = "RMSE"),
  )

Figure_Est_ByMail_Bias <- ggplot(Table_Est_ByMail, aes(x = Group, y = Abs_Bias*100, group = Election)) +
  geom_line(aes(linetype = Election)) +
  geom_point(aes(shape = Election)) +
  labs(x = "", y = "Bias (Absolute Value)", group = "Election") +
  guides(group = FALSE, linetype = FALSE, shape = FALSE) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

Figure_Est_ByMail_SE <- ggplot(Table_Est_ByMail, aes(x = Group, y = SE*100, group = Election)) +
  geom_line(aes(linetype = Election)) +
  geom_point(aes(shape = Election)) +
  labs(x = "", y = "Standard Error", group = "Election") +
  guides(group = FALSE, linetype = FALSE, shape = FALSE) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

Figure_Est_ByMail_RMSE <- ggplot(Table_Est_ByMail, aes(x = Group, y = RMSE*100, group = Election)) +
  geom_line(aes(linetype = Election)) +
  geom_point(aes(shape = Election)) +
  labs(x = "", y = "Root Mean Squared Error", group = "Election") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

ggsave("./Result/Figure_Est_ByMail_Bias.tiff", Figure_Est_ByMail_Bias, width = 2.25, height = 4, units = "in", dpi = 600)
ggsave("./Result/Figure_Est_ByMail_SE.tiff", Figure_Est_ByMail_SE, width = 2.25, height = 4, units = "in", dpi = 600)
ggsave("./Result/Figure_Est_ByMail_RMSE.tiff", Figure_Est_ByMail_RMSE, width = 3.5, height = 4, units = "in", dpi = 600)
