#####################################
## Integrating CYber Technologies   #
## Main Analysis                    #
## Nadiya Kostyuk                   #
## August 3, 2024                   #
## R version: 4.4.1                 #
#####################################


rm(list=ls())

# set working directory:
#setwd("C:/Users/nkostyuk/Dropbox/dissertation/organizations/Publications/Complementarity/II/replication_files")
library(survival)
library(dplyr)


# loading data:
load("data_final.RData")

######################
# TABLE1
######################


# influence of adversaries
form1 <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_anyadversary2_oldlong+',
  'weights_anyadversary2_newlong+',
  'cluster(ISO)'
  ) 
form1
mod1<- coxph(as.formula(form1), data=data_final, x=TRUE, id=ISO)
summary(mod1)


# influence of allies:
form2 <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_ally_oldlong+',
  'weights_ally_newlong+',
  'cluster(ISO)'
) 
form2
mod2 <- coxph(as.formula(form2), data=data_final, x=TRUE, id=ISO)
summary(mod2)


# interservice rivalry:
form3 <- paste0(
  'Surv(tstart,tstop, event2)~',
  'service_dominance+',
  'cluster(ISO)'
) 
form3
mod3 <- coxph(as.formula(form3), data=data_final, x=TRUE, id=ISO)
summary(mod3)



# all predictors + control:
form4 <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_anyadversary2_oldlong+',
  'weights_anyadversary2_newlong+',
  'weights_ally_oldlong+',
  'weights_ally_newlong+', #+',#,
  'service_dominance+', 
  'LOG_GDP_PERCAP_sc+cluster(ISO)' 
) 
form4
mod4 <- coxph(as.formula(form4), data=data_final, x=TRUE, id=ISO)
summary(mod4)



# saving the results
mod_list = list()
mod_list[[1]] = mod1
mod_list[[2]] = mod2
mod_list[[3]] = mod3
mod_list[[4]] = mod4
#mod_list[[5]] = mod5
#mod_list[[6]] = mod6
mod_list

#saveRDS(mod_list,file=paste0('analysis/output/main_res.rds'))



##############################
# TABLE 2
# Different types of alliances
##############################

# using model 4 as a base: 
# defensive
form4a <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_anyadversary2_oldlong+',
  'weights_anyadversary2_newlong+',
  'weights_defenseally_oldlong+',
  'weights_defenseally_newlong+', #+',#,
  'service_dominance+', 
  'LOG_GDP_PERCAP_sc+cluster(ISO)' 
) 
form4a
mod4a <- coxph(as.formula(form4a), data=data_final, x=TRUE, id=ISO)
summary(mod4a)


# neutral
form4b <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_anyadversary2_oldlong+',
  'weights_anyadversary2_newlong+',
  'weights_neutralally_oldlong+',
  'weights_neutralally_newlong+', #+',#,
  'service_dominance+', 
  'LOG_GDP_PERCAP_sc+cluster(ISO)' 
) 
form4b
mod4b <- coxph(as.formula(form4b), data=data_final, x=TRUE, id=ISO)
summary(mod4b)


# nonagg
form4c <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_anyadversary2_oldlong+',
  'weights_anyadversary2_newlong+',
  'weights_nonaggally_oldlong+',
  'weights_nonaggally_newlong+', #+',#,
  'service_dominance+', 
  'LOG_GDP_PERCAP_sc+cluster(ISO)' 
) 
form4c
mod4c <- coxph(as.formula(form4c), data=data_final, x=TRUE, id=ISO)
summary(mod4c)


# consultation
form4d <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_anyadversary2_oldlong+',
  'weights_anyadversary2_newlong+',
  'weights_consulally_oldlong+',
  'weights_consulally_newlong+', #+',#,
  'service_dominance+', 
  'LOG_GDP_PERCAP_sc+cluster(ISO)' 
) 
form4d
mod4d <- coxph(as.formula(form4d), data=data_final, x=TRUE, id=ISO)
summary(mod4d)


# saving the results
mod_list = list()
mod_list[[1]] = mod4a
mod_list[[2]] = mod4b
mod_list[[3]] = mod4c
mod_list[[4]] = mod4d
mod_list

#saveRDS(mod_list,file=paste0('analysis/output/tab2_ally_type.rds'))



#####################################
# ROBUSTNESS CHECKS
# Oneline Appendix
#####################################

# Function to extract hazard ratios and confidence intervals
extract_hr_ci <- function(cox_model) {
  # Ensure the input is a Cox proportional hazards model
  if (!inherits(cox_model, "coxph")) {
    stop("The input model is not a Cox proportional hazards model.")
  }
  
  # Extract hazard ratios
  hr <- exp(coef(cox_model))
  
  # Extract confidence intervals
  ci <- exp(confint(cox_model))
  
  # Round hazard ratios and confidence intervals
  hr_rounded <- round(hr, 3)
  ci_lower_rounded <- round(ci[,1], 2)
  ci_upper_rounded <- round(ci[,2], 2)
  
  # Create formatted confidence intervals
  ci_formatted <- paste0("(", ci_lower_rounded, "; ", ci_upper_rounded, ")")
  
  # Create a data frame to store results
  results <- data.frame(
    Variable = names(hr),
    HazardRatio = hr_rounded,
    CI = ci_formatted
  )
  
  return(results)
}



############################
# 1. an alternative measure of alliances 
# (the Correlates of War (COW) Project's data on formal alliances (v4.1) 
# \citep{gibler2008international})

# replication dynamic:
form5a <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_anyadversary2_oldlong+',
  'weights_anyadversary2_newlong+',
  'weights_cowally_oldlong+',
  'weights_cowally_newlong+', #+',#,
  'service_dominance+', 
  'LOG_GDP_PERCAP_sc+cluster(ISO)' 
) 
form5a
mod5a <- coxph(as.formula(form5a), data=data_final, x=TRUE, id=ISO)
summary(mod5a)


extract_hr_ci(mod5a)


##############################################
# 2. alternative network specifications 
# (socio-cultural partners and neighbors)


form5b <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_anyadversary2_oldlong+',
  'weights_anyadversary2_newlong+',
  'weights_ally_oldlong+',
  'weights_ally_newlong+', 
  'weights_contiguity_oldlong+',
  'weights_contiguity_newlong+', 
  'service_dominance+', 
  'LOG_GDP_PERCAP_sc+cluster(ISO)' 
) 
form5b
mod5b <- coxph(as.formula(form5b), data=data_final, x=TRUE, id=ISO)
summary(mod5b)

extract_hr_ci(mod5b)


form5c <- paste0(
  'Surv(tstart,tstop, event2)~',
  'weights_anyadversary2_oldlong+',
  'weights_anyadversary2_newlong+',
  'weights_ally_oldlong+',
  'weights_ally_newlong+', 
  'weights_language_oldlong+',
  'weights_language_newlong+', 
  'service_dominance+', 
  'LOG_GDP_PERCAP_sc+cluster(ISO)' 
) 
form5c
mod5c <- coxph(as.formula(form5c), data=data_final, x=TRUE, id=ISO)
summary(mod5c)

extract_hr_ci(mod5c)


########################################################
# 3. an alternative functional form of the covariates 
# (the inverse hyperbolic sine function)


form6 <- paste0(
  'Surv(tstart,tstop, event2)~',
  'asinh(weights_anyadversary2_oldlong)+',
  'asinh(weights_anyadversary2_newlong)+',
  'asinh(weights_ally_oldlong)+',
  'asinh(weights_ally_newlong)+', #+',#,
  'asinh(service_dominance)+', 
  'asinh(LOG_GDP_PERCAP_sc)+cluster(ISO)' 
) 
form6
mod6 <- coxph(as.formula(form6), data=data_final, x=TRUE, id=ISO)
summary(mod6)

extract_hr_ci(mod6)



####################################
# PREDICTIONS
####################################

# loading data:
load("data_predictions.RData")

data_predictions_2019_2020 = data_predictions %>%
  filter(YEAR>=2019) 



# filter year to only 2019-2020:
fitted_mod = readRDS('analysis/output/main_res.rds')
# main model:
fitted_mod = fitted_mod[[4]]
fitted_mod


# creating manually: x*B
X = model.matrix(fitted_mod, data=data_predictions_2019_2020)
dim(X)
dim(data_predictions)

b = matrix(coef(fitted_mod), nrow = 6, ncol = 2)
lp = X %*% b
# linear predictors for each type of model; relative hazards
lp

data_predictions_2019_2020$lp_old = lp[,1]
data_predictions_2019_2020$lp_new = lp[,2]



# 2019:
adopters_2019 = data_predictions_2019_2020%>%
  filter(YEAR==2019)%>%
  mutate(RANK=rank(lp_old))%>% 
  filter(event2=='assign')%>%  
  mutate(LABEL = paste0(ISO, '(', RANK, ')'))

# 2020_old
adopters_2020_old = data_predictions_2019_2020%>%
  filter(YEAR==2020)%>%
  mutate(RANK=rank(lp_old))%>% 
  filter(event2=='assign')%>% 
  mutate(LABEL = paste0(ISO, '(', RANK, ')'))

# 2020_new
adopters_2020_new = data_predictions_2019_2020%>%
  filter(YEAR==2020)%>%
  mutate(RANK=rank(lp_new))%>% 
  filter(event2=='create')%>%  
  mutate(LABEL = paste0(ISO, '(', RANK, ')'))


adopters19 = adopters_2019 %>%
  dplyr::select(ISO, YEAR, LABEL, RANK, DATE)%>%
  # total 66
  mutate(PERC = case_when(
    ISO =='MRT' ~ ceiling(RANK/64*100), 
    ISO =='ARM' ~ ceiling(RANK/63*100), 
    ISO =='LBN' ~ ceiling(RANK/62*100), 
    ISO =='GHA' ~ ceiling(RANK/61*100), 
    ISO =='URY' ~ ceiling(RANK/60*100)
  )
  ) %>%
  mutate(LABEL = paste0(ISO, '(', PERC, '%)'))


adopters20_new = adopters_2020_new %>%
  dplyr::select(ISO, YEAR, LABEL, RANK, DATE)%>%
  mutate(PERC = case_when(
    ISO =='BWA' ~ ceiling(RANK/59*100)
  )
  ) %>%
  mutate(LABEL = paste0(ISO, '(', PERC, '%)'))


adopters20_old = adopters_2020_old %>%
  dplyr::select(ISO, YEAR, LABEL, RANK, DATE)%>%
  mutate(PERC = case_when(
    ISO =='BHR' ~ ceiling(RANK/59*100) 
  )
  ) %>%
  mutate(LABEL = paste0(ISO, '(', PERC, '%)'))

adopters_all = adopters19 %>%
  full_join(adopters20_old) %>%
  full_join(adopters20_new) %>% 
  mutate(TYPE = ifelse(ISO == 'BWA', 'New', 'Existing'))


adopters_all %>%
  mutate("Agency Type" = TYPE) %>%
  ggplot(aes(x=as.Date(DATE), y=PERC, shape = `Agency Type`))+
  geom_point()+
  scale_x_date(date_labels = "%Y-%m",
               date_breaks = "3 months")+
  geom_text(aes(label=LABEL), vjust = -0.5, size = 3.5, position = position_dodge(width = 6))+
  # geom_hline(yintercept=50, linetype="dotted", 
  #            color = "lightgray", size=1)+
  theme_bw()+
  # theme(legend.background = element_rect(size=0.5, linetype="solid",
  #                                        colour ="black"))+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title =  element_text()#, #element_blank(),
        #legend.box.background = element_rect(colour = "black")
  )+
  labs(x = "Date",y='Percentile Rank of Relative Risk for Incorporating \n Cyber Technologies within Militaries (out of 100%)')+
  # complete blank backgroun
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
#ggsave('draft/Figures/predictions_v2.pdf')











