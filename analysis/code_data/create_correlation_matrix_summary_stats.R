#####################################
## Integrating CYber Technologies   #
## Summary Statistics               #
## Nadiya Kostyuk                   #
## August 3, 2024                   #
## R version: 4.4.1                 #
#####################################
rm(list=ls())


## Install & load packages (all at once)
list.of.packages <- c("corrplot", 'dplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages,dependencies=TRUE)}
lapply(list.of.packages, require, character.only = TRUE)

# set working directory:
#setwd("C:/Users/nkostyuk/Dropbox/dissertation/organizations/Publications/Complementarity/II/replication_files")


load("data_final.RData")


# checking correlation here:
cor_M <- data_final %>%
  dplyr::select(
    weights_anyadversary2_oldlong, weights_anyadversary2_newlong,
    weights_defenseally_oldlong, weights_defenseally_newlong,
    # service dominance
    service_dominance, 
    # controls: 
    LOG_GDP_PERCAP_sc
     )
names(cor_M) <- c(
  'Adversaries Integrating Cyber \n via Existing Agencies (lag, sc)',
  "Adversaries Integrating Cyber \n via New Agencies (lag, sc)",
  'Allies Integrating Cyber \n via Existing Agencies (lag, sc)',
  "Allies Integrating Cyber \n via New Agencies (lasg, sc)",
  'Bureaucratic Dominance',
  "GDP per Capita (log, sc)")

png(filename = "draft/Figures/corrplot.png", width = 800, height = 600)

corrplot(cor(cor_M, use = "complete.obs"),
         method="number", # 'shade'
         number.cex=1.2,
         tl.cex=1.2)
dev.off()

# saving the plot: 



# summary stats:
main_varz = c(
  # not including it here because it is a factor variable:
  #'event2',
  'weights_anyadversary2_oldlong', 
  'weights_anyadversary2_newlong',
  'weights_defenseally_oldlong',
  'weights_defenseally_newlong',
  'service_dominance',
  'LOG_GDP_PERCAP_sc'
)

tab = summary(data_final[, main_varz])
dim(tab)
# creating latex table: 
tab = tab[c(1, 3, 4, 6),]
tab
class(tab)
dim(tab)
tab = as.data.frame(tab)
tab = tab %>%
  mutate(Freq=as.numeric(sapply(strsplit(Freq, ":"),"[[",2)))%>%
  mutate(Freq=round(Freq, digits = 2))
tab
# rename Va2: 
tab = tab %>% 
  mutate(Var2 = case_when(
    Var2 == 'weights_anyadversary2_oldlong' ~ 'Adversaries Integrating Cyber \n via Existing Agencies (lasg, sc)',
    Var2 == 'weights_anyadversary2_newlong' ~ 'Adversaries Integrating Cyber \n via New Agencies (lag, sc)',
    Var2 == 'weights_defenseally_oldlong' ~ 'Allies Integrating Cyber \n via Existing Agencies (lasg, sc)',
    Var2 == 'weights_defenseally_newlong' ~ 'Allies Integrating Cyber \n via New Agencies (lasg, sc)',
    Var2 == 'service_dominance' ~ 'Service Dominance',
    Var2 == 'LOG_GDP_PERCAP_sc' ~ 'GDP per Capita (log, sc)'
  )
  )

# from long to wide:
tab_wide = tab %>%
  mutate(ID=seq(1, 24, by=1))%>%
  tidyr::spread(Var2, Freq)%>%
  select(-c(--ID, Var1))

tab_wide = lapply(tab_wide, function(x) {x[!is.na(x)]}) 
tab_wide = do.call(rbind, tab_wide)
colnames(tab_wide) = c('Minimum', 'Median', 'Mean', 'Maximum')
tab_wide

library(xtable)
xtable(tab_wide)
writeLines(capture.output(xtable(tab_wide)), paste0('draft/Tables/summary_stats.tex'))




