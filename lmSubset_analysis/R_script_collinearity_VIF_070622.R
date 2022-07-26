getwd()
setwd("/Users/sujoyghosh/Ghosh Dropbox/SUJOY GHOSH/covid_analysis/analysis_June10data/lmSubsets_May_June")
dir()

install.packages("lmSubsets")
library(lmSubsets)
install.packages("tidyverse")
library(tidyverse)
install.packages("car")
library(car)

#---------------------------------------------------------------------------------------------
#use lmSubset to find important variables
master_may = select(master, -c("CountryName","CountryCode","log_tot_case_per_million_jun",
                               "log_H_covidduration_10Jun"))

#remove rows with NA in any variable
master_may = na.omit(master_may)
dim(master_may)
str(master_may)

masterslim_may = select(master_may, -c('log_G_Rain_mm_Jan2016','log_G_Rain_mm_Mar2016','log_G_Rain_mm_Apr2016',
                               'H_DALY_CVD_70yrs','H_DALY_resp_70yrs','H_DALY_CVD_all','H_DALY_resp_all',
                               'H_Diabetes2019'))

dim(masterslim_may)
# generate models
mod_all_slim_permillion_may = lmSubsets(log_tot_case_per_million_may ~ ., data=masterslim_may)
summary(mod_all_slim_permillion_may)
#see which variables are used in which models
mod_all_slim_permillion_may


pdf("image_allvariables_slimvar_may11data_hilite_AIC_090721.pdf", height=10, width=10)
png("image_allvariables_slimvar_may11data_hilite_AIC_090721.png", height=10, width=10, units="in", res=600)
par(mai=c(0.1,0.82,0.82,0.82),cex=1.8)
image(mod_all_slim_permillion_may, size = 1:24, hilite =1, hilite_penalty = "AIC", lab="lab",
      col_hilite=cbind("red","yellow"), axis_pos = -3, axis_lab=-10)
dev.off()


#=================================================================
#calculate variance inflation factor for variables selected by lmsubsets

model_selectfact_may = lm(log_tot_case_per_million_may ~ G_Temp_C_Mar2016 + E_Urban_pct2018 + 
                         E_Employ2018_agri_pct_tot_emp + log_D_Popden2018 + D_Age_15_64y_2018, 
                       data= masterslim_may)
  
summary(model_selectfact_may)

vif_val = vif(model_selectfact_may)
vif_val

png("vif_lmsubset_variables_maydata_070622.png", width=8, height=6, res=555)
pdf("vif_lmsubset_variables_maydata_070622.pdf", width=8, height=6)
par(mar = c(4,15,3,3), mgp=c(1,1,0))
barplot(vif_val, main = "VIF Values", horiz = T, col = "gray58", las=2)
dev.off()

#calculate correlation among selected variables

data_selectvar = masterslim_may[, c("G_Temp_C_Mar2016","E_Urban_pct2018","E_Employ2018_agri_pct_tot_emp",
                                    "log_D_Popden2018","D_Age_15_64y_2018")]

cor(data_selectvar)



#refit for variable selection by BIC, AIC
k
plot(mod_sel_permillion_aic)



