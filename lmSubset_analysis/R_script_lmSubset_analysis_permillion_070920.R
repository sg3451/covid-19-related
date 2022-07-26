getwd()
setwd("E:\\Ghosh Dropbox\\SUJOY GHOSH\\covid_analysis\\analysis_June10data\\lmSubsets_May_June")
dir()

install.packages("lmSubsets")
library(lmSubsets)
install.packages("tidyverse")
library(tidyverse)
#=======================================================================================
#practice
#data("IbkTemperature", package = "lmSubsets")
IbkTemperature = na.omit(IbkTemperature)

#MOS0 = lm(temp ~ t2m + time + sin + cos + sin2 + cos2,
           #data = IbkTemperature)
#summary(MOS0)

MOS2_all = lmSubsets(temp ~ ., data = IbkTemperature)
MOS2 = refit(lmSelect(MOS2_all, penalty = "BIC"))
#MOS2
#plot(MOS2)
summary(MOS2_all)
summary(MOS2)

#image(MOS2_all, size = 8:27, hilite = 1, hilite_penalty = "BIC")
#==========================================================================================
#modeling by subset selection using lmSubsets
master = read.delim('input_lmSubset_formatted_variables_may_jun_070920.txt', sep='\t', header=T)
#remove OWID_WRL row (data for whole world)
master = filter(master, CountryName!="World")
str(master)
dim(master)
#remove rows with NA for modeling 

#select tables relevant for modeling with June 2020 data
master_jun = select(master, -c("CountryName","CountryCode","log_tot_case_per_million_may",
                           "log_H_covidduration_11May"))

#remove rows with NA in any variable
master_jun = na.omit(master_jun)
dim(master_jun)
str(master_jun)

masterslim_jun = select(master_jun, -c('log_G_Rain_mm_Jan2016','log_G_Rain_mm_Mar2016','log_G_Rain_mm_Apr2016',
                               'H_DALY_CVD_70yrs','H_DALY_resp_70yrs','H_DALY_CVD_all','H_DALY_resp_all',
                               'H_Diabetes2019', 'log_G_Rain_mm_Feb2016', 'log_D_Popden2018'))

dim(masterslim_jun)
# generate models
mod_all_slim_permillion_jun = lmSubsets(log_tot_case_per_million_jun ~ ., data=masterslim_jun)
summary(mod_all_slim_permillion_jun)
#see which variables are used in which models
mod_all_slim_permillion_jun


png("image_allvariables_slimvar_jun10data_hilite_AIC_070920.png", height=12, width=12, units="in", res=600)
pdf("image_allvariables_slimvar_jun10data_hilite_AIC_070920.pdf", height=12, width=12)
par(mai=c(1.2,0.82,0.82,0.42),cex=1.2)
image(mod_all_slim_permillion_jun, size = 1:24, hilite =1, hilite_penalty = "AIC", lab="lab",
      col_hilite=cbind("red","cyan"), axis_pos = -2, axis_lab=-11)
dev.off()
#---------------------------------------------------------------------------------------------
#select tables relevant for modeling with May 2020 data
master_may = dplyr::select(master, -c("CountryName","CountryCode","log_tot_case_per_million_jun",
                               "log_H_covidduration_10Jun"))

#remove rows with NA in any variable
master_may = na.omit(master_may)
dim(master_may)
str(master_may)

masterslim_may = dplyr::select(master_may, -c('log_G_Rain_mm_Jan2016','log_G_Rain_mm_Mar2016','log_G_Rain_mm_Apr2016',
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
#======================================================================================================

#best subset model
mod_sel = refit(lmSelect(mod_all_slim_permillion_may, penalty="AIC"))
summary(mod_sel)
plot(mod_sel)
#====================================================================================================



