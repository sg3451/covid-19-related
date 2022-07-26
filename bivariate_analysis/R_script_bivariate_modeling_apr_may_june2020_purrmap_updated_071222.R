## Default repo
local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})

setwd("E:\\Ghosh Dropbox\\SUJOY GHOSH\\covid_analysis\\analysis_June10data\\bivariate_analysis_Apr_May_Jun_updated_071222")
dir()

install.packages("tidyverse")
library(tidyverse)


#read data
master = read.delim("master_Jan10_Jun10_selectvariables_bivariate_071222.txt", sep='\t', header=T)
str(master)
dim(master)

#log10 transform specific columns (using just log would get the natural logarithms)
#the specified columns will be log10 transformed although variable names will remain the same
master_formatted = mutate_each(master, funs(log10(.)), 2:4,14:26,29:33)
str(master_formatted)

#stack columns to prepare for linear regression by variables

long = pivot_longer(master_formatted, cols = G_Temp_C_Jan2016:Temp_Mar_2010_2016_avg , names_to = "variable", values_to = "value" )
str(long)

#analysis by linear regression 
#group the stacked data by variable name (column name =variable) via nest function
by_var = long %>% group_by(variable)%>% nest()
by_var

#check the resulting grouped table for 1 variable
by_var$data[[2]]

#run linear regression (or other actions) by variables, distinguish models by month
# lm(value column from stacked table ~ variables)
by_var_apr = by_var %>% mutate(model= purrr::map(data,~ lm(tot_case_permillion_apr ~ value, data=.)))
by_var_may = by_var %>% mutate(model= purrr::map(data,~ lm(tot_case_permillion_may ~ value, data=.)))
by_var_jun = by_var %>% mutate(model= purrr::map(data,~ lm(tot_case_permillion_jun ~ value, data=.)))

#models for each gene are saved in a new column in the new tibble
by_var_jun

#check one linear fit
by_var_jun$model[[2]]

#ungroup the data by unnest function
by_var_jun %>% unnest(data)

#model summaries via broom function
summary = by_var_apr %>% mutate(glance = purrr::map(model, broom::glance))%>% unnest(glance)
# remove the data and model columns which are 'lists' to enable saving summary data
summary2 = subset(summary,select = -c(data,model))
summary2
write.table(summary2,"out_modelsummaries_bivariate_apr2020data_with2010_2016_temp_rain_avg_072222.txt", sep='\t')

#extract the residuals and save them as text file
residuals = by_var_apr %>% mutate(res = purrr::map(model, broom::augment))%>% unnest(res)
residuals
#remove any list from residuals tibble to be able to save the file
residuals2 = subset(residuals, select =-c(data, model))
write.table(residuals2,"out_residuals_bivariate_apr2020data_with2010_2016_temp_rain_avg_072222.txt", sep='\t')

#save the coefficients and save them as text file
coefficients = by_var_apr %>% mutate(coeff = purrr::map(model,broom::tidy)) %>% unnest(coeff)
coefficients
#remove any list from coefficients to be able to save the file
coefficients2 = subset(coefficients, select = -c(data,model))
write.table(coefficients2, "out_coefficients_bivariate_apr2020data_with2010_2016_temp_rain_avg_072222.txt", sep='\t')