## Default repo
local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})


#best model fits are applied to country-specific COVID-19 case trajectories
#best model fits are previously determined for each country via AIC 
# drc and aomisc are used for fitting the models

getwd()
setwd("E:\\Ghosh Dropbox\\SUJOY GHOSH\\covid_analysis\\covid_timecourse\\jun10_analysis_usethis_062420")
dir()

#install aomisc before tidyverse because aomisc will also instally plyr which is recommended to be installed prior to dplyr
install.packages("devtools")
devtools::install_github("onofriAndreaPG/aomisc")

install.packages(c("tidyverse","drc","multcompView"))
library(tidyverse)
library(drc)
library(multcompView)
#install the aomisc package
#install.packages("remotes")
#remotes::install_github("OnofriAndreaPG/aomisc")
library(aomisc)

library(ggpubr)
library(ggpmisc)
library(stats)
#following packages were separately installed as they had problems installing with aomisc
#install.packages("devtools")
#devtools::install_github("onofriAndreaPG/aomisc")

data = read.delim("timecourse_jun10_updated_062420.txt", sep='\t', header=T)
dim(data)
str(data)

#remove unwanted columns and rows (if any)
data = filter(data, iso_code !="BEN")
dim(data)
data = data[,1:17]
str(data)

#convert date column to class Date
data$date = as.Date(data$date, format = "%m/%d/%y")
class(data$date)



#subset data by countries based on best fitting model
quad = filter(data, iso_code %in% c("CYM",	"CRI",	"CIV",	"JOR",	"KGZ",	"LBN",	"PAN",	"PHL",	"RWA"))
expo = filter(data, iso_code %in% c("ARG",	"ARM",	"KEN",	"MOZ",	"OMN",	"SYR"))
gomp = filter(data, iso_code %in% c("ABW",	"AUS",	"BHS",	"BLR",	"BEL",	"KHM",	"CAN",	"COM",	"HRV",	"FIN",	"FRA",	"DEU",	"GIN",	"GNB",	"ISR",	"ITA",	"MYS",	"MDV",	"MRT",	"NZL",	"NIC",	"NGA",	"PSE",	"ROU",	"RUS",	"SGP",	"SVN",	"SOM",	"KOR",	"ESP",	"SUR",	"SWZ",	"SWE",	"THA",	"TTO",	"UKR",	"ARE",	"GBR",	"USA",	"VNM",	"YEM"))
logis = filter(data, iso_code %in% c("AFG",	"DZA",	"AZE",	"BOL",	"BRA",	"BRN",	"BGR",	"CMR",	"CHL",	"COL",	"COD",	"DJI",	"EGY",	"SLV",	"GNQ",	"ETH",	"FRO",	"GAB",	"GTM",	"HND",	"ISL",	"IRQ",	"JPN",	"KWT",	"LBY",	"MDG",	"MWI",	"MUS",	"MNG",	"MNE",	"NPL",	"PAK",	"PRY",	"PRI",	"SMR",	"SLE",	"SVK",	"ZAF",	"TJK",	"TZA",	"TGO",	"UGA",	"VEN",	"ZWE"))
loglogis = filter(data, iso_code %in% c("TCD",	"CHN",	"COG",	"CUB",	"CYP",	"CZE",	"DNK",	
                                       "DOM",	"ECU",	"EST",	"GEO",	"GHA",	"GIB",	"GRC",	"GUM",	
                                       "GGY",	"GUY",	"HTI",	"HUN",	"IND",	"IDN",	"IRN",	"IRL",	
                                       "IMN",	"JAM",	"JEY",	"KAZ",	"OWID_KOS",	"LVA",	"LBR",	"LTU",	
                                       "LUX",	"MKD",	"MLI",	"MLT",	"MEX",	"MDA",	"MAR",	"MMR",	
                                       "NLD",	"NER",	"NOR",	"PER",	"POL",	"PRT",	"QAT",	"STP",	
                                       "SAU",	"SEN",	"SRB",	"SSD",	"LKA",	"SDN",	"CHE",	"TWN",	
                                       "TUN",	"TUR",	"URY",	"UZB",	"ZMB"))


#check model performance on one country
india = filter(data, location=="India")

india_E = drm(totcases_by_maxtotcases ~ num_days,fct = DRC.expoGrowth(),
             data = india)
summary(india_E)
AIC(india_E)

plot(india_E, col=2)


india2g = drm(totcases_by_maxtotcases ~ num_days, fct=G.4(), data=india)
AIC(india2g)
plot(india2g)
india_p = drm(total_cases/max(total_cases) ~ num_days, fct = DRC.poly2(), data = india)
AIC(india_p)
plot(india_p, col=4)

#================================================================================================
#use ggplot to plot drm based models on all countries, based on best fitting models
#models: gompertz, logistic, log logistic, weibull, polynomial(quadratic), exponential
ggplot(loglogis, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1)+
  geom_smooth(method='drm', linetype="solid", color="red", se=F, 
              method.args = list(fct = LL.4())) +
  theme(axis.text.x = element_text(size=12, angle=90)) +
  theme(axis.title = element_text(size=16, face="bold"))+
  theme(strip.text = element_text(size=12, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("Best fit = loglogistic")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")+
  facet_wrap(vars(iso_code), scales="free", nrow=8)
ggsave("loglogistic_bestfit_june10data_updatedformanu_usethis_071122.pdf", height=15, width=15)
ggsave("loglogistic_bestfit_june10data_updatedformanu_usethis_071122.png", height=15, width=15, units="in", dpi=600)

#==========================================================================================================
#gompertz models
ggplot(gomp, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1)+
  geom_smooth(method='drm', linetype="solid", color="red", se=F, 
              method.args = list(fct = G.4())) +
  theme(axis.text.x = element_text(size=12, angle=90)) +
  theme(axis.title = element_text(size=16, face="bold"))+
  theme(strip.text = element_text(size=12, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("Best fit = Gompertz")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")+
  facet_wrap(vars(iso_code), scales="free", nrow=8)
ggsave("gompertz_bestfit_june10data_updatedformanu_usethis_071122.pdf", height=15, width=15)
ggsave("gompertz_bestfit_june10data_updatedformanu_usethis_071122.png", height=15, width=15, units="in", dpi=600)
#======================================================================================================
#logistic models
ggplot(logis, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1)+
  geom_smooth(method='drm', linetype="solid", color="red", se=F, 
              method.args = list(fct = L.4())) +
  theme(axis.text.x = element_text(size=12, angle=90)) +
  theme(axis.title = element_text(size=16, face="bold"))+
  theme(strip.text = element_text(size=12, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("Best fit = logistic")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")+
  facet_wrap(vars(iso_code), scales="free", nrow=8)
ggsave("logistic_bestfit_june10data_updatedformanu_usethis_071122.pdf", height=15, width=15)
ggsave("logistic_bestfit_june10data_updatedformanu_usethis_071122.png", height=15, width=15, units="in", dpi=600)
#======================================================================================================
#exponential models
ggplot(expo, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1)+
  geom_smooth(method="glm",formula = y~x, linetype="solid", color="red", se=F,
              method.args=list(family=stats::gaussian(link='log'))) +
  #geom_smooth(method='drm', linetype="solid", color="red", se=F, 
             #method.args = list(fct = DRC.expoGrowth())) +
  theme(axis.text.x = element_text(size=12, angle=90)) +
  theme(axis.title = element_text(size=16, face="bold"))+
  theme(strip.text = element_text(size=12, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("Best fit = exponential")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")+
  facet_wrap(vars(iso_code), scales="free", nrow=3)
ggsave("exponential_bestfit_june10data_updatedformanu_usethis_071122.pdf", height=15, width=15)
ggsave("exponential_bestfit_june10data_updatedformanu_usethis_071122.png", height=15, width=15, units="in", dpi=600)

#======================================================================================================
#quadratic
#quadratic fit
ggplot(quad, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1, color="black")+
  #geom_smooth(method="glm",formula=y~poly(x,2), method.args=list(family="poisson"),
              #linetype="solid", color="red", se=F)+
  geom_smooth(method=drm,linetype="solid", color="red", se=F, 
              method.args = list(fct = DRC.poly2())) +
  theme(axis.text.x = element_text(size=8, angle=90))+
  theme(axis.text.x = element_text(size=12, angle=90)) +
  theme(axis.title = element_text(size=16, face="bold"))+
  theme(strip.text = element_text(size=12, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("Best fit = quadratic")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")+
  facet_wrap(vars(iso_code), scales="free", nrow=3)

ggsave("quadratic_bestfit_june10data_updatedformanu_usethis_071122.pdf", height=15, width=15)
ggsave("quadratic_bestfit_june10data_updatedformanu_usethis_071122.png", height=15, width=15, units="in", dpi=600)
#=========================================================================================================
#generate plots for specific countries (Fig 1 in manuscript)
ind = filter(data, location=="India")
arg = filter(data, location == "Argentina")
omn = filter(data, location == "Oman")
sgp = filter(data, location == "Singapore")
ita = filter(data, location == "Italy")
jpn = filter(data, location == "Japan")
bra = filter(data, location == "Brazil")
chn = filter(data, location == 'China')
civ = filter(data, location == "Cote d'Ivoire")
phl = filter(data, location == "Philippines")
#------------------------------------------------------------------------------------------
#log-logistic fits for IND, CHN
ggplot(chn, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1, size = 3)+
  geom_smooth(method='drm', linetype="solid", color="red", se=F, 
              method.args = list(fct = LL.4())) +
  theme(axis.text = element_text(size=14, angle=90)) +
  theme(axis.title.x = element_text(size=18, face="bold"))+
  theme(axis.title.y = element_text(size=16, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  theme(panel.background  = element_rect(fill="white"))+
  theme(axis.line = element_line(color="black", size=1))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("CHN")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")
  
ggsave("loglogistic_bestfit_june10data_CHINA_071122.pdf", height=6, width=6)
ggsave("loglogistic_bestfit_june10data_CHINA_071122.png", height=6, width=6, units="in", dpi=600)
#---------------------------------------------------------------------------------------------
#exponential fits for ARG and OMN
ggplot(omn, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1, size=3)+
  geom_smooth(method="glm",formula = y~x, linetype="solid", color="red", se=F,
              method.args=list(family=stats::gaussian(link='log'))) +
  #geom_smooth(method='drm', linetype="solid", color="red", se=F, 
  #method.args = list(fct = DRC.expoGrowth())) +
  theme(axis.text = element_text(size=14, angle=90)) +
  theme(axis.title.x = element_text(size=18, face="bold"))+
  theme(axis.title.y = element_text(size=16, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  theme(panel.background  = element_rect(fill="white"))+
  theme(axis.line = element_line(color="black", size=1))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("OMN")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")
  
ggsave("exponential_bestfit_june10data_OMAN_071122.pdf", height=6, width=6)
ggsave("exponential_bestfit_june10data_OMAN_071122.png", height=6, width=6, units="in", dpi=600)
#---------------------------------------------------------------------------------------------------
#gompertz fits for SGP and ITA
ggplot(ita, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1, size=3)+
  geom_smooth(method='drm', linetype="solid", color="red", se=F, 
              method.args = list(fct = G.4())) +
  theme(axis.text = element_text(size=14, angle=90)) +
  theme(axis.title.x = element_text(size=18, face="bold"))+
  theme(axis.title.y = element_text(size=16, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  theme(panel.background  = element_rect(fill="white"))+
  theme(axis.line = element_line(color="black", size=1))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("ITA")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")

ggsave("gompertz_bestfit_june10data_ITALY_071122.pdf", height=6, width=6)
ggsave("gompertz_bestfit_june10data_ITALY_071122.png", height=6, width=6, units="in", dpi=600)
#----------------------------------------------------------------------------------------------------
#logistic fits for JPN and BRA
ggplot(bra, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1, size=3)+
  geom_smooth(method='drm', linetype="solid", color="red", se=F, 
              method.args = list(fct = L.4())) +
  theme(axis.text = element_text(size=14, angle=90)) +
  theme(axis.title.x = element_text(size=18, face="bold"))+
  theme(axis.title.y = element_text(size=16, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  theme(panel.background  = element_rect(fill="white"))+
  theme(axis.line = element_line(color="black", size=1))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("BRA")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")

ggsave("logistic_bestfit_june10data_BRAZIL_071122.pdf", height=6, width=6)
ggsave("logistic_bestfit_june10data_BRAZIL_071122.png", height=6, width=6, units="in", dpi=600)
#--------------------------------------------------------------------------------------------------------
#quadratic fits for CIV and PHL
ggplot(phl, aes(x=date, y=totcases_by_maxtotcases)) +
  geom_point(shape=1, size=3)+
  geom_smooth(method=drm,linetype="solid", color="red", se=F, 
              method.args = list(fct = DRC.poly2())) +
  theme(axis.text = element_text(size=14, angle=90)) +
  theme(axis.title.x = element_text(size=18, face="bold"))+
  theme(axis.title.y = element_text(size=16, face="bold"))+
  theme(plot.title = element_text(size=18, face="bold"))+
  theme(panel.background  = element_rect(fill="white"))+
  theme(axis.line = element_line(color="black", size=1))+
  xlab("Date")+
  ylab("proportion (total cases / max. total cases)")+
  ggtitle("PHL")+
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %y")

ggsave("quadratic_bestfit_june10data_PHILIPPINES_071122.pdf", height=6, width=6)
ggsave("quadratic_bestfit_june10data_PHILIPPINES_071122.png", height=6, width=6, units="in", dpi=600)

#generate models and aic estimates for all countries
by_country = data %>% group_by(iso_code)%>% nest()
#model of fits from drs and aomisc
by_country = by_country %>% mutate(model= purrr::map(data,~ drm(totcases_by_maxtotcases ~ num_days, fct=W1.4(), data=.x)))
for (i in c(1:170)) {print(AIC(by_country$model[[i]]))}


