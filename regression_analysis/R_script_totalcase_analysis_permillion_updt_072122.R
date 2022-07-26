## Default repo
local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})

getwd()
setwd("E:\\Ghosh Dropbox\\SUJOY GHOSH\\covid_analysis\\analysis_June10data")
dir()

install.packages("tidyverse")
library(tidyverse)
library(ggrepel)
install.packages(c("ggpubr","ggpmisc"))
library(ggpubr)
library(ggpmisc)
library(viridis)
library(RColorBrewer)
library(readr)

master = read.delim("master_covidcases_Jan10toJun10_permillion_updt_temprange_rainrange_072122.txt", sep='\t', header=T)
#remove OWID_WRL row (data for whole world)
master = filter(master, CountryName!="World")
str(master)
dim(master)
#remove unwanted columns
master = dplyr::select(master, -c(X:X.13))

#rename D_Popden2018 for easy label
#master = rename(master, D_Popden2018 = log10_D_population_2016)

#formula
myformula = y~x

display.brewer.all()

#plots
#plot by visitor


ggplot(master, aes(x=log10(TotVisitor2018_permillion), y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_distiller(palette = "PuRd", direction = 1)+
  ##scale_color_viridis_c(alpha=1,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid", size=1.5, color="black", se=F, formula=myformula) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=2.5, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue",size=4,aes(x=log10(TotVisitor2018_permillion), 
                                        y=log10(tot_case_permillion_jun), label=annot_graph))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))
  
ggsave("TotalCovidCases_permillion_June10__by_visitors_permillion_lm_070120.png", width=6, height=6, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_visitors_permillion_lm_070120.pdf", width=6, height=6)


#plot by temp
str(master)

ggplot(master, aes(x=G_Temp_C_Apr2016, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid", color="black",size=1.5, se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=-20, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") +
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))


ggsave("TotalCovidCases_permillion_June10__by_temps_Apr016_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_temps_Apr2016_lm_070120.pdf", width=7, height=7)


#plot by pollution exposure
#str(master)
#ggplot(master, aes(x=log10_H_PM2.5_mean_annual_exp2017, y=tot_case_permillion_jun, label=annot_graph)) +
#scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
geom_point(color="black", shape=19) +
  geom_smooth(method="lm", linetype="solid", color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=1.3, label.y=5.3, size=5)+
  stat_poly_eq(formula=myformula, label.x="right", label.y="top", aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=5)+ 
  geom_text_repel(color="blue") + labs(color="D_Popden2018")

ggsave("TotalCovidCases_permillion_June10__by_log10pollution_exp_apr_lm_051420.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_log10pollution_exp_lm_051420.pdf", width=7, height=7)

#plot by pct of population exposed
str(master)
#ggplot(master, aes(x=log10_H_PM2.5_pct_pop_exposed2017, y=tot_case_permillion_jun, label=annot_graph)) +
#scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
geom_point(color="black", shape=19) +
  geom_smooth(method="lm", linetype="solid", color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=0, label.y=5.22, size=5)+
  stat_poly_eq(formula=myformula, label.x="right", label.y="top", aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=5)+ 
  geom_text_repel(color="blue") + labs(color="G_Land_area_sqkm")

ggsave("TotalCovidCases_permillion_June10__by_log10pollution_pctpopexp_apr_lm_051420.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_log10pollution_pctpopexp_lm_051420.pdf", width=7, height=7)

#plot by population density
ggplot(master, aes(x=log10(D_Popden2018), y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid", size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=0, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_popden_2016_lm_051420.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_popden_2016_lm_051420.pdf", width=7, height=7)

#plot by rainfall
str(master)
ggplot(master, aes(x=log10(G_Rain_mm_Jan2016), y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid", size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=0.1, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_Jun10__by_log10rain_Jan2016_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_Jun10__by_log10rain_Jan2016_lm_070120.pdf", width=7, height=7)

#plot by diabetes
ggplot(master, aes(x=H_Diabetes2019, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid", size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=12, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="right", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_diabetes_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_diabetes_lm_070120.pdf", width=7, height=7)

#plot by urban
ggplot(master, aes(x=E_Urban_pct2018, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid", size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=10, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_urbanpct_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_urbanpct_lm_070120.pdf", width=7, height=7)

#plot by age15_64
ggplot(master, aes(x=D_Age_15_64y_2018, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=45, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_Age_15_64y_2018_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_Age_15_64y_2018_lm_070120.pdf", width=7, height=7)

#plot by land area
ggplot(master, aes(x=log10(G_Land_area_sqkm), y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=4, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="right", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_landarea_sqkm_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_landarea_sqkm_lm_070120.pdf", width=7, height=7)

#plot by tourism receipts
ggplot(master, aes(x=log10(E_Tourism_receipts_2018+1), y=tot_case_permillion_jun, label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=19) +
  geom_smooth(method="lm", linetype="solid", color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~")), label.x=0.85, label.y=5.5, size=5)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=5)+ 
  geom_text_repel(color="blue") + labs(color="G_Land_area_sqkm")

ggsave("TotalCovidCases_permillion_June10__by_logTourism_receipts_2018_lm_051420.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_logTourism_receipts_2018_lm_051420.pdf", width=7, height=7)

#plot by population over 65yrs - tried with or without log transformation of population percentages
ggplot(master, aes(x=D_Pop_over65_2018, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~")), label.x=0.3, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_nolog_Pop_over65_2018_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_nologPop_over65_2018_lm_070120.pdf", width=7, height=7)

#plot by mobile phone subscriptions
ggplot(master, aes(x=E_Mobile_subsper100_2018, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=19) +
  geom_smooth(method="lm", linetype="solid", color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~")), label.x=30, label.y=1.1, size=5)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=5)+ 
  geom_text_repel(color="blue") + labs(color="G_Land_area_sqkm")

ggsave("TotalCovidCases_permillion_June10__by_Mobile_subsper100_2018_lm_051420.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_Mobile_subsper100_2018_lm_051420.pdf", width=7, height=7)

#plot by Gini index
ggplot(master, aes(x=E_Gini_2015, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=19) +
  geom_smooth(method="lm", linetype="solid", color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~")), label.x=42, label.y=5.5, size=5)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=5)+ 
  geom_text_repel(color="blue") + labs(color="G_Land_area_sqkm")

ggsave("TotalCovidCases_permillion_June10__by_Gini_2015_lm_051420.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_Gini_2015_lm_051420.pdf", width=7, height=7)

#plot by employment in services, male
ggplot(master, aes(x=E_Employ2018_serv_pct_tot_emp, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=20, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_Employ_serv_male_2019_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_Employ_serv_male_2019_lm_070120.pdf", width=7, height=7)

#plot by employment in agriculture, male
ggplot(master, aes(x=E_Employ2018_agri_pct_tot_emp, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~")), label.x=45, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="right", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_Employ_agri_male_2019_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_Employ_agri_male_2019_lm_070120.pdf", width=7, height=7)

#plot by employment in industry, male
ggplot(master, aes(x=E_Employ2018_ind_pct_tot_emp, y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=0, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y="top", 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_Jun10__by_Employ_ind_male_2019_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_Jun10__by_Employ_ind_male_2019_lm_070120.pdf", width=7, height=7)


#plot by DALY_CVD_70yrs
ggplot(master, aes(x=log10(H_DALY_CVD_70yrs), y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid", color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=0, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x=0, label.y=5.6, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_DALY_CVD_70yrs_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_DALY_CVD_70yrs_lm_070120.pdf", width=7, height=7)

#plot by DALY_resp_70yrs 
ggplot(master, aes(x=log10(H_DALY_resp_70yrs), y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid", color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=0, label.y=4.2, size=6)+
  stat_poly_eq(formula=myformula, label.x=0, label.y=5.6, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_DALY_resp_70yrs_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_DALY_resp_70yrs_lm_070120.pdf", width=7, height=7)

#plot by DALY_CVD_all 
ggplot(master, aes(x=log10(H_DALY_CVD_all), y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=1, label.y=4.2, size=6)+
  stat_poly_eq(formula=myformula, label.x=0, label.y=5.6, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_DALY_CVD_all_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_DALY_CVD_all_lm_070120.pdf", width=7, height=7)

#plot by DALY_resp_all
ggplot(master, aes(x=log10(H_DALY_resp_all), y=log10(tot_case_permillion_jun), label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=0, label.y=4.2, size=6)+
  stat_poly_eq(formula=myformula, label.x=0, label.y=5.6, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_DALY_resp_all_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_DALY_resp_all_lm_070120.pdf", width=7, height=7)


#plot covid duration
ggplot(master, aes(x=log10(H_covidduration_10Jun), y=log10(tot_case_permillion_jun),label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=1.6, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y=5.7, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10__by_covid_duration_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10__by_covid_duration_lm_070120.pdf", width=7, height=7)
#-------------------------------------------------------------------------------------------------------
#updated analysis - plot average temps/rainfall from 2010-2016
#plot by average temperatures Jan 2010-2016 (repeat for covid cases per million in apr, may, june)
ggplot(master, aes(x=Temp_Jan_2010_2016_avg, y=log10(tot_case_permillion_jun),label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=-25, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y=5.7, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10_by_avgTemp_Jan_2010_2016_avg_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10_by_avgTemp_Jan_2010_2016_avg_lm_070120.pdf", width=7, height=7)


#plot by average temperatures Feb 2010-2016 (repeat for covid cases per million in apr, may, june)
ggplot(master, aes(x=Temp_Feb_2010_2016_avg, y=log10(tot_case_permillion_jun),label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=-25, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y=5.7, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10_by_avgTemp_Feb_2010_2016_avg_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10_by_avgTemp_Feb_2010_2016_avg_lm_070120.pdf", width=7, height=7)

#plot by average temperatures Mar 2010-2016 (repeat for covid cases per million in apr, may, june)
ggplot(master, aes(x=Temp_Mar_2010_2016_avg, y=log10(tot_case_permillion_jun),label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=-25, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y=5.7, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10_by_avgTemp_Mar_2010_2016_avg_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10_by_avgTemp_Mar_2010_2016_avg_lm_070120.pdf", width=7, height=7)

#plot by average temperatures Apr 2010-2016 (repeat for covid cases per million in apr, may, june)
ggplot(master, aes(x=Temp_Apr_2010_2016_avg, y=log10(tot_case_permillion_jun),label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=-25, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y=5.7, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10_by_avgTemp_Apr_2010_2016_avg_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10_by_avgTemp_Ap_2010_2016_avg_lm_070120.pdf", width=7, height=7)


#plot by log10 average rainfall Jan 2010-2016 (repeat for covid cases per million in apr, may, june)
ggplot(master, aes(x=log10(Rain_Jan_2010_2016_Average), y=log10(tot_case_permillion_jun),label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=-4, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y=5.7, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10_by_log10avgrainfall_Jan_2010_2016_avg_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10_by_log10avgrainfall_Jan_2010_2016_avg_lm_070120.pdf", width=7, height=7)


#plot by average rainfall Feb 2010-2016 (repeat for covid cases per million in apr, may, june)
ggplot(master, aes(x=log10(Rain_Feb_2010_2016_Average), y=log10(tot_case_permillion_jun),label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=-1.8, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y=5.7, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10_by_log10avgrainfall_Feb_2010_2016_avg_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10_by_log10avgrainfall_Feb_2010_2016_avg_lm_070120.pdf", width=7, height=7)

#plot by average rainfall Mar 2010-2016 (repeat for covid cases per million in apr, may, june)
ggplot(master, aes(x=log10(Rain_Mar_2010_2016_Average), y=log10(tot_case_permillion_jun),label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=-3.3, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y=5.7, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10_by_log10avgrainfall_Mar_2010_2016_avg_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10_by_log10avgrainfall_Mar_2010_2016_avg_lm_070120.pdf", width=7, height=7)

#plot by average rainfall Apr 2010-2016 (repeat for covid cases per million in apr, may, june)
ggplot(master, aes(x=log10(Rain_Apr_2010_2016_Average), y=log10(tot_case_permillion_jun),label=annot_graph)) +
  #scale_color_viridis_c(alpha=0.7,option="A", direction = -1)+
  geom_point(color="black", shape=1, size=5) +
  geom_smooth(method="lm", linetype="solid",size=1.5, color="black", se=F) +
  stat_cor(aes(label = paste(stat(rr.label),stat(p.label), sep="~~~~")), label.x=-0.5, label.y=4.1, size=6)+
  stat_poly_eq(formula=myformula, label.x="left", label.y=5.7, 
               aes(label = paste(..eq.label.., sep="~~~")),parse=T, size=6)+ 
  geom_text_repel(color="blue") + 
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(axis.text = element_text(size=14))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color="black", size=1.2))

ggsave("TotalCovidCases_permillion_June10_by_log10avgrainfall_Apr_2010_2016_avg_lm_070120.png", width=7, height=7, units="in", dpi=600)
ggsave("TotalCovidCases_permillion_June10_by_log10avgrainfall_Apr_2010_2016_avg_lm_070120.pdf", width=7, height=7)

#==============================================================================================

