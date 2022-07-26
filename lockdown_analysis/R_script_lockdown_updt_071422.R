getwd()
setwd("E:\\Ghosh Dropbox\\SUJOY GHOSH\\covid_analysis\\lockdown_related\\lockdown_June30")
dir()

install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggthemes)

data = read.delim("input_lockdown_jun30_070520.txt", sep='\t', header=T)
dim(data)
str(data)

#convert date column to class Date
data$Date2_reported = as.Date(data$Date2_reported, format = "%d/%m/%y")
class(data$Date2_reported)

#rename Date2reported column for ease of plotting
names(data)[10] = "Date"
str(data)

data$Date = as.Date(data$Date, format = "%d/%m/%y")

#lockdown related plots
#change the y-axis in aes as desired 
Afghanistan= c(data$Date[34],data$Date[88])
ggplot(data=subset(data, location=="Afghanistan"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Afghanistan, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Afghanistan")+
scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Afghanistan_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Afghanistan_071422.pdf", height=6, width=6)

Albania = c(data$Date[133],data$Date[213])
ggplot(data=subset(data, location=="Albania"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Albania, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Albania")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Albania_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Albania_071422.pdf", height=6, width=6)


Algeria = c(data$Date[269],data$Date[346])
ggplot(data=subset(data, location=="Algeria"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Algeria, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Algeria")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Algeria_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Algeria_071422.pdf", height=6, width=6)

Argentina = c(data$Date[507],data$Date[587])
ggplot(data=subset(data, location=="Argentina"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Argentina, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Argentina")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Argentina_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Argentina_071422.pdf", height=6, width=6)

Armenia = c(data$Date[634],data$Date[675])
ggplot(data=subset(data, location=="Armenia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Armenia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Armenia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Armenia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Armenia_071422.pdf", height=6, width=6)

Australia = c(data$Date[897],data$Date[950])
ggplot(data=subset(data, location=="Australia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Australia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Australia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Australia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Australia_071422.pdf", height=6, width=6)

Austria = c(data$Date[1017],data$Date[1046])
ggplot(data=subset(data, location=="Austria"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Austria, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Austria")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Austria_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Austria_071422.pdf", height=6, width=6)

Azerbaijan = c(data$Date[1155],data$Date[1189])
ggplot(data=subset(data, location=="Azerbaijan"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Azerbaijan, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Azerbaijan")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Azerbaijan_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Azerbaijan_071422.pdf", height=6, width=6)

Bangladesh = c(data$Date[1499],data$Date[1564])
ggplot(data=subset(data, location=="Bangladesh"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Bangladesh, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Bangladesh")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Bangladesh_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Bangladesh_071422.pdf", height=6, width=6)

Belarus =c(data$Date[1635],data$Date[1635])
ggplot(data=subset(data, location=="Belarus"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Belarus, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Belarus")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Belarus_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Belarus_071422.pdf", height=6, width=6)

Belgium = c(data$Date[1763],data$Date[1817])
ggplot(data=subset(data, location=="Belgium"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Belgium, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Belgium")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Belgium_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Belgium_071422.pdf", height=6, width=6)

Bermuda = c(data$Date[1885],data$Date[1913])
ggplot(data=subset(data, location=="Bermuda"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Bermuda, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Bermuda")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Bermuda_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Bermuda_071422.pdf", height=6, width=6)

Bolivia = c(data$Date[19372],data$Date[19442])
ggplot(data=subset(data, location=="Bolivia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Bolivia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Bolivia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Bolivia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Bolivia_071422.pdf", height=6, width=6)

Brazil = c(data$Date[2111],data$Date[2188])
ggplot(data=subset(data, location=="Brazil"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept=Brazil, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Brazil")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Brazil_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Brazil_071422.pdf", height=6, width=6)

Botswana = c(data$Date[19474],data$Date[19509])
ggplot(data=subset(data, location=="Botswana"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Botswana, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Botswana")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Botswana_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Botswana_071422.pdf", height=6, width=6)

Bulgaria = c(data$Date[2222],data$Date[2283])
ggplot(data=subset(data, location=="Bulgaria"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Bulgaria, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Bulgaria")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Bulgaria_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Bulgaria_071422.pdf", height=6, width=6)

Cameroon = c(data$Date[2611],data$Date[2687])
ggplot(data=subset(data, location=="Cameroon"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Cameroon, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Cameroon")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Cameroon_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Cameroon_071422.pdf", height=6, width=6)

Canada = c(data$Date[2764],data$Date[2816])
ggplot(data=subset(data, location=="Canada"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Canada, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Canada")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Canada_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Canada_071422.pdf", height=6, width=6)

Chile = c(data$Date[3213],data$Date[3213])
ggplot(data=subset(data, location=="Chile"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Chile, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Chile")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Chile_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Chile_071422.pdf", height=6, width=6)

Chad = c(data$Date[3143],data$Date[3143])
ggplot(data=subset(data, location=="Chad"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Chad, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Chad")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Chad_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Chad_071422.pdf", height=6, width=6)

China = c(data$Date[3330],data$Date[3391])
ggplot(data=subset(data, location=="China"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = China, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_China")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_China_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_China_071422.pdf", height=6, width=6)

Colombia = c(data$Date[3507],data$Date[3569])
ggplot(data=subset(data, location=="Colombia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Colombia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Colombia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Colombia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Colombia_071422.pdf", height=6, width=6)

Congo = c(data$Date[3683],data$Date[3703])
ggplot(data=subset(data, location=="Congo"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Congo, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Congo")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Congo_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Congo_071422.pdf", height=6, width=6)

Costa_Rica = c(data$Date[16452],data$Date[16491])
ggplot(data=subset(data, location=="Costa_Rica"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Costa_Rica, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Costa_Rica")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Costa_Rica_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Costa_Rica_071422.pdf", height=6, width=6)

Croatia = c(data$Date[3797],data$Date[3851])
ggplot(data=subset(data, location=="Croatia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Croatia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Croatia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Croatia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Croatia_071422.pdf", height=6, width=6)

Cuba = c(data$Date[3912],data$Date[3999])
ggplot(data=subset(data, location=="Cuba"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Cuba, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Cuba")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Cuba_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Cuba_071422.pdf", height=6, width=6)

Czech_Republic = c(data$Date[16567],data$Date[16599])
ggplot(data=subset(data, location=="Czech_Republic"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Czech_Republic, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Czech_Republic")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Czech_Republic_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Czech_Republic_071422.pdf", height=6, width=6)

Denmark = c(data$Date[4141],data$Date[4199])
ggplot(data=subset(data, location=="Denmark"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Denmark, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Denmark")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Denmark_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Denmark_071422.pdf", height=6, width=6)


Dominican_Republic = c(data$Date[16701],data$Date[16753])
ggplot(data=subset(data, location=="Dominican_Republic"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Dominican_Republic, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Dominican_Republic")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Dominican_Republic_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Dominican_Republic_071422.pdf", height=6, width=6)

Ecuador = c(data$Date[4380],data$Date[4451])
ggplot(data=subset(data, location=="Ecuador"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Ecuador, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Ecuador")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Ecuador_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Ecuador_071422.pdf", height=6, width=6)

Egypt = c(data$Date[4518],data$Date[4613])
ggplot(data=subset(data, location=="Egypt"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Egypt, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Egypt")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Egypt_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Egypt_071422.pdf", height=6, width=6)

Eritrea = c(data$Date[16807],data$Date[16828])
ggplot(data=subset(data, location=="Eritrea"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Eritrea, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Eritrea")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Eritrea_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Eritrea_071422.pdf", height=6, width=6)

Estonia = c(data$Date[4844],data$Date[4911])
ggplot(data=subset(data, location=="Estonia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Estonia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Estonia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Estonia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Estonia_071422.pdf", height=6, width=6)

Finland = c(data$Date[5122],data$Date[5188])
ggplot(data=subset(data, location=="Finland"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Finland, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Finland")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Finland_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Finland_071422.pdf", height=6, width=6)

France = c(data$Date[5271],data$Date[5326])
ggplot(data=subset(data, location=="France"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = France, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_France")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_France_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_France_071422.pdf", height=6, width=6)

Georgia = c(data$Date[5520],data$Date[5582])
ggplot(data=subset(data, location=="Georgia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Georgia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Georgia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Georgia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Georgia_071422.pdf", height=6, width=6)

Germany = c(data$Date[5661],data$Date[5705])
ggplot(data=subset(data, location=="Germany"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Germany, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Germany")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Germany_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Germany_071422.pdf", height=6, width=6)

Ghana = c(data$Date[5783],data$Date[5804])
ggplot(data=subset(data, location=="Ghana"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Ghana, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Ghana")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Ghana_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Ghana_071422.pdf", height=6, width=6)

Gibraltar = c(data$Date[5895],data$Date[5936])
ggplot(data=subset(data, location=="Gibraltar"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Gibraltar, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Gibraltar")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Gibraltar_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Gibraltar_071422.pdf", height=6, width=6)

Greece = c(data$Date[6022],data$Date[6092])
ggplot(data=subset(data, location=="Greece"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Greece, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Greece")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Greece_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Greece_071422.pdf", height=6, width=6)

Honduras = c(data$Date[6877],data$Date[6914])
ggplot(data=subset(data, location=="Honduras"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Honduras, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Honduras")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Honduras_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Honduras_071422.pdf", height=6, width=6)

Hungary = c(data$Date[7004],data$Date[7041])
ggplot(data=subset(data, location=="Hungary"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Hungary, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Hungary")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Hungary_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Hungary_071422.pdf", height=6, width=6)

India = c(data$Date[7276],data$Date[7351])
ggplot(data=subset(data, location=="India"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = India, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_India")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_India_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_India_071422.pdf", height=6, width=6)

Indonesia = c(data$Date[7418],data$Date[7472])
ggplot(data=subset(data, location=="Indonesia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Indonesia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Indonesia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Indonesia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Indonesia_071422.pdf", height=6, width=6)

Iran = c(data$Date[17049],data$Date[17078])
ggplot(data=subset(data, location=="Iran"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Iran, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Iran")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Iran_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Iran_071422.pdf", height=6, width=6)

Iraq = c(data$Date[7524],data$Date[7573])
ggplot(data=subset(data, location=="Iraq"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Iraq, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Iraq")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Iraq_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Iraq_071422.pdf", height=6, width=6)

Ireland = c(data$Date[7649],data$Date[7701])
ggplot(data=subset(data, location=="Ireland"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Ireland, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Ireland")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Ireland_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Ireland_071422.pdf", height=6, width=6)

Israel = c(data$Date[7889],data$Date[7921])
ggplot(data=subset(data, location=="Israel"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Israel, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Israel")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Israel_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Israel_071422.pdf", height=6, width=6)

Italy = c(data$Date[8019],data$Date[8075])
ggplot(data=subset(data, location=="Italy"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Italy, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Italy")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Italy_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Italy_071422.pdf", height=6, width=6)

Jamaica = c(data$Date[8169],data$Date[8216])
ggplot(data=subset(data, location=="Jamaica"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Jamaica, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Jamaica")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Jamaica_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Jamaica_071422.pdf", height=6, width=6)

Japan = c(data$Date[8330],data$Date[8379])
ggplot(data=subset(data, location=="Japan"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Japan, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Japan")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Japan_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Japan_071422.pdf", height=6, width=6)

Jordan = c(data$Date[8542],data$Date[8622])
ggplot(data=subset(data, location=="Jordan"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Jordan, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Jordan")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Jordan_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Jordan_071422.pdf", height=6, width=6)

Kazakhstan = c(data$Date[8652],data$Date[8705])
ggplot(data=subset(data, location=="Kazakhstan"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Kazakhstan, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Kazakhstan")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Kazakhstan_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Kazakhstan_071422.pdf", height=6, width=6)

Kosovo = c(data$Date[17166],data$Date[17214])
ggplot(data=subset(data, location=="Kosovo"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Kosovo, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Kosovo")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Kosovo_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Kosovo_071422.pdf", height=6, width=6)

Kuwait = c(data$Date[8881],data$Date[8961])
ggplot(data=subset(data, location=="Kuwait"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Kuwait, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Kuwait")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Kuwait_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Kuwait_071422.pdf", height=6, width=6)

Kyrgyzstan = c(data$Date[9000],data$Date[9061])
ggplot(data=subset(data, location=="Kyrgyzstan"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Kyrgyzstan, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Kyrgyzstan")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Kyrgyzstan_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Kyrgyzstan_071422.pdf", height=6, width=6)

Latvia = c(data$Date[9109],data$Date[9197])
ggplot(data=subset(data, location=="Latvia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Latvia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Latvia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Latvia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Latvia_071422.pdf", height=6, width=6)

Lebanon = c(data$Date[9249],data$Date[9306])
ggplot(data=subset(data, location=="Lebanon"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Lebanon, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Lebanon")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Lebanon_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Lebanon_071422.pdf", height=6, width=6)

Liberia = c(data$Date[9356],data$Date[9375])
ggplot(data=subset(data, location=="Liberia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Liberia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Liberia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Liberia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Liberia_071422.pdf", height=6, width=6)

Lithuania = c(data$Date[9571],data$Date[9643])
ggplot(data=subset(data, location=="Lithuania"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Lithuania, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Lithuania")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Lithuania_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Lithuania_071422.pdf", height=6, width=6)

Luxembourg = c(data$Date[9695],data$Date[9728])
ggplot(data=subset(data, location=="Luxembourg"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Luxembourg, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Luxembourg")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Luxembourg_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Luxembourg_071422.pdf", height=6, width=6)

Madagascar = c(data$Date[9802],data$Date[9830])
ggplot(data=subset(data, location=="Madagascar"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Madagascar, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Madagascar")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Madagascar_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Madagascar_071422.pdf", height=6, width=6)

Malaysia = c(data$Date[10044],data$Date[10128])
ggplot(data=subset(data, location=="Malaysia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Malaysia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Malaysia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Malaysia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Malaysia_071422.pdf", height=6, width=6)

Maldives = c(data$Date[10188],data$Date[10249])
ggplot(data=subset(data, location=="Maldives"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Maldives, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Maldives")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Maldives_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Maldives_071422.pdf", height=6, width=6)

Mauritania = c(data$Date[10481],data$Date[10532])
ggplot(data=subset(data, location=="Mauritania"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Mauritania, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Mauritania")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Mauritania_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Mauritania_071422.pdf", height=6, width=6)

Mauritius = c(data$Date[10593],data$Date[10645])
ggplot(data=subset(data, location=="Mauritius"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Mauritius, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Mauritius")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Mauritius_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Mauritius_071422.pdf", height=6, width=6)

Mexico = c(data$Date[10714],data$Date[10754])
ggplot(data=subset(data, location=="Mexico"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Mexico, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Mexico")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Mexico_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Mexico_071422.pdf", height=6, width=6)

Montenegro = c(data$Date[10936],data$Date[10964])
ggplot(data=subset(data, location=="Montenegro"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Montenegro, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Montenegro")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Montenegro_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Montenegro_071422.pdf", height=6, width=6)

Morocco = c(data$Date[11052],data$Date[11134])
ggplot(data=subset(data, location=="Morocco"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Morocco, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Morocco")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Morocco_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Morocco_071422.pdf", height=6, width=6)

Mozambique = c(data$Date[11165],data$Date[11255])
ggplot(data=subset(data, location=="Mozambique"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Mozambique, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Mozambique")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Mozambique_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Mozambique_071422.pdf", height=6, width=6)

Nepal = c(data$Date[11416],data$Date[11416])
ggplot(data=subset(data, location=="Nepal"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Nepal, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Nepal")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Nepal_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Nepal_071422.pdf", height=6, width=6)

Netherlands = c(data$Date[11533],data$Date[11610])
ggplot(data=subset(data, location=="Netherlands"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Netherlands, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Netherlands")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Netherlands_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Netherlands_071422.pdf", height=6, width=6)

New_Zealand = c(data$Date[17285],data$Date[17341])
ggplot(data=subset(data, location=="New_Zealand"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = New_Zealand, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_New_Zealand")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_New_Zealand_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_New_Zealand_071422.pdf", height=6, width=6)

Nigeria = c(data$Date[11879],data$Date[11921])
ggplot(data=subset(data, location=="Nigeria"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Nigeria, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Nigeria")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Nigeria_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Nigeria_071422.pdf", height=6, width=6)

Norway = c(data$Date[11987],data$Date[12033])
ggplot(data=subset(data, location=="Norway"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Norway, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Norway")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Norway_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Norway_071422.pdf", height=6, width=6)

Oman = c(data$Date[12144],data$Date[12193])
ggplot(data=subset(data, location=="Oman"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Oman, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Oman")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Oman_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Oman_071422.pdf", height=6, width=6)

Pakistan = c(data$Date[12253],data$Date[12300])
ggplot(data=subset(data, location=="Pakistan"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Pakistan, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Pakistan")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Pakistan_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Pakistan_071422.pdf", height=6, width=6)

Panama = c(data$Date[12367],data$Date[12435])
ggplot(data=subset(data, location=="Panama"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Panama, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Panama")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Panama_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Panama_071422.pdf", height=6, width=6)

Paraguay = c(data$Date[12477],data$Date[12521])
ggplot(data=subset(data, location=="Paraguay"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Paraguay, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Paraguay")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Paraguay_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Paraguay_071422.pdf", height=6, width=6)

Peru = c(data$Date[12589],data$Date[12589])
ggplot(data=subset(data, location=="Peru"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Peru, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Peru")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Peru_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Peru_071422.pdf", height=6, width=6)

Philippines = c(data$Date[12741],data$Date[12818])
ggplot(data=subset(data, location=="Philippines"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Philippines, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Philippines")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Philippines_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Philippines_071422.pdf", height=6, width=6)

Poland = c(data$Date[12857],data$Date[12929])
ggplot(data=subset(data, location=="Poland"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Poland, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Poland")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Poland_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Poland_071422.pdf", height=6, width=6)

Puerto_Rico = c(data$Date[17383],data$Date[17476])
ggplot(data=subset(data, location=="Puerto_Rico"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Puerto_Rico, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Puerto_Rico")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Puerto_Rico_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Puerto_Rico_071422.pdf", height=6, width=6)

Portugal = c(data$Date[12984],data$Date[13030])
ggplot(data=subset(data, location=="Portugal"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Portugal, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Portugal")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Portugal_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Portugal_071422.pdf", height=6, width=6)

Qatar = c(data$Date[13099],data$Date[13195])
ggplot(data=subset(data, location=="Qatar"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Qatar, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Qatar")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Qatar_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Qatar_071422.pdf", height=6, width=6)

Romania = c(data$Date[13239],data$Date[13287])
ggplot(data=subset(data, location=="Romania"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Romania, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Romania")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Romania_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Romania_071422.pdf", height=6, width=6)

Rwanda = c(data$Date[13343],data$Date[13387])
ggplot(data=subset(data, location=="Rwanda"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Rwanda, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Rwanda")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Rwanda_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Rwanda_071422.pdf", height=6, width=6)

Saudi_Arabia = c(data$Date[17831],data$Date[17918])
ggplot(data=subset(data, location=="Saudi_Arabia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Saudi_Arabia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Saudi_Arabia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Saudi_Arabia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Saudi_Arabia_071422.pdf", height=6, width=6)

Senegal = c(data$Date[13683],data$Date[13775])
ggplot(data=subset(data, location=="Senegal"), aes(x=Date, y=New_cases))+
  geom_point(shape=19, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Senegal, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Senegal")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Senegal_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Senegal_071422.pdf", height=6, width=6)

Serbia = c(data$Date[13785],data$Date[13837])
ggplot(data=subset(data, location=="Serbia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Serbia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Serbia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Serbia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Serbia_071422.pdf", height=6, width=6)

Sierra_Leone = c(data$Date[17931],data$Date[17993])
ggplot(data=subset(data, location=="Sierra_Leone"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Sierra_Leone, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Sierra_Leone")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Sierra_Leone_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Sierra_Leone_071422.pdf", height=6, width=6)

Singapore = c(data$Date[13968],data$Date[14024])
ggplot(data=subset(data, location=="Singapore"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Singapore, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Singapore")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Singapore_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Singapore_071422.pdf", height=6, width=6)

Slovakia = c(data$Date[14063],data$Date[14129])
ggplot(data=subset(data, location=="Slovakia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Slovakia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Slovakia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Slovakia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Slovakia_071422.pdf", height=6, width=6)

Slovenia = c(data$Date[14180],data$Date[14217])
ggplot(data=subset(data, location=="Slovenia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Slovenia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Slovenia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Slovenia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Slovenia_071422.pdf", height=6, width=6)

South_Africa = c(data$Date[18040],data$Date[18076])
ggplot(data=subset(data, location=="South_Africa"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = South_Africa, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_South_Africa")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_South_Africa_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_South_Africa_071422.pdf", height=6, width=6)

Spain = c(data$Date[14525],data$Date[14583])
ggplot(data=subset(data, location=="Spain"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Spain, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Spain")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Spain_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Spain_071422.pdf", height=6, width=6)

Sri_Lanka = c(data$Date[18190],data$Date[18241])
ggplot(data=subset(data, location=="Sri_Lanka"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Sri_Lanka, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Sri_Lanka")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Sri_Lanka_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Sri_Lanka_071422.pdf", height=6, width=6)


Sudan = c(data$Date[14669],data$Date[14669])
ggplot(data=subset(data, location=="Sudan"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Sudan, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Sudan")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Sudan_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Sudan_071422.pdf", height=6, width=6)

Switzerland = c(data$Date[15024],data$Date[15079])
ggplot(data=subset(data, location=="Switzerland"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Switzerland, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Switzerland")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Switzerland_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Switzerland_071422.pdf", height=6, width=6)

Thailand = c(data$Date[15263],data$Date[15302])
ggplot(data=subset(data, location=="Thailand"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Thailand, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Thailand")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Thailand_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Thailand_071422.pdf", height=6, width=6)

Trinidad_and_Tobago = c(data$Date[18449],data$Date[18505])
ggplot(data=subset(data, location=="Trinidad_and_Tobago"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Trinidad_and_Tobago, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Trinidad_and_Tobago")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Trinidad_and_Tobago_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Trinidad_and_Tobago_071422.pdf", height=6, width=6)

Tunisia = c(data$Date[15497],data$Date[15540])
ggplot(data=subset(data, location=="Tunisia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Tunisia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Tunisia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Tunisia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Tunisia_071422.pdf", height=6, width=6)

Turkey = c(data$Date[15629],data$Date[15659])
ggplot(data=subset(data, location=="Turkey"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Turkey, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Turkey")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Turkey_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Turkey_071422.pdf", height=6, width=6)

Ukraine = c(data$Date[15826],data$Date[15881])
ggplot(data=subset(data, location=="Ukraine"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Ukraine, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Ukraine")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Ukraine_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Ukraine_071422.pdf", height=6, width=6)

Uganda = c(data$Date[15719],data$Date[15783])
ggplot(data=subset(data, location=="Uganda"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Uganda, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Uganda")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Uganda_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Uganda_071422.pdf", height=6, width=6)

United_Arab_Emirates = c(data$Date[18608],data$Date[18641])
ggplot(data=subset(data, location=="United_Arab_Emirates"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = United_Arab_Emirates, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_United_Arab_Emirates")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_United_Arab_Emirates_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_United_Arab_Emirates_071422.pdf", height=6, width=6)

United_Kingdom = c(data$Date[18346],data$Date[18429])
ggplot(data=subset(data, location=="United_Kingdom"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = United_Kingdom, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_United_Kingdom")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_United_Kingdom_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_United_Kingdom_071422.pdf", height=6, width=6)

United_States = c(data$Date[18879],data$Date[18920])
ggplot(data=subset(data, location=="United_States"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = United_States, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_United_States")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_United_States_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_United_States_071422.pdf", height=6, width=6)

Uzbekistan = c(data$Date[16047],data$Date[16117])
ggplot(data=subset(data, location=="Uzbekistan"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Uzbekistan, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Uzbekistan")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Uzbekistan_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Uzbekistan_071422.pdf", height=6, width=6)

Venezuela = c(data$Date[18981],data$Date[18981])
ggplot(data=subset(data, location=="Venezuela"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Venezuela, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Venezuela")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Venezuela_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Venezuela_071422.pdf", height=6, width=6)

Zimbabwe = c(data$Date[16344],data$Date[16419])
ggplot(data=subset(data, location=="Zimbabwe"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Zimbabwe, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Zimbabwe")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Zimbabwe_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Zimbabwe_071422.pdf", height=6, width=6)

Vietnam = c(data$Date[19154],data$Date[19175])
ggplot(data=subset(data, location=="Vietnam"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Vietnam, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Vietnam")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Vietnam_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Vietnam_071422.pdf", height=6, width=6)

Russia = c(data$Date[17714],data$Date[17756])
ggplot(data=subset(data, location=="Russia"), aes(x=Date, y=New_cases))+
  geom_point(shape=1, size=3, color="darkorange")+
  geom_line(color="gray50", linetype = "solid", size=0.25)+
  geom_vline(xintercept = Russia, size=1, color="blue")+
  theme(axis.text = element_text(size=14, angle=90))+
  theme(axis.title = element_text(size=18, face="bold"))+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(color = "grey50"))+
  ggtitle ("lockdown_New_cases_Russia")+
  scale_x_date(date_breaks = "15 day", date_labels = "%m/%d/%y")
ggsave("lockdown_New_cases_Russia_071422.png", height=6, width=6, units="in", dpi=600)
ggsave("lockdown_New_cases_Russia_071422.pdf", height=6, width=6)



