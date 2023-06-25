#Importing necessary libraries
library(pdftools)
library(tidyverse)
library(dplyr)

#DATA PREPARATION

#1. import and merge datasets
gi<-read.csv("C:/Users/Isha/Documents/GIPE/SEM 4/ggi.csv")
di<-read.csv("C:/Users/Isha/Documents/GIPE/SEM 4/di.csv")
gidi=inner_join(gi,di,by='state')

#2. per village number of total maps, good condition maps and digitized maps
pv_map=gidi$map_total/gidi$total_villages
pv_goodmap=gidi$map_goodcondition/gidi$total_villages
pv_digitizedmap=gidi$map_digitized/gidi$total_villages

#3.per village allocation under CLR and survey
pv_clrallocation=gidi$CLR_allocation/gidi$total_villages
pv_surveyallocation=gidi$survey_allocation/gidi$total_villages

#4.share of villages with completed land digitization
share_clrcompleted=gidi$CLR_completed/gidi$total_villages

#DATA ANALYSIS

#1.1]histograms of the variables pv_map, pv_goodmap and pv_digitizedmap.
vignette("dplyr")
hist(pv_map, breaks=36, col='cyan4', main='Number Maps per Village', xlab='Number Maps per Village')
hist(pv_goodmap, breaks=36, col='cyan4', main='Good Condition Maps per Village', xlab='Good Condition Maps per Village')
hist(pv_digitizedmap, breaks=36, col='cyan4', main='Digitized Maps per Village', xlab='Digitized Maps per Village')

#1.2]scatter plot with pv_map in the y-axis and compsite_points in the x-axis
library(ggplot2)
ggplot(gidi, aes(x=composite_points, y=pv_map))+ geom_point()+geom_smooth(method="lm")+ggtitle('Average number of maps to composite points')

#1.3]run regression
m1<-lm(pv_map~composite_points+total_villages,data=gidi)
summary(m1)
plot(pv_map~composite_points+total_villages,data=gidi, main='Number of maps per village VS Composite_points')
abline(m1)

#1.6] regress for pv_goodmap and pv_digitizedmap
m2=lm(pv_goodmap~composite_points+total_villages,data=gidi)
summary(m2)
plot(pv_goodmap~composite_points+total_villages,data=gidi,main='Good condition maps per village VS Composite_points')
abline(m2)
m3=lm(pv_digitizedmap~composite_points+total_villages,data=gidi)
summary(m3)
plot(pv_digitizedmap~composite_points+total_villages,data=gidi)
abline(m3)

#1.7]regress share_clrcompleted
m4=lm(share_clrcompleted~composite_points+total_villages,data=gidi)
summary(m4)
plot(share_clrcompleted~composite_points+total_villages,data=gidi)
abline(m4)

#1.9] which of the two component scores are more important?
m5<-lm(share_clrcompleted~infra_points+econ_points+total_villages,data=gidi)
summary(m5)
plot(share_clrcompleted~infra_points+econ_points+total_villages,data=gidi)
abline(m5)

#1.11] pv_clrallocation and pv_surveyallocation as dependent variables
m6<-lm(pv_clrallocation~infra_points+econ_points+total_villages,data=gidi)
summary(m6)
plot(pv_clrallocation~infra_points+econ_points+total_villages,data=gidi)
abline(m6)
m7<-lm(pv_surveyallocation~infra_points+econ_points+total_villages,data=gidi)
summary(m7)
plot(pv_surveyallocation~infra_points+econ_points+total_villages,data=gidi)
abline(m7)
