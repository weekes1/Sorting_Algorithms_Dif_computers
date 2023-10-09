#Bert
library(mosaic)
library(tidyverse)
library(readr)
library(pgirmess)
SortingData <- read_csv("Data Analysis/SortingData.csv")
View(SortingData)


data(SortingData)


for (i in 1:45){

  if( SortingData$Program[i]=="Bubble Sort"){
    SortingData$Program[i]="B-S"
  }


if( SortingData$Program[i]=="Quick Sort"){
  SortingData$Program[i]="Q-S"
}
  if( SortingData$Program[i]=="Selection Sort"){
    SortingData$Program[i]="S-S"
  }
  
}  

SortingData
boxplot(Time~Program, data=SortingData)
boxplot(Time~Computer, data=SortingData,col='thistle1', xlab="Computer", ylab="Time (in sec)", main="Sorting Algorithm Speed by Computer")
boxplot(Time~Program+Computer, data=SortingData,col='thistle1', xlab="Sorting Algorithm", ylab="Time (in sec)", main="Sorting Algorithm Speed by Computer and program")
favstat_algos<-favstats(Time~Program+Computer, data=SortingData)



write_csv(favstat_algos,"C:\\Users\\weeke\\OneDrive\\Documents\\Data Analysis\\favstats_algos.csv")


interaction.plot(SortingData$Program, SortingData$Computer, SortingData$Time, col='thistle1', xlab="Computer", ylab="Time (in sec)", main="Sorting Algorithm Speed by Computer")
interaction.plot(SortingData$Computer,SortingData$Program, SortingData$Time)



fullmod = aov(Time~Program*Computer, data=SortingData)
nullmod = aov(Time~1, data=SortingData)
nested_f<-anova(nullmod,fullmod)
nested_f
write_csv(nested_f,"C:\\Users\\weeke\\OneDrive\\Documents\\Data Analysis\\nested_f_algos.csv")

plot(fullmod, 1:2)
levene_algos<-leveneTest(SortingData$Time, factor(SortingData$Program):factor(SortingData$Computer))
levene_algos
write_csv(levene_algos,"C:\\Users\\weeke\\OneDrive\\Documents\\Data Analysis\\levene_algos.csv")

int.trt <- with(SortingData, interaction(Program, Computer))

Krusk_mod<-kruskal.test(Time~int.trt, data=SortingData)
Krusk_mod

library(pgirmess)
library(agricolae)
kruskal(SortingData$Time,int.trt,group = TRUE, p.adj = "bonferroni")$groups
LSDKRU<-kruskal(SortingData$Time,int.trt,group = TRUE)$groups
LSDKRU
LSDKRU<-kruskal(SortingData$Time,int.trt,group = TRUE)
plot(LSDKRU)


ANOVA_algos<-anova(fullmod)
ANOVA_algos
write_csv(ANOVA_algos,"C:\\Users\\weeke\\OneDrive\\Documents\\Data Analysis\\ANOVA_algos.csv")


library(foreign)
int.trt <- with(SortingData, interaction(Program, Computer))
amod <- aov(Time ~ int.trt, data=SortingData)
library(agricolae)
HSD<-HSD.test(amod, "int.trt", group=TRUE)$groups
HSD<-HSD.test(amod, "int.trt", group=TRUE)
plot(HSD)
HSD


LSD<-LSD.test(amod, "int.trt", group=TRUE)$groups
LSD<-LSD.test(amod, "int.trt", group=TRUE)
plot(LSD)
LSD

Bon<-LSD.test(amod, "int.trt", group=TRUE, p.adj="bonferroni")$groups
Bon<-LSD.test(amod, "int.trt", group=TRUE, p.adj="bonferroni")
plot(Bon)
Bon
