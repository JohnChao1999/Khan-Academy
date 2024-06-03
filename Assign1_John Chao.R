library(dplyr)
library("readxl")
library(ggplot2)
library(tidyr)
library(reshape2)
library(DataExplorer)
Num <- read_excel("C:\\Users\\John\\Downloads\\M&E Insight Analyst Assignment.xlsx",
                  sheet = "Number of students",skip =1)
Time <- read_excel("C:\\Users\\John\\Downloads\\M&E Insight Analyst Assignment.xlsx",
                   sheet="Learning Time",skip = 1)
plot_missing(Num)
plot_missing(Time)
colnames(Num)[c(1:3)] <- c("District Name",	"School Name"	,"Total Number of Students Registered
	")
colnames(Time)[c(1:3)] <- c("District Name",	"School Name"	,"Total Number of Students Registered
	")
avg_total_stu <- 
  Num %>%
  group_by(`District Name`) %>%
  summarise_at(vars(c("Total Number of Students Registered\n\t")), list(name = mean))%>%
  `colnames<-`(c("District Name","Avg Num of Students Registered"))
avg_total_stu <- avg_total_stu[c(1,8:15,2:7),]
avg_total_stu$`District Name` <- factor(avg_total_stu$`District Name`, 
                    levels = c("District 1", "District 2", "District 3",
                               "District 4", "District 5", "District 6",
                               "District 7", "District 8", "District 9",
                               "District 10", "District 11", "District 12",
                               "District 13", "District 14", "District 15"))
total.mean = mean(avg_total_stu$`Avg Num of Students Registered`)
ggplot(data=avg_total_stu, aes(x=`District Name`, y=`Avg Num of Students Registered`)) +
  geom_bar(stat="identity")+scale_x_discrete(guide = guide_axis(n.dodge=3))+
  geom_abline(slope=0, intercept=total.mean,  col = "red",lty=2,lwd=1.5)+
  ggtitle("Avg Number of Students in Each District")
  

aNum <- 
  Num %>%
  group_by(`District Name`) %>%
  summarise_at(vars(c("M1","M2","M3","M4","M5","M6",
                      "M7","M8","M9","M10","M11","M12")), list(Mean = mean,Std=sd))
aNum_mean <- aNum[,c(1:13)]
aNum_sd <- aNum[,c(1,14:25)]

aNum_mean_long <- melt(aNum_mean, id.vars=c("District Name"))
Month <- as.factor(rep(c(1:12),each=15))
aNum_mean_long <- cbind(aNum_mean_long,Month)
colnames(aNum_mean_long)[c(3)] <- c("Mean")
aNum_std_long <- melt(aNum_sd, id.vars=c("District Name"))
aNum_std_long <- cbind(aNum_std_long,Month)
colnames(aNum_std_long)[c(3)] <- c("Std")
aNum_total <- merge(aNum_mean_long,aNum_std_long,by=c("District Name","Month"))
aNum_total$`District Name` <- factor(aNum_total$`District Name`, 
                                        levels = c("District 1", "District 2", "District 3",
                                                   "District 4", "District 5", "District 6",
                                                   "District 7", "District 8", "District 9",
                                                   "District 10", "District 11", "District 12",
                                                   "District 13", "District 14", "District 15"))
ggplot(aNum_total, aes(x=Month, y=Mean, group=`District Name`, color=`District Name`)) + 
  geom_line() +
  geom_point()+ylab("Avg Num of Active Students")+
  ggtitle("Line Plot of Avg Num of Active Students")

aTime <- 
  Time %>%
  group_by(`District Name`) %>%
  summarise_at(vars(c("M1","M2","M3","M4","M5","M6",
                      "M7","M8","M9","M10","M11","M12")), list(Mean = mean,Std=sd))
aTime_mean <- aTime[,c(1:13)]
aTime_sd <- aTime[,c(1,14:25)]

aTime_mean_long <- melt(aTime_mean, id.vars=c("District Name"))
aTime_mean_long <- cbind(aTime_mean_long,Month)
colnames(aTime_mean_long)[c(3)] <- c("Mean")
aTime_std_long <- melt(aTime_sd, id.vars=c("District Name"))
aTime_std_long <- cbind(aTime_std_long,Month)
colnames(aTime_std_long)[c(3)] <- c("Std")
aTime_total <- merge(aTime_mean_long,aTime_std_long,by=c("District Name","Month"))
aTime_total$`District Name` <- factor(aTime_total$`District Name`, 
                                     levels = c("District 1", "District 2", "District 3",
                                                "District 4", "District 5", "District 6",
                                                "District 7", "District 8", "District 9",
                                                "District 10", "District 11", "District 12",
                                                "District 13", "District 14", "District 15"))
aTime_total$Month <- as.factor(aTime_total$Month)
ggplot(aTime_total, aes(x=Month, y=Mean, group=`District Name`, color=`District Name`)) + 
  geom_line() +
  geom_point()+ylab("Avg Learning Time")+
  ggtitle("Line Plot of Avg Learning Time")


gen.total.num <- Num %>%
  summarise_at(vars(c("M1","M2","M3","M4","M5","M6",
                      "M7","M8","M9","M10","M11","M12")), list(Mean = mean))
gen.total.num.long <- melt(gen.total.num)
colnames(gen.total.num.long)[c(2)] <- c("Mean Num of Stu")

gen.total.time <- Time %>%
  summarise_at(vars(c("M1","M2","M3","M4","M5","M6",
                      "M7","M8","M9","M10","M11","M12")), list(Mean = mean))
gen.total.time.long <- melt(gen.total.time)
colnames(gen.total.time.long)[c(2)] <- c("Mean Learning Time")

gen.total <- merge(gen.total.num.long,gen.total.time.long,by=c("variable"))
gen.total <- gen.total[c(1,5:12,2:4),]
Month <- as.factor(c(1:12))
gen.total <- cbind(gen.total,Month)
gen.total[,c(2,3)] <- round(gen.total[,c(2,3)],2)

ggplot(gen.total,aes(Month))+
  geom_bar( aes( y=`Mean Num of Stu`)
           , stat="identity", position="identity")+
  geom_line( aes( y=`Mean Learning Time`,group=1,color="Mean Learning Time" ),
            linewidth=1.5)+
  geom_text(aes( y=`Mean Num of Stu`, label=`Mean Num of Stu`),
            vjust=-0.4, color="black") +
  scale_color_manual(name=NULL, values = c("Mean Learning Time"="red"))+
  theme(legend.key=element_blank(),
        legend.title=element_blank(),
        legend.box="horizontal")
