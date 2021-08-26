require(ggplot2)
require(data.table)
require(reshape2)
require(patchwork)

setwd(main_wd)
setwd("respondent and contact data")

respondent_data_KZN<-read.table("KZN_respondents_casual.csv",header=TRUE,sep=",")
respondent_data_WC<-read.table("WC_respondents_casual.csv",header=TRUE,sep=",")
setwd(main_wd)
setwd("figures")

########KZN#########

respondent_data_KZN<-respondent_data_KZN[order(respondent_data_KZN$locationPeople),]
respondent_data_KZN[respondent_data_KZN$locationPeople>100,]<-100
respondent_data_KZN$contact_minutes<-respondent_data_KZN$locationMinutesWeighted*respondent_data_KZN$locationPeople
total_contact_minutes_KZN<-sum(respondent_data_KZN$contact_minutes)
respondent_data_KZN$cummulative_contact_minutes<-cumsum(respondent_data_KZN$contact_minutes)
respondent_data_KZN$cum_prop_contact_min<-respondent_data_KZN$cummulative_contact_minutes/total_contact_minutes_KZN

total_minutes_KZN<-sum(respondent_data_KZN$locationMinutesWeighted)
respondent_data_KZN$cummulative_minutes<-cumsum(respondent_data_KZN$locationMinutesWeighted)
respondent_data_KZN$cum_prop_min<-respondent_data_KZN$cummulative_minutes/total_minutes_KZN

respondent_data_KZN<-respondent_data_KZN[order(-respondent_data_KZN$cummulative_contact_minutes),]
respondent_data_KZN<-respondent_data_KZN[!duplicated(data.table::rleidv(respondent_data_KZN, c("locationPeople"))), ]
respondent_data_KZN<-respondent_data_KZN[order(respondent_data_KZN$cummulative_contact_minutes),]

graph_data_KZN<-cbind(respondent_data_KZN$locationPeople,respondent_data_KZN$cum_prop_min,respondent_data_KZN$cum_prop_contact_min)
graph_data_KZN<-as.data.frame(graph_data_KZN)
colnames(graph_data_KZN)<-c("People","Time","Contact time")
graph_data_KZN<-reshape2::melt(graph_data_KZN,id.vars="People")
colnames(graph_data_KZN)<-c("People","Type","Proportion")
graph_data_KZN$Location<-"KwaZulu-Natal"

########WC#########

respondent_data_WC<-respondent_data_WC[order(respondent_data_WC$locationPeople),]
respondent_data_WC[respondent_data_WC$locationPeople>100,]<-100
respondent_data_WC$contact_minutes<-respondent_data_WC$locationMinutesWeighted*respondent_data_WC$locationPeople
total_contact_minutes_WC<-sum(respondent_data_WC$contact_minutes)
respondent_data_WC$cummulative_contact_minutes<-cumsum(respondent_data_WC$contact_minutes)
respondent_data_WC$cum_prop_contact_min<-respondent_data_WC$cummulative_contact_minutes/total_contact_minutes_WC

total_minutes_WC<-sum(respondent_data_WC$locationMinutesWeighted)
respondent_data_WC$cummulative_minutes<-cumsum(respondent_data_WC$locationMinutesWeighted)
respondent_data_WC$cum_prop_min<-respondent_data_WC$cummulative_minutes/total_minutes_WC

respondent_data_WC<-respondent_data_WC[order(-respondent_data_WC$cummulative_contact_minutes),]
respondent_data_WC<-respondent_data_WC[!duplicated(data.table::rleidv(respondent_data_WC, c("locationPeople"))), ]
respondent_data_WC<-respondent_data_WC[order(respondent_data_WC$cummulative_contact_minutes),]

graph_data_WC<-cbind(respondent_data_WC$locationPeople,respondent_data_WC$cum_prop_min,respondent_data_WC$cum_prop_contact_min)
graph_data_WC<-as.data.frame(graph_data_WC)
colnames(graph_data_WC)<-c("People","Time","Contact time")
graph_data_WC<-reshape2::melt(graph_data_WC,id.vars="People")
colnames(graph_data_WC)<-c("People","Type","Proportion")
graph_data_WC$Location<-"Western Cape"


################graphs#####################

graph_data<-rbind(graph_data_KZN,graph_data_WC)

plot<-ggplot(data=graph_data) +
  theme_bw() +
  geom_line(aes(x=People,y=Proportion,colour=Type)) +
  xlab("Number of other people present*") +
  ylab("Cumulative proportion") +
  ggtitle("Cumulative proportion of time and casual contact time occuring in\nlocations, by number of other people present")+
  theme(legend.title=element_blank()) +
  facet_grid(.~Location,scales = "free_x")


pdf(paste0("supporting figure-cummulative contact time by people present.pdf"), paper="a4", width=10,height=4,onefile=FALSE)
(plot)
dev.off()

setwd(main_wd)

