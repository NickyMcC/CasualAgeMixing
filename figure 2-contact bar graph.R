require(ggplot2)
require(reshape2)
require(patchwork)

setwd(main_wd)
setwd("respondent and contact data")
close_data_all<-read.csv("Mean_close_contact_numbers.csv",header=TRUE,sep=",")
time_data_all<-read.csv("Mean_close_contact_time.csv",header=TRUE,sep=",")
casual_data_all<-read.csv("Mean_casual_contact_time.csv",header=TRUE,sep=",")

close_data<-as.data.frame(matrix(NA,nrow=78,ncol=7))
colnames(close_data)<-c("Survey", "Type", "Variable", "Class", "Contacts", "lb", "ub")
close_data$Survey<-rep(c("KZN","WC"),each=(length(close_data$Survey)/2))
close_data$Type<-rep(rep(c("all","household","non-household"),each=13),2)
close_data$Variable<-rep(c(rep("Sex",2),rep("Age",5),rep("Household size",5),"Overall"),6)
close_data$Class<-rep(c(close_data_all[2:8,2],close_data_all[21:25,2],"Overall"),6)
close_data$Class[close_data$Class=="15-19" & close_data$Survey=="KZN"]<-"17-19"


casual_data<-close_data
time_data<-close_data
close_data$Contacts<-c(close_data_all[2:8,3],close_data_all[21:25,3],close_data_all[33,3],
                       close_data_all[2:8,11],close_data_all[21:25,11],close_data_all[33,11],
                       close_data_all[2:8,19],close_data_all[21:25,19],close_data_all[33,19],
                       close_data_all[2:8,7],close_data_all[21:25,7],close_data_all[33,7],
                       close_data_all[2:8,15],close_data_all[21:25,15],close_data_all[33,15],
                       close_data_all[2:8,23],close_data_all[21:25,23],close_data_all[33,23])
close_data$lb<-c(rep(c(close_data_all[2:8,4],close_data_all[21:25,4],close_data_all[33,4]),3),
                 rep(c(close_data_all[2:8,8],close_data_all[21:25,8],close_data_all[33,8]),3))
close_data$ub<-c(rep(c(close_data_all[2:8,5],close_data_all[21:25,5],close_data_all[33,5]),3),
                 rep(c(close_data_all[2:8,9],close_data_all[21:25,9],close_data_all[33,9]),3))
casual_data$Contacts<-c(casual_data_all[2:8,3],casual_data_all[21:25,3],casual_data_all[33,3],
                        casual_data_all[2:8,11],casual_data_all[21:25,11],casual_data_all[33,11],
                        casual_data_all[2:8,19],casual_data_all[21:25,19],casual_data_all[33,19],
                        casual_data_all[2:8,7],casual_data_all[21:25,7],casual_data_all[33,7],
                        casual_data_all[2:8,15],casual_data_all[21:25,15],casual_data_all[33,15],
                        casual_data_all[2:8,23],casual_data_all[21:25,23],casual_data_all[33,23])
casual_data$lb<-c(rep(c(casual_data_all[2:8,4],casual_data_all[21:25,4],casual_data_all[33,4]),3),
                  rep(c(casual_data_all[2:8,8],casual_data_all[21:25,8],casual_data_all[33,8]),3))
casual_data$ub<-c(rep(c(casual_data_all[2:8,5],casual_data_all[21:25,5],casual_data_all[33,5]),3),
                  rep(c(casual_data_all[2:8,9],casual_data_all[21:25,9],casual_data_all[33,9]),3))
time_data$Contacts<-c(time_data_all[2:8,3],time_data_all[21:25,3],time_data_all[33,3],
                      time_data_all[2:8,11],time_data_all[21:25,11],time_data_all[33,11],
                      time_data_all[2:8,19],time_data_all[21:25,19],time_data_all[33,19],
                      time_data_all[2:8,7],time_data_all[21:25,7],time_data_all[33,7],
                      time_data_all[2:8,15],time_data_all[21:25,15],time_data_all[33,15],
                      time_data_all[2:8,23],time_data_all[21:25,23],time_data_all[33,23])
time_data$lb<-c(rep(c(time_data_all[2:8,4],time_data_all[21:25,4],time_data_all[33,4]),3),
                rep(c(time_data_all[2:8,8],time_data_all[21:25,8],time_data_all[33,8]),3))
time_data$ub<-c(rep(c(time_data_all[2:8,5],time_data_all[21:25,5],time_data_all[33,5]),3),
                rep(c(time_data_all[2:8,9],time_data_all[21:25,9],time_data_all[33,9]),3))

close_data$Contacts<-as.numeric(close_data$Contacts)
close_data$lb<-as.numeric(close_data$lb)
close_data$ub<-as.numeric(close_data$ub)
close_data$Variable<-factor(close_data$Variable,levels=c("Sex","Age","Household size","Overall"))
close_data$Class<-factor(close_data$Class,levels=c(close_data$Class[1:2],close_data$Class[42],close_data$Class[3:13]))
casual_data$Contacts<-as.numeric(casual_data$Contacts)
casual_data$lb<-as.numeric(casual_data$lb)
casual_data$ub<-as.numeric(casual_data$ub)
casual_data$Variable<-factor(casual_data$Variable,levels=c("Sex","Age","Household size","Overall"))
casual_data$Class<-factor(casual_data$Class,levels=c(casual_data$Class[1:2],casual_data$Class[42],casual_data$Class[3:13]))
time_data$Contacts<-as.numeric(time_data$Contacts)
time_data$lb<-as.numeric(time_data$lb)
time_data$ub<-as.numeric(time_data$ub)
time_data$Variable<-factor(time_data$Variable,levels=c("Sex","Age","Household size","Overall"))
time_data$Class<-factor(time_data$Class,levels=c(time_data$Class[1:2],time_data$Class[42],time_data$Class[3:13]))


data_close_KZN<-close_data[close_data$Survey=="KZN",]
data_close_KZN<-data_close_KZN[data_close_KZN$Type!="all",]
#data_close_KZN$Type<-factor(data_close_KZN$Type,levels=c("non-household","household"))
data_close_KZN<-data_close_KZN[order(data_close_KZN$Type,decreasing=T),]
data_casual_KZN<-casual_data[casual_data$Survey=="KZN",]
data_casual_KZN<-data_casual_KZN[data_casual_KZN$Type!="all",]
#data_casual_KZN$Type<-factor(data_casual_KZN$Type,levels=c("non-household","household"))
data_casual_KZN<-data_casual_KZN[order(data_casual_KZN$Type,decreasing=T),]
data_time_KZN<-time_data[time_data$Survey=="KZN",]
data_time_KZN<-data_time_KZN[data_time_KZN$Type!="all",]
#data_time_KZN$Type<-factor(data_time_KZN$Type,levels=c("non-household","household"))
data_time_KZN<-data_time_KZN[order(data_time_KZN$Type,decreasing=T),]

data_close_WC<-close_data[close_data$Survey=="WC",]
data_close_WC<-data_close_WC[data_close_WC$Type!="all",]
#data_close_WC$Type<-factor(data_close_WC$Type,levels=c("non-household","household"))
data_close_WC<-data_close_WC[order(data_close_WC$Type,decreasing=T),]
data_casual_WC<-casual_data[casual_data$Survey=="WC",]
data_casual_WC<-data_casual_WC[data_casual_WC$Type!="all",]
#data_casual_WC$Type<-factor(data_casual_WC$Type,levels=c("non-household","household"))
data_casual_WC<-data_casual_WC[order(data_casual_WC$Type,decreasing=T),]
data_time_WC<-time_data[time_data$Survey=="WC",]
data_time_WC<-data_time_WC[data_time_WC$Type!="all",]
#data_time_WC$Type<-factor(data_time_WC$Type,levels=c("non-household","household"))
data_time_WC<-data_time_WC[order(data_time_WC$Type,decreasing=T),]


graph_close_KZN<-ggplot(data_close_KZN, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean close contact numbers (per day), KwaZulu-Natal") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 8,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  #theme(legend.justification=c(0,0), legend.position=c(0.05,0.5)) +
  theme(legend.position="right") +
  scale_fill_discrete(name="Contact type",
                      labels=c("Household", "Non-household"))+
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.position="none") +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  coord_cartesian(ylim=c(0, 16)) +
  facet_grid(.~Variable,scales = "free_x")

graph_casual_KZN<-ggplot(data_casual_KZN, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean casual contact time (hours per day), KwaZulu-Natal") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 8,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  #theme(legend.justification=c(0,0), legend.position=c(0.05,0.5)) +
  theme(legend.position="none") +
  #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73"),
  #                  name="Intervention",
  #                  #breaks=c("ctrl", "trt1", "trt2"),
  #                  labels=c("Disease-triggered\nHACF", "Infection-triggered\nHACF (12m intervals)", "Infection-triggered\nHACF (24m intervals)")) +
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  coord_cartesian(ylim=c(0, 420)) +
  facet_grid(.~Variable,scales = "free_x")

graph_time_KZN<-ggplot(data_time_KZN, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean close contact time (hours per day), KwaZulu-Natal") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 8,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  #theme(legend.justification=c(0,0), legend.position=c(0.05,0.5)) +
  theme(legend.position="none") +
  #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73"),
  #                  name="Intervention",
  #                  #breaks=c("ctrl", "trt1", "trt2"),
  #                  labels=c("Disease-triggered\nHACF", "Infection-triggered\nHACF (12m intervals)", "Infection-triggered\nHACF (24m intervals)")) +
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  coord_cartesian(ylim=c(0, 210)) +
  facet_grid(.~Variable,scales = "free_x")

graph_close_WC<-ggplot(data_close_WC, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean close contact numbers (per day), Western Cape") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 8,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  #theme(legend.justification=c(0,0), legend.position=c(0.05,0.5)) +
  theme(legend.position="right") +
  scale_fill_discrete(name="Contact type",
                      labels=c("Household", "Non-household"))+
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  coord_cartesian(ylim=c(0, 16)) +
  facet_grid(.~Variable,scales = "free_x")

graph_casual_WC<-ggplot(data_casual_WC, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean casual contact time (hours per day), Western Cape") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 8,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  #theme(legend.justification=c(0,0), legend.position=c(0.05,0.5)) +
  theme(legend.position="none") +
  #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73"),
  #                  name="Intervention",
  #                  #breaks=c("ctrl", "trt1", "trt2"),
  #                  labels=c("Disease-triggered\nHACF", "Infection-triggered\nHACF (12m intervals)", "Infection-triggered\nHACF (24m intervals)")) +
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  coord_cartesian(ylim=c(0, 420)) +
  facet_grid(.~Variable,scales = "free_x")

graph_time_WC<-ggplot(data_time_WC, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean close contact time (hours per day), Western Cape") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 8,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  #theme(legend.justification=c(0,0), legend.position=c(0.05,0.5)) +
  theme(legend.position="none") +
  #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73"),
  #                  name="Intervention",
  #                  #breaks=c("ctrl", "trt1", "trt2"),
  #                  labels=c("Disease-triggered\nHACF", "Infection-triggered\nHACF (12m intervals)", "Infection-triggered\nHACF (24m intervals)")) +
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  coord_cartesian(ylim=c(0, 210)) +
  facet_grid(.~Variable,scales = "free_x")

setwd(main_wd)
setwd("figures")
pdf(paste0("Figure 2.pdf"), paper="a4r", width=12,height=8,onefile=FALSE)
(graph_close_KZN + graph_close_WC + graph_time_KZN + graph_time_WC + graph_casual_KZN + graph_casual_WC) + plot_layout(ncol = 2)
dev.off()
setwd(main_wd)

