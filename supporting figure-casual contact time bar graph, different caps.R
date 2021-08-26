require(ggplot2)
require(reshape2)
require(patchwork)
require(xlsx)


setwd(main_wd)
setwd("respondent and contact data")

casual_data_all_100<-read.csv("Mean_casual_contact_time.csv",header=TRUE,sep=",")
casual_data_all_nocap<-read.csv("Mean_casual_contact_time_no_cap.csv",header=TRUE,sep=",")
casual_data_all_20<-read.csv("Mean_casual_contact_time_cap20.csv",header=TRUE,sep=",")
setwd(main_wd)
setwd("figures")

casual_data_100<-as.data.frame(matrix(NA,nrow=78,ncol=7))
colnames(casual_data_100)<-c("Survey", "Type", "Variable", "Class", "Contacts", "lb", "ub")
casual_data_100$Survey<-rep(c("KZN","WC"),each=(length(casual_data_100$Survey)/2))
casual_data_100$Type<-rep(rep(c("all","household","non-household"),each=13),2)
casual_data_100$Variable<-rep(c(rep("Sex",2),rep("Age",5),rep("Household size",5),"Overall"),6)
casual_data_100$Class<-rep(c(casual_data_all_100[2:8,2],casual_data_all_100[21:25,2],"Overall"),6)
casual_data_100$Class[casual_data_100$Class=="15-19" & casual_data_100$Survey=="KZN"]<-"17-19"
casual_data_20<-casual_data_100
casual_data_nocap<-casual_data_100



casual_data_100$Contacts<-c(casual_data_all_100[2:8,3],casual_data_all_100[21:25,3],casual_data_all_100[33,3],
                            casual_data_all_100[2:8,11],casual_data_all_100[21:25,11],casual_data_all_100[33,11],
                            casual_data_all_100[2:8,19],casual_data_all_100[21:25,19],casual_data_all_100[33,19],
                            casual_data_all_100[2:8,7],casual_data_all_100[21:25,7],casual_data_all_100[33,7],
                            casual_data_all_100[2:8,15],casual_data_all_100[21:25,15],casual_data_all_100[33,15],
                            casual_data_all_100[2:8,23],casual_data_all_100[21:25,23],casual_data_all_100[33,23])
casual_data_100$lb<-c(rep(c(casual_data_all_100[2:8,4],casual_data_all_100[21:25,4],casual_data_all_100[33,4]),3),
                      rep(c(casual_data_all_100[2:8,8],casual_data_all_100[21:25,8],casual_data_all_100[33,8]),3))
casual_data_100$ub<-c(rep(c(casual_data_all_100[2:8,5],casual_data_all_100[21:25,5],casual_data_all_100[33,5]),3),
                      rep(c(casual_data_all_100[2:8,9],casual_data_all_100[21:25,9],casual_data_all_100[33,9]),3))

casual_data_20$Contacts<-c(casual_data_all_20[2:8,3],casual_data_all_20[21:25,3],casual_data_all_20[33,3],
                           casual_data_all_20[2:8,11],casual_data_all_20[21:25,11],casual_data_all_20[33,11],
                           casual_data_all_20[2:8,19],casual_data_all_20[21:25,19],casual_data_all_20[33,19],
                           casual_data_all_20[2:8,7],casual_data_all_20[21:25,7],casual_data_all_20[33,7],
                           casual_data_all_20[2:8,15],casual_data_all_20[21:25,15],casual_data_all_20[33,15],
                           casual_data_all_20[2:8,23],casual_data_all_20[21:25,23],casual_data_all_20[33,23])
casual_data_20$lb<-c(rep(c(casual_data_all_20[2:8,4],casual_data_all_20[21:25,4],casual_data_all_20[33,4]),3),
                     rep(c(casual_data_all_20[2:8,8],casual_data_all_20[21:25,8],casual_data_all_20[33,8]),3))
casual_data_20$ub<-c(rep(c(casual_data_all_20[2:8,5],casual_data_all_20[21:25,5],casual_data_all_20[33,5]),3),
                     rep(c(casual_data_all_20[2:8,9],casual_data_all_20[21:25,9],casual_data_all_20[33,9]),3))

casual_data_nocap$Contacts<-c(casual_data_all_nocap[2:8,3],casual_data_all_nocap[21:25,3],casual_data_all_nocap[33,3],
                              casual_data_all_nocap[2:8,11],casual_data_all_nocap[21:25,11],casual_data_all_nocap[33,11],
                              casual_data_all_nocap[2:8,19],casual_data_all_nocap[21:25,19],casual_data_all_nocap[33,19],
                              casual_data_all_nocap[2:8,7],casual_data_all_nocap[21:25,7],casual_data_all_nocap[33,7],
                              casual_data_all_nocap[2:8,15],casual_data_all_nocap[21:25,15],casual_data_all_nocap[33,15],
                              casual_data_all_nocap[2:8,23],casual_data_all_nocap[21:25,23],casual_data_all_nocap[33,23])
casual_data_nocap$lb<-c(rep(c(casual_data_all_nocap[2:8,4],casual_data_all_nocap[21:25,4],casual_data_all_nocap[33,4]),3),
                        rep(c(casual_data_all_nocap[2:8,8],casual_data_all_nocap[21:25,8],casual_data_all_nocap[33,8]),3))
casual_data_nocap$ub<-c(rep(c(casual_data_all_nocap[2:8,5],casual_data_all_nocap[21:25,5],casual_data_all_nocap[33,5]),3),
                        rep(c(casual_data_all_nocap[2:8,9],casual_data_all_nocap[21:25,9],casual_data_all_nocap[33,9]),3))


casual_data_100$Contacts<-as.numeric(casual_data_100$Contacts)
casual_data_100$lb<-as.numeric(casual_data_100$lb)
casual_data_100$ub<-as.numeric(casual_data_100$ub)
casual_data_100$Variable<-factor(casual_data_100$Variable,levels=c("Sex","Age","Household size","Overall"))
casual_data_100$Class<-factor(casual_data_100$Class,levels=c(casual_data_100$Class[1:2],casual_data_100$Class[42],casual_data_100$Class[3:13]))
casual_data_20$Contacts<-as.numeric(casual_data_20$Contacts)
casual_data_20$lb<-as.numeric(casual_data_20$lb)
casual_data_20$ub<-as.numeric(casual_data_20$ub)
casual_data_20$Variable<-factor(casual_data_20$Variable,levels=c("Sex","Age","Household size","Overall"))
casual_data_20$Class<-factor(casual_data_20$Class,levels=c(casual_data_20$Class[1:2],casual_data_20$Class[42],casual_data_20$Class[3:13]))
casual_data_nocap$Contacts<-as.numeric(casual_data_nocap$Contacts)
casual_data_nocap$lb<-as.numeric(casual_data_nocap$lb)
casual_data_nocap$ub<-as.numeric(casual_data_nocap$ub)
casual_data_nocap$Variable<-factor(casual_data_nocap$Variable,levels=c("Sex","Age","Household size","Overall"))
casual_data_nocap$Class<-factor(casual_data_nocap$Class,levels=c(casual_data_nocap$Class[1:2],casual_data_nocap$Class[42],casual_data_nocap$Class[3:13]))


data_casual_100_KZN<-casual_data_100[casual_data_100$Survey=="KZN",]
data_casual_100_KZN<-data_casual_100_KZN[data_casual_100_KZN$Type!="all",]
data_casual_100_KZN$Type<-factor(data_casual_100_KZN$Type,levels=c("household","non-household"))
data_casual_100_KZN<-data_casual_100_KZN[order(data_casual_100_KZN$Type,decreasing=F),]
data_casual_20_KZN<-casual_data_20[casual_data_20$Survey=="KZN",]
data_casual_20_KZN<-data_casual_20_KZN[data_casual_20_KZN$Type!="all",]
data_casual_20_KZN$Type<-factor(data_casual_20_KZN$Type,levels=c("household","non-household"))
data_casual_20_KZN<-data_casual_20_KZN[order(data_casual_20_KZN$Type,decreasing=F),]
data_casual_nocap_KZN<-casual_data_nocap[casual_data_nocap$Survey=="KZN",]
data_casual_nocap_KZN<-data_casual_nocap_KZN[data_casual_nocap_KZN$Type!="all",]
data_casual_nocap_KZN$Type<-factor(data_casual_nocap_KZN$Type,levels=c("household","non-household"))
data_casual_nocap_KZN<-data_casual_nocap_KZN[order(data_casual_nocap_KZN$Type,decreasing=F),]

data_casual_100_WC<-casual_data_100[casual_data_100$Survey=="WC",]
data_casual_100_WC<-data_casual_100_WC[data_casual_100_WC$Type!="all",]
data_casual_100_WC$Type<-factor(data_casual_100_WC$Type,levels=c("household","non-household"))
data_casual_100_WC<-data_casual_100_WC[order(data_casual_100_WC$Type,decreasing=F),]
data_casual_20_WC<-casual_data_20[casual_data_20$Survey=="WC",]
data_casual_20_WC<-data_casual_20_WC[data_casual_20_WC$Type!="all",]
data_casual_20_WC$Type<-factor(data_casual_20_WC$Type,levels=c("household","non-household"))
data_casual_20_WC<-data_casual_20_WC[order(data_casual_20_WC$Type,decreasing=F),]
data_casual_nocap_WC<-casual_data_nocap[casual_data_nocap$Survey=="WC",]
data_casual_nocap_WC<-data_casual_nocap_WC[data_casual_nocap_WC$Type!="all",]
data_casual_nocap_WC$Type<-factor(data_casual_nocap_WC$Type,levels=c("household","non-household"))
data_casual_nocap_WC<-data_casual_nocap_WC[order(data_casual_nocap_WC$Type,decreasing=F),]


graph_casual_KZN_100<-ggplot(data_casual_100_KZN, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean casual contact time (hours per day), cap of 100 people, KwaZulu-Natal") +
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

graph_casual_KZN_20<-ggplot(data_casual_20_KZN, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean casual contact time (hours per day), cap of 20 people, KwaZulu-Natal") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 8,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  #theme(legend.justification=c(0,0), legend.position=c(0.05,0.5)) +
  #theme(legend.position="none") +
  #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73"),
  #                  name="Intervention",
  #                  #breaks=c("ctrl", "trt1", "trt2"),
  #                  labels=c("Disease-triggered\nHACF", "Infection-triggered\nHACF (12m intervals)", "Infection-triggered\nHACF (24m intervals)")) +
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  coord_cartesian(ylim=c(0, 420)) +
  facet_grid(.~Variable,scales = "free_x")

graph_casual_KZN_nocap<-ggplot(data_casual_nocap_KZN, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean casual contact time (hours per day), no cap, KwaZulu-Natal") +
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

graph_casual_WC_100<-ggplot(data_casual_100_WC, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean casual contact time (hours per day), cap of 100 people, Western Cape") +
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

graph_casual_WC_20<-ggplot(data_casual_20_WC, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean casual contact time (hours per day), cap of 20 people, Western Cape") +
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

graph_casual_WC_nocap<-ggplot(data_casual_nocap_WC, aes(x=Class,y=Contacts,fill=Type)) +
  theme_bw() +
  geom_col(position="stack",stat="identity") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2) +
  ggtitle ("Mean casual contact time (hours per day), no cap, Western Cape") +
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


pdf(paste0("supporting figure-casual contact time by cap.pdf"), paper="a4", width=8,height=12,onefile=FALSE)
(graph_casual_KZN_20 + graph_casual_KZN_nocap + graph_casual_WC_20 + graph_casual_WC_nocap) + plot_layout(ncol = 1)
dev.off()

setwd(main_wd)