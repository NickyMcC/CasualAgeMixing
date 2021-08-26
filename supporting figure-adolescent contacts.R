require(ggplot2)
require(reshape2)
require(patchwork)
require(xlsx)


setwd(main_wd)
setwd("respondent and contact data")

data<-read.csv("Mean_contact_15-19.csv",header=TRUE,sep=",")
data$group<-NA
data<-data[data$survey==2,]
data$group[data$agegp==0]<-"15-17"
data$group[data$agegp==1]<-"18-19"
data$mean[data$type=="close_time"]<-data$mean[data$type=="close_time"]/60
data$mean[data$type=="casual_time"]<-data$mean[data$type=="casual_time"]/60
data$lb[data$type=="close_time"]<-data$lb[data$type=="close_time"]/60
data$lb[data$type=="casual_time"]<-data$lb[data$type=="casual_time"]/60
data$ub[data$type=="close_time"]<-data$ub[data$type=="close_time"]/60
data$ub[data$type=="casual_time"]<-data$ub[data$type=="casual_time"]/60
close_num_data<-data[data$type=="close_num",]
close_time_data<-data[data$type=="close_time",]
casual_time_data<-data[data$type=="casual_time",]

close_num_plot<-ggplot(close_num_data, aes(x=contact_agegp,y=mean,fill=group)) +
  theme_bw() +
  geom_col(position="dodge") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2,position = position_dodge(width = 0.9)) +
  ggtitle ("Close contact numbers") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 10,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.position="none")

close_time_plot<-ggplot(close_time_data, aes(x=contact_agegp,y=mean,fill=group)) +
  theme_bw() +
  geom_col(position="dodge") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2,position = position_dodge(width = 0.9)) +
  ggtitle ("Close contact time (hours)") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 10,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  scale_fill_discrete(name="Age group")

casual_time_plot<-ggplot(casual_time_data, aes(x=contact_agegp,y=mean,fill=group)) +
  theme_bw() +
  geom_col(position="dodge") + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2,position = position_dodge(width = 0.9)) +
  ggtitle ("Casual contact time (hours)") +
  theme(plot.title = element_text(size = 12)) +
  #theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(size = 10,angle=35, vjust=0.5)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.background = element_rect(fill="white", color="black", size=0.5)) +
  theme(legend.title = element_text(size=12)) +
  theme(legend.text = element_text(size=9)) +
  theme(legend.position="none")

setwd(main_wd)
setwd("figures")
pdf(paste0("supporting figure-adolescent contact.pdf"), paper="a4", width=8,height=7,onefile=FALSE)
(close_num_plot + close_time_plot + plot_spacer() + casual_time_plot) + plot_layout(ncol = 2)
dev.off()
setwd(main_wd)

