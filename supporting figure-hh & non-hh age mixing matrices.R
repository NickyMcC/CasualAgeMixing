require(ggplot2)
require(reshape2)
require(Rmisc)
require(egg)
require(viridis)
require(cowplot)
require(scales)
require(ggnewscale)
require(patchwork)


#####read in data##############
setwd(main_wd)
setwd("matrices-close numbers")
KZN_close_hh_lb<-read.table("KZN_lb_home.csv",header=FALSE,sep=",")
KZN_close_hh_ub<-read.table("KZN_ub_home.csv",header=FALSE,sep=",")
KZN_close_hh_best<-read.table("KZN_best_home.csv",header=FALSE,sep=",")
KZN_close_other_lb<-read.table("KZN_lb_other.csv",header=FALSE,sep=",")
KZN_close_other_ub<-read.table("KZN_ub_other.csv",header=FALSE,sep=",")
KZN_close_other_best<-read.table("KZN_best_other.csv",header=FALSE,sep=",")
WC_close_hh_lb<-read.table("WC_lb_home.csv",header=FALSE,sep=",")
WC_close_hh_ub<-read.table("WC_ub_home.csv",header=FALSE,sep=",")
WC_close_hh_best<-read.table("WC_best_home.csv",header=FALSE,sep=",")
WC_close_other_lb<-read.table("WC_lb_other.csv",header=FALSE,sep=",")
WC_close_other_ub<-read.table("WC_ub_other.csv",header=FALSE,sep=",")
WC_close_other_best<-read.table("WC_best_other.csv",header=FALSE,sep=",")

setwd(main_wd)
setwd("matrices-close time")
KZN_time_hh_lb<-read.table("KZN_lb_home.csv",header=FALSE,sep=",")
KZN_time_hh_ub<-read.table("KZN_ub_home.csv",header=FALSE,sep=",")
KZN_time_hh_best<-read.table("KZN_best_home.csv",header=FALSE,sep=",")
KZN_time_other_lb<-read.table("KZN_lb_other.csv",header=FALSE,sep=",")
KZN_time_other_ub<-read.table("KZN_ub_other.csv",header=FALSE,sep=",")
KZN_time_other_best<-read.table("KZN_best_other.csv",header=FALSE,sep=",")
WC_time_hh_lb<-read.table("WC_lb_home.csv",header=FALSE,sep=",")
WC_time_hh_ub<-read.table("WC_ub_home.csv",header=FALSE,sep=",")
WC_time_hh_best<-read.table("WC_best_home.csv",header=FALSE,sep=",")
WC_time_other_lb<-read.table("WC_lb_other.csv",header=FALSE,sep=",")
WC_time_other_ub<-read.table("WC_ub_other.csv",header=FALSE,sep=",")
WC_time_other_best<-read.table("WC_best_other.csv",header=FALSE,sep=",")

setwd(main_wd)
setwd("matrices-casual time")
KZN_casual_hh_lb<-read.table("KZN_lb_home.csv",header=FALSE,sep=",")
KZN_casual_hh_ub<-read.table("KZN_ub_home.csv",header=FALSE,sep=",")
KZN_casual_hh_best<-read.table("KZN_best_home.csv",header=FALSE,sep=",")
KZN_casual_other_lb<-read.table("KZN_lb_other.csv",header=FALSE,sep=",")
KZN_casual_other_ub<-read.table("KZN_ub_other.csv",header=FALSE,sep=",")
KZN_casual_other_best<-read.table("KZN_best_other.csv",header=FALSE,sep=",")
WC_casual_hh_lb<-read.table("WC_lb_home.csv",header=FALSE,sep=",")
WC_casual_hh_ub<-read.table("WC_ub_home.csv",header=FALSE,sep=",")
WC_casual_hh_best<-read.table("WC_best_home.csv",header=FALSE,sep=",")
WC_casual_other_lb<-read.table("WC_lb_other.csv",header=FALSE,sep=",")
WC_casual_other_ub<-read.table("WC_ub_other.csv",header=FALSE,sep=",")
WC_casual_other_best<-read.table("WC_best_other.csv",header=FALSE,sep=",")

setwd(main_wd)
setwd("figures")

agecat_number<-length(KZN_close_hh_best[,1])

#to create KZN_all_matrix WC_all_matrix

matrices<-ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
j<-1
for (i in matrices) {
  data<-get(i)
  colnames(data)<-c(agecat_names,"Overall")
  
  if (j<=8) {
    pop_age_dist<-KZN_pop_age_dist
  } else {
    pop_age_dist<-WC_pop_age_dist  
  }
  
  data_rate<-data/rep(c(pop_age_dist,sum(pop_age_dist)),each=agecat_number)
  
  sum_data<-apply(data[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*pop_age_dist
  sum_data_rate<-apply(data_rate[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data_rate<-sum_data_rate*pop_age_dist
  mean_contacts<-sum(sum_data[2:agecat_number])/sum(pop_age_dist[2:agecat_number])
  mean_contacts_rate<-sum(sum_data_rate[2:agecat_number])/sum(pop_age_dist[2:agecat_number])
  
  data$respondent_age<-agecat_names
  data_melt<-melt(data = data, id.vars = "respondent_age")
  colnames(data_melt)<-c("respondent_age","contact_age","mean_contacts")
  data_rate$respondent_age<-agecat_names
  data_melt_rate<-melt(data = data_rate, id.vars = "respondent_age")
  colnames(data_melt_rate)<-c("respondent_age","contact_age","mean_contacts")
  
  assign(i,data)
  assign(paste0(i,"_melt"),data_melt)
  assign(paste0("mc_",i),mean_contacts)
  assign(paste0(i,"_rate"),data_rate)
  assign(paste0(i,"_melt_rate"),data_melt_rate)
  assign(paste0("mc_",i,"_rate"),mean_contacts_rate)
  j<-j+1
}

KZN_close_hh_range<-cbind(KZN_close_hh_best_melt,KZN_close_hh_lb_melt[,3],KZN_close_hh_ub_melt[,3])
KZN_close_other_range<-cbind(KZN_close_other_best_melt,KZN_close_other_lb_melt[,3],KZN_close_other_ub_melt[,3])
KZN_time_hh_range<-cbind(KZN_time_hh_best_melt,KZN_time_hh_lb_melt[,3],KZN_time_hh_ub_melt[,3])
KZN_time_other_range<-cbind(KZN_time_other_best_melt,KZN_time_other_lb_melt[,3],KZN_time_other_ub_melt[,3])
KZN_casual_hh_range<-cbind(KZN_casual_hh_best_melt,KZN_casual_hh_lb_melt[,3],KZN_casual_hh_ub_melt[,3])
KZN_casual_other_range<-cbind(KZN_casual_other_best_melt,KZN_casual_other_lb_melt[,3],KZN_casual_other_ub_melt[,3])
WC_close_hh_range<-cbind(WC_close_hh_best_melt,WC_close_hh_lb_melt[,3],WC_close_hh_ub_melt[,3])
WC_close_other_range<-cbind(WC_close_other_best_melt,WC_close_other_lb_melt[,3],WC_close_other_ub_melt[,3])
WC_time_hh_range<-cbind(WC_time_hh_best_melt,WC_time_hh_lb_melt[,3],WC_time_hh_ub_melt[,3])
WC_time_other_range<-cbind(WC_time_other_best_melt,WC_time_other_lb_melt[,3],WC_time_other_ub_melt[,3])
WC_casual_hh_range<-cbind(WC_casual_hh_best_melt,WC_casual_hh_lb_melt[,3],WC_casual_hh_ub_melt[,3])
WC_casual_other_range<-cbind(WC_casual_other_best_melt,WC_casual_other_lb_melt[,3],WC_casual_other_ub_melt[,3])

KZN_close_hh_range_rate<-cbind(KZN_close_hh_best_melt_rate,KZN_close_hh_lb_melt_rate[,3],KZN_close_hh_ub_melt_rate[,3])
KZN_close_other_range_rate<-cbind(KZN_close_other_best_melt_rate,KZN_close_other_lb_melt_rate[,3],KZN_close_other_ub_melt_rate[,3])
KZN_time_hh_range_rate<-cbind(KZN_time_hh_best_melt_rate,KZN_time_hh_lb_melt_rate[,3],KZN_time_hh_ub_melt_rate[,3])
KZN_time_other_range_rate<-cbind(KZN_time_other_best_melt_rate,KZN_time_other_lb_melt_rate[,3],KZN_time_other_ub_melt_rate[,3])
KZN_casual_hh_range_rate<-cbind(KZN_casual_hh_best_melt_rate,KZN_casual_hh_lb_melt_rate[,3],KZN_casual_hh_ub_melt_rate[,3])
KZN_casual_other_range_rate<-cbind(KZN_casual_other_best_melt_rate,KZN_casual_other_lb_melt_rate[,3],KZN_casual_other_ub_melt_rate[,3])
WC_close_hh_range_rate<-cbind(WC_close_hh_best_melt_rate,WC_close_hh_lb_melt_rate[,3],WC_close_hh_ub_melt_rate[,3])
WC_close_other_range_rate<-cbind(WC_close_other_best_melt_rate,WC_close_other_lb_melt_rate[,3],WC_close_other_ub_melt_rate[,3])
WC_time_hh_range_rate<-cbind(WC_time_hh_best_melt_rate,WC_time_hh_lb_melt_rate[,3],WC_time_hh_ub_melt_rate[,3])
WC_time_other_range_rate<-cbind(WC_time_other_best_melt_rate,WC_time_other_lb_melt_rate[,3],WC_time_other_ub_melt_rate[,3])
WC_casual_hh_range_rate<-cbind(WC_casual_hh_best_melt_rate,WC_casual_hh_lb_melt_rate[,3],WC_casual_hh_ub_melt_rate[,3])
WC_casual_other_range_rate<-cbind(WC_casual_other_best_melt_rate,WC_casual_other_lb_melt_rate[,3],WC_casual_other_ub_melt_rate[,3])

KZN_time_hh_range[,3:5]<-KZN_time_hh_range[,3:5]/60
KZN_time_other_range[,3:5]<-KZN_time_other_range[,3:5]/60
WC_time_hh_range[,3:5]<-WC_time_hh_range[,3:5]/60
WC_time_other_range[,3:5]<-WC_time_other_range[,3:5]/60
KZN_casual_hh_range[,3:5]<-KZN_casual_hh_range[,3:5]/60
KZN_casual_other_range[,3:5]<-KZN_casual_other_range[,3:5]/60
WC_casual_hh_range[,3:5]<-WC_casual_hh_range[,3:5]/60
WC_casual_other_range[,3:5]<-WC_casual_other_range[,3:5]/60

KZN_close_hh_range_rate[,3:5]<-10000*KZN_close_hh_range_rate[,3:5]
KZN_close_other_range_rate[,3:5]<-10000*KZN_close_other_range_rate[,3:5]
KZN_time_hh_range_rate[,3:5]<-100*KZN_time_hh_range_rate[,3:5]
KZN_time_other_range_rate[,3:5]<-100*KZN_time_other_range_rate[,3:5]
KZN_casual_hh_range_rate[,3:5]<-100*KZN_casual_hh_range_rate[,3:5]
KZN_casual_other_range_rate[,3:5]<-100*KZN_casual_other_range_rate[,3:5]
WC_close_hh_range_rate[,3:5]<-10000*WC_close_hh_range_rate[,3:5]
WC_close_other_range_rate[,3:5]<-10000*WC_close_other_range_rate[,3:5]
WC_time_hh_range_rate[,3:5]<-100*WC_time_hh_range_rate[,3:5]
WC_time_other_range_rate[,3:5]<-100*WC_time_other_range_rate[,3:5]
WC_casual_hh_range_rate[,3:5]<-100*WC_casual_hh_range_rate[,3:5]
WC_casual_other_range_rate[,3:5]<-100*WC_casual_other_range_rate[,3:5]



sigfig <- function(vec, digits){
  return(gsub("\\.$", "", formatC(signif(vec,digits=digits), digits=digits, format="fg", flag="#")))
}

dataframes<-c(
  "KZN_close_hh_range",
  "KZN_close_other_range",
  "KZN_time_hh_range",
  "KZN_time_other_range",
  "KZN_casual_hh_range",
  "KZN_casual_other_range",
  "WC_close_hh_range",
  "WC_close_other_range",
  "WC_time_hh_range",
  "WC_time_other_range",
  "WC_casual_hh_range",
  "WC_casual_other_range",
  "KZN_close_hh_range_rate",
  "KZN_close_other_range_rate",
  "KZN_time_hh_range_rate",
  "KZN_time_other_range_rate",
  "KZN_casual_hh_range_rate",
  "KZN_casual_other_range_rate",
  "WC_close_hh_range_rate",
  "WC_close_other_range_rate",
  "WC_time_hh_range_rate",
  "WC_time_other_range_rate",
  "WC_casual_hh_range_rate",
  "WC_casual_other_range_rate"
)

j<-1
for (i in dataframes) {
  data<-get(i)
  colnames(data)<-c(colnames(KZN_close_hh_best_melt),"lb","ub")
  data$text<-paste0(sigfig(data$mean_contacts,2),
                    "\n(",sigfig(data$lb,2),
                    "-",sigfig(data$ub,2),")")
  data$text[is.na(data$mean_contacts)]<-""
  
  assign(i,data)
  j<-j+1
}

max_contacts_close_KZN<-max(KZN_close_hh_range$mean_contacts[-which(KZN_close_hh_range$contact_age=="Overall")],
                            KZN_close_other_range$mean_contacts[-which(KZN_close_other_range$contact_age=="Overall")],
                            na.rm=T)
max_contacts_close_all_KZN<-max(KZN_close_hh_range$mean_contacts,
                                KZN_close_other_range$mean_contacts,
                                na.rm=T)
max_contacts_time_KZN<-max(KZN_time_hh_range$mean_contacts[-which(KZN_time_hh_range$contact_age=="Overall")],
                           KZN_time_other_range$mean_contacts[-which(KZN_time_other_range$contact_age=="Overall")],
                           na.rm=T)
max_contacts_time_all_KZN<-max(KZN_time_hh_range$mean_contacts,
                               KZN_time_other_range$mean_contacts,
                               na.rm=T)
max_contacts_casual_KZN<-max(KZN_casual_hh_range$mean_contacts[-which(KZN_casual_hh_range$contact_age=="Overall")],
                             KZN_casual_other_range$mean_contacts[-which(KZN_casual_other_range$contact_age=="Overall")],
                             na.rm=T)
max_contacts_casual_all_KZN<-max(KZN_casual_hh_range$mean_contacts,
                                 KZN_casual_other_range$mean_contacts,
                                 na.rm=T)
max_contacts_close_WC<-max(WC_close_hh_range$mean_contacts[-which(WC_close_hh_range$contact_age=="Overall")],
                           WC_close_other_range$mean_contacts[-which(WC_close_other_range$contact_age=="Overall")],
                           na.rm=T)
max_contacts_close_all_WC<-max(WC_close_hh_range$mean_contacts,
                               WC_close_other_range$mean_contacts,
                               na.rm=T)
max_contacts_time_WC<-max(WC_time_hh_range$mean_contacts[-which(WC_time_hh_range$contact_age=="Overall")],
                          WC_time_other_range$mean_contacts[-which(WC_time_other_range$contact_age=="Overall")],
                          na.rm=T)
max_contacts_time_all_WC<-max(WC_time_hh_range$mean_contacts,
                              WC_time_other_range$mean_contacts,
                              na.rm=T)
max_contacts_casual_WC<-max(WC_casual_hh_range$mean_contacts[-which(WC_casual_hh_range$contact_age=="Overall")],
                            WC_casual_other_range$mean_contacts[-which(WC_casual_other_range$contact_age=="Overall")],
                            na.rm=T)
max_contacts_casual_all_WC<-max(WC_casual_hh_range$mean_contacts,
                                WC_casual_other_range$mean_contacts,
                                na.rm=T)



max_contacts_rate_close_KZN<-max(KZN_close_hh_range_rate$mean_contacts[-which(KZN_close_hh_range_rate$contact_age=="Overall")],
                                 KZN_close_other_range_rate$mean_contacts[-which(KZN_close_other_range_rate$contact_age=="Overall")],
                                 na.rm=T)
max_contacts_rate_close_all_KZN<-max(KZN_close_hh_range_rate$mean_contacts,
                                     KZN_close_other_range_rate$mean_contacts,
                                     na.rm=T)
max_contacts_rate_time_KZN<-max(KZN_time_hh_range_rate$mean_contacts[-which(KZN_time_hh_range_rate$contact_age=="Overall")],
                                KZN_time_other_range_rate$mean_contacts[-which(KZN_time_other_range_rate$contact_age=="Overall")],
                                na.rm=T)
max_contacts_rate_time_all_KZN<-max(KZN_time_hh_range_rate$mean_contacts,
                                    KZN_time_other_range_rate$mean_contacts,
                                    na.rm=T)
max_contacts_rate_casual_KZN<-max(KZN_casual_hh_range_rate$mean_contacts[-which(KZN_casual_hh_range_rate$contact_age=="Overall")],
                                  KZN_casual_other_range_rate$mean_contacts[-which(KZN_casual_other_range_rate$contact_age=="Overall")],
                                  na.rm=T)
max_contacts_rate_casual_all_KZN<-max(KZN_casual_hh_range_rate$mean_contacts,
                                      KZN_casual_other_range_rate$mean_contacts,
                                      na.rm=T)
max_contacts_rate_close_WC<-max(WC_close_hh_range_rate$mean_contacts[-which(WC_close_hh_range_rate$contact_age=="Overall")],
                                WC_close_other_range_rate$mean_contacts[-which(WC_close_other_range_rate$contact_age=="Overall")],
                                na.rm=T)
max_contacts_rate_close_all_WC<-max(WC_close_hh_range_rate$mean_contacts,
                                    WC_close_other_range_rate$mean_contacts,
                                    na.rm=T)
max_contacts_rate_time_WC<-max(WC_time_hh_range_rate$mean_contacts[-which(WC_time_hh_range_rate$contact_age=="Overall")],
                               WC_time_other_range_rate$mean_contacts[-which(WC_time_other_range_rate$contact_age=="Overall")],
                               na.rm=T)
max_contacts_rate_time_all_WC<-max(WC_time_hh_range_rate$mean_contacts,
                                   WC_time_other_range_rate$mean_contacts,
                                   na.rm=T)
max_contacts_rate_casual_WC<-max(WC_casual_hh_range_rate$mean_contacts[-which(WC_casual_hh_range_rate$contact_age=="Overall")],
                                 WC_casual_other_range_rate$mean_contacts[-which(WC_casual_other_range_rate$contact_age=="Overall")],
                                 na.rm=T)
max_contacts_rate_casual_all_WC<-max(WC_casual_hh_range_rate$mean_contacts,
                                     WC_casual_other_range_rate$mean_contacts,
                                     na.rm=T)


##numbers

KZN_close_hh_plot<-ggplot(KZN_close_hh_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_close_hh_range[KZN_close_hh_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_close_hh_range[KZN_close_hh_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_close_KZN),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_close_all_KZN),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("a) Close contact numbers,\nhousehold members") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_close_hh_range,aes(label=KZN_close_hh_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")

KZN_close_other_plot<-ggplot(KZN_close_other_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_close_other_range[KZN_close_other_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_close_other_range[KZN_close_other_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_close_KZN),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_close_all_KZN),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("b) Close contact numbers,\nnon-household") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_close_other_range,aes(label=KZN_close_other_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))

KZN_time_hh_plot<-ggplot(KZN_time_hh_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_time_hh_range[KZN_time_hh_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_time_hh_range[KZN_time_hh_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_time_KZN),
    direction=1,option="turbo",name="Mean close\ncontact time by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_time_all_KZN),
    high="gray40",low="white",name="Mean overall\nclose contact\ntime",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("c) Close contact time (hours),\nwithin own house") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_time_hh_range,aes(label=KZN_time_hh_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")


KZN_time_other_plot<-ggplot(KZN_time_other_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_time_other_range[KZN_time_other_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_time_other_range[KZN_time_other_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_time_KZN),
    direction=1,option="turbo",name="Mean contact\ntime by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_time_all_KZN),
    high="gray40",low="white",name="Mean overall\ncontact time\n(hours)",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("d) Close contact time (hours),\nother congregate settings") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_time_other_range,aes(label=KZN_time_other_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))


KZN_casual_hh_plot<-ggplot(KZN_casual_hh_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_casual_hh_range[KZN_casual_hh_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_casual_hh_range[KZN_casual_hh_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_casual_KZN),
    direction=1,option="turbo",name="Mean contact\ntime by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_casual_all_KZN),
    high="gray40",low="white",name="Mean overall\ncontact time\n(hours)",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("e) Casual contact time (hours),\nwithin own house") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_casual_hh_range,aes(label=KZN_casual_hh_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")


KZN_casual_other_plot<-ggplot(KZN_casual_other_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_casual_other_range[KZN_casual_other_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_casual_other_range[KZN_casual_other_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_casual_KZN),
    direction=1,option="turbo",name="Mean contact\ntime by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_casual_all_KZN),
    high="gray40",low="white",name="Mean overall\ncontact time\n(hours)",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("f) Casual contact time (hours),\nother congregate settings") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_casual_other_range,aes(label=KZN_casual_other_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))



pdf(paste0("supporting figure-hh & non-hh age mixing, numbers, KZN.pdf"), paper="a4", width=9,height=12,onefile=FALSE)
(KZN_close_hh_plot + KZN_close_other_plot + KZN_time_hh_plot + KZN_time_other_plot + KZN_casual_hh_plot + KZN_casual_other_plot) + plot_layout(ncol = 2)
dev.off()





WC_close_hh_plot<-ggplot(WC_close_hh_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_close_hh_range[WC_close_hh_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_close_hh_range[WC_close_hh_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_close_WC),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_close_all_WC),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("a) Close contact numbers,\nhousehold members") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_close_hh_range,aes(label=WC_close_hh_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")

WC_close_other_plot<-ggplot(WC_close_other_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_close_other_range[WC_close_other_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_close_other_range[WC_close_other_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_close_WC),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_close_all_WC),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("b) Close contact numbers,\nnon-household") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_close_other_range,aes(label=WC_close_other_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))

WC_time_hh_plot<-ggplot(WC_time_hh_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_time_hh_range[WC_time_hh_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_time_hh_range[WC_time_hh_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_time_WC),
    direction=1,option="turbo",name="Mean close\ncontact time by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_time_all_WC),
    high="gray40",low="white",name="Mean overall\nclose contact time\n(hours)",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("c) Close contact time (hours),\nwithin own house") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_time_hh_range,aes(label=WC_time_hh_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")


WC_time_other_plot<-ggplot(WC_time_other_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_time_other_range[WC_time_other_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_time_other_range[WC_time_other_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_time_WC),
    direction=1,option="turbo",name="Mean contact\ntime by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_time_all_WC),
    high="gray40",low="white",name="Mean overall\ncontact time\n(hours)",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("d) Close contact time (hours),\nother congregate settings") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_time_other_range,aes(label=WC_time_other_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))


WC_casual_hh_plot<-ggplot(WC_casual_hh_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_casual_hh_range[WC_casual_hh_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_casual_hh_range[WC_casual_hh_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_casual_WC),
    direction=1,option="turbo",name="Mean contact\ntime by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_casual_all_WC),
    high="gray40",low="white",name="Mean overall\ncontact time\n(hours)",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("e) Casual contact time (hours),\nwithin own house") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_casual_hh_range,aes(label=WC_casual_hh_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")


WC_casual_other_plot<-ggplot(WC_casual_other_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_casual_other_range[WC_casual_other_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_casual_other_range[WC_casual_other_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_casual_WC),
    direction=1,option="turbo",name="Mean contact\ntime by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_casual_all_WC),
    high="gray40",low="white",name="Mean overall\ncontact time\n(hours)",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("f) Casual contact time (hours),\nother congregate settings") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_casual_other_range,aes(label=WC_casual_other_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))



pdf(paste0("supporting figure-hh & non-hh age mixing, numbers, WC.pdf"), paper="a4", width=9,height=12,onefile=FALSE)
(WC_close_hh_plot + WC_close_other_plot + WC_time_hh_plot + WC_time_other_plot + WC_casual_hh_plot + WC_casual_other_plot) + plot_layout(ncol = 2)
dev.off()




##rates

KZN_close_hh_plot_rate<-ggplot(KZN_close_hh_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_close_hh_range_rate[KZN_close_hh_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_close_hh_range_rate[KZN_close_hh_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_close_KZN),
    direction=1,option="turbo",name="Mean contact rate\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_close_all_KZN),
    high="gray40",low="white",name="Mean overall\ncontact rate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("a) Close contact rates,\nhousehold members") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_close_hh_range_rate,aes(label=KZN_close_hh_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")

KZN_close_other_plot_rate<-ggplot(KZN_close_other_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_close_other_range_rate[KZN_close_other_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_close_other_range_rate[KZN_close_other_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_close_KZN),
    direction=1,option="turbo",name="Mean contact rate\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_close_all_KZN),
    high="gray40",low="white",name="Mean overall\ncontact rate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("b) Close contact rates,\nnon-household") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_close_other_range_rate,aes(label=KZN_close_other_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))

KZN_time_hh_plot_rate<-ggplot(KZN_time_hh_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_time_hh_range_rate[KZN_time_hh_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_time_hh_range_rate[KZN_time_hh_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_time_KZN),
    direction=1,option="turbo",name="Mean close\ncontact time rate\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_time_all_KZN),
    high="gray40",low="white",name="Mean overall\nclose contact\ntime rate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("c) Close contact time rates,\nwithin own house") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_time_hh_range_rate,aes(label=KZN_time_hh_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")


KZN_time_other_plot_rate<-ggplot(KZN_time_other_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_time_other_range_rate[KZN_time_other_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_time_other_range_rate[KZN_time_other_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_time_KZN),
    direction=1,option="turbo",name="Mean contact\ntime rate by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_time_all_KZN),
    high="gray40",low="white",name="Mean overall\ncontact time\nrate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("d) Close contact time rates,\nother congregate settings") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_time_other_range_rate,aes(label=KZN_time_other_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))


KZN_casual_hh_plot_rate<-ggplot(KZN_casual_hh_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_casual_hh_range_rate[KZN_casual_hh_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_casual_hh_range_rate[KZN_casual_hh_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_casual_KZN),
    direction=1,option="turbo",name="Mean contact\ntime rate by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_casual_all_KZN),
    high="gray40",low="white",name="Mean overall\nclose contact\ntime rate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("e) Casual contact time rates,\nwithin own house") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_casual_hh_range_rate,aes(label=KZN_casual_hh_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")


KZN_casual_other_plot_rate<-ggplot(KZN_casual_other_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_casual_other_range_rate[KZN_casual_other_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_casual_other_range_rate[KZN_casual_other_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_casual_KZN),
    direction=1,option="turbo",name="Mean contact\ntime rate by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_casual_all_KZN),
    high="gray40",low="white",name="Mean overall\ncontact time\nrate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("f) Casual contact time rates,\nother congregate settings") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = KZN_casual_other_range_rate,aes(label=KZN_casual_other_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))



pdf(paste0("supporting figure-hh & non-hh age mixing, rates, KZN.pdf"), paper="a4", width=9,height=12,onefile=FALSE)
(KZN_close_hh_plot_rate + KZN_close_other_plot_rate + KZN_time_hh_plot_rate + KZN_time_other_plot_rate + KZN_casual_hh_plot_rate + KZN_casual_other_plot_rate) + plot_layout(ncol = 2)
dev.off()


WC_close_hh_plot_rate<-ggplot(WC_close_hh_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_close_hh_range_rate[WC_close_hh_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_close_hh_range_rate[WC_close_hh_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_close_WC),
    direction=1,option="turbo",name="Mean contact rate\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_close_all_WC),
    high="gray40",low="white",name="Mean overall\ncontact rate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("a) Close contact rates,\nhousehold members") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_close_hh_range_rate,aes(label=WC_close_hh_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")

WC_close_other_plot_rate<-ggplot(WC_close_other_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_close_other_range_rate[WC_close_other_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_close_other_range_rate[WC_close_other_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_close_WC),
    direction=1,option="turbo",name="Mean contact rate\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_close_all_WC),
    high="gray40",low="white",name="Mean overall\ncontact rate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("b) Close contact rates,\nnon-household") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_close_other_range_rate,aes(label=WC_close_other_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))

WC_time_hh_plot_rate<-ggplot(WC_time_hh_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_time_hh_range_rate[WC_time_hh_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_time_hh_range_rate[WC_time_hh_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_time_WC),
    direction=1,option="turbo",name="Mean close\ncontact time rate\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_time_all_WC),
    high="gray40",low="white",name="Mean overall\nclose contact\ntime rate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("c) Close contact time rates,\nwithin own house") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_time_hh_range_rate,aes(label=WC_time_hh_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")


WC_time_other_plot_rate<-ggplot(WC_time_other_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_time_other_range_rate[WC_time_other_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_time_other_range_rate[WC_time_other_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_time_WC),
    direction=1,option="turbo",name="Mean contact\ntime rate by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_time_all_WC),
    high="gray40",low="white",name="Mean overall\ncontact time\nrate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("d) Close contact time rates,\nother congregate settings") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_time_other_range_rate,aes(label=WC_time_other_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))


WC_casual_hh_plot_rate<-ggplot(WC_casual_hh_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_casual_hh_range_rate[WC_casual_hh_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_casual_hh_range_rate[WC_casual_hh_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_casual_WC),
    direction=1,option="turbo",name="Mean contact\ntime rate by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_casual_all_WC),
    high="gray40",low="white",name="Mean overall\nclose contact\ntime rate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("e) Casual contact time rates,\nwithin own house") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_casual_hh_range_rate,aes(label=WC_casual_hh_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.position="none")


WC_casual_other_plot_rate<-ggplot(WC_casual_other_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_casual_other_range_rate[WC_casual_other_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_casual_other_range_rate[WC_casual_other_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 12.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_rate_casual_WC),
    direction=1,option="turbo",name="Mean contact\ntime rate by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_rate_casual_all_WC),
    high="gray40",low="white",name="Mean overall\ncontact time\nrate",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("f) Casual contact time rates,\nother congregate settings") +
  theme(axis.title.x = element_text(),
        axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1)) +
  geom_text(data = WC_casual_other_range_rate,aes(label=WC_casual_other_range_rate$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text(size=8))



pdf(paste0("supporting figure-hh & non-hh age mixing, rates, WC.pdf"), paper="a4", width=9,height=12,onefile=FALSE)
(WC_close_hh_plot_rate + WC_close_other_plot_rate + WC_time_hh_plot_rate + WC_time_other_plot_rate + WC_casual_hh_plot_rate + WC_casual_other_plot_rate) + plot_layout(ncol = 2)
dev.off()
