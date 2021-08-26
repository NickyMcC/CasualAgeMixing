require(ggplot2)
require(reshape2)
require(Rmisc)
require(egg)
require(viridis)
require(cowplot)
require(scales)
require(ggnewscale)
require(patchwork)
require(ggpubr)

#####max_contacts and max_reduction fixed values
setwd(main_wd)
setwd("matrices-close numbers")
KZN_close_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_close_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
KZN_close_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
WC_close_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_close_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")
WC_close_best<-read.table("WC_best.csv",header=FALSE,sep=",")
setwd(main_wd)
setwd("combined matrices-mtb")
KZN_combined_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_combined_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
KZN_combined_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
WC_combined_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_combined_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")
WC_combined_best<-read.table("WC_best.csv",header=FALSE,sep=",")
KZN_ratio_combined_close_lb<-read.table("KZN_ratio_lb.csv",header=FALSE,sep=",")
KZN_ratio_combined_close_ub<-read.table("KZN_ratio_ub.csv",header=FALSE,sep=",")
WC_ratio_combined_close_lb<-read.table("WC_ratio_lb.csv",header=FALSE,sep=",")
WC_ratio_combined_close_ub<-read.table("WC_ratio_ub.csv",header=FALSE,sep=",")
setwd(main_wd)
setwd("combined matrices-airborne")
KZN_casual_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_casual_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
KZN_casual_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
WC_casual_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_casual_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")
WC_casual_best<-read.table("WC_best.csv",header=FALSE,sep=",")
KZN_ratio_casual_close_lb<-read.table("KZN_ratio_lb.csv",header=FALSE,sep=",")
KZN_ratio_casual_close_ub<-read.table("KZN_ratio_ub.csv",header=FALSE,sep=",")
WC_ratio_casual_close_lb<-read.table("WC_ratio_lb.csv",header=FALSE,sep=",")
WC_ratio_casual_close_ub<-read.table("WC_ratio_ub.csv",header=FALSE,sep=",")
setwd(main_wd)
setwd("figures")

agecat_number<-length(KZN_close_best[,1])

#to create KZN_all_matrix WC_all_matrix

matrices<-ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
j<-1
for (i in matrices) {
  data<-get(i)
  colnames(data)<-c(agecat_names,"Overall")
  
  if (j<=13) {
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


KZN_close_range<-cbind(KZN_close_best_melt,KZN_close_lb_melt[,3],KZN_close_ub_melt[,3])
KZN_combined_range<-cbind(KZN_combined_best_melt,KZN_combined_lb_melt[,3],KZN_combined_ub_melt[,3])
KZN_combined_range[,3:5]<-KZN_combined_range[,3:5]/mc_KZN_combined_best*mc_KZN_close_best
KZN_casual_range<-cbind(KZN_casual_best_melt,KZN_casual_lb_melt[,3],KZN_casual_ub_melt[,3])
KZN_casual_range[,3:5]<-KZN_casual_range[,3:5]/mc_KZN_casual_best*mc_KZN_close_best
WC_close_range<-cbind(WC_close_best_melt,WC_close_lb_melt[,3],WC_close_ub_melt[,3])
WC_combined_range<-cbind(WC_combined_best_melt,WC_combined_lb_melt[,3],WC_combined_ub_melt[,3])
WC_combined_range[,3:5]<-WC_combined_range[,3:5]/mc_WC_combined_best*mc_WC_close_best
WC_casual_range<-cbind(WC_casual_best_melt,WC_casual_lb_melt[,3],WC_casual_ub_melt[,3])
WC_casual_range[,3:5]<-WC_casual_range[,3:5]/mc_WC_casual_best*mc_WC_close_best

KZN_close_range_rate<-cbind(KZN_close_best_melt_rate,KZN_close_lb_melt_rate[,3],KZN_close_ub_melt_rate[,3])
KZN_combined_range_rate<-cbind(KZN_combined_best_melt_rate,KZN_combined_lb_melt_rate[,3],KZN_combined_ub_melt_rate[,3])
KZN_combined_range_rate[,3:5]<-KZN_combined_range_rate[,3:5]/mc_KZN_combined_best*mc_KZN_close_best
KZN_casual_range_rate<-cbind(KZN_casual_best_melt_rate,KZN_casual_lb_melt_rate[,3],KZN_casual_ub_melt_rate[,3])
KZN_casual_range_rate[,3:5]<-KZN_casual_range_rate[,3:5]/mc_KZN_casual_best*mc_KZN_close_best
WC_close_range_rate<-cbind(WC_close_best_melt_rate,WC_close_lb_melt_rate[,3],WC_close_ub_melt_rate[,3])
WC_combined_range_rate<-cbind(WC_combined_best_melt_rate,WC_combined_lb_melt_rate[,3],WC_combined_ub_melt_rate[,3])
WC_combined_range_rate[,3:5]<-WC_combined_range_rate[,3:5]/mc_WC_combined_best*mc_WC_close_best
WC_casual_range_rate<-cbind(WC_casual_best_melt_rate,WC_casual_lb_melt_rate[,3],WC_casual_ub_melt_rate[,3])
WC_casual_range_rate[,3:5]<-WC_casual_range_rate[,3:5]/mc_WC_casual_best*mc_WC_close_best


KZN_close_range_rate[,3:5]<-10000*KZN_close_range_rate[,3:5]
KZN_combined_range_rate[,3:5]<-10000*KZN_combined_range_rate[,3:5]
KZN_casual_range_rate[,3:5]<-10000*KZN_casual_range_rate[,3:5]
WC_close_range_rate[,3:5]<-10000*WC_close_range_rate[,3:5]
WC_combined_range_rate[,3:5]<-10000*WC_combined_range_rate[,3:5]
WC_casual_range_rate[,3:5]<-10000*WC_casual_range_rate[,3:5]

sigfig <- function(vec, digits){
  return(gsub("\\.$", "", formatC(signif(vec,digits=digits), digits=digits, format="fg", flag="#")))
}

dataframes<-c(
  "KZN_close_range",
  "KZN_combined_range",
  "KZN_casual_range",
  "KZN_close_range_rate",
  "KZN_combined_range_rate",
  "KZN_casual_range_rate",
  "WC_close_range",
  "WC_combined_range",
  "WC_casual_range",
  "WC_close_range_rate",
  "WC_combined_range_rate",
  "WC_casual_range_rate"
)

j<-1
for (i in dataframes) {
  data<-get(i)
  colnames(data)<-c(colnames(KZN_close_best_melt),"lb","ub")
  
  data$text1<-paste0(sigfig(data$mean_contacts,2))
  data$text1[is.na(data$mean_contacts)]<-""
  
  data$text2<-paste0("\n(",sigfig(data$lb,2),
                     "-",sigfig(data$ub,2),")")
  data$text2[is.na(data$mean_contacts)]<-""
  
  assign(i,data)
  j<-j+1
}

max_contacts_KZN<-max(KZN_close_range$mean_contacts[-which(KZN_close_range$contact_age=="Overall")],
                      KZN_combined_range$mean_contacts[-which(KZN_combined_range$contact_age=="Overall")],
                      KZN_casual_range$mean_contacts[-which(KZN_casual_range$contact_age=="Overall")],
                      na.rm=TRUE)
max_contacts_WC<-max(WC_close_range$mean_contacts[-which(WC_close_range$contact_age=="Overall")],
                     WC_combined_range$mean_contacts[-which(WC_combined_range$contact_age=="Overall")],
                     WC_casual_range$mean_contacts[-which(WC_casual_range$contact_age=="Overall")],
                     na.rm=TRUE)
max_contacts_all_KZN<-max(KZN_close_range$mean_contacts[which(KZN_close_range$contact_age=="Overall")],
                          KZN_combined_range$mean_contacts[which(KZN_combined_range$contact_age=="Overall")],
                          KZN_casual_range$mean_contacts[which(KZN_casual_range$contact_age=="Overall")],
                          na.rm=TRUE)
max_contacts_all_WC<-max(WC_close_range$mean_contacts[which(WC_close_range$contact_age=="Overall")],
                         WC_combined_range$mean_contacts[which(WC_combined_range$contact_age=="Overall")],
                         WC_casual_range$mean_contacts[which(WC_casual_range$contact_age=="Overall")],
                         na.rm=TRUE)

max_rate_KZN<-max(KZN_close_range_rate$mean_contacts[-which(KZN_close_range_rate$contact_age=="Overall")],
                  KZN_combined_range_rate$mean_contacts[-which(KZN_combined_range_rate$contact_age=="Overall")],
                  KZN_casual_range_rate$mean_contacts[-which(KZN_casual_range_rate$contact_age=="Overall")],
                  na.rm=TRUE)
max_rate_WC<-max(WC_close_range_rate$mean_contacts[-which(WC_close_range_rate$contact_age=="Overall")],
                 WC_combined_range_rate$mean_contacts[-which(WC_combined_range_rate$contact_age=="Overall")],
                 WC_casual_range_rate$mean_contacts[-which(WC_casual_range_rate$contact_age=="Overall")],
                 na.rm=TRUE)
max_rate_all_KZN<-max(KZN_close_range_rate$mean_contacts[which(KZN_close_range_rate$contact_age=="Overall")],
                      KZN_combined_range_rate$mean_contacts[which(KZN_combined_range_rate$contact_age=="Overall")],
                      KZN_casual_range_rate$mean_contacts[which(KZN_casual_range_rate$contact_age=="Overall")],
                      na.rm=TRUE)
max_rate_all_WC<-max(WC_close_range_rate$mean_contacts[which(WC_close_range_rate$contact_age=="Overall")],
                     WC_combined_range_rate$mean_contacts[which(WC_combined_range_rate$contact_age=="Overall")],
                     WC_casual_range_rate$mean_contacts[which(WC_casual_range_rate$contact_age=="Overall")],
                     na.rm=TRUE)


######RR

KZN_RR_combined_close<-KZN_close_range[,1:3]
colnames(KZN_RR_combined_close)<-c("respondent_age","contact_age","RR")
KZN_RR_combined_close$RR<-KZN_combined_range$mean_contacts/KZN_close_range$mean_contacts
WC_RR_combined_close<-WC_close_range[,1:3]
colnames(WC_RR_combined_close)<-c("respondent_age","contact_age","RR")
WC_RR_combined_close$RR<-WC_combined_range$mean_contacts/WC_close_range$mean_contacts

KZN_RR_casual_close<-KZN_close_range[,1:3]
colnames(KZN_RR_casual_close)<-c("respondent_age","contact_age","RR")
KZN_RR_casual_close$RR<-KZN_casual_range$mean_contacts/KZN_close_range$mean_contacts
WC_RR_casual_close<-WC_close_range[,1:3]
colnames(WC_RR_casual_close)<-c("respondent_age","contact_age","RR")
WC_RR_casual_close$RR<-WC_casual_range$mean_contacts/WC_close_range$mean_contacts

min_RR_KZN<-min(KZN_RR_combined_close$RR,KZN_RR_casual_close$RR,na.rm=T)
max_RR_KZN<-max(KZN_RR_combined_close$RR,KZN_RR_casual_close$RR,na.rm=T)
min_RR_WC<-min(WC_RR_combined_close$RR,WC_RR_casual_close$RR,na.rm=T)
max_RR_WC<-max(WC_RR_combined_close$RR,WC_RR_casual_close$RR,na.rm=T)
min_RR_all<-min(min_RR_KZN,min_RR_WC,na.rm=T)
max_RR_all<-max(max_RR_KZN,max_RR_WC,na.rm=T)

min_range_KZN=1-max(max_RR_KZN-1,1-min_RR_KZN)
max_range_KZN=1+max(max_RR_KZN-1,1-min_RR_KZN)
min_range_WC=1-max(max_RR_WC-1,1-min_RR_WC)
max_range_WC=1+max(max_RR_WC-1,1-min_RR_WC)
KZN_RR_combined_close$range<-NA
KZN_RR_combined_close$range[2]<-min_range_KZN
KZN_RR_combined_close$range[3]<-max_range_KZN
WC_RR_combined_close$range<-NA
WC_RR_combined_close$range[2]<-min_range_WC
WC_RR_combined_close$range[3]<-max_range_WC


KZN_RR_combined_close$below<-NA
KZN_RR_combined_close$below[is.na(KZN_RR_combined_close$RR)==F & KZN_RR_combined_close$RR<1]<-KZN_RR_combined_close$RR[is.na(KZN_RR_combined_close$RR)==F & KZN_RR_combined_close$RR<1]
KZN_RR_combined_close$above<-NA
KZN_RR_combined_close$above[is.na(KZN_RR_combined_close$RR)==F & KZN_RR_combined_close$RR>1]<-KZN_RR_combined_close$RR[is.na(KZN_RR_combined_close$RR)==F & KZN_RR_combined_close$RR>1]
WC_RR_combined_close$below<-NA
WC_RR_combined_close$below[is.na(WC_RR_combined_close$RR)==F & WC_RR_combined_close$RR<1]<-WC_RR_combined_close$RR[is.na(WC_RR_combined_close$RR)==F & WC_RR_combined_close$RR<1]
WC_RR_combined_close$above<-NA
WC_RR_combined_close$above[is.na(WC_RR_combined_close$RR)==F & WC_RR_combined_close$RR>1]<-WC_RR_combined_close$RR[is.na(WC_RR_combined_close$RR)==F & WC_RR_combined_close$RR>1]

KZN_RR_combined_close$lb<-KZN_ratio_combined_close_lb_melt$mean_contacts
KZN_RR_combined_close$ub<-KZN_ratio_combined_close_ub_melt$mean_contacts
WC_RR_combined_close$lb<-WC_ratio_combined_close_lb_melt$mean_contacts
WC_RR_combined_close$ub<-WC_ratio_combined_close_ub_melt$mean_contacts

KZN_RR_combined_close$text1<-paste0(sigfig(KZN_RR_combined_close$RR,2))
KZN_RR_combined_close$text1[is.na(KZN_RR_combined_close$RR)]<-""
KZN_RR_combined_close$text2<-paste0("\n(",sigfig(KZN_RR_combined_close$lb,2),
                                    "-",sigfig(KZN_RR_combined_close$ub,2),")")
KZN_RR_combined_close$text2[is.na(KZN_RR_combined_close$RR)]<-""
WC_RR_combined_close$text1<-paste0(sigfig(WC_RR_combined_close$RR,2))
WC_RR_combined_close$text1[is.na(WC_RR_combined_close$RR)]<-""
WC_RR_combined_close$text2<-paste0("\n(",sigfig(WC_RR_combined_close$lb,2),
                                   "-",sigfig(WC_RR_combined_close$ub,2),")")
WC_RR_combined_close$text2[is.na(WC_RR_combined_close$RR)]<-""

min_range_KZN=1-max(max_RR_KZN-1,1-min_RR_KZN)
max_range_KZN=1+max(max_RR_KZN-1,1-min_RR_KZN)
min_range_WC=1-max(max_RR_WC-1,1-min_RR_WC)
max_range_WC=1+max(max_RR_WC-1,1-min_RR_WC)
KZN_RR_casual_close$range<-NA
KZN_RR_casual_close$range[2]<-min_range_KZN
KZN_RR_casual_close$range[3]<-max_range_KZN
WC_RR_casual_close$range<-NA
WC_RR_casual_close$range[2]<-min_range_WC
WC_RR_casual_close$range[3]<-max_range_WC


KZN_RR_casual_close$below<-NA
KZN_RR_casual_close$below[is.na(KZN_RR_casual_close$RR)==F & KZN_RR_casual_close$RR<1]<-KZN_RR_casual_close$RR[is.na(KZN_RR_casual_close$RR)==F & KZN_RR_casual_close$RR<1]
KZN_RR_casual_close$above<-NA
KZN_RR_casual_close$above[is.na(KZN_RR_casual_close$RR)==F & KZN_RR_casual_close$RR>1]<-KZN_RR_casual_close$RR[is.na(KZN_RR_casual_close$RR)==F & KZN_RR_casual_close$RR>1]
WC_RR_casual_close$below<-NA
WC_RR_casual_close$below[is.na(WC_RR_casual_close$RR)==F & WC_RR_casual_close$RR<1]<-WC_RR_casual_close$RR[is.na(WC_RR_casual_close$RR)==F & WC_RR_casual_close$RR<1]
WC_RR_casual_close$above<-NA
WC_RR_casual_close$above[is.na(WC_RR_casual_close$RR)==F & WC_RR_casual_close$RR>1]<-WC_RR_casual_close$RR[is.na(WC_RR_casual_close$RR)==F & WC_RR_casual_close$RR>1]

KZN_RR_casual_close$lb<-KZN_ratio_casual_close_lb_melt$mean_contacts
KZN_RR_casual_close$ub<-KZN_ratio_casual_close_ub_melt$mean_contacts
WC_RR_casual_close$lb<-WC_ratio_casual_close_lb_melt$mean_contacts
WC_RR_casual_close$ub<-WC_ratio_casual_close_ub_melt$mean_contacts

KZN_RR_casual_close$text1<-paste0(sigfig(KZN_RR_casual_close$RR,2))
KZN_RR_casual_close$text1[is.na(KZN_RR_casual_close$RR)]<-""
KZN_RR_casual_close$text2<-paste0("\n(",sigfig(KZN_RR_casual_close$lb,2),
                                  "-",sigfig(KZN_RR_casual_close$ub,2),")")
KZN_RR_casual_close$text2[is.na(KZN_RR_casual_close$RR)]<-""
WC_RR_casual_close$text1<-paste0(sigfig(WC_RR_casual_close$RR,2))
WC_RR_casual_close$text1[is.na(WC_RR_casual_close$RR)]<-""
WC_RR_casual_close$text2<-paste0("\n(",sigfig(WC_RR_casual_close$lb,2),
                                 "-",sigfig(WC_RR_casual_close$ub,2),")")
WC_RR_casual_close$text2[is.na(WC_RR_casual_close$RR)]<-""


##numbers

KZN_close_plot<-ggplot(KZN_close_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_close_range[KZN_close_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_close_range[KZN_close_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_KZN),
    #direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-numbers",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_all_KZN),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-numbers",
    na.value = "white",
    breaks=c(0,5,10),
    labels=c("0","5","10")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("a) Droplet transmission\n    - numbers") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = KZN_close_range,aes(label=KZN_close_range$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data =KZN_close_range,aes(label=KZN_close_range$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  #guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) +
  guides(fill=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

KZN_numbers_abs_legend<-ggpubr::get_legend(KZN_close_plot)
KZN_numbers_abs_legend<-as_ggplot(KZN_numbers_abs_legend)
KZN_close_plot<-KZN_close_plot+
  theme(legend.position="none")

KZN_casual_plot<-ggplot(KZN_casual_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_casual_range[KZN_casual_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_casual_range[KZN_casual_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_KZN),
    #direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-numbers",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_all_KZN),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-numbers",
    na.value = "white",
    breaks=c(0,5,10),
    labels=c("0","5","10")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("c) Airborne transmission\n    - numbers") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = KZN_casual_range,aes(label=KZN_casual_range$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = KZN_casual_range,aes(label=KZN_casual_range$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  #guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) +
  guides(color=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

KZN_numbers_abs_legend2<-ggpubr::get_legend(KZN_casual_plot)
KZN_numbers_abs_legend2<-as_ggplot(KZN_numbers_abs_legend2)
KZN_casual_plot<-KZN_casual_plot+
  theme(legend.position="none")

KZN_combined_plot<-ggplot(KZN_combined_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_combined_range[KZN_combined_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_combined_range[KZN_combined_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_KZN),
    #direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-numbers",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_all_KZN),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-numbers",
    na.value = "white",
    breaks=c(0,5,10),
    labels=c("0","5","10")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("f) Mycobacterium tuberculosis\n    (Mtb) - numbers") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = KZN_combined_range,aes(label=KZN_combined_range$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = KZN_combined_range,aes(label=KZN_combined_range$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) + 
  theme(legend.position="none")




##rates

KZN_close_rate_plot<-ggplot(KZN_close_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_close_range_rate[KZN_close_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_close_range_rate[KZN_close_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_rate_KZN),
    #direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_rate_all_KZN),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-rates",
    na.value = "white",
    breaks=c(0,1,2),
    labels=c("0","1","2")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("b) Droplet transmission\n    - rates") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = KZN_close_range_rate,aes(label=KZN_close_range_rate$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = KZN_close_range_rate,aes(label=KZN_close_range_rate$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  #guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) +
  guides(fill=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

KZN_rates_abs_legend<-ggpubr::get_legend(KZN_close_rate_plot)
KZN_rates_abs_legend<-as_ggplot(KZN_rates_abs_legend)
KZN_close_rate_plot<-KZN_close_rate_plot+
  theme(legend.position="none")

KZN_casual_rate_plot<-ggplot(KZN_casual_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_casual_range_rate[KZN_casual_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_casual_range_rate[KZN_casual_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_rate_KZN),
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_rate_all_KZN),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-rates",
    na.value = "white",
    breaks=c(0,1,2),
    labels=c("0","1","2")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("d) Airborne transmission\n    - rates") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = KZN_casual_range_rate,aes(label=KZN_casual_range_rate$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = KZN_casual_range_rate,aes(label=KZN_casual_range_rate$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  #guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) +
  guides(color=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

KZN_rates_abs_legend2<-ggpubr::get_legend(KZN_casual_rate_plot)
KZN_rates_abs_legend2<-as_ggplot(KZN_rates_abs_legend2)
KZN_casual_rate_plot<-KZN_casual_rate_plot+
  theme(legend.position="none")

KZN_combined_rate_plot<-ggplot(KZN_combined_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_combined_range_rate[KZN_combined_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_combined_range_rate[KZN_combined_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_rate_KZN),
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_rate_all_KZN),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-rates",
    na.value = "white",
    breaks=c(0,1,2),
    labels=c("0","1","2")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("g) Mycobacterium tuberculosis\n    (Mtb) - rates") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = KZN_combined_range_rate,aes(label=KZN_combined_range_rate$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = KZN_combined_range_rate,aes(label=KZN_combined_range_rate$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) + 
  theme(legend.position="none")





##RR

KZN_RR_combined_close_plot<-ggplot(KZN_RR_combined_close, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_RR_combined_close[KZN_RR_combined_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=range)) +
  scale_fill_gradient2(
    low="blue",high="red",mid="grey90",
    na.value = "white",
    midpoint=1,
    limits = c(min_range_KZN,max_range_KZN),
    name="Relative contact\nintensity",
    breaks=c(min_range_KZN,1,max_range_KZN),labels=c(format(round(min_RR_KZN, digits=2), nsmall = 2) ,"1.00",format(round(max_RR_KZN, digits=2), nsmall = 2))
  ) +
  new_scale("fill") +
  geom_tile(data = KZN_RR_combined_close[KZN_RR_combined_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=below)) +
  scale_fill_gradientn(colours=c("blue","grey90"),
                       na.value = NA,
                       name="Mean contacts\nby contact age",
                       limits = c(min_RR_KZN,1),
                       guide=FALSE
  ) +
  new_scale("fill") +
  geom_tile(data = KZN_RR_combined_close[KZN_RR_combined_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=above)) +
  scale_fill_gradientn(colours=c("grey90","red"),
                       na.value = NA,
                       name="Mean contacts\nby contact age",
                       limits = c(1,max_RR_KZN),
                       guide=FALSE
  ) +
  geom_point(data = KZN_RR_combined_close[KZN_RR_combined_close$contact_age=="Overall",], 
             aes(col = below), size = 8.5, shape = 15) +
  scale_color_gradientn(colours=c("blue","grey90"),
                        na.value = NA,
                        name="Relative difference\nin contact",
                        limits = c(min_RR_KZN,1),
                        guide=FALSE
  ) +
  new_scale("color") +
  geom_point(data = KZN_RR_combined_close[KZN_RR_combined_close$contact_age=="Overall",], 
             aes(col = above), size = 8.5, shape = 15) +
  scale_color_gradientn(colours=c("grey90","red"),
                        na.value = NA,
                        name="Relative difference\nin contact",
                        limits = c(1,max_RR_KZN),
                        guide=FALSE
  ) +
  geom_text(data = KZN_RR_combined_close,aes(label=KZN_RR_combined_close$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = KZN_RR_combined_close,aes(label=KZN_RR_combined_close$text2,x=respondent_age, y=contact_age), size=1.3) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("h) Relative intensity of contact,\n    Mtb compared to droplet") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

KZN_RR_abs_legend<-ggpubr::get_legend(KZN_RR_combined_close_plot)
KZN_RR_abs_legend<-as_ggplot(KZN_RR_abs_legend)
KZN_RR_combined_close_plot<-KZN_RR_combined_close_plot+
  theme(legend.position="none")


KZN_RR_casual_close_plot<-ggplot(KZN_RR_casual_close, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_RR_casual_close[KZN_RR_casual_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=range)) +
  scale_fill_gradient2(
    low="blue",high="red",mid="grey90",
    na.value = "white",
    midpoint=1,
    limits = c(min_range_KZN,max_range_KZN),
    name="Relative\ncontact\nintensity",
    breaks=c(min_range_KZN,1,max_range_KZN),labels=c(format(round(min_RR_KZN, digits=2), nsmall = 2) ,"1.00",format(round(max_RR_KZN, digits=2), nsmall = 2))
  ) +
  new_scale("fill") +
  geom_tile(data = KZN_RR_casual_close[KZN_RR_casual_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=below)) +
  scale_fill_gradientn(colours=c("blue","grey90"),
                       na.value = NA,
                       name="Mean contacts\nby contact age",
                       limits = c(min_RR_KZN,1),
                       guide=FALSE
  ) +
  new_scale("fill") +
  geom_tile(data = KZN_RR_casual_close[KZN_RR_casual_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=above)) +
  scale_fill_gradientn(colours=c("grey90","red"),
                       na.value = NA,
                       name="Mean contacts\nby contact age",
                       limits = c(1,max_RR_KZN),
                       guide=FALSE
  ) +
  geom_point(data = KZN_RR_casual_close[KZN_RR_casual_close$contact_age=="Overall",], 
             aes(col = below), size = 8.5, shape = 15) +
  scale_color_gradientn(colours=c("blue","grey90"),
                        na.value = NA,
                        name="Relative difference\nin contact",
                        limits = c(min_RR_KZN,1),
                        guide=FALSE
  ) +
  new_scale("color") +
  geom_point(data = KZN_RR_casual_close[KZN_RR_casual_close$contact_age=="Overall",], 
             aes(col = above), size = 8.5, shape = 15) +
  scale_color_gradientn(colours=c("grey90","red"),
                        na.value = NA,
                        name="Relative difference\nin contact",
                        limits = c(1,max_RR_KZN),
                        guide=FALSE
  ) +
  geom_text(data = KZN_RR_casual_close,aes(label=KZN_RR_casual_close$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = KZN_RR_casual_close,aes(label=KZN_RR_casual_close$text2,x=respondent_age, y=contact_age), size=1.3) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("e) Relative intensity of contact,\n    airborne compared to droplet") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  guides(fill=FALSE) +
  guides(alpha=FALSE)+
  theme(legend.position="none")

layout <- "
ABC
ABD
ABE
ABF
ABG
HIJ
HIJ
HIJ
HIJ
HIJ
KLM
KLM
KLM
KLM
KLM
"

pdf(paste0("Figure 3.pdf"), paper="a4", width=10,height=9,onefile=FALSE)
(KZN_close_plot + KZN_close_rate_plot + 
    KZN_numbers_abs_legend + KZN_numbers_abs_legend2 + KZN_rates_abs_legend + KZN_rates_abs_legend2 + KZN_RR_abs_legend + 
    KZN_casual_plot + KZN_casual_rate_plot + KZN_RR_casual_close_plot + 
    KZN_combined_plot + KZN_combined_rate_plot + KZN_RR_combined_close_plot) +
  plot_layout(design=layout)
dev.off()











##numbers

WC_close_plot<-ggplot(WC_close_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_close_range[WC_close_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_close_range[WC_close_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_WC),
    #direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-numbers",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_all_WC),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-numbers",
    na.value = "white",
    breaks=c(0,5,10),
    labels=c("0","5","10")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("a) Droplet transmission\n    - numbers") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = WC_close_range,aes(label=WC_close_range$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = WC_close_range,aes(label=WC_close_range$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  #guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) +
  guides(fill=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

WC_numbers_abs_legend<-ggpubr::get_legend(WC_close_plot)
WC_numbers_abs_legend<-as_ggplot(WC_numbers_abs_legend)
WC_close_plot<-WC_close_plot+
  theme(legend.position="none")

WC_casual_plot<-ggplot(WC_casual_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_casual_range[WC_casual_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_casual_range[WC_casual_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_WC),
    #direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-numbers",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_all_WC),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-numbers",
    na.value = "white",
    breaks=c(0,5,10),
    labels=c("0","5","10")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("c) Airborne transmission\n    - numbers") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = WC_casual_range,aes(label=WC_casual_range$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = WC_casual_range,aes(label=WC_casual_range$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  #guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) +
  guides(color=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

WC_numbers_abs_legend2<-ggpubr::get_legend(WC_casual_plot)
WC_numbers_abs_legend2<-as_ggplot(WC_numbers_abs_legend2)
WC_casual_plot<-WC_casual_plot+
  theme(legend.position="none")

WC_combined_plot<-ggplot(WC_combined_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_combined_range[WC_combined_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_combined_range[WC_combined_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_contacts_WC),
    #direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-numbers",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_contacts_all_WC),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-numbers",
    na.value = "white",
    breaks=c(0,5,10),
    labels=c("0","5","10")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("f) Mycobacterium tuberculosis\n    (Mtb) - numbers") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = WC_combined_range,aes(label=WC_combined_range$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = WC_combined_range,aes(label=WC_combined_range$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) + 
  theme(legend.position="none")




##rates

WC_close_rate_plot<-ggplot(WC_close_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_close_range_rate[WC_close_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_close_range_rate[WC_close_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_rate_WC),
    #direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_rate_all_WC),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-rates",
    na.value = "white",
    breaks=c(0,1,2),
    labels=c("0","1","2")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("b) Droplet transmission\n    - rates") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = WC_close_range_rate,aes(label=WC_close_range_rate$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = WC_close_range_rate,aes(label=WC_close_range_rate$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  #guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) +
  guides(fill=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

WC_rates_abs_legend<-ggpubr::get_legend(WC_close_rate_plot)
WC_rates_abs_legend<-as_ggplot(WC_rates_abs_legend)
WC_close_rate_plot<-WC_close_rate_plot+
  theme(legend.position="none")

WC_casual_rate_plot<-ggplot(WC_casual_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_casual_range_rate[WC_casual_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_casual_range_rate[WC_casual_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_rate_WC),
    breaks = c(0,4,8,12),
    labels = c(0,4,8,12),
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_rate_all_WC),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-rates",
    na.value = "white",
    breaks=c(0,1,2),
    labels=c("0","1","2")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("d) Airborne transmission\n    - rates") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = WC_casual_range_rate,aes(label=WC_casual_range_rate$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = WC_casual_range_rate,aes(label=WC_casual_range_rate$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  #guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) +
  guides(color=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

WC_rates_abs_legend2<-ggpubr::get_legend(WC_casual_rate_plot)
WC_rates_abs_legend2<-as_ggplot(WC_rates_abs_legend2)
WC_casual_rate_plot<-WC_casual_rate_plot+
  theme(legend.position="none")

WC_combined_rate_plot<-ggplot(WC_combined_range_rate, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_combined_range_rate[WC_combined_range_rate$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_combined_range_rate[WC_combined_range_rate$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 8.5, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max_rate_WC),
    direction=1,option="turbo",name="Mean contact by\ncontact age\n-rates",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max_rate_all_WC),
    #high="gray40",low="white",name="Mean overall      \ncontact",
    high="gray40",low="white",name="Mean overall      \ncontact-rates",
    na.value = "white",
    breaks=c(0,1,2),
    labels=c("0","1","2")) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("g) Mycobacterium tuberculosis\n    (Mtb) - rates") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  geom_text(data = WC_combined_range_rate,aes(label=WC_combined_range_rate$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = WC_combined_range_rate,aes(label=WC_combined_range_rate$text2,x=respondent_age, y=contact_age), size=1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  guides(color=guide_colourbar(order=1),fill=guide_colourbar(order=2)) + 
  theme(legend.position="none")





##RR

WC_RR_combined_close_plot<-ggplot(WC_RR_combined_close, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_RR_combined_close[WC_RR_combined_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=range)) +
  scale_fill_gradient2(
    low="blue",high="red",mid="grey90",
    na.value = "white",
    midpoint=1,
    limits = c(min_range_WC,max_range_WC),
    name="Relative contact\nintensity",
    breaks=c(min_range_WC,1,max_range_WC),labels=c(format(round(min_RR_WC, digits=2), nsmall = 2) ,"1.00",format(round(max_RR_WC, digits=2), nsmall = 2))
  ) +
  new_scale("fill") +
  geom_tile(data = WC_RR_combined_close[WC_RR_combined_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=below)) +
  scale_fill_gradientn(colours=c("blue","grey90"),
                       na.value = NA,
                       name="Mean contacts\nby contact age",
                       limits = c(min_RR_WC,1),
                       guide=FALSE
  ) +
  new_scale("fill") +
  geom_tile(data = WC_RR_combined_close[WC_RR_combined_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=above)) +
  scale_fill_gradientn(colours=c("grey90","red"),
                       na.value = NA,
                       name="Mean contacts\nby contact age",
                       limits = c(1,max_RR_WC),
                       guide=FALSE
  ) +
  geom_point(data = WC_RR_combined_close[WC_RR_combined_close$contact_age=="Overall",], 
             aes(col = below), size = 8.5, shape = 15) +
  scale_color_gradientn(colours=c("blue","grey90"),
                        na.value = NA,
                        name="Relative difference\nin contact",
                        limits = c(min_RR_WC,1),
                        guide=FALSE
  ) +
  new_scale("color") +
  geom_point(data = WC_RR_combined_close[WC_RR_combined_close$contact_age=="Overall",], 
             aes(col = above), size = 8.5, shape = 15) +
  scale_color_gradientn(colours=c("grey90","red"),
                        na.value = NA,
                        name="Relative difference\nin contact",
                        limits = c(1,max_RR_WC),
                        guide=FALSE
  ) +
  geom_text(data = WC_RR_combined_close,aes(label=WC_RR_combined_close$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = WC_RR_combined_close,aes(label=WC_RR_combined_close$text2,x=respondent_age, y=contact_age), size=1.3) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("h) Relative intensity of contact,\n    Mtb compared to droplet") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme(legend.position="top") + 
  theme(legend.key.size = unit(0.4, "cm"))

WC_RR_abs_legend<-ggpubr::get_legend(WC_RR_combined_close_plot)
WC_RR_abs_legend<-as_ggplot(WC_RR_abs_legend)
WC_RR_combined_close_plot<-WC_RR_combined_close_plot+
  theme(legend.position="none")


WC_RR_casual_close_plot<-ggplot(WC_RR_casual_close, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_RR_casual_close[WC_RR_casual_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=range)) +
  scale_fill_gradient2(
    low="blue",high="red",mid="grey90",
    na.value = "white",
    midpoint=1,
    limits = c(min_range_WC,max_range_WC),
    name="Relative\ncontact\nintensity",
    breaks=c(min_range_WC,1,max_range_WC),labels=c(format(round(min_RR_WC, digits=2), nsmall = 2) ,"1.00",format(round(max_RR_WC, digits=2), nsmall = 2))
  ) +
  new_scale("fill") +
  geom_tile(data = WC_RR_casual_close[WC_RR_casual_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=below)) +
  scale_fill_gradientn(colours=c("blue","grey90"),
                       na.value = NA,
                       name="Mean contacts\nby contact age",
                       limits = c(min_RR_WC,1),
                       guide=FALSE
  ) +
  new_scale("fill") +
  geom_tile(data = WC_RR_casual_close[WC_RR_casual_close$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=above)) +
  scale_fill_gradientn(colours=c("grey90","red"),
                       na.value = NA,
                       name="Mean contacts\nby contact age",
                       limits = c(1,max_RR_WC),
                       guide=FALSE
  ) +
  geom_point(data = WC_RR_casual_close[WC_RR_casual_close$contact_age=="Overall",], 
             aes(col = below), size = 8.5, shape = 15) +
  scale_color_gradientn(colours=c("blue","grey90"),
                        na.value = NA,
                        name="Relative difference\nin contact",
                        limits = c(min_RR_WC,1),
                        guide=FALSE
  ) +
  new_scale("color") +
  geom_point(data = WC_RR_casual_close[WC_RR_casual_close$contact_age=="Overall",], 
             aes(col = above), size = 8.5, shape = 15) +
  scale_color_gradientn(colours=c("grey90","red"),
                        na.value = NA,
                        name="Relative difference\nin contact",
                        limits = c(1,max_RR_WC),
                        guide=FALSE
  ) +
  geom_text(data = WC_RR_casual_close,aes(label=WC_RR_casual_close$text1,x=respondent_age, y=contact_age), size=2.2, vjust=-0.4) +
  geom_text(data = WC_RR_casual_close,aes(label=WC_RR_casual_close$text2,x=respondent_age, y=contact_age), size=1.3) +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("e) Relative intensity of contact,\n    airborne compared to droplet") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8,angle=45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=8),
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=9)) + 
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  guides(fill=FALSE) +
  guides(alpha=FALSE)+
  theme(legend.position="none")


#layout <- "
#AAAAABBBBBCDEFG
#HHHHHIIIIIJJJJJ
#KKKKKLLLLLMMMMM"

layout <- "
ABC
ABD
ABE
ABF
ABG
HIJ
HIJ
HIJ
HIJ
HIJ
KLM
KLM
KLM
KLM
KLM
"

pdf(paste0("Figure 4.pdf"), paper="a4", width=10,height=9,onefile=FALSE)
(WC_close_plot + WC_close_rate_plot + 
    WC_numbers_abs_legend + WC_numbers_abs_legend2 + WC_rates_abs_legend + WC_rates_abs_legend2 + WC_RR_abs_legend + 
    WC_casual_plot + WC_casual_rate_plot + WC_RR_casual_close_plot + 
    WC_combined_plot + WC_combined_rate_plot + WC_RR_combined_close_plot) +
  plot_layout(design=layout)
dev.off()

setwd(main_wd)
