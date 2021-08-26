require(ggplot2)
require(patchwork)
require(viridis)
require(reshape2)


#####read in data baseline##############
setwd(main_wd)
setwd("combined matrices-airborne")
KZN_casual_base_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
KZN_casual_base_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_casual_base_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
WC_casual_base_best<-read.table("WC_best.csv",header=FALSE,sep=",")
WC_casual_base_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_casual_base_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")
setwd(main_wd)
setwd("combined matrices-mtb")
KZN_mtb_base_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
KZN_mtb_base_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_mtb_base_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
WC_mtb_base_best<-read.table("WC_best.csv",header=FALSE,sep=",")
WC_mtb_base_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_mtb_base_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")

agecat_number<-length(KZN_casual_base_best[,1])


matrices<-c("KZN_casual_base_best",
            "KZN_casual_base_lb",
            "KZN_casual_base_ub",
            "KZN_mtb_base_best",
            "KZN_mtb_base_lb",     
            "KZN_mtb_base_ub",
            "WC_casual_base_best",
            "WC_casual_base_lb",
            "WC_casual_base_ub",
            "WC_mtb_base_best",    
            "WC_mtb_base_lb",
            "WC_mtb_base_ub")
j<-1
for (i in matrices) {
  data<-get(i)
  colnames(data)<-c(agecat_names,"Overall")
  
  if (j<=6) {
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

KZN_casual_base_range<-cbind(KZN_casual_base_best_melt,KZN_casual_base_lb_melt[,3],KZN_casual_base_ub_melt[,3])
KZN_mtb_base_range<-cbind(KZN_mtb_base_best_melt,KZN_mtb_base_lb_melt[,3],KZN_mtb_base_ub_melt[,3])
WC_casual_base_range<-cbind(WC_casual_base_best_melt,WC_casual_base_lb_melt[,3],WC_casual_base_ub_melt[,3])
WC_mtb_base_range<-cbind(WC_mtb_base_best_melt,WC_mtb_base_lb_melt[,3],WC_mtb_base_ub_melt[,3])

KZN_casual_base_range_rate<-cbind(KZN_casual_base_best_melt_rate,KZN_casual_base_lb_melt_rate[,3],KZN_casual_base_ub_melt_rate[,3])
KZN_mtb_base_range_rate<-cbind(KZN_mtb_base_best_melt_rate,KZN_mtb_base_lb_melt_rate[,3],KZN_mtb_base_ub_melt_rate[,3])
WC_casual_base_range_rate<-cbind(WC_casual_base_best_melt_rate,WC_casual_base_lb_melt_rate[,3],WC_casual_base_ub_melt_rate[,3])
WC_mtb_base_range_rate<-cbind(WC_mtb_base_best_melt_rate,WC_mtb_base_lb_melt_rate[,3],WC_mtb_base_ub_melt_rate[,3])

KZN_casual_base_range[,3:5]<-KZN_casual_base_range[,3:5]/60
KZN_mtb_base_range[,3:5]<-KZN_mtb_base_range[,3:5]/60
WC_casual_base_range[,3:5]<-WC_casual_base_range[,3:5]/60
WC_mtb_base_range[,3:5]<-WC_mtb_base_range[,3:5]/60

KZN_casual_base_range_rate[,3:5]<-10000*KZN_casual_base_range_rate[,3:5]
KZN_mtb_base_range_rate[,3:5]<-10000*KZN_mtb_base_range_rate[,3:5]
WC_casual_base_range_rate[,3:5]<-10000*WC_casual_base_range_rate[,3:5]
WC_mtb_base_range_rate[,3:5]<-10000*WC_mtb_base_range_rate[,3:5]

sigfig <- function(vec, digits){
  return(gsub("\\.$", "", formatC(signif(vec,digits=digits), digits=digits, format="fg", flag="#")))
}

dataframes<-c(
  "KZN_casual_base_range",
  "KZN_mtb_base_range",
  "WC_casual_base_range",
  "WC_mtb_base_range",
  "KZN_casual_base_range_rate",
  "KZN_mtb_base_range_rate",
  "WC_casual_base_range_rate",
  "WC_mtb_base_range_rate"
)

j<-1
for (i in dataframes) {
  data<-get(i)
  colnames(data)<-c(colnames(KZN_casual_base_best_melt),"lb","ub")
  data$text<-NA
  data$text[2:(agecat_number)^2]<-paste0(sigfig(data$mean_contacts[2:(agecat_number)^2],2),
                                         "\n(",sigfig(data$lb[2:(agecat_number)^2],2),
                                         "-",sigfig(data$ub[2:(agecat_number)^2],2),")")
  data$text[((agecat_number)^2 + 2):((agecat_number)^2 + agecat_number)]<-paste0(sigfig(data$mean_contacts[((agecat_number)^2 + 2):((agecat_number)^2 + agecat_number)],3),
                                                                                 "\n(",sigfig(data$lb[((agecat_number)^2 + 2):((agecat_number)^2 + agecat_number)],3),
                                                                                 "-",sigfig(data$ub[((agecat_number)^2 + 2):((agecat_number)^2 + agecat_number)],3),")")
  
  data$text[is.na(data$mean_contacts)]<-""
  
  assign(i,data)
  j<-j+1
}




#####read in data sens##############
setwd(main_wd)
setwd("matrices-casual time")
KZN_casual_sens_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
KZN_casual_sens_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_casual_sens_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
WC_casual_sens_best<-read.table("WC_best.csv",header=FALSE,sep=",")
WC_casual_sens_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_casual_sens_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")
setwd(main_wd)
setwd("combined matrices-supporting, mtb from casual time")
KZN_mtb_sens_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
KZN_mtb_sens_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_mtb_sens_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
WC_mtb_sens_best<-read.table("WC_best.csv",header=FALSE,sep=",")
WC_mtb_sens_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_mtb_sens_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")

agecat_number<-length(KZN_casual_sens_best[,1])


matrices<-c("KZN_casual_sens_best",
            "KZN_casual_sens_lb",
            "KZN_casual_sens_ub",
            "KZN_mtb_sens_best",
            "KZN_mtb_sens_lb",     
            "KZN_mtb_sens_ub",
            "WC_casual_sens_best",
            "WC_casual_sens_lb",
            "WC_casual_sens_ub",
            "WC_mtb_sens_best",    
            "WC_mtb_sens_lb",
            "WC_mtb_sens_ub")
j<-1
for (i in matrices) {
  data<-get(i)
  colnames(data)<-c(agecat_names,"Overall")
  
  if (j<=6) {
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

KZN_casual_sens_range<-cbind(KZN_casual_sens_best_melt,KZN_casual_sens_lb_melt[,3],KZN_casual_sens_ub_melt[,3])
KZN_casual_sens_range[,3:5]<-KZN_casual_sens_range[,3:5]/mc_KZN_casual_sens_best*mc_KZN_casual_base_best
KZN_mtb_sens_range<-cbind(KZN_mtb_sens_best_melt,KZN_mtb_sens_lb_melt[,3],KZN_mtb_sens_ub_melt[,3])
KZN_mtb_sens_range[,3:5]<-KZN_mtb_sens_range[,3:5]/mc_KZN_mtb_sens_best*mc_KZN_mtb_base_best
WC_casual_sens_range<-cbind(WC_casual_sens_best_melt,WC_casual_sens_lb_melt[,3],WC_casual_sens_ub_melt[,3])
WC_casual_sens_range[,3:5]<-WC_casual_sens_range[,3:5]/mc_WC_casual_sens_best*mc_WC_casual_base_best
WC_mtb_sens_range<-cbind(WC_mtb_sens_best_melt,WC_mtb_sens_lb_melt[,3],WC_mtb_sens_ub_melt[,3])
WC_mtb_sens_range[,3:5]<-WC_mtb_sens_range[,3:5]/mc_WC_mtb_sens_best*mc_WC_mtb_base_best

KZN_casual_sens_range_rate<-cbind(KZN_casual_sens_best_melt_rate,KZN_casual_sens_lb_melt_rate[,3],KZN_casual_sens_ub_melt_rate[,3])
KZN_mtb_sens_range_rate<-cbind(KZN_mtb_sens_best_melt_rate,KZN_mtb_sens_lb_melt_rate[,3],KZN_mtb_sens_ub_melt_rate[,3])
WC_casual_sens_range_rate<-cbind(WC_casual_sens_best_melt_rate,WC_casual_sens_lb_melt_rate[,3],WC_casual_sens_ub_melt_rate[,3])
WC_mtb_sens_range_rate<-cbind(WC_mtb_sens_best_melt_rate,WC_mtb_sens_lb_melt_rate[,3],WC_mtb_sens_ub_melt_rate[,3])

KZN_casual_sens_range[,3:5]<-KZN_casual_sens_range[,3:5]/60
KZN_mtb_sens_range[,3:5]<-KZN_mtb_sens_range[,3:5]/60
WC_casual_sens_range[,3:5]<-WC_casual_sens_range[,3:5]/60
WC_mtb_sens_range[,3:5]<-WC_mtb_sens_range[,3:5]/60

KZN_casual_sens_range_rate[,3:5]<-10000*KZN_casual_sens_range_rate[,3:5]
KZN_mtb_sens_range_rate[,3:5]<-10000*KZN_mtb_sens_range_rate[,3:5]
WC_casual_sens_range_rate[,3:5]<-10000*WC_casual_sens_range_rate[,3:5]
WC_mtb_sens_range_rate[,3:5]<-10000*WC_mtb_sens_range_rate[,3:5]

sigfig <- function(vec, digits){
  return(gsub("\\.$", "", formatC(signif(vec,digits=digits), digits=digits, format="fg", flag="#")))
}

dataframes<-c(
  "KZN_casual_sens_range",
  "KZN_mtb_sens_range",
  "WC_casual_sens_range",
  "WC_mtb_sens_range",
  "KZN_casual_sens_range_rate",
  "KZN_mtb_sens_range_rate",
  "WC_casual_sens_range_rate",
  "WC_mtb_sens_range_rate"
)

j<-1
for (i in dataframes) {
  data<-get(i)
  colnames(data)<-c(colnames(KZN_casual_sens_best_melt),"lb","ub")
  data$text<-NA
  data$text[2:(agecat_number)^2]<-paste0(sigfig(data$mean_contacts[2:(agecat_number)^2],2),
                                         "\n(",sigfig(data$lb[2:(agecat_number)^2],2),
                                         "-",sigfig(data$ub[2:(agecat_number)^2],2),")")
  data$text[((agecat_number)^2 + 2):((agecat_number)^2 + agecat_number)]<-paste0(sigfig(data$mean_contacts[((agecat_number)^2 + 2):((agecat_number)^2 + agecat_number)],3),
                                                                                 "\n(",sigfig(data$lb[((agecat_number)^2 + 2):((agecat_number)^2 + agecat_number)],3),
                                                                                 "-",sigfig(data$ub[((agecat_number)^2 + 2):((agecat_number)^2 + agecat_number)],3),")")
  
  data$text[is.na(data$mean_contacts)]<-""
  
  assign(i,data)
  j<-j+1
}


#############plots##################

KZN_casual_base_range_plot<-ggplot(KZN_casual_base_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_casual_base_range[KZN_casual_base_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_casual_base_range[KZN_casual_base_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(KZN_casual_base_range$mean_contacts[KZN_casual_base_range$contact_age!="Overall"],
                     KZN_casual_sens_range$mean_contacts[KZN_casual_sens_range$contact_age!="Overall"],na.rm=T)),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(KZN_casual_base_range$mean_contacts,
                     KZN_casual_sens_range$mean_contacts,na.rm=T)),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("a) Airborne transmission, baseline") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = KZN_casual_base_range,aes(label=KZN_casual_base_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  theme(legend.position="none")

KZN_mtb_base_range_plot<-ggplot(KZN_mtb_base_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_mtb_base_range[KZN_mtb_base_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_mtb_base_range[KZN_mtb_base_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(KZN_mtb_base_range$mean_contacts[KZN_mtb_base_range$contact_age!="Overall"],
                     KZN_mtb_sens_range$mean_contacts[KZN_mtb_sens_range$contact_age!="Overall"],na.rm=T)),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(KZN_mtb_base_range$mean_contacts,
                     KZN_mtb_sens_range$mean_contacts,na.rm=T)),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("c) Mycobacterium tuberculosis, baseline") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = KZN_mtb_base_range,aes(label=KZN_mtb_base_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  theme(legend.position="none")


KZN_casual_sens_range_plot<-ggplot(KZN_casual_sens_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_casual_sens_range[KZN_casual_sens_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_casual_sens_range[KZN_casual_sens_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(KZN_casual_base_range$mean_contacts[KZN_casual_base_range$contact_age!="Overall"],
                     KZN_casual_sens_range$mean_contacts[KZN_casual_sens_range$contact_age!="Overall"],na.rm=T)),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(KZN_casual_base_range$mean_contacts,
                     KZN_casual_sens_range$mean_contacts,na.rm=T)),   
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("b) Airborne transmission, sensitivity") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = KZN_casual_sens_range,aes(label=KZN_casual_sens_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8))

KZN_mtb_sens_range_plot<-ggplot(KZN_mtb_sens_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_mtb_sens_range[KZN_mtb_sens_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_mtb_sens_range[KZN_mtb_sens_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(KZN_mtb_base_range$mean_contacts[KZN_mtb_base_range$contact_age!="Overall"],
                     KZN_mtb_sens_range$sens_contacts[KZN_mtb_sens_range$contact_age!="Overall"],na.rm=T)),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(KZN_mtb_base_range$mean_contacts,
                     KZN_mtb_sens_range$mean_contacts,na.rm=T)),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("d) Mycobacterium tuberculosis, sensitivity") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = KZN_mtb_sens_range,aes(label=KZN_mtb_sens_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8))


WC_casual_base_range_plot<-ggplot(WC_casual_base_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_casual_base_range[WC_casual_base_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_casual_base_range[WC_casual_base_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(WC_casual_base_range$mean_contacts[WC_casual_base_range$contact_age!="Overall"],
                     WC_casual_sens_range$mean_contacts[WC_casual_sens_range$contact_age!="Overall"],na.rm=T)),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(WC_casual_base_range$mean_contacts,
                     WC_casual_sens_range$mean_contacts,na.rm=T)),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("a) Airborne transmission, baseline") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = WC_casual_base_range,aes(label=WC_casual_base_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  theme(legend.position="none")

WC_mtb_base_range_plot<-ggplot(WC_mtb_base_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_mtb_base_range[WC_mtb_base_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_mtb_base_range[WC_mtb_base_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(WC_mtb_base_range$mean_contacts[WC_mtb_base_range$contact_age!="Overall"],
                     WC_mtb_sens_range$mean_contacts[WC_mtb_sens_range$contact_age!="Overall"],na.rm=T)),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(WC_mtb_base_range$mean_contacts,
                     WC_mtb_sens_range$mean_contacts,na.rm=T)),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("c) Mycobacterium tuberculosis, baseline") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = WC_mtb_base_range,aes(label=WC_mtb_base_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  theme(legend.position="none")


WC_casual_sens_range_plot<-ggplot(WC_casual_sens_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_casual_sens_range[WC_casual_sens_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_casual_sens_range[WC_casual_sens_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(WC_casual_base_range$mean_contacts[WC_casual_base_range$contact_age!="Overall"],
                     WC_casual_sens_range$mean_contacts[WC_casual_sens_range$contact_age!="Overall"],na.rm=T)),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(WC_casual_base_range$mean_contacts,
                     WC_casual_sens_range$mean_contacts,na.rm=T)),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("b) Airborne transmission, sensitivity") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = WC_casual_sens_range,aes(label=WC_casual_sens_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8))

WC_mtb_sens_range_plot<-ggplot(WC_mtb_sens_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_mtb_sens_range[WC_mtb_sens_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_mtb_sens_range[WC_mtb_sens_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(WC_mtb_base_range$mean_contacts[WC_mtb_base_range$contact_age!="Overall"],
                     WC_mtb_sens_range$mean_contacts[WC_mtb_sens_range$contact_age!="Overall"],na.rm=T)),
    direction=1,option="turbo",name="Mean contacts\nby contact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(WC_mtb_base_range$mean_contacts,
                     WC_mtb_sens_range$mean_contacts,na.rm=T)),
    high="gray40",low="white",name="Mean overall\ncontacts",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("d) Mycobacterium tuberculosis, sensitivity") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = WC_mtb_sens_range,aes(label=WC_mtb_sens_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8))



setwd(main_wd)
setwd("figures")
pdf(paste0("supporting figure-age mixing, hh from casual, KZN.pdf"), paper="a4", width=9,height=7,onefile=FALSE)
(KZN_casual_base_range_plot + KZN_casual_sens_range_plot + KZN_mtb_base_range_plot + KZN_mtb_sens_range_plot) + plot_layout(ncol = 2)
dev.off()
pdf(paste0("supporting figure-age mixing, hh from casual, WC.pdf"), paper="a4", width=9,height=7,onefile=FALSE)
(WC_casual_base_range_plot + WC_casual_sens_range_plot + WC_mtb_base_range_plot + WC_mtb_sens_range_plot) + plot_layout(ncol = 2)
dev.off()
setwd(main_wd)



