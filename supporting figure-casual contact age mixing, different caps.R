require(ggplot2)
require(patchwork)
require(viridis)
require(reshape2)


#####read in data##############
setwd(main_wd)
setwd("matrices-casual time")
KZN_100_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
KZN_100_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_100_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
WC_100_best<-read.table("WC_best.csv",header=FALSE,sep=",")
WC_100_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_100_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")
setwd(main_wd)
setwd("matrices-casual time-cap 20")
KZN_20_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
KZN_20_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_20_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
WC_20_best<-read.table("WC_best.csv",header=FALSE,sep=",")
WC_20_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_20_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")
setwd(main_wd)
setwd("matrices-casual time-no cap")
KZN_no_cap_best<-read.table("KZN_best.csv",header=FALSE,sep=",")
KZN_no_cap_lb<-read.table("KZN_lb.csv",header=FALSE,sep=",")
KZN_no_cap_ub<-read.table("KZN_ub.csv",header=FALSE,sep=",")
WC_no_cap_best<-read.table("WC_best.csv",header=FALSE,sep=",")
WC_no_cap_lb<-read.table("WC_lb.csv",header=FALSE,sep=",")
WC_no_cap_ub<-read.table("WC_ub.csv",header=FALSE,sep=",")

agecat_number<-length(KZN_100_best[,1])

#to create KZN_all_matrix WC_all_matrix

matrices<-ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']
j<-1
for (i in matrices) {
  data<-get(i)
  colnames(data)<-c(agecat_names,"Overall")
  
  if (j<=9) {
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

KZN_100_range<-cbind(KZN_100_best_melt,KZN_100_lb_melt[,3],KZN_100_ub_melt[,3])
KZN_20_range<-cbind(KZN_20_best_melt,KZN_20_lb_melt[,3],KZN_20_ub_melt[,3])
KZN_20_range[,3:5]<-KZN_20_range[,3:5]/mc_KZN_20_best*mc_KZN_100_best
KZN_no_cap_range<-cbind(KZN_no_cap_best_melt,KZN_no_cap_lb_melt[,3],KZN_no_cap_ub_melt[,3])
KZN_no_cap_range[,3:5]<-KZN_no_cap_range[,3:5]/mc_KZN_no_cap_best*mc_KZN_100_best
WC_100_range<-cbind(WC_100_best_melt,WC_100_lb_melt[,3],WC_100_ub_melt[,3])
WC_20_range<-cbind(WC_20_best_melt,WC_20_lb_melt[,3],WC_20_ub_melt[,3])
WC_20_range[,3:5]<-WC_20_range[,3:5]/mc_WC_20_best*mc_WC_100_best
WC_no_cap_range<-cbind(WC_no_cap_best_melt,WC_no_cap_lb_melt[,3],WC_no_cap_ub_melt[,3])
WC_no_cap_range[,3:5]<-WC_no_cap_range[,3:5]/mc_WC_no_cap_best*mc_WC_100_best

KZN_100_range_rate<-cbind(KZN_100_best_melt_rate,KZN_100_lb_melt_rate[,3],KZN_100_ub_melt_rate[,3])
KZN_20_range_rate<-cbind(KZN_20_best_melt_rate,KZN_20_lb_melt_rate[,3],KZN_20_ub_melt_rate[,3])
KZN_no_cap_range_rate<-cbind(KZN_no_cap_best_melt_rate,KZN_no_cap_lb_melt_rate[,3],KZN_no_cap_ub_melt_rate[,3])
WC_100_range_rate<-cbind(WC_100_best_melt_rate,WC_100_lb_melt_rate[,3],WC_100_ub_melt_rate[,3])
WC_20_range_rate<-cbind(WC_20_best_melt_rate,WC_20_lb_melt_rate[,3],WC_20_ub_melt_rate[,3])
WC_no_cap_range_rate<-cbind(WC_no_cap_best_melt_rate,WC_no_cap_lb_melt_rate[,3],WC_no_cap_ub_melt_rate[,3])

KZN_100_range[,3:5]<-KZN_100_range[,3:5]/60
KZN_20_range[,3:5]<-KZN_20_range[,3:5]/60
KZN_no_cap_range[,3:5]<-KZN_no_cap_range[,3:5]/60
WC_100_range[,3:5]<-WC_100_range[,3:5]/60
WC_20_range[,3:5]<-WC_20_range[,3:5]/60
WC_no_cap_range[,3:5]<-WC_no_cap_range[,3:5]/60

KZN_100_range_rate[,3:5]<-10000*KZN_100_range_rate[,3:5]
KZN_20_range_rate[,3:5]<-10000*KZN_20_range_rate[,3:5]
KZN_no_cap_range_rate[,3:5]<-10000*KZN_no_cap_range_rate[,3:5]
WC_100_range_rate[,3:5]<-10000*WC_100_range_rate[,3:5]
WC_20_range_rate[,3:5]<-10000*WC_20_range_rate[,3:5]
WC_no_cap_range_rate[,3:5]<-10000*WC_no_cap_range_rate[,3:5]

sigfig <- function(vec, digits){
  return(gsub("\\.$", "", formatC(signif(vec,digits=digits), digits=digits, format="fg", flag="#")))
}

dataframes<-c(
  "KZN_100_range",
  "KZN_20_range",
  "KZN_no_cap_range",
  "WC_100_range",
  "WC_20_range",
  "WC_no_cap_range",
  "KZN_100_range_rate",
  "KZN_20_range_rate",
  "KZN_no_cap_range_rate",
  "WC_100_range_rate",
  "WC_20_range_rate",
  "WC_no_cap_range_rate"
)

j<-1
for (i in dataframes) {
  data<-get(i)
  colnames(data)<-c(colnames(KZN_100_best_melt),"lb","ub")
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

KZN_100_range_plot<-ggplot(KZN_100_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_100_range[KZN_100_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_100_range[KZN_100_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(KZN_100_range$mean_contacts[KZN_100_range$contact_age!="Overall"],
                     KZN_20_range$mean_contacts[KZN_20_range$contact_age!="Overall"],
                     KZN_no_cap_range$mean_contacts[KZN_no_cap_range$contact_age!="Overall"],na.rm=TRUE)),
    direction=1,option="turbo",name="Mean contact\nhours by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(KZN_100_range$mean_contacts,
                     KZN_20_range$mean_contacts,
                     KZN_no_cap_range$mean_contacts,na.rm=TRUE)),
    high="gray40",low="white",name="Mean overall\ncontact hours",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("a) Cap = 100 people, KZN") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = KZN_100_range,aes(label=KZN_100_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8))

KZN_20_range_plot<-ggplot(KZN_20_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_20_range[KZN_20_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_20_range[KZN_20_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(KZN_100_range$mean_contacts[KZN_100_range$contact_age!="Overall"],
                     KZN_20_range$mean_contacts[KZN_20_range$contact_age!="Overall"],
                     KZN_no_cap_range$mean_contacts[KZN_no_cap_range$contact_age!="Overall"],na.rm=TRUE)),
    direction=1,option="turbo",name="Mean contact\nhours by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(KZN_100_range$mean_contacts,
                     KZN_20_range$mean_contacts,
                     KZN_no_cap_range$mean_contacts,na.rm=TRUE)),
    high="gray40",low="white",name="Mean overall\ncontact hours",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("c) Cap = 20 people, KZN") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = KZN_20_range,aes(label=KZN_20_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  theme(legend.position="none")

KZN_no_cap_range_plot<-ggplot(KZN_no_cap_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = KZN_no_cap_range[KZN_no_cap_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = KZN_no_cap_range[KZN_no_cap_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(KZN_100_range$mean_contacts[KZN_100_range$contact_age!="Overall"],
                     KZN_20_range$mean_contacts[KZN_20_range$contact_age!="Overall"],
                     KZN_no_cap_range$mean_contacts[KZN_no_cap_range$contact_age!="Overall"],na.rm=TRUE)),
    direction=1,option="turbo",name="Mean contact\nhours by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(KZN_100_range$mean_contacts,
                     KZN_20_range$mean_contacts,
                     KZN_no_cap_range$mean_contacts,na.rm=TRUE)),
    high="gray40",low="white",name="Mean overall\ncontact hours",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("e) No cap, KZN") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = KZN_no_cap_range,aes(label=KZN_no_cap_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  theme(legend.position="none")

WC_100_range_plot<-ggplot(WC_100_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_100_range[WC_100_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_100_range[WC_100_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(WC_100_range$mean_contacts[WC_100_range$contact_age!="Overall"],
                     WC_20_range$mean_contacts[WC_20_range$contact_age!="Overall"],
                     WC_no_cap_range$mean_contacts[WC_no_cap_range$contact_age!="Overall"],na.rm=TRUE)),
    direction=1,option="turbo",name="Mean contact\nhours by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(WC_100_range$mean_contacts,
                     WC_20_range$mean_contacts,
                     WC_no_cap_range$mean_contacts,na.rm=TRUE)),
    high="gray40",low="white",name="Mean overall\ncontact hours",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("b) Cap = 100 people, WC") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = WC_100_range,aes(label=WC_100_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8))

WC_20_range_plot<-ggplot(WC_20_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_20_range[WC_20_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_20_range[WC_20_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(WC_100_range$mean_contacts[WC_100_range$contact_age!="Overall"],
                     WC_20_range$mean_contacts[WC_20_range$contact_age!="Overall"],
                     WC_no_cap_range$mean_contacts[WC_no_cap_range$contact_age!="Overall"],na.rm=TRUE)),
    direction=1,option="turbo",name="Mean contact\nhours by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(WC_100_range$mean_contacts,
                     WC_20_range$mean_contacts,
                     WC_no_cap_range$mean_contacts,na.rm=TRUE)),
    high="gray40",low="white",name="Mean overall\ncontact hours",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("d) Cap = 20 people, WC") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = WC_20_range,aes(label=WC_20_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  theme(legend.position="none")

WC_no_cap_range_plot<-ggplot(WC_no_cap_range, aes(x = respondent_age, y = contact_age)) +
  theme_bw() +
  geom_tile(data = WC_no_cap_range[WC_no_cap_range$contact_age!="Overall",], aes(x=respondent_age, y=contact_age, fill=mean_contacts),alpha=0.7) +
  geom_point(data = WC_no_cap_range[WC_no_cap_range$contact_age=="Overall",], 
             aes(col = mean_contacts), size = 10, shape = 15) +
  scale_fill_viridis_c(
    limits = c(0,max(WC_100_range$mean_contacts[WC_100_range$contact_age!="Overall"],
                     WC_20_range$mean_contacts[WC_20_range$contact_age!="Overall"],
                     WC_no_cap_range$mean_contacts[WC_no_cap_range$contact_age!="Overall"],na.rm=TRUE)),
    direction=1,option="turbo",name="Mean contact\nhours by\ncontact age",alpha=0.7,
    na.value = "white") +
  scale_color_gradient(
    limits = c(0,max(WC_100_range$mean_contacts,
                     WC_20_range$mean_contacts,
                     WC_no_cap_range$mean_contacts,na.rm=TRUE)),
    high="gray40",low="white",name="Mean overall\ncontact hours",
    na.value = "white") +
  xlab("Respondent age (years)") +
  ylab("Contact age (years)") +
  ggtitle("f) No cap, WC") +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x  = element_text(size=8, angle=45, vjust = 1, hjust = 1)) +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y  = element_text(size=8)) +
  geom_text(data = WC_no_cap_range,aes(label=WC_no_cap_range$text,x=respondent_age, y=contact_age), size=1.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size=10)) +
  guides(color = guide_colourbar(order = 1),
         fill = guide_colourbar(order = 2)) + 
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(legend.title = element_text(size=8)) +
  theme(legend.text = element_text(size=8)) +
  theme(legend.position="none")

setwd(main_wd)
setwd("figures")
pdf(paste0("supporting figure-age mixing, different caps.pdf"), paper="a4", width=9,height=9,onefile=FALSE)
(KZN_100_range_plot + WC_100_range_plot + KZN_20_range_plot + WC_20_range_plot + KZN_no_cap_range_plot + WC_no_cap_range_plot) + plot_layout(ncol = 2)
dev.off()
setwd(main_wd)
