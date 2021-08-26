require(ggplot2)
require(patchwork)
require(reshape2)
require(dplyr)
require(viridis)

setwd(main_wd)
setwd("respondent and contact data")
respondent_data_KZN<-read.table("KZN_respondents_casual.csv",header=TRUE,sep=",")
respondent_data_WC<-read.table("WC_respondents_casual.csv",header=TRUE,sep=",")
contact_data_KZN<-read.table("KZN_contacts_casual.csv",header=TRUE,sep=",")
contact_data_WC<-read.table("WC_contacts_casual.csv",header=TRUE,sep=",")
agecats<-length(unique(contact_data_KZN$agecat))
setwd(main_wd)
setwd("figures")

respondent_data_KZN$locationAdults100<-respondent_data_KZN$locationAdults
respondent_data_KZN$locationAdults100[respondent_data_KZN$locationPeople > cap]<-
  respondent_data_KZN$locationAdults100[respondent_data_KZN$locationPeople > cap] /
  respondent_data_KZN$locationPeople[respondent_data_KZN$locationPeople > cap] * cap
respondent_data_KZN$locationChildren100<-respondent_data_KZN$locationChildren
respondent_data_KZN$locationChildren100[respondent_data_KZN$locationPeople > cap]<-
  respondent_data_KZN$locationChildren100[respondent_data_KZN$locationPeople > cap] /
  respondent_data_KZN$locationPeople[respondent_data_KZN$locationPeople > cap] * cap
respondent_data_KZN$roundLocationAdults100<-round(respondent_data_KZN$locationAdults100)
respondent_data_KZN$roundLocationChildren100<-round(respondent_data_KZN$locationChildren100)

respondent_data_WC$locationAdults100<-respondent_data_WC$locationAdults
respondent_data_WC$locationAdults100[respondent_data_WC$locationPeople > cap]<-
  respondent_data_WC$locationAdults100[respondent_data_WC$locationPeople > cap] /
  respondent_data_WC$locationPeople[respondent_data_WC$locationPeople > cap] * cap
respondent_data_WC$locationChildren100<-respondent_data_WC$locationChildren
respondent_data_WC$locationChildren100[respondent_data_WC$locationPeople > cap]<-
  respondent_data_WC$locationChildren100[respondent_data_WC$locationPeople > cap] /
  respondent_data_WC$locationPeople[respondent_data_WC$locationPeople > cap] * cap
respondent_data_WC$roundLocationAdults100<-round(respondent_data_WC$locationAdults100)
respondent_data_WC$roundLocationChildren100<-round(respondent_data_WC$locationChildren100)


##############################KZN#########################################

total_contact_location_KZN<-contact_data_KZN %>%
  group_by(locationLabels) %>%
  summarize(locationMinutesWeighted=sum(locationMinutesWeighted),
            locationType=mean(locationType))

total_contact_location_KZN<-total_contact_location_KZN[total_contact_location_KZN$locationType!=0,]
total_contact_location_KZN$proportion_contact_time<-total_contact_location_KZN$locationMinutesWeighted/sum(total_contact_location_KZN$locationMinutesWeighted)

contact_time_age_KZN<-contact_data_KZN %>%
  group_by(agecat,locationLabels) %>%
  summarize(locationMinutesWeighted=sum(locationMinutesWeighted),
            locationType=mean(locationType))

contact_time_age_KZN<-contact_time_age_KZN[contact_time_age_KZN$locationType!=0,]

contact_time_age_KZN<-dcast(contact_time_age_KZN, locationLabels + locationType ~ agecat,value.var = "locationMinutesWeighted")


respondent_data_KZN$contact_hours_age0<-respondent_data_KZN$locationMinutesWeighted*respondent_data_KZN$locationChildren100
respondent_data_KZN$adult_contact_hours<-respondent_data_KZN$locationMinutesWeighted*respondent_data_KZN$locationAdults100
contact_time_age_KZN<-contact_time_age_KZN[order(contact_time_age_KZN$locationType),]

contact_time_child_KZN<-respondent_data_KZN %>%
  group_by(locationType) %>%
  summarize(sum(contact_hours_age0))
contact_time_child_KZN<-contact_time_child_KZN[contact_time_child_KZN$locationType!=0,]

contact_time_adult_KZN<-respondent_data_KZN %>%
  group_by(locationType) %>%
  summarize(sum(adult_contact_hours))
contact_time_adult_KZN<-contact_time_adult_KZN[contact_time_adult_KZN$locationType!=0,]

contact_time_age_KZN<-cbind(contact_time_age_KZN[,1:2],contact_time_child_KZN[,2],contact_time_age_KZN[,3:7],contact_time_adult_KZN[,2])
colnames(contact_time_age_KZN)<-c("locationLabels","locationType",paste0("contact_hours_age",seq(0,agecats)),"adult_contact_time")

contact_time_age_KZN$reported_adult_contact_time<-rowSums(contact_time_age_KZN[,4:8],na.rm=TRUE)

contact_time_age_KZN$contact_hours_age1<-contact_time_age_KZN$contact_hours_age1*
  contact_time_age_KZN$adult_contact_time/contact_time_age_KZN$reported_adult_contact_time
contact_time_age_KZN$contact_hours_age2<-contact_time_age_KZN$contact_hours_age2*
  contact_time_age_KZN$adult_contact_time/contact_time_age_KZN$reported_adult_contact_time
contact_time_age_KZN$contact_hours_age3<-contact_time_age_KZN$contact_hours_age3*
  contact_time_age_KZN$adult_contact_time/contact_time_age_KZN$reported_adult_contact_time
contact_time_age_KZN$contact_hours_age4<-contact_time_age_KZN$contact_hours_age4*
  contact_time_age_KZN$adult_contact_time/contact_time_age_KZN$reported_adult_contact_time
contact_time_age_KZN$contact_hours_age5<-contact_time_age_KZN$contact_hours_age5*
  contact_time_age_KZN$adult_contact_time/contact_time_age_KZN$reported_adult_contact_time

contact_time_age_KZN$total<-rowSums(contact_time_age_KZN[,3:8],na.rm=TRUE)
contact_time_age_KZN$proportion_age0<-contact_time_age_KZN$contact_hours_age0/contact_time_age_KZN$total
contact_time_age_KZN$proportion_age1<-contact_time_age_KZN$contact_hours_age1/contact_time_age_KZN$total
contact_time_age_KZN$proportion_age2<-contact_time_age_KZN$contact_hours_age2/contact_time_age_KZN$total
contact_time_age_KZN$proportion_age3<-contact_time_age_KZN$contact_hours_age3/contact_time_age_KZN$total
contact_time_age_KZN$proportion_age4<-contact_time_age_KZN$contact_hours_age4/contact_time_age_KZN$total
contact_time_age_KZN$proportion_age5<-contact_time_age_KZN$contact_hours_age5/contact_time_age_KZN$total

contact_time_age_KZN$proportion_age0[is.na(contact_time_age_KZN$proportion_age0)]<-0
contact_time_age_KZN$proportion_age1[is.na(contact_time_age_KZN$proportion_age1)]<-0
contact_time_age_KZN$proportion_age2[is.na(contact_time_age_KZN$proportion_age2)]<-0
contact_time_age_KZN$proportion_age3[is.na(contact_time_age_KZN$proportion_age3)]<-0
contact_time_age_KZN$proportion_age4[is.na(contact_time_age_KZN$proportion_age4)]<-0
contact_time_age_KZN$proportion_age5[is.na(contact_time_age_KZN$proportion_age5)]<-0

total_contact_location_KZN<-total_contact_location_KZN[order(total_contact_location_KZN$locationType),]

graph_data_KZN<-cbind(contact_time_age_KZN[,1:2],total_contact_location_KZN[,4],contact_time_age_KZN[,12:17])
graph_data_KZN<-graph_data_KZN[order(-graph_data_KZN$proportion_contact_time),]

graph_data_KZN$locationLabels[graph_data_KZN$locationLabels=="in Bar / Spotini / Nightclub"]<-"Bar/club"
graph_data_KZN$locationLabels[graph_data_KZN$locationLabels=="Spaza"]<-"Spaza shop"
graph_data_KZN$locationLabels[graph_data_KZN$locationLabels=="Guest House"]<-"Guest house"
graph_data_KZN$locationLabels[graph_data_KZN$locationLabels=="Bakkie"]<-"Bakkie truck"
graph_data_KZN$locationLabels[graph_data_KZN$locationLabels=="Counselling Centre"]<-"Counselling centre"
graph_data_KZN$locationLabels[graph_data_KZN$locationLabels=="Taxi"]<-"Minibus taxi"
graph_data_KZN$locationLabels[graph_data_KZN$locationLabels=="Prefer not to say/missing"]<-"Missing"
graph_data_KZN$locationLabels<-factor(graph_data_KZN$locationLabels,levels=graph_data_KZN$locationLabels)

graph_data_KZN<-melt(graph_data_KZN,id=(c("locationLabels","locationType","proportion_contact_time")))
graph_data_KZN$variable<-factor(graph_data_KZN$variable,levels=c("proportion_age5","proportion_age4","proportion_age3","proportion_age2","proportion_age1","proportion_age0"))
levels(graph_data_KZN$variable)[levels(graph_data_KZN$variable)=="proportion_age0"] <- "0-14"
levels(graph_data_KZN$variable)[levels(graph_data_KZN$variable)=="proportion_age1"] <- "15-19"
levels(graph_data_KZN$variable)[levels(graph_data_KZN$variable)=="proportion_age2"] <- "20-29"
levels(graph_data_KZN$variable)[levels(graph_data_KZN$variable)=="proportion_age3"] <- "30-39"
levels(graph_data_KZN$variable)[levels(graph_data_KZN$variable)=="proportion_age4"] <- "40-49"
levels(graph_data_KZN$variable)[levels(graph_data_KZN$variable)=="proportion_age5"] <- "50+"


##############################WC######################################################

total_contact_location_WC<-contact_data_WC %>%
  group_by(locationLabels) %>%
  summarize(locationMinutesWeighted=sum(locationMinutesWeighted),
            locationType=mean(locationType))

total_contact_location_WC<-total_contact_location_WC[total_contact_location_WC$locationType!=0,]
total_contact_location_WC$proportion_contact_time<-total_contact_location_WC$locationMinutesWeighted/sum(total_contact_location_WC$locationMinutesWeighted)

contact_time_age_WC<-contact_data_WC %>%
  group_by(agecat,locationLabels) %>%
  summarize(locationMinutesWeighted=sum(locationMinutesWeighted),
            locationType=mean(locationType))

contact_time_age_WC<-contact_time_age_WC[contact_time_age_WC$locationType!=0,]

#contact_time_age_WC<-reshape(contact_time_age_WC,idvar=c("locationLabels","locationType"),timevar="agecat",direction="wide")
contact_time_age_WC<-dcast(contact_time_age_WC, locationLabels + locationType ~ agecat,value.var = "locationMinutesWeighted")


respondent_data_WC$contact_hours_age0<-respondent_data_WC$locationMinutesWeighted*respondent_data_WC$locationChildren100
respondent_data_WC$adult_contact_hours<-respondent_data_WC$locationMinutesWeighted*respondent_data_WC$locationAdults100
contact_time_age_WC<-contact_time_age_WC[order(contact_time_age_WC$locationType),]

contact_time_child_WC<-respondent_data_WC %>%
  group_by(locationType) %>%
  summarize(sum(contact_hours_age0))
contact_time_child_WC<-contact_time_child_WC[contact_time_child_WC$locationType!=0,]

contact_time_adult_WC<-respondent_data_WC %>%
  group_by(locationType) %>%
  summarize(sum(adult_contact_hours))
contact_time_adult_WC<-contact_time_adult_WC[contact_time_adult_WC$locationType!=0,]

contact_time_age_WC<-cbind(contact_time_age_WC[,1:2],contact_time_child_WC[,2],contact_time_age_WC[,3:7],contact_time_adult_WC[,2])
colnames(contact_time_age_WC)<-c("locationLabels","locationType",paste0("contact_hours_age",seq(0,agecats)),"adult_contact_time")

contact_time_age_WC$reported_adult_contact_time<-rowSums(contact_time_age_WC[,4:8],na.rm=TRUE)

contact_time_age_WC$contact_hours_age1<-contact_time_age_WC$contact_hours_age1*
  contact_time_age_WC$adult_contact_time/contact_time_age_WC$reported_adult_contact_time
contact_time_age_WC$contact_hours_age2<-contact_time_age_WC$contact_hours_age2*
  contact_time_age_WC$adult_contact_time/contact_time_age_WC$reported_adult_contact_time
contact_time_age_WC$contact_hours_age3<-contact_time_age_WC$contact_hours_age3*
  contact_time_age_WC$adult_contact_time/contact_time_age_WC$reported_adult_contact_time
contact_time_age_WC$contact_hours_age4<-contact_time_age_WC$contact_hours_age4*
  contact_time_age_WC$adult_contact_time/contact_time_age_WC$reported_adult_contact_time
contact_time_age_WC$contact_hours_age5<-contact_time_age_WC$contact_hours_age5*
  contact_time_age_WC$adult_contact_time/contact_time_age_WC$reported_adult_contact_time

contact_time_age_WC$total<-rowSums(contact_time_age_WC[,3:8],na.rm=TRUE)
contact_time_age_WC$proportion_age0<-contact_time_age_WC$contact_hours_age0/contact_time_age_WC$total
contact_time_age_WC$proportion_age1<-contact_time_age_WC$contact_hours_age1/contact_time_age_WC$total
contact_time_age_WC$proportion_age2<-contact_time_age_WC$contact_hours_age2/contact_time_age_WC$total
contact_time_age_WC$proportion_age3<-contact_time_age_WC$contact_hours_age3/contact_time_age_WC$total
contact_time_age_WC$proportion_age4<-contact_time_age_WC$contact_hours_age4/contact_time_age_WC$total
contact_time_age_WC$proportion_age5<-contact_time_age_WC$contact_hours_age5/contact_time_age_WC$total

contact_time_age_WC$proportion_age0[is.na(contact_time_age_WC$proportion_age0)]<-0
contact_time_age_WC$proportion_age1[is.na(contact_time_age_WC$proportion_age1)]<-0
contact_time_age_WC$proportion_age2[is.na(contact_time_age_WC$proportion_age2)]<-0
contact_time_age_WC$proportion_age3[is.na(contact_time_age_WC$proportion_age3)]<-0
contact_time_age_WC$proportion_age4[is.na(contact_time_age_WC$proportion_age4)]<-0
contact_time_age_WC$proportion_age5[is.na(contact_time_age_WC$proportion_age5)]<-0

total_contact_location_WC<-total_contact_location_WC[order(total_contact_location_WC$locationType),]

graph_data_WC<-cbind(contact_time_age_WC[,1:2],total_contact_location_WC[,4],contact_time_age_WC[,12:17])
graph_data_WC<-graph_data_WC[order(-graph_data_WC$proportion_contact_time),]

graph_data_WC$locationLabels[graph_data_WC$locationLabels=="Other home on your plot"]<-"Other house on plot"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="Other home off your plot"]<-"House off plot"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="School - high school"]<-"High School"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="Bar/shebeen/nightclub"]<-"Bar/club"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="CrÃ©che/daycare/after school care"]<-"Creche/after school care"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="School - primary school"]<-"Primary school"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="Internet CafÃ©"]<-"Internet cafe"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="Barbershop/Salon"]<-"Salon/barber"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="Library/community hall/youth centre"]<-"Community building"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="Clinic/hospital/medical facility"]<-"Medical facility"
graph_data_WC$locationLabels[graph_data_WC$locationLabels=="Shop - clothes or cosmetics"]<-"Shop - clothes/cosmetics"


graph_data_WC$locationLabels<-factor(graph_data_WC$locationLabels,levels=graph_data_WC$locationLabels)

graph_data_WC<-melt(graph_data_WC,id=(c("locationLabels","locationType","proportion_contact_time")))
graph_data_WC$variable<-factor(graph_data_WC$variable,levels=c("proportion_age5","proportion_age4","proportion_age3","proportion_age2","proportion_age1","proportion_age0"))
levels(graph_data_WC$variable)[levels(graph_data_WC$variable)=="proportion_age0"] <- "0-14"
levels(graph_data_WC$variable)[levels(graph_data_WC$variable)=="proportion_age1"] <- "15-19"
levels(graph_data_WC$variable)[levels(graph_data_WC$variable)=="proportion_age2"] <- "20-29"
levels(graph_data_WC$variable)[levels(graph_data_WC$variable)=="proportion_age3"] <- "30-39"
levels(graph_data_WC$variable)[levels(graph_data_WC$variable)=="proportion_age4"] <- "40-49"
levels(graph_data_WC$variable)[levels(graph_data_WC$variable)=="proportion_age5"] <- "50+"

graph_data_WC<-graph_data_WC[graph_data_WC$locationType!=23,]

#####################################graphs###########################################

scale_factor<-0.4
KZN_plot<-ggplot(data=graph_data_KZN) +
  theme_bw() +
  geom_col(aes(x=locationLabels,y=value,fill=variable)) +
  geom_point(aes(x=locationLabels,y=proportion_contact_time/scale_factor),
             size=2.5, col="black", bg="white",pch=21,stroke=1) +
  scale_y_continuous(name="Age distribution of people present",
                     sec.axis = sec_axis(~.*scale_factor, name="Proportion of contact time")) +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) +
  ggtitle ("a) Contact time by location type, KwaZulu-Natal") +
  theme(plot.title = element_text(size = 14)) +
  xlab("Location type") +
  scale_fill_viridis(name="Age group (years)",
                     option="turbo", discrete = TRUE)

scale_factor<-0.4
WC_plot<-ggplot(data=graph_data_WC) +
  theme_bw() +
  geom_col(aes(x=locationLabels,y=value,fill=variable)) +
  geom_point(aes(x=locationLabels,y=proportion_contact_time/scale_factor),
             size=2.5, col="black", bg="white",pch=21,stroke=1) +
  scale_y_continuous(name="Age distribution of people present",
                     sec.axis = sec_axis(~.*scale_factor, name="Proportion of contact time")) +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) +
  ggtitle ("b) Contact time by location type, Western Cape") +
  theme(plot.title = element_text(size = 14)) +
  xlab("Location type") +
  scale_fill_viridis(name="Age group (years)",
                     option="turbo", discrete = TRUE)


pdf(paste0("supporting figure-contact time by location.pdf"), paper="a4", width=8,height=12,onefile=FALSE)
(KZN_plot + WC_plot) + plot_layout(ncol = 1)
dev.off()