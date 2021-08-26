require(splitstackshape)
require(expss)
require(Rmisc)
require(dplyr)
require(abind)

#####set cap#####
cap<-100

##########################read in data

setwd("respondent and contact data")
if (location==1) {
  respondent_data<-"KZN_respondents_casual.csv"
  contact_data<-"KZN_contacts_casual.csv"
  pop_age_dist<-KZN_pop_age_dist
} else {
  respondent_data<-"WC_respondents_casual.csv"
  contact_data<-"WC_contacts_casual.csv"
  pop_age_dist<-WC_pop_age_dist
}


respondent_data<-read.table(respondent_data,header=TRUE,sep=",")
contact_data<-read.table(contact_data,header=TRUE,sep=",")

respondent_data$locationAdults100<-respondent_data$locationAdults
respondent_data$locationAdults100[respondent_data$locationPeople > cap]<-
  respondent_data$locationAdults100[respondent_data$locationPeople > cap] /
  respondent_data$locationPeople[respondent_data$locationPeople > cap] * cap
respondent_data$locationChildren100<-respondent_data$locationChildren
respondent_data$locationChildren100[respondent_data$locationPeople > cap]<-
  respondent_data$locationChildren100[respondent_data$locationPeople > cap] /
  respondent_data$locationPeople[respondent_data$locationPeople > cap] * cap
respondent_data$roundLocationAdults100<-round(respondent_data$locationAdults100)
respondent_data$roundLocationChildren100<-round(respondent_data$locationChildren100)

respondent_number<-length(respondent_data$id)
agecat_number<-max(respondent_data$age)

#################################main results

best_estimate<-as.data.frame(matrix(NA,nrow=agecat_number+1,ncol=agecat_number+2))
colnames(best_estimate)<-c("respondent_age",agecat_names)
best_estimate[,1]<-agecat_names

loctype_num<-length(unique(respondent_data$locationType))
adult_prop<-as.data.frame(matrix(NA,nrow=loctype_num,ncol=agecat_number+1))
colnames(adult_prop)<-c("locationType",paste0("a",seq(1:agecat_number)))
adult_prop[,1]<-unique(respondent_data$locationType)

i<-1
for (loctype in unique(respondent_data$locationType)) {
  contact_data_subset<-contact_data[which(contact_data$locationType==loctype),]
  total_min<-sum(contact_data_subset$locationMinutesWeighted)
  for (agecat in seq(1,agecat_number)) {
    contact_data_subset2<-contact_data_subset[which(contact_data_subset$agecat==agecat),]
    adult_prop[i,agecat+1]<-sum(contact_data_subset2$locationMinutesWeighted)/total_min
  }
  i<-i+1
}

full_data <- merge(respondent_data,adult_prop,by="locationType")
contact_numbers<-as.data.frame(matrix(NA,nrow=length(full_data[,1]),ncol=agecat_number))
colnames(contact_numbers)<-paste0("c_a",seq(1:agecat_number))
full_data$c_a0<-full_data$locationChildren100*full_data$locationMinutes
for (agecat in seq(1:agecat_number)) {
  contact_numbers[,agecat]<-full_data[,agecat+13] * full_data$locationAdults100 * full_data$locationMinutes
}


best_contact_rates_home<-matrix(data=NA,nrow=agecat_number,ncol=agecat_number+1)
best_contact_numbers_home<-matrix(data=NA,nrow=agecat_number,ncol=agecat_number+1)
best_contact_numbers_sym_home<-matrix(data=NA,nrow=agecat_number+1,ncol=agecat_number+1)
best_contact_rates_sym_home<-matrix(data=NA,nrow=agecat_number+1,ncol=agecat_number+1)
best_contact_rates_sym_flip_home<-matrix(data=NA,nrow=agecat_number+1,ncol=agecat_number+1)

best_contact_rates_other<-matrix(data=NA,nrow=agecat_number,ncol=agecat_number+1)
best_contact_numbers_other<-matrix(data=NA,nrow=agecat_number,ncol=agecat_number+1)
best_contact_numbers_sym_other<-matrix(data=NA,nrow=agecat_number+1,ncol=agecat_number+1)
best_contact_rates_sym_other<-matrix(data=NA,nrow=agecat_number+1,ncol=agecat_number+1)
best_contact_rates_sym_flip_other<-matrix(data=NA,nrow=agecat_number+1,ncol=agecat_number+1)


#home
best_contact_data<-cbind(full_data$id,full_data$agecat,full_data$locationType,full_data$sampling_weight,full_data$c_a0,contact_numbers)
colnames(best_contact_data)<-c("id","agecat","locationType","sampling_weight","c_a0",colnames(contact_numbers))
best_contact_data<-best_contact_data[which(best_contact_data$locationType==0),]
best_contact_data<-best_contact_data %>%
  group_by(id,agecat,sampling_weight) %>%
  summarize(sum(c_a0),sum(c_a1),sum(c_a2),sum(c_a3),sum(c_a4),sum(c_a5))
best_contact_data<-merge(best_contact_data,full_data[!duplicated(full_data$id),c(2,3,5)],all=TRUE)
best_contact_data[is.na(best_contact_data)] = 0

for (agecat in seq(1,agecat_number)) {
  best_contact_data_subset<-best_contact_data[which(best_contact_data$agecat==agecat),]
  best_contact_rates_home[agecat,1]<-weighted.mean(best_contact_data_subset$`sum(c_a0)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_home[agecat,2]<-weighted.mean(best_contact_data_subset$`sum(c_a1)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_home[agecat,3]<-weighted.mean(best_contact_data_subset$`sum(c_a2)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_home[agecat,4]<-weighted.mean(best_contact_data_subset$`sum(c_a3)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_home[agecat,5]<-weighted.mean(best_contact_data_subset$`sum(c_a4)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_home[agecat,6]<-weighted.mean(best_contact_data_subset$`sum(c_a5)`,1/best_contact_data_subset$sampling_weight)
}

for (i in seq(1,agecat_number)) {
  best_contact_numbers_home[i,]<-best_contact_rates_home[i,]*pop_age_dist[i+1]
}

for (i in seq(1,agecat_number)) {
  for (j in seq(1,agecat_number)) {
    best_contact_numbers_sym_home[i+1,j+1]<-(best_contact_numbers_home[i,j+1] + best_contact_numbers_home[j,i+1])/2  
  }
}

for (i in seq(1,agecat_number)) {
  best_contact_numbers_sym_home[1,i+1]<-best_contact_numbers_home[i,1]
  best_contact_numbers_sym_home[i+1,1]<-best_contact_numbers_home[i,1]
}

for (i in seq(1,agecat_number+1)) {
  best_contact_rates_sym_home[i,]<-best_contact_numbers_sym_home[i,]/pop_age_dist[i]
}

for (i in seq(1,agecat_number+1)) {
  for (j in seq(1,agecat_number+1)) {
    best_contact_rates_sym_flip_home[i,j]<-best_contact_rates_sym_home[j,i]
  }
}

#other
best_contact_data<-cbind(full_data$id,full_data$agecat,full_data$locationType,full_data$sampling_weight,full_data$c_a0,contact_numbers)
colnames(best_contact_data)<-c("id","agecat","locationType","sampling_weight","c_a0",colnames(contact_numbers))
best_contact_data<-best_contact_data[which(best_contact_data$locationType!=0),]
best_contact_data<-best_contact_data %>%
  group_by(id,agecat,sampling_weight) %>%
  summarize(sum(c_a0),sum(c_a1),sum(c_a2),sum(c_a3),sum(c_a4),sum(c_a5))
best_contact_data<-merge(best_contact_data,full_data[!duplicated(full_data$id),c(2,3,5)],all=TRUE)
best_contact_data[is.na(best_contact_data)] = 0

for (agecat in seq(1,agecat_number)) {
  best_contact_data_subset<-best_contact_data[which(best_contact_data$agecat==agecat),]
  best_contact_rates_other[agecat,1]<-weighted.mean(best_contact_data_subset$`sum(c_a0)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_other[agecat,2]<-weighted.mean(best_contact_data_subset$`sum(c_a1)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_other[agecat,3]<-weighted.mean(best_contact_data_subset$`sum(c_a2)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_other[agecat,4]<-weighted.mean(best_contact_data_subset$`sum(c_a3)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_other[agecat,5]<-weighted.mean(best_contact_data_subset$`sum(c_a4)`,1/best_contact_data_subset$sampling_weight)
  best_contact_rates_other[agecat,6]<-weighted.mean(best_contact_data_subset$`sum(c_a5)`,1/best_contact_data_subset$sampling_weight)
}

for (i in seq(1,agecat_number)) {
  best_contact_numbers_other[i,]<-best_contact_rates_other[i,]*pop_age_dist[i+1]
}

for (i in seq(1,agecat_number)) {
  for (j in seq(1,agecat_number)) {
    best_contact_numbers_sym_other[i+1,j+1]<-(best_contact_numbers_other[i,j+1] + best_contact_numbers_other[j,i+1])/2  
  }
}

for (i in seq(1,agecat_number)) {
  best_contact_numbers_sym_other[1,i+1]<-best_contact_numbers_other[i,1]
  best_contact_numbers_sym_other[i+1,1]<-best_contact_numbers_other[i,1]
}

for (i in seq(1,agecat_number+1)) {
  best_contact_rates_sym_other[i,]<-best_contact_numbers_sym_other[i,]/pop_age_dist[i]
}

for (i in seq(1,agecat_number+1)) {
  for (j in seq(1,agecat_number+1)) {
    best_contact_rates_sym_flip_other[i,j]<-best_contact_rates_sym_other[j,i]
  }
}

best_contact_rates_sym_overall<-best_contact_rates_sym_other + best_contact_rates_sym_home


##########################create arrays

bootstrapped_contact_rates_home<-array(data=NA,dim=c(agecat_number,agecat_number+1,bootstrap_number))
bootstrapped_contact_numbers_home<-array(data=NA,dim=c(agecat_number,agecat_number+1,bootstrap_number))
bootstrapped_contact_numbers_sym_home<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))
bootstrapped_contact_rates_sym_home<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))
bootstrapped_contact_rates_sym_flip_home<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))

bootstrapped_contact_rates_other<-array(data=NA,dim=c(agecat_number,agecat_number+1,bootstrap_number))
bootstrapped_contact_numbers_other<-array(data=NA,dim=c(agecat_number,agecat_number+1,bootstrap_number))
bootstrapped_contact_numbers_sym_other<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))
bootstrapped_contact_rates_sym_other<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))
bootstrapped_contact_rates_sym_flip_other<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))

bootstrapped_contact_rates_sym_flip<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))

##########################getting age of random contact function
random_contact_age <- function(location) {
  age<-sample(contact_data[which(contact_data$locationType==location),2],1,replace=TRUE,prob=contact_data[which(contact_data$locationType==location),3])
  return(age)
}

##########################bootstrap contact matrices

#home

for (agecat in seq(1,agecat_number)) {
  respondent_data_subset<-respondent_data[which(respondent_data$agecat==agecat),]
  respondent_data_subset_unique<-respondent_data_subset[,1:3]
  respondent_data_subset_unique<-respondent_data_subset_unique[!duplicated(respondent_data_subset_unique$id),]
  respondent_number_subset_weighted<-sum(1/respondent_data_subset_unique$sampling_weight)
  respondent_data_subset<-respondent_data_subset[which(respondent_data_subset$locationType==0),]
  respondent_number_subset<-length(unique(respondent_data_subset$id))
  respondent_data_subset_location_unique<-respondent_data_subset[!duplicated(respondent_data_subset$id),]
  
  for (b in seq(1,bootstrap_number)) {
    respondent_data_sample_compact<-respondent_data_subset_location_unique[sample(nrow(respondent_data_subset_location_unique), respondent_number_subset, replace=TRUE),]
    respondent_data_sample<-as.data.frame(matrix(ncol=length(respondent_data_sample_compact[1,]),nrow=0))
    colnames(respondent_data_sample)<-colnames(respondent_data_sample_compact)
    for (i in seq(1,length(respondent_data_sample_compact[,1]))) {
      id<-respondent_data_sample_compact$id[i]
      respondent_data_sample<-rbind(respondent_data_sample,respondent_data_subset[which(respondent_data_subset$id==id),])
    }
    respondent_data_sample_compact<-respondent_data_sample
    
    respondent_data_sample<-respondent_data_sample[which(respondent_data_sample$roundLocationAdults100>0),]
    respondent_data_sample<-expandRows(respondent_data_sample,"roundLocationAdults100")
    respondent_data_sample$contact_age<-lapply(respondent_data_sample$locationType,random_contact_age)
    
    for (contact_agecat in seq(1,agecat_number)) {
      a<-respondent_data_sample[which(respondent_data_sample$contact_age==contact_agecat),]
      bootstrapped_contact_rates_home[agecat,contact_agecat+1,b]<-sum(a$locationMinutes/a$sampling_weight)/respondent_number_subset_weighted
      #bootstrapped_contact_rates[agecat,contact_agecat+1,b]<-sum(1/a$sampling_weight)/respondent_number_subset_weighted
    }
    respondent_data_sample_children<-respondent_data_sample_compact[which(respondent_data_sample_compact$roundLocationChildren100>0),]
    respondent_data_sample_children<-expandRows(respondent_data_sample_children,"roundLocationChildren100")
    bootstrapped_contact_rates_home[agecat,1,b]<- sum(respondent_data_sample_children$locationMinutes /respondent_data_sample_children$sampling_weight)/respondent_number_subset_weighted
    print(paste("home",agecat,b))
  }
}

mean_bootstrapped_contact_rates_home<-apply(bootstrapped_contact_rates_home,c(1,2),mean)

#other

for (agecat in seq(1,agecat_number)) {
  respondent_data_subset<-respondent_data[which(respondent_data$agecat==agecat),]
  respondent_data_subset_unique<-respondent_data_subset[,1:3]
  respondent_data_subset_unique<-respondent_data_subset_unique[!duplicated(respondent_data_subset_unique$id),]
  respondent_number_subset_weighted<-sum(1/respondent_data_subset_unique$sampling_weight)
  respondent_data_subset<-respondent_data_subset[which(respondent_data_subset$locationType!=0),]
  respondent_number_subset<-length(unique(respondent_data_subset$id))
  respondent_data_subset_location_unique<-respondent_data_subset[!duplicated(respondent_data_subset$id),]
  
  for (b in seq(1,bootstrap_number)) {
    respondent_data_sample_compact<-respondent_data_subset_location_unique[sample(nrow(respondent_data_subset_location_unique), respondent_number_subset, replace=TRUE),]
    respondent_data_sample<-as.data.frame(matrix(ncol=length(respondent_data_sample_compact[1,]),nrow=0))
    colnames(respondent_data_sample)<-colnames(respondent_data_sample_compact)
    for (i in seq(1,length(respondent_data_sample_compact[,1]))) {
      id<-respondent_data_sample_compact$id[i]
      respondent_data_sample<-rbind(respondent_data_sample,respondent_data_subset[which(respondent_data_subset$id==id),])
    }
    respondent_data_sample_compact<-respondent_data_sample
    
    respondent_data_sample<-respondent_data_sample[which(respondent_data_sample$roundLocationAdults100>0),]
    respondent_data_sample<-expandRows(respondent_data_sample,"roundLocationAdults100")
    respondent_data_sample$contact_age<-lapply(respondent_data_sample$locationType,random_contact_age)
    
    for (contact_agecat in seq(1,agecat_number)) {
      a<-respondent_data_sample[which(respondent_data_sample$contact_age==contact_agecat),]
      bootstrapped_contact_rates_other[agecat,contact_agecat+1,b]<-sum(a$locationMinutes/a$sampling_weight)/respondent_number_subset_weighted
      #bootstrapped_contact_rates[agecat,contact_agecat+1,b]<-sum(1/a$sampling_weight)/respondent_number_subset_weighted
    }
    respondent_data_sample_children<-respondent_data_sample_compact[which(respondent_data_sample_compact$roundLocationChildren100>0),]
    respondent_data_sample_children<-expandRows(respondent_data_sample_children,"roundLocationChildren100")
    bootstrapped_contact_rates_other[agecat,1,b]<- sum(respondent_data_sample_children$locationMinutes /respondent_data_sample_children$sampling_weight)/respondent_number_subset_weighted
    print(paste("other",agecat,b))
  }
}

mean_bootstrapped_contact_rates_other<-apply(bootstrapped_contact_rates_other,c(1,2),mean)


##########################make matrices symmetrical

#home
for (i in seq(1,agecat_number)) {
  bootstrapped_contact_numbers_home[i,,]<-bootstrapped_contact_rates_home[i,,]*pop_age_dist[i+1]
}

for (i in seq(1,agecat_number)) {
  for (j in seq(1,agecat_number)) {
    bootstrapped_contact_numbers_sym_home[i+1,j+1,]<-(bootstrapped_contact_numbers_home[i,j+1,] + bootstrapped_contact_numbers_home[j,i+1,])/2  
  }
}

for (i in seq(1,agecat_number)) {
  bootstrapped_contact_numbers_sym_home[1,i+1,]<-bootstrapped_contact_numbers_home[i,1,]
  bootstrapped_contact_numbers_sym_home[i+1,1,]<-bootstrapped_contact_numbers_home[i,1,]
}

for (i in seq(1,agecat_number+1)) {
  bootstrapped_contact_rates_sym_home[i,,]<-bootstrapped_contact_numbers_sym_home[i,,]/pop_age_dist[i]
}

for (i in seq(1,agecat_number+1)) {
  for (j in seq(1,agecat_number+1)) {
    bootstrapped_contact_rates_sym_flip_home[i,j,]<-bootstrapped_contact_rates_sym_home[j,i,]
  }
}

#other
for (i in seq(1,agecat_number)) {
  bootstrapped_contact_numbers_other[i,,]<-bootstrapped_contact_rates_other[i,,]*pop_age_dist[i+1]
}

for (i in seq(1,agecat_number)) {
  for (j in seq(1,agecat_number)) {
    bootstrapped_contact_numbers_sym_other[i+1,j+1,]<-(bootstrapped_contact_numbers_other[i,j+1,] + bootstrapped_contact_numbers_other[j,i+1,])/2  
  }
}

for (i in seq(1,agecat_number)) {
  bootstrapped_contact_numbers_sym_other[1,i+1,]<-bootstrapped_contact_numbers_other[i,1,]
  bootstrapped_contact_numbers_sym_other[i+1,1,]<-bootstrapped_contact_numbers_other[i,1,]
}

for (i in seq(1,agecat_number+1)) {
  bootstrapped_contact_rates_sym_other[i,,]<-bootstrapped_contact_numbers_sym_other[i,,]/pop_age_dist[i]
}

for (i in seq(1,agecat_number+1)) {
  for (j in seq(1,agecat_number+1)) {
    bootstrapped_contact_rates_sym_flip_other[i,j,]<-bootstrapped_contact_rates_sym_other[j,i,]
  }
}

bootstrapped_contact_rates_sym_flip<-bootstrapped_contact_rates_sym_flip_other + bootstrapped_contact_rates_sym_flip_home

bootstrapped_contact_rates_table<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number+1)^2))
bootstrapped_contact_rates_table_home<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number+1)^2))
bootstrapped_contact_rates_table_other<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number+1)^2))
for (b in seq(1,bootstrap_number)) {
  bootstrapped_contact_rates_table[b,]<-as.list(bootstrapped_contact_rates_sym_flip[,,b])
  bootstrapped_contact_rates_table_home[b,]<-as.list(bootstrapped_contact_rates_sym_flip_home[,,b])
  bootstrapped_contact_rates_table_other[b,]<-as.list(bootstrapped_contact_rates_sym_flip_other[,,b])
}

best_contact_rates_sym_overall<-cbind(best_contact_rates_sym_overall,apply(best_contact_rates_sym_overall,1,sum))
best_contact_rates_sym_home<-cbind(best_contact_rates_sym_home,apply(best_contact_rates_sym_home,1,sum))
best_contact_rates_sym_other<-cbind(best_contact_rates_sym_other,apply(best_contact_rates_sym_other,1,sum))


bootstrapped_contact_rates_sym_flip_total<-apply(bootstrapped_contact_rates_sym_flip,c(2,3),sum)
bootstrapped_contact_rates_sym_flip<-abind(bootstrapped_contact_rates_sym_flip,bootstrapped_contact_rates_sym_flip_total,along=1)
bootstrapped_contact_rates_sym_flip_total_home<-apply(bootstrapped_contact_rates_sym_flip_home,c(2,3),sum)
bootstrapped_contact_rates_sym_flip_home<-abind(bootstrapped_contact_rates_sym_flip_home,bootstrapped_contact_rates_sym_flip_total_home,along=1)
bootstrapped_contact_rates_sym_flip_total_other<-apply(bootstrapped_contact_rates_sym_flip_other,c(2,3),sum)
bootstrapped_contact_rates_sym_flip_other<-abind(bootstrapped_contact_rates_sym_flip_other,bootstrapped_contact_rates_sym_flip_total_other,along=1)


lb_rate<-as.data.frame(matrix(NA,nrow=agecat_number+1,ncol=agecat_number+2))
colnames(lb_rate)<-agecat_names
lb_rate[,1]<-agecat_names
ub_rate<-lb_rate
median_rate<-lb_rate
lb_rate_home<-lb_rate
ub_rate_home<-lb_rate
median_rate_home<-lb_rate
lb_rate_other<-lb_rate
ub_rate_other<-lb_rate
median_rate_other<-lb_rate

lb_rate[,]<-t(apply(bootstrapped_contact_rates_sym_flip,c(1,2),quantile,probs=c(0.025),na.rm=TRUE))
ub_rate[,]<-t(apply(bootstrapped_contact_rates_sym_flip,c(1,2),quantile,probs=c(0.975),na.rm=TRUE))
median_rate[,]<-t(apply(bootstrapped_contact_rates_sym_flip,c(1,2),quantile,probs=c(0.5),na.rm=TRUE))

lb_rate_home[,]<-t(apply(bootstrapped_contact_rates_sym_flip_home,c(1,2),quantile,probs=c(0.025),na.rm=TRUE))
ub_rate_home[,]<-t(apply(bootstrapped_contact_rates_sym_flip_home,c(1,2),quantile,probs=c(0.975),na.rm=TRUE))
median_rate_home[,]<-t(apply(bootstrapped_contact_rates_sym_flip_home,c(1,2),quantile,probs=c(0.5),na.rm=TRUE))

lb_rate_other[,]<-t(apply(bootstrapped_contact_rates_sym_flip_other,c(1,2),quantile,probs=c(0.025),na.rm=TRUE))
ub_rate_other[,]<-t(apply(bootstrapped_contact_rates_sym_flip_other,c(1,2),quantile,probs=c(0.975),na.rm=TRUE))
median_rate_other[,]<-t(apply(bootstrapped_contact_rates_sym_flip_other,c(1,2),quantile,probs=c(0.5),na.rm=TRUE))


setwd(main_wd)
setwd("matrices-casual time")
#setwd("matrices-casual time-no cap")
#setwd("matrices-casual time-cap 20")

if (location==1) {
  write.table(lb_rate, file = paste0("KZN_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(ub_rate, file = paste0("KZN_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(best_contact_rates_sym_overall, file = paste0("KZN_best.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(lb_rate_home, file = paste0("KZN_lb_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(ub_rate_home, file = paste0("KZN_ub_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(best_contact_rates_sym_home, file = paste0("KZN_best_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(lb_rate_other, file = paste0("KZN_lb_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(ub_rate_other, file = paste0("KZN_ub_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(best_contact_rates_sym_other, file = paste0("KZN_best_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(median_rate, file = paste0("KZN_median.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(median_rate_home, file = paste0("KZN_median_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(median_rate_other, file = paste0("KZN_median_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(bootstrapped_contact_rates_table, file = paste0("KZN_bootstrapped_rates.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(bootstrapped_contact_rates_table_home, file = paste0("KZN_bootstrapped_rates_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(bootstrapped_contact_rates_table_other, file = paste0("KZN_bootstrapped_rates_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
} else {
  write.table(lb_rate, file = paste0("WC_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(ub_rate, file = paste0("WC_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(best_contact_rates_sym_overall, file = paste0("WC_best.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(lb_rate_home, file = paste0("WC_lb_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(ub_rate_home, file = paste0("WC_ub_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(best_contact_rates_sym_home, file = paste0("WC_best_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(lb_rate_other, file = paste0("WC_lb_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(ub_rate_other, file = paste0("WC_ub_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(best_contact_rates_sym_other, file = paste0("WC_best_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(median_rate, file = paste0("WC_median.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(median_rate_home, file = paste0("WC_median_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(median_rate_other, file = paste0("WC_median_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(bootstrapped_contact_rates_table, file = paste0("WC_bootstrapped_rates.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(bootstrapped_contact_rates_table_home, file = paste0("WC_bootstrapped_rates_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(bootstrapped_contact_rates_table_other, file = paste0("WC_bootstrapped_rates_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
}
setwd(main_wd)