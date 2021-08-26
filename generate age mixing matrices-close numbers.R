require(splitstackshape)
require(expss)
require(Rmisc)
require(abind)

##########################read in data

setwd("respondent and contact data")
if (location==1) {
  respondent_data<-"KZN_respondents_close.csv"
  contact_data<-"KZN_contacts_close.csv"
  pop_age_dist<-KZN_pop_age_dist
} else {
  respondent_data<-"WC_respondents_close.csv"
  contact_data<-"WC_contacts_close.csv"
  pop_age_dist<-WC_pop_age_dist
}

respondent_data<-read.table(respondent_data,header=TRUE,sep=",")
contact_data<-read.table(contact_data,header=TRUE,sep=",")
contact_data<-contact_data[,1:3]
contact_data$contact_id<-1:nrow(contact_data)

respondent_number<-length(respondent_data$id)
agecat_number<-max(respondent_data$age)

##########################create arrays

bootstrapped_contact_rates<-array(data=NA,dim=c(agecat_number,agecat_number+1,bootstrap_number))
bootstrapped_contact_numbers<-array(data=NA,dim=c(agecat_number,agecat_number+1,bootstrap_number))
bootstrapped_contact_numbers_sym<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))
bootstrapped_contact_rates_sym<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))
bootstrapped_contact_rates_sym_flip<-array(data=NA,dim=c(agecat_number+1,agecat_number+1,bootstrap_number))
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
#respondent age, contact age, bootstrap number
best_contact_rates<-matrix(data=NA,nrow=agecat_number,ncol=agecat_number+1)
best_contact_numbers<-matrix(data=NA,nrow=agecat_number,ncol=agecat_number+1)
best_contact_numbers_sym<-matrix(data=NA,nrow=agecat_number+1,ncol=agecat_number+1)
best_contact_rates_sym<-matrix(data=NA,nrow=agecat_number+1,ncol=agecat_number+1)
best_contact_rates_sym_flip<-matrix(data=NA,nrow=agecat_number+1,ncol=agecat_number+1)
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


##########################getting age of random contact function
random_contact_id <- function(id) {
  contact<-sample(contact_data[which(contact_data$id==id),4],1)
  return(contact)
}
get_contact_age <-function(contactID) {
  contact_age<-contact_data[which(contact_data$contact_id==contactID),2]
  return(contact_age)
}
get_contact_hhMem <-function(contactID) {
  contact_hhMem<-contact_data[which(contact_data$contact_id==contactID),3]
  return(contact_hhMem)
}

#################################main results

full_data <- merge(respondent_data,contact_data,by="id")
full_data$listed<-NA
for (i in seq(1,length(full_data[,1]))) {
  full_data$listed[i]<-sum(full_data$id==full_data$id[i])
}
full_data$contact_weight<-full_data$contactstotal/full_data$listed

for (agecat in seq(1,agecat_number)) {
  #create subset respondent dataframe
  respondent_data_subset<-full_data[which(full_data$agecat==agecat),]
  all_respondent_data_subset<-respondent_data[which(respondent_data$agecat==agecat),]
  respondent_number_subset<-length(all_respondent_data_subset$id)
  respondent_number_subset_weighted<-sum(1 /all_respondent_data_subset$weight)
  
  for (contact_agecat in seq(0,agecat_number)) {
    a<-respondent_data_subset[which(respondent_data_subset$contactage==contact_agecat),]
    best_contact_rates[agecat,contact_agecat+1]<- sum(a$contact_weight /a$weight)/respondent_number_subset_weighted
    a<-respondent_data_subset[which(respondent_data_subset$contactage==contact_agecat & respondent_data_subset$hhMem==1),]
    best_contact_rates_home[agecat,contact_agecat+1]<- sum(a$contact_weight /a$weight)/respondent_number_subset_weighted
    a<-respondent_data_subset[which(respondent_data_subset$contactage==contact_agecat & respondent_data_subset$hhMem==0),]
    best_contact_rates_other[agecat,contact_agecat+1]<- sum(a$contact_weight /a$weight)/respondent_number_subset_weighted
  }
}

for (i in seq(1,agecat_number)) {
  best_contact_numbers[i,]<-best_contact_rates[i,]*pop_age_dist[i+1]
}

for (i in seq(1,agecat_number)) {
  for (j in seq(1,agecat_number)) {
    best_contact_numbers_sym[i+1,j+1]<-(best_contact_numbers[i,j+1] + best_contact_numbers[j,i+1])/2  
  }
}
for (i in seq(1,agecat_number)) {
  best_contact_numbers_sym[1,i+1]<-best_contact_numbers[i,1]
  best_contact_numbers_sym[i+1,1]<-best_contact_numbers[i,1]
}

for (i in seq(1,agecat_number+1)) {
  best_contact_rates_sym[i,]<-best_contact_numbers_sym[i,]/pop_age_dist[i]
}


for (i in seq(1,agecat_number+1)) {
  for (j in seq(1,agecat_number+1)) {
    best_contact_rates_sym_flip[i,j]<-best_contact_rates_sym[j,i]
  }
}

#home
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


##########################bootstrap contact matrices

for (agecat in seq(1,agecat_number)) {
  #create subset respondent dataframe
  respondent_data_subset<-respondent_data[which(respondent_data$agecat==agecat),]
  respondent_number_subset<-length(respondent_data_subset$id)
  respondent_number_subset_weighted<-sum(1 /respondent_data_subset$weight)
  
  for (b in seq(1,bootstrap_number)) {
    respondent_data_sample<-respondent_data_subset[sample(nrow(respondent_data_subset), respondent_number_subset, replace=TRUE),]
    respondent_data_sample<-respondent_data_sample[which(respondent_data_sample$contactstotal>0),]
    respondent_data_sample<-expandRows(respondent_data_sample,"contactstotal")
    respondent_data_sample<-respondent_data_subset[sample(nrow(respondent_data_subset), respondent_number_subset, replace=TRUE),]
    respondent_data_sample<-respondent_data_sample[which(respondent_data_sample$contactstotal>0),]
    respondent_data_sample<-expandRows(respondent_data_sample,"contactstotal")
    respondent_data_sample$contactID<-lapply(respondent_data_sample$id,random_contact_id)
    respondent_data_sample$contact_age<-lapply(respondent_data_sample$contactID,get_contact_age)
    respondent_data_sample$hhMem<-lapply(respondent_data_sample$contactID,get_contact_hhMem)
    
    for (contact_agecat in seq(0,agecat_number)) {
      a<-respondent_data_sample[which(respondent_data_sample$contact_age==contact_agecat),]
      bootstrapped_contact_rates[agecat,contact_agecat+1,b]<- sum(1 /a$weight)/respondent_number_subset_weighted
      a<-respondent_data_sample[which(respondent_data_sample$contact_age==contact_agecat & respondent_data_sample$hhMem==1),]
      bootstrapped_contact_rates_home[agecat,contact_agecat+1,b]<- sum(1 /a$weight)/respondent_number_subset_weighted
      a<-respondent_data_sample[which(respondent_data_sample$contact_age==contact_agecat & respondent_data_sample$hhMem==0),]
      bootstrapped_contact_rates_other[agecat,contact_agecat+1,b]<- sum(1 /a$weight)/respondent_number_subset_weighted
    }
    print(paste(agecat,b))
  }
}

mean_bootstrapped_contact_rates<-apply(bootstrapped_contact_rates,c(1,2),mean)

##########################make matrices symmetrical

#all
for (i in seq(1,agecat_number)) {
  bootstrapped_contact_numbers[i,,]<-bootstrapped_contact_rates[i,,]*pop_age_dist[i+1]
}

for (i in seq(1,agecat_number)) {
  for (j in seq(1,agecat_number)) {
    bootstrapped_contact_numbers_sym[i+1,j+1,]<-(bootstrapped_contact_numbers[i,j+1,] + bootstrapped_contact_numbers[j,i+1,])/2  
  }
}
for (i in seq(1,agecat_number)) {
  bootstrapped_contact_numbers_sym[1,i+1,]<-bootstrapped_contact_numbers[i,1,]
  bootstrapped_contact_numbers_sym[i+1,1,]<-bootstrapped_contact_numbers[i,1,]
}

for (i in seq(1,agecat_number+1)) {
  bootstrapped_contact_rates_sym[i,,]<-bootstrapped_contact_numbers_sym[i,,]/pop_age_dist[i]
}


for (i in seq(1,agecat_number+1)) {
  for (j in seq(1,agecat_number+1)) {
    bootstrapped_contact_rates_sym_flip[i,j,]<-bootstrapped_contact_rates_sym[j,i,]
  }
}

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

bootstrapped_contact_rates_table<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number+1)^2))
bootstrapped_contact_rates_table_home<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number+1)^2))
bootstrapped_contact_rates_table_other<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number+1)^2))
for (b in seq(1,bootstrap_number)) {
  bootstrapped_contact_rates_table[b,]<-as.list(bootstrapped_contact_rates_sym_flip[,,b])
  bootstrapped_contact_rates_table_home[b,]<-as.list(bootstrapped_contact_rates_sym_flip_home[,,b])
  bootstrapped_contact_rates_table_other[b,]<-as.list(bootstrapped_contact_rates_sym_flip_other[,,b])
}

bootstrapped_contact_rates_sym_flip_total<-apply(bootstrapped_contact_rates_sym_flip,c(2,3),sum)
bootstrapped_contact_rates_sym_flip<-abind(bootstrapped_contact_rates_sym_flip,bootstrapped_contact_rates_sym_flip_total,along=1)
bootstrapped_contact_rates_sym_flip_total_home<-apply(bootstrapped_contact_rates_sym_flip_home,c(2,3),sum)
bootstrapped_contact_rates_sym_flip_home<-abind(bootstrapped_contact_rates_sym_flip_home,bootstrapped_contact_rates_sym_flip_total_home,along=1)
bootstrapped_contact_rates_sym_flip_total_other<-apply(bootstrapped_contact_rates_sym_flip_other,c(2,3),sum)
bootstrapped_contact_rates_sym_flip_other<-abind(bootstrapped_contact_rates_sym_flip_other,bootstrapped_contact_rates_sym_flip_total_other,along=1)

best_contact_rates_sym<-cbind(best_contact_rates_sym,apply(best_contact_rates_sym,1,sum))
best_contact_rates_sym_home<-cbind(best_contact_rates_sym_home,apply(best_contact_rates_sym_home,1,sum))
best_contact_rates_sym_other<-cbind(best_contact_rates_sym_other,apply(best_contact_rates_sym_other,1,sum))


lb<-as.data.frame(matrix(NA,nrow=agecat_number+1,ncol=agecat_number+2))
colnames(lb)<-agecat_names
lb[,1]<-agecat_names
ub<-lb
median<-lb
median_home<-lb
lb_home<-lb
ub_home<-lb
median_other<-lb
lb_other<-lb
ub_other<-lb
lb[,]<-t(apply(bootstrapped_contact_rates_sym_flip,c(1,2),quantile,probs=c(0.025),na.rm=TRUE))
ub[,]<-t(apply(bootstrapped_contact_rates_sym_flip,c(1,2),quantile,probs=c(0.975),na.rm=TRUE))
median[,]<-t(apply(bootstrapped_contact_rates_sym_flip,c(1,2),quantile,probs=c(0.5),na.rm=TRUE))
lb_home[,]<-t(apply(bootstrapped_contact_rates_sym_flip_home,c(1,2),quantile,probs=c(0.025),na.rm=TRUE))
ub_home[,]<-t(apply(bootstrapped_contact_rates_sym_flip_home,c(1,2),quantile,probs=c(0.975),na.rm=TRUE))
median_home[,]<-t(apply(bootstrapped_contact_rates_sym_flip_home,c(1,2),quantile,probs=c(0.5),na.rm=TRUE))
lb_other[,]<-t(apply(bootstrapped_contact_rates_sym_flip_other,c(1,2),quantile,probs=c(0.025),na.rm=TRUE))
ub_other[,]<-t(apply(bootstrapped_contact_rates_sym_flip_other,c(1,2),quantile,probs=c(0.975),na.rm=TRUE))
median_other[,]<-t(apply(bootstrapped_contact_rates_sym_flip_other,c(1,2),quantile,probs=c(0.5),na.rm=TRUE))

setwd(main_wd)
setwd("matrices-close numbers")
 if (location==1) {
   write.table(lb, file = paste0("KZN_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(ub, file = paste0("KZN_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(median, file = paste0("KZN_median.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(best_contact_rates_sym, file = paste0("KZN_best.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(lb_home, file = paste0("KZN_lb_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(ub_home, file = paste0("KZN_ub_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(median_home, file = paste0("KZN_median_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(best_contact_rates_sym_home, file = paste0("KZN_best_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(lb_other, file = paste0("KZN_lb_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(ub_other, file = paste0("KZN_ub_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(median_other, file = paste0("KZN_median_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(best_contact_rates_sym_other, file = paste0("KZN_best_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(bootstrapped_contact_rates_table, file = paste0("KZN_bootstrapped_rates.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(bootstrapped_contact_rates_table_home, file = paste0("KZN_bootstrapped_rates_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(bootstrapped_contact_rates_table_other, file = paste0("KZN_bootstrapped_rates_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
 } else {
   write.table(lb, file = paste0("WC_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(ub, file = paste0("WC_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(median, file = paste0("WC_median.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(best_contact_rates_sym, file = paste0("WC_best.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(lb_home, file = paste0("WC_lb_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(ub_home, file = paste0("WC_ub_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(median_home, file = paste0("WC_median_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(best_contact_rates_sym_home, file = paste0("WC_best_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(lb_other, file = paste0("WC_lb_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(ub_other, file = paste0("WC_ub_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(median_other, file = paste0("WC_median_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(best_contact_rates_sym_other, file = paste0("WC_best_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(bootstrapped_contact_rates_table, file = paste0("WC_bootstrapped_rates.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(bootstrapped_contact_rates_table_home, file = paste0("WC_bootstrapped_rates_home.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
   write.table(bootstrapped_contact_rates_table_other, file = paste0("WC_bootstrapped_rates_other.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
 }
setwd(main_wd)