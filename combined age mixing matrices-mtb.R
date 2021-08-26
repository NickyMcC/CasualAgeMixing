require(abind)

household_prop_min<-0.08
household_prop_best<-0.12
household_prop_max<-0.16

supporting<-FALSE   ####generate combined matrices-supporting, mtb from casual time?

if (supporting==TRUE) {
  setwd("matrices-casual time") 
} else {
  setwd("matrices-close numbers")
}
KZN_home<-read.csv("KZN_bootstrapped_rates_home.csv",header=FALSE,sep=",")
WC_home<-read.csv("WC_bootstrapped_rates_home.csv",header=FALSE,sep=",")
KZN_home_best<-read.csv("KZN_best_home.csv",header=FALSE,sep=",")
WC_home_best<-read.csv("WC_best_home.csv",header=FALSE,sep=",")
KZN_close_all<-read.csv("KZN_bootstrapped_rates.csv",header=FALSE,sep=",")
WC_close_all<-read.csv("WC_bootstrapped_rates.csv",header=FALSE,sep=",")
setwd(main_wd)
setwd("matrices-casual time")
KZN_other<-read.csv("KZN_bootstrapped_rates_other.csv",header=FALSE,sep=",")
WC_other<-read.csv("WC_bootstrapped_rates_other.csv",header=FALSE,sep=",")
KZN_other_best<-read.csv("KZN_best_other.csv",header=FALSE,sep=",")
WC_other_best<-read.csv("WC_best_other.csv",header=FALSE,sep=",")
KZN_casual_all<-read.csv("KZN_bootstrapped_rates.csv",header=FALSE,sep=",")
WC_casual_all<-read.csv("WC_bootstrapped_rates.csv",header=FALSE,sep=",")
setwd(main_wd)
if (supporting==TRUE) {
  setwd("combined matrices-supporting, mtb from casual time") 
} else {
  setwd("combined matrices-mtb")
}

agecat_number<-length(KZN_home[1,])^0.5
bootstrap_number<-length(KZN_other[,1])

KZN_home_best<-KZN_home_best[,1:agecat_number]
KZN_other_best<-KZN_other_best[,1:agecat_number]
WC_home_best<-WC_home_best[,1:agecat_number]
WC_other_best<-WC_other_best[,1:agecat_number]

popsize_matrix_KZN<-as.data.frame(matrix(KZN_pop_age_dist,nrow=agecat_number,ncol=agecat_number))
popsize_matrix_WC<-as.data.frame(matrix(WC_pop_age_dist,nrow=agecat_number,ncol=agecat_number))

matrices_home_KZN<-aperm(array(data=unlist(KZN_home),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_home_WC<-aperm(array(data=unlist(WC_home),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_other_KZN<-aperm(array(data=unlist(KZN_other),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_other_WC<-aperm(array(data=unlist(WC_other),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_close_all_KZN<-aperm(array(data=unlist(KZN_close_all),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_close_all_WC<-aperm(array(data=unlist(WC_close_all),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_casual_all_KZN<-aperm(array(data=unlist(KZN_casual_all),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_casual_all_WC<-aperm(array(data=unlist(WC_casual_all),dim=c(bootstrap_number,agecat_number,agecat_number)))


matrices_combined_equal_KZN<-matrices_home_KZN+matrices_other_KZN
matrices_combined_equal_WC<-matrices_home_KZN+matrices_other_WC

median_home_KZN<-apply(matrices_home_KZN,c(1,2),median)
median_other_KZN<-apply(matrices_other_KZN,c(1,2),median)
median_home_WC<-apply(matrices_home_WC,c(1,2),median)
median_other_WC<-apply(matrices_home_WC,c(1,2),median)

matrices_combined_weighted_KZN<-array(NA,dim=c(agecat_number,agecat_number,bootstrap_number))
for (b in seq(1,bootstrap_number)) {
  matrix_home<-matrices_home_KZN[,,b]
  matrix_other<-matrices_other_KZN[,,b]
  total_contact_home<-sum(matrix_home[2:agecat_number,]*popsize_matrix_KZN[2:agecat_number,],na.rm=TRUE)
  total_contact_other<-sum(matrix_other[2:agecat_number,]*popsize_matrix_KZN[2:agecat_number,],na.rm=TRUE)
  r<-total_contact_other/total_contact_home
  household_prop<-runif(1,min=household_prop_min,max=household_prop_max)
  matrices_combined_weighted_KZN[,,b]<-matrix_other + matrix_home * household_prop * r / (1 - household_prop)
}
matrices_combined_weighted_WC<-array(NA,dim=c(agecat_number,agecat_number,bootstrap_number))
for (b in seq(1,bootstrap_number)) {
  matrix_home<-matrices_home_WC[,,b]
  matrix_other<-matrices_other_WC[,,b]
  total_contact_home<-sum(matrix_home[2:agecat_number,]*popsize_matrix_WC[2:agecat_number,],na.rm=TRUE)
  total_contact_other<-sum(matrix_other[2:agecat_number,]*popsize_matrix_WC[2:agecat_number,],na.rm=TRUE)
  r<-total_contact_other/total_contact_home
  household_prop<-runif(1,min=household_prop_min,max=household_prop_max)
  matrices_combined_weighted_WC[,,b]<-matrix_other + matrix_home * household_prop * r / (1 - household_prop)
}

total_contact_home<-sum(KZN_home_best[2:agecat_number,]*popsize_matrix_KZN[2:agecat_number,],na.rm=TRUE)
total_contact_other<-sum(KZN_other_best[2:agecat_number,]*popsize_matrix_KZN[2:agecat_number,],na.rm=TRUE)
r<-total_contact_other/total_contact_home
best_combined_weighted_KZN<-KZN_other_best + KZN_home_best * household_prop_best * r / (1 - household_prop_best)
total_contact_home<-sum(WC_home_best[2:agecat_number,]*popsize_matrix_WC[2:agecat_number,],na.rm=TRUE)
total_contact_other<-sum(WC_other_best[2:agecat_number,]*popsize_matrix_WC[2:agecat_number,],na.rm=TRUE)
r<-total_contact_other/total_contact_home
best_combined_weighted_WC<-WC_other_best + WC_home_best * household_prop_best * r / (1 - household_prop_best)

matrices_combined_weighted_KZN_total<-apply(matrices_combined_weighted_KZN,c(1,3),sum)
matrices_combined_weighted_KZN<-abind(matrices_combined_weighted_KZN,matrices_combined_weighted_KZN_total,along=2)
matrices_combined_weighted_WC_total<-apply(matrices_combined_weighted_WC,c(1,3),sum)
matrices_combined_weighted_WC<-abind(matrices_combined_weighted_WC,matrices_combined_weighted_WC_total,along=2)
best_combined_weighted_KZN<-cbind(best_combined_weighted_KZN,apply(best_combined_weighted_KZN,1,sum))
best_combined_weighted_WC<-cbind(best_combined_weighted_WC,apply(best_combined_weighted_WC,1,sum))

matrices_casual_weighted_KZN_total<-apply(matrices_casual_all_KZN,c(1,3),sum)
matrices_casual_weighted_KZN<-abind(matrices_casual_all_KZN,matrices_casual_weighted_KZN_total,along=2)
matrices_casual_weighted_WC_total<-apply(matrices_casual_all_WC,c(1,3),sum)
matrices_casual_weighted_WC<-abind(matrices_casual_all_WC,matrices_casual_weighted_WC_total,along=2)

lb_rate_KZN<-as.data.frame(matrix(NA,nrow=agecat_number,ncol=agecat_number+1))
colnames(lb_rate_KZN)<-agecat_names
lb_rate_KZN[,1]<-agecat_names
ub_rate_KZN<-lb_rate_KZN
median_rate_KZN<-lb_rate_KZN
lb_rate_WC<-lb_rate_KZN
ub_rate_WC<-lb_rate_KZN
median_rate_WC<-lb_rate_KZN

lb_rate_KZN[,]<-apply(matrices_combined_weighted_KZN,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
ub_rate_KZN[,]<-apply(matrices_combined_weighted_KZN,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
median_rate_KZN[,]<-apply(matrices_combined_weighted_KZN,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)
lb_rate_WC[,]<-apply(matrices_combined_weighted_WC,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
ub_rate_WC[,]<-apply(matrices_combined_weighted_WC,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
median_rate_WC[,]<-apply(matrices_combined_weighted_WC,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)

write.table(lb_rate_KZN, file = paste0("KZN_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(ub_rate_KZN, file = paste0("KZN_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(median_rate_KZN, file = paste0("KZN_median.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(best_combined_weighted_KZN, file = paste0("KZN_best.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(lb_rate_WC, file = paste0("WC_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(ub_rate_WC, file = paste0("WC_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(median_rate_WC, file = paste0("WC_median.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(best_combined_weighted_WC, file = paste0("WC_best.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")



######create CIs for combined/close
matrices_close_all_KZN_total<-apply(matrices_close_all_KZN,c(1,3),sum)
matrices_close_all_KZN<-abind(matrices_close_all_KZN,matrices_close_all_KZN_total,along=2)
matrices_close_all_WC_total<-apply(matrices_close_all_WC,c(1,3),sum)
matrices_close_all_WC<-abind(matrices_close_all_WC,matrices_close_all_WC_total,along=2)

matrices_combined_weighted_std_KZN<-matrices_close_all_KZN
matrices_casual_weighted_std_KZN<-matrices_close_all_KZN
matrices_ratio_combined_close_KZN<-matrices_close_all_KZN
matrices_ratio_casual_close_KZN<-matrices_close_all_KZN
matrices_combined_weighted_std_WC<-matrices_close_all_WC
matrices_casual_weighted_std_WC<-matrices_close_all_WC
matrices_ratio_combined_close_WC<-matrices_close_all_WC
matrices_ratio_casual_close_WC<-matrices_close_all_WC
for (b in seq(1,bootstrap_number)) {
  close<-matrices_close_all_KZN[,,b]
  combined<-matrices_combined_weighted_KZN[,,b]
  casual<-matrices_casual_all_KZN[,,b]
  sum_data<-apply(close[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*KZN_pop_age_dist
  mean_contacts_close<-sum(sum_data[2:agecat_number])/sum(KZN_pop_age_dist[2:agecat_number])
  sum_data<-apply(combined[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*KZN_pop_age_dist
  mean_contacts_combined<-sum(sum_data[2:agecat_number])/sum(KZN_pop_age_dist[2:agecat_number])
  sum_data<-apply(casual[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*KZN_pop_age_dist
  mean_contacts_casual<-sum(sum_data[2:agecat_number])/sum(KZN_pop_age_dist[2:agecat_number])
  matrices_combined_weighted_std_KZN[,,b]<-matrices_combined_weighted_KZN[,,b]*mean_contacts_close/mean_contacts_combined
  matrices_casual_weighted_std_KZN[,,b]<-matrices_casual_weighted_KZN[,,b]*mean_contacts_close/mean_contacts_casual
  matrices_ratio_combined_close_KZN[,,b]<-matrices_combined_weighted_std_KZN[,,b]/matrices_close_all_KZN[,,b]
  matrices_ratio_casual_close_KZN[,,b]<-matrices_casual_weighted_std_KZN[,,b]/matrices_close_all_KZN[,,b]
  
  close<-matrices_close_all_WC[,,b]
  combined<-matrices_combined_weighted_WC[,,b]
  casual<-matrices_casual_all_WC[,,b]
  sum_data<-apply(close[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*WC_pop_age_dist
  mean_contacts_close<-sum(sum_data[2:agecat_number])/sum(WC_pop_age_dist[2:agecat_number])
  sum_data<-apply(combined[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*WC_pop_age_dist
  mean_contacts_combined<-sum(sum_data[2:agecat_number])/sum(WC_pop_age_dist[2:agecat_number])
  sum_data<-apply(casual[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*WC_pop_age_dist
  mean_contacts_casual<-sum(sum_data[2:agecat_number])/sum(WC_pop_age_dist[2:agecat_number])
  matrices_combined_weighted_std_WC[,,b]<-matrices_combined_weighted_WC[,,b]*mean_contacts_close/mean_contacts_combined
  matrices_casual_weighted_std_WC[,,b]<-matrices_casual_weighted_WC[,,b]*mean_contacts_close/mean_contacts_casual
  matrices_ratio_combined_close_WC[,,b]<-matrices_combined_weighted_std_WC[,,b]/matrices_close_all_WC[,,b]
  matrices_ratio_casual_close_WC[,,b]<-matrices_casual_weighted_std_WC[,,b]/matrices_close_all_WC[,,b]
}

bootstrapped_contact_rates_table_KZN<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number)^2))
bootstrapped_contact_rates_table_WC<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number)^2))
matrices_combined_weighted_std_KZN_nototal<-matrices_combined_weighted_std_KZN[1:agecat_number,1:agecat_number,1:bootstrap_number]
matrices_combined_weighted_std_WC_nototal<-matrices_combined_weighted_std_WC[1:agecat_number,1:agecat_number,1:bootstrap_number]
matrices_combined_weighted_std_KZN_nototal<-aperm(matrices_combined_weighted_std_KZN_nototal,c(2,1,3))
matrices_combined_weighted_std_WC_nototal<-aperm(matrices_combined_weighted_std_WC_nototal,c(2,1,3))
for (b in seq(1,bootstrap_number)) {
  bootstrapped_contact_rates_table_KZN[b,]<-as.list(matrices_combined_weighted_std_KZN_nototal[,,b])
  bootstrapped_contact_rates_table_WC[b,]<-as.list(matrices_combined_weighted_std_WC_nototal[,,b])
}
write.table(bootstrapped_contact_rates_table_KZN, file = paste0("KZN_bootstrapped_rates.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(bootstrapped_contact_rates_table_WC, file = paste0("WC_bootstrapped_rates.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")



#contact rates

#ratio combined close
lb_ratio_KZN<-as.data.frame(matrix(NA,nrow=agecat_number,ncol=agecat_number+1))
colnames(lb_ratio_KZN)<-agecat_names
lb_ratio_KZN[,1]<-agecat_names
ub_ratio_KZN<-lb_ratio_KZN
median_ratio_KZN<-lb_ratio_KZN
lb_ratio_WC<-lb_ratio_KZN
ub_ratio_WC<-lb_ratio_KZN
median_ratio_WC<-lb_ratio_KZN

lb_ratio_KZN[,]<-apply(matrices_ratio_combined_close_KZN,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
ub_ratio_KZN[,]<-apply(matrices_ratio_combined_close_KZN,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
median_ratio_KZN[,]<-apply(matrices_ratio_combined_close_KZN,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)
lb_ratio_WC[,]<-apply(matrices_ratio_combined_close_WC,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
ub_ratio_WC[,]<-apply(matrices_ratio_combined_close_WC,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
median_ratio_WC[,]<-apply(matrices_ratio_combined_close_WC,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)

write.table(lb_ratio_KZN, file = paste0("KZN_ratio_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(ub_ratio_KZN, file = paste0("KZN_ratio_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(lb_ratio_WC, file = paste0("WC_ratio_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(ub_ratio_WC, file = paste0("WC_ratio_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")

if (supporting==FALSE) {
  #ratio casual close
  lb_ratio_KZN<-as.data.frame(matrix(NA,nrow=agecat_number,ncol=agecat_number+1))
  colnames(lb_ratio_KZN)<-agecat_names
  lb_ratio_KZN[,1]<-agecat_names
  ub_ratio_KZN<-lb_ratio_KZN
  median_ratio_KZN<-lb_ratio_KZN
  lb_ratio_WC<-lb_ratio_KZN
  ub_ratio_WC<-lb_ratio_KZN
  median_ratio_WC<-lb_ratio_KZN
  
  lb_ratio_KZN[,]<-apply(matrices_ratio_casual_close_KZN,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
  ub_ratio_KZN[,]<-apply(matrices_ratio_casual_close_KZN,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
  median_ratio_KZN[,]<-apply(matrices_ratio_casual_close_KZN,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)
  lb_ratio_WC[,]<-apply(matrices_ratio_casual_close_WC,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
  ub_ratio_WC[,]<-apply(matrices_ratio_casual_close_WC,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
  median_ratio_WC[,]<-apply(matrices_ratio_casual_close_WC,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)
  
  setwd(main_wd)
  setwd("matrices-casual time")
  write.table(lb_ratio_KZN, file = paste0("KZN_ratio_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(ub_ratio_KZN, file = paste0("KZN_ratio_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(lb_ratio_WC, file = paste0("WC_ratio_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
  write.table(ub_ratio_WC, file = paste0("WC_ratio_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
}
