require(abind)

setwd(main_wd)
setwd("matrices-close time")
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
setwd(main_wd)
setwd("combined matrices-airborne")

agecat_number<-length(KZN_home[1,])^0.5

KZN_home_best<-KZN_home_best[,1:agecat_number]
KZN_other_best<-KZN_other_best[,1:agecat_number]
WC_home_best<-WC_home_best[,1:agecat_number]
WC_other_best<-WC_other_best[,1:agecat_number]
best_combined_equal_KZN<-KZN_home_best+KZN_other_best
best_combined_equal_WC<-WC_home_best+WC_other_best

popsize_matrix_KZN<-as.data.frame(matrix(KZN_pop_age_dist,nrow=agecat_number,ncol=agecat_number))
popsize_matrix_WC<-as.data.frame(matrix(WC_pop_age_dist,nrow=agecat_number,ncol=agecat_number))

matrices_home_KZN<-aperm(array(data=unlist(KZN_home),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_home_WC<-aperm(array(data=unlist(WC_home),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_other_KZN<-aperm(array(data=unlist(KZN_other),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_other_WC<-aperm(array(data=unlist(WC_other),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_close_all_KZN<-aperm(array(data=unlist(KZN_close_all),dim=c(bootstrap_number,agecat_number,agecat_number)))
matrices_close_all_WC<-aperm(array(data=unlist(WC_close_all),dim=c(bootstrap_number,agecat_number,agecat_number)))


matrices_combined_equal_KZN<-matrices_home_KZN+matrices_other_KZN
matrices_combined_equal_WC<-matrices_home_KZN+matrices_other_WC

median_home_KZN<-apply(matrices_home_KZN,c(1,2),median)
median_other_KZN<-apply(matrices_other_KZN,c(1,2),median)
median_home_WC<-apply(matrices_home_WC,c(1,2),median)
median_other_WC<-apply(matrices_home_WC,c(1,2),median)


matrices_combined_equal_KZN_total<-apply(matrices_combined_equal_KZN,c(1,3),sum)
matrices_combined_equal_KZN<-abind(matrices_combined_equal_KZN,matrices_combined_equal_KZN_total,along=2)
matrices_combined_equal_WC_total<-apply(matrices_combined_equal_WC,c(1,3),sum)
matrices_combined_equal_WC<-abind(matrices_combined_equal_WC,matrices_combined_equal_WC_total,along=2)
best_combined_equal_KZN<-cbind(best_combined_equal_KZN,apply(best_combined_equal_KZN,1,sum))
best_combined_equal_WC<-cbind(best_combined_equal_WC,apply(best_combined_equal_WC,1,sum))


lb_rate_KZN<-as.data.frame(matrix(NA,nrow=agecat_number,ncol=agecat_number+1))
colnames(lb_rate_KZN)<-agecat_names
lb_rate_KZN[,1]<-agecat_names
ub_rate_KZN<-lb_rate_KZN
median_rate_KZN<-lb_rate_KZN
lb_rate_WC<-lb_rate_KZN
ub_rate_WC<-lb_rate_KZN
median_rate_WC<-lb_rate_KZN

lb_rate_KZN[,]<-apply(matrices_combined_equal_KZN,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
ub_rate_KZN[,]<-apply(matrices_combined_equal_KZN,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
median_rate_KZN[,]<-apply(matrices_combined_equal_KZN,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)
lb_rate_WC[,]<-apply(matrices_combined_equal_KZN,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
ub_rate_WC[,]<-apply(matrices_combined_equal_KZN,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
median_rate_WC[,]<-apply(matrices_combined_equal_KZN,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)

write.table(lb_rate_KZN, file = paste0("KZN_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(ub_rate_KZN, file = paste0("KZN_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(median_rate_KZN, file = paste0("KZN_median.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(best_combined_equal_KZN, file = paste0("KZN_best.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(lb_rate_WC, file = paste0("WC_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(ub_rate_WC, file = paste0("WC_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(median_rate_WC, file = paste0("WC_median.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(best_combined_equal_WC, file = paste0("WC_best.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")



######create CIs for combined/close

matrices_overall_KZN<-matrices_home_KZN+matrices_other_KZN
matrices_overall_KZN_total<-apply(matrices_overall_KZN,c(1,3),sum)
matrices_overall_KZN<-abind(matrices_overall_KZN,matrices_overall_KZN_total,along=2)
matrices_overall_WC<-matrices_home_WC+matrices_other_WC
matrices_overall_WC_total<-apply(matrices_overall_WC,c(1,3),sum)
matrices_overall_WC<-abind(matrices_overall_WC,matrices_overall_WC_total,along=2)
matrices_close_all_KZN_total<-apply(matrices_close_all_KZN,c(1,3),sum)
matrices_close_all_KZN<-abind(matrices_close_all_KZN,matrices_close_all_KZN_total,along=2)
matrices_close_all_WC_total<-apply(matrices_close_all_WC,c(1,3),sum)
matrices_close_all_WC<-abind(matrices_close_all_WC,matrices_close_all_WC_total,along=2)


matrices_overall_std_KZN<-matrices_overall_KZN
matrices_ratio_KZN<-matrices_overall_KZN
matrices_overall_std_WC<-matrices_overall_WC
matrices_ratio_WC<-matrices_overall_WC


for (b in seq(1,bootstrap_number)) {
  close<-matrices_close_all_KZN[,,b]
  combined<-matrices_overall_KZN[,,b]
  
  sum_data<-apply(close[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*KZN_pop_age_dist
  mean_contacts_close<-sum(sum_data[2:agecat_number])/sum(KZN_pop_age_dist[2:agecat_number])
  sum_data<-apply(combined[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*KZN_pop_age_dist
  mean_contacts_combined<-sum(sum_data[2:agecat_number])/sum(KZN_pop_age_dist[2:agecat_number])
  
  matrices_overall_std_KZN[,,b]<-matrices_overall_KZN[,,b]*mean_contacts_close/mean_contacts_combined
  matrices_ratio_KZN[,,b]<-matrices_overall_std_KZN[,,b]/matrices_close_all_KZN[,,b]
  
  close<-matrices_close_all_WC[,,b]
  combined<-matrices_overall_WC[,,b]
  
  sum_data<-apply(close[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*WC_pop_age_dist
  mean_contacts_close<-sum(sum_data[2:agecat_number])/sum(WC_pop_age_dist[2:agecat_number])
  sum_data<-apply(combined[,1:agecat_number],1,sum,na.rm=TRUE)
  sum_data<-sum_data*WC_pop_age_dist
  mean_contacts_combined<-sum(sum_data[2:agecat_number])/sum(WC_pop_age_dist[2:agecat_number])
  
  matrices_overall_std_WC[,,b]<-matrices_overall_WC[,,b]*mean_contacts_close/mean_contacts_combined
  matrices_ratio_WC[,,b]<-matrices_overall_std_WC[,,b]/matrices_close_all_WC[,,b]
}

bootstrapped_contact_rates_table_KZN<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number)^2))
bootstrapped_contact_rates_table_WC<-as.data.frame(matrix(NA,nrow=bootstrap_number,ncol=(agecat_number)^2))
matrices_overall_std_KZN_nototal<-matrices_overall_std_KZN[1:agecat_number,1:agecat_number,1:bootstrap_number]
matrices_overall_std_WC_nototal<-matrices_overall_std_WC[1:agecat_number,1:agecat_number,1:bootstrap_number]
matrices_overall_std_KZN_nototal<-aperm(matrices_overall_std_KZN_nototal,c(2,1,3))
matrices_overall_std_WC_nototal<-aperm(matrices_overall_std_WC_nototal,c(2,1,3))
for (b in seq(1,bootstrap_number)) {
  bootstrapped_contact_rates_table_KZN[b,]<-as.list(matrices_overall_std_KZN_nototal[,,b])
  bootstrapped_contact_rates_table_WC[b,]<-as.list(matrices_overall_std_WC_nototal[,,b])
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

lb_ratio_KZN[,]<-apply(matrices_ratio_KZN,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
ub_ratio_KZN[,]<-apply(matrices_ratio_KZN,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
median_ratio_KZN[,]<-apply(matrices_ratio_KZN,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)
lb_ratio_WC[,]<-apply(matrices_ratio_WC,c(1,2),quantile,probs=c(0.025),na.rm=TRUE)
ub_ratio_WC[,]<-apply(matrices_ratio_WC,c(1,2),quantile,probs=c(0.975),na.rm=TRUE)
median_ratio_WC[,]<-apply(matrices_ratio_WC,c(1,2),quantile,probs=c(0.5),na.rm=TRUE)

write.table(lb_ratio_KZN, file = paste0("KZN_ratio_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(ub_ratio_KZN, file = paste0("KZN_ratio_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(lb_ratio_WC, file = paste0("WC_ratio_lb.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")
write.table(ub_ratio_WC, file = paste0("WC_ratio_ub.csv"),row.names=FALSE, col.names=FALSE, na="", sep=",")

