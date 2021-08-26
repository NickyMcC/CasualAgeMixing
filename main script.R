
main_wd<-"C:/Users/Nicky/Dropbox/infection location grant/social contact-both SA datasets/data analysis/final"
setwd(main_wd)

#############info##################
agecat_names<-c("<15","15-19","20-29","30-39","40-49","50+")
KZN_pop_age_dist<-c(18566,#<15
                    4878, #15-19
                    8722, #20-29
                    6925, #30-39
                    4359, #40-49
                    8404  #50+
)
WC_pop_age_dist<-c(6703,#<15
                   3716, #15-19
                   6812, #20-29
                   7537, #30-39
                   3082, #40-49
                   1346 #50+
)
#location 1 KZN, 2 WC
bootstrap_number<-10000


#############generate age mixing matrices#######################


for (location in seq(1,2)) {
  #source("generate age mixing matrices-close numbers.R")
  #source("generate age mixing matrices-close time.R")
  #source("generate age mixing matrices-casual time.R")
}


###########generate combined age mixing matrices###################

#source("combined age mixing matrices-airborne.R")
#source("combined age mixing matrices-mtb.R")


##########main paper figures#################

#source("figure 2-contact bar graph.R")
#source("figures 3&4-main age mixing matrices.R")


##########supporting figures#################

#source("supporting figure-adolescent contacts.R")
#source("supporting figure-age mixing, hh from casual time.R")
#source("supporting figure-casual contact age mixing, different caps.R")
#source("supporting figure-casual contact time bar graph, different caps.R")
#source("supporting figure-contact time by location.R")
#source("supporting figure-cumulative contact time by people present.R")
#source("supporting figure-hh & non-hh age mixing matrices.R")





