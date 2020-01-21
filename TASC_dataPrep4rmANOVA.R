#for mixed-effect models: https://www.jaredknowles.com/journal/2013/11/25/getting-started-with-mixed-effect-models-in-r
# http://dwoll.de/rexrepos/posts/anovaMixed.html#using-lme-from-package-nlme-3
#install.packages('R.matlab')
#install.packages("lme4")
#install.packages("rlist")
#install.packages("nlme")
library(R.matlab)
library(lme4)
library(rlist)
library(arm)
library(nlme)
library(lmerTest)
library(ggplot2)

path <- '/Users/chendanlei/Google Drive/Predictive Attention/TASC R files'
#change this for TASC2, TASC3, TASC4, TASC6
#switch: 4,6; stay:2,3
#16: 2,6; 8: 3,4
#TASC_files <- c('2','3','4','6')
TASC_files <- c('4','6')
# block <- c(6:10)
# block_name = c('b6','b7','b8','b9','b10')
# block <- c(1:5)
# block_name = c('b1','b2','b3','b4','b5')
block <- c(1:10)
block_name = c('b1','b2','b3','b4','b5','b6','b7','b8','b9','b10')
block_name = c('1','2','3','4','5','6','7','8','9','10')
# block <- c(5:6)
# block_name = c('b5','b6')
# N1,N2,D1,D2,NovN,NovD,Nov1,Nov2,NovN1,NovD1,NovN2,NovD2
condition <- c(1:4,5:6)
level_of_conditions <- length(condition)
condition_total <- c('N1','N2','D1','D2','Nov(N)','Nov(D)','Nov1','Nov2','Nov(N1)','Nov(D1)','Nov(N2)','Nov(D2)')
condition_name <- condition_total[condition]
novelty_total <- c('repeated','repeated','repeated','repeated','novel','novel','novel','novel','novel','novel','novel','novel')
novelty_name <- novelty_total[condition]
position_total <- c('1st','2nd','1st','2nd','novel','novel','1st','2nd','1st','2nd','1st','2nd')
position_name <- position_total[condition]
distractor_total <- c('normal','normal','distractor','distractor','normal','distractor','novel','novel','normal','distractor','normal','distractor')
# distractor_total_before_switch <- c('normal','normal','normal','normal','normal','normal','novel','novel','normal','normal','normal','normal')
distractor_total_before_switch <- c('normal','normal','distractor','distractor','normal','distractor','novel','novel','normal','distractor','normal','distractor')
distractor_total_after_switch <- c('normal','normal','distractor','distractor','normal','distractor','novel','novel','normal','distractor','normal','distractor')
distractor_name <- distractor_total[condition]
distractor_name_before_switch <- distractor_total_before_switch[condition]
distractor_name_after_switch <- distractor_total_after_switch[condition]
num_subj <- list('2'=40, '3'=38, '4'=40, '6'=40)
num_subj <- num_subj[TASC_files]

rawRT=list()
# TASC2
pathname <- file.path(path, "TASC2_medianRT_all_conditions.mat")
#each column is #of subject median RT in each block (#of subject*10)
rawRT[['2']] <- readMat(pathname)$medianRT
# TASC3
pathname <- file.path(path, "TASC3_medianRT_all_conditions.mat")
rawRT[['3']] <- readMat(pathname)$medianRT
# TASC4
pathname <- file.path(path, "TASC4_medianRT_all_conditions.mat")
rawRT[['4']] <- readMat(pathname)$medianRT
# pathname <- file.path(path, "TASC4_subjAcc_all_conditions.mat")
# rawRT[['4']] <- readMat(pathname)$subjAcc
# TASC6
pathname <- file.path(path, "TASC6_medianRT_all_conditions.mat")
rawRT[['6']] <- readMat(pathname)$medianRT
# pathname <- file.path(path, "TASC6_subjAcc_all_conditions.mat")
# rawRT[['6']] <- readMat(pathname)$subjAcc

trial_used_in_each_condition <-list()
for (t in TASC_files){
  for (b in block){
    trial_used_in_each_condition[[t]]=c(trial_used_in_each_condition[[t]],c((num_subj[[t]]*(b-1)+1)) : (num_subj[[t]]*b))
  }
}
#initiate the length of desired RT for each data set
TASC_RT=list()
valid_trials=list()
for (t in TASC_files){
  TASC_RT[t]=lapply(1, function(x) matrix(NA, nrow=dim(rawRT[[t]])[1], ncol=length(trial_used_in_each_condition[[t]])))
  for (condi in condition){
    TASC_RT[[t]][condi,] = rawRT[[t]][condi,trial_used_in_each_condition[[t]]]
  }
  TASC_RT[[t]] = as.vector(t(TASC_RT[[t]]))
  TASC_RT[[t]][TASC_RT[[t]] == 'NaN'] <- NA
  
  valid_trials[[t]] <- !is.na(TASC_RT[[t]])
  TASC_RT[[t]] <- TASC_RT[[t]][valid_trials[[t]]]
}

#labels
block_labels=list()
distractor_labels=list()
position_labels=list()
novelty_labels=list()
subject_labels=list()
item_labels=list()
switch_labels=list()
for (t in TASC_files){
  #block
  for (condi in condition){
    if (condi == 1){
      block_labels[[t]] <- gl(length(block),num_subj[[t]],labels=block_name)
    }else{
      block_labels[[t]] <- unlist(list(block_labels[[t]], gl(length(block),num_subj[[t]],labels=block_name)))
    }
  }
  block_labels[[t]] <- block_labels[[t]][valid_trials[[t]]]
  
  #distractor
  for (condi in condition){
    if ((t=='2' || t=='3') && condi == 1){
      distractor_labels[[t]] <- gl(1,num_subj[[t]]*length(block),labels=distractor_name[condi])
    }else if ((t=='2' || t=='3') && condi != 1){
      distractor_labels[[t]] <- unlist(list(distractor_labels[[t]], gl(1,num_subj[[t]]*length(block),labels=distractor_name[condi])))
    }
  }
  for (condi in condition){
    if ((t=='4' || t=='6') && condi == 1){
      distractor_labels[[t]] <- gl(1,num_subj[[t]]*(length(block)/2),labels=distractor_name_before_switch[condi])
      distractor_labels[[t]] <- unlist(list(distractor_labels[[t]], gl(1,num_subj[[t]]*(length(block)/2),labels=distractor_name_after_switch[condi])))
    }else if ((t=='4' || t=='6') && condi != 1){
      distractor_labels[[t]] <- unlist(list(distractor_labels[[t]], gl(1,num_subj[[t]]*(length(block)/2),labels=distractor_name_before_switch[condi])))
      distractor_labels[[t]] <- unlist(list(distractor_labels[[t]], gl(1,num_subj[[t]]*(length(block)/2),labels=distractor_name_after_switch[condi])))
      print(length(gl(1,num_subj[[t]]*(length(block)/2),labels=distractor_name_before_switch[condi])))
      print(length(gl(1,num_subj[[t]]*(length(block)/2),labels=distractor_name_after_switch[condi])))
      print(length(distractor_labels[[t]]))
    }
  }
  distractor_labels[[t]] <- distractor_labels[[t]][valid_trials[[t]]]
  
  #position
  for (condi in condition){
    if (condi == 1){
      position_labels[[t]] <- gl(1,num_subj[[t]]*length(block),labels=position_name[condi])
    }else{
      position_labels[[t]] <- unlist(list(position_labels[[t]], gl(1,num_subj[[t]]*length(block),labels=position_name[condi])))
    }
  }
  position_labels[[t]] <- position_labels[[t]][valid_trials[[t]]]
  
  #novelty
  for (condi in condition){
    if (condi == 1){
      novelty_labels[[t]] <- gl(1,num_subj[[t]]*length(block),labels=novelty_name[condi])
    }else{
      novelty_labels[[t]] <- unlist(list(novelty_labels[[t]], gl(1,num_subj[[t]]*length(block),labels=novelty_name[condi])))
    }
  }
  novelty_labels[[t]] <- novelty_labels[[t]][valid_trials[[t]]]
  
  #subj
  if (t == TASC_files[1]){
    subject_labels[[t]] <- as.factor(rep(c(1:num_subj[[t]]),length(block)*length(condition)))
    max_subj <- max(as.numeric(as.character(subject_labels[[t]])))
  }else{
    subject_labels[[t]] <- as.factor(rep(c((1+max_subj):(num_subj[[t]]+max_subj)),length(block)*length(condition)))
    max_subj <- max(as.numeric(as.character(subject_labels[[t]])))
  }
  subject_labels[[t]] <- subject_labels[[t]][valid_trials[[t]]]
  
  #item
  for (condi in condition){
    if (t == '2' || t == '6'){
      item_labels[[t]] <- gl(1,num_subj[[t]]*level_of_conditions*length(block),labels=c('16 items'))
    }else if (t == '3' || t == '4'){
      item_labels[[t]] <- gl(1,num_subj[[t]]*level_of_conditions*length(block),labels=c('8 items'))
    }
  }
  item_labels[[t]] <- item_labels[[t]][valid_trials[[t]]]
  
  #switch
  for (condi in condition){
    if (t == '2' || t == '3'){
      switch_labels[[t]] <- gl(1,num_subj[[t]]*level_of_conditions*length(block),labels=c('stay'))
    }else if (t == '4' || t == '6'){
      switch_labels[[t]] <- gl(1,num_subj[[t]]*level_of_conditions*length(block),labels=c('switch'))
    }
  }
  switch_labels[[t]] <- switch_labels[[t]][valid_trials[[t]]]
}

RT=list()
block_labels_all=list()
distractor_labels_all=list()
position_labels_all=list()
novelty_labels_all=list()
set_labels_all=list()
switch_labels_all=list()
subject_labels_all=list()
for (t in TASC_files){
  if (t==TASC_files[1]){
    RT <- TASC_RT[[t]]
    block_labels_all <- block_labels[[t]]
    distractor_labels_all <- distractor_labels[[t]]
    position_labels_all <- position_labels[[t]]
    novelty_labels_all <- novelty_labels[[t]]
    set_labels_all <- item_labels[[t]]
    switch_labels_all <- switch_labels[[t]]
    subject_labels_all <- subject_labels[[t]]
  }else{
    RT <- unlist(c(RT,TASC_RT[[t]]))
    block_labels_all<- unlist(list(block_labels_all,block_labels[[t]]))
    distractor_labels_all <- unlist(list(distractor_labels_all,distractor_labels[[t]]))
    position_labels_all <- unlist(list(position_labels_all,position_labels[[t]]))
    novelty_labels_all <- unlist(list(novelty_labels_all,novelty_labels[[t]]))
    subject_labels_all <- unlist(list(subject_labels_all,subject_labels[[t]]))
    set_labels_all <- unlist(list(set_labels_all,item_labels[[t]]))
    switch_labels_all <- unlist(list(switch_labels_all,switch_labels[[t]]))
  }
}

RT=RT*1000

TASC <- data.frame("RT"=RT,"block_labels"=block_labels_all,"distractor_labels"=distractor_labels_all,"position_labels"=position_labels_all,"novelty_labels"=novelty_labels_all,
                   "subject_labels"=subject_labels_all,"switch_labels"=switch_labels_all,"set_labels"=set_labels_all)
TASC$noveltyAndDistractor <- ifelse(TASC$novelty_labels == 'repeated' & TASC$distractor_labels == 'normal', 'repeated nondistractor',
                                    ifelse(TASC$novelty_labels == 'novel' & TASC$distractor_labels == 'normal', 'novel nondistractor',
                                           ifelse(TASC$novelty_labels == 'repeated' & TASC$distractor_labels == 'distractor', 'repeated distractor',
                                                  ifelse(TASC$novelty_labels == 'novel' & TASC$distractor_labels == 'distractor', 'novel distractor','others'))))
TASC$positionAndDistractor <- ifelse(TASC$position_labels == '1st' & TASC$distractor_labels == 'normal', '1st nondistractor',
                                     ifelse(TASC$position_labels == '2nd' & TASC$distractor_labels == 'normal', '2nd nondistractor',
                                            ifelse(TASC$position_labels == '1st' & TASC$distractor_labels == 'distractor', '1st distractor',
                                                   ifelse(TASC$position_labels == '2nd' & TASC$distractor_labels == 'distractor', '2nd distractor','others'))))
#summary(TASC)
plot.design(TASC$RT ~ ., data = TASC)
TASC.switch <- TASC[TASC$switch_labels =='switch',]
TASC.stay <- TASC[TASC$switch_labels =='stay',]
TASC.8items <- TASC[TASC$set_labels =='8 items',]
TASC.16items <- TASC[TASC$set_labels =='16 items',]
TASC.2 <- TASC[TASC$set_labels =='16 items' & TASC$switch_labels =='stay',]
TASC.3 <- TASC[TASC$set_labels =='8 items' & TASC$switch_labels =='stay',]
TASC.4 <- TASC[TASC$set_labels =='8 items' & TASC$switch_labels =='switch',]
TASC.6 <- TASC[TASC$set_labels =='16 items' & TASC$switch_labels =='switch',]

