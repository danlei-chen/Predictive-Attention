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
TASC_files <- c('2','3','4','6')
# block <- c(6:10)
# block_name = c('b6','b7','b8','b9','b10')
#block <- c(1:5)
# block_name = c('b1','b2','b3','b4','b5')
block <- c(1:10)
block_name = c('b1','b2','b3','b4','b5','b6','b7','b8','b9','b10')
# block <- c(5:6)
# block_name = c('b5','b6')
#N1,N2,D1,D2,NovN,NovD,Nov1,Nov2,NovN1,NovD1,NovN2,NovD2
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
num_subj <- list('2'=40, '3'=38, '4'=39, '6'=39)
num_subj <- num_subj[TASC_files]

rawRT=list()
# TASC2
pathname <- file.path(path, "TASC2_medianRT_all_conditions.mat")
#each row is N1,N2,D1,D2,Nov(N),Nov(D)
#each column is #of subject median RT in each block (#of subject*10)
rawRT[['2']] <- readMat(pathname)$medianRT
# TASC3
pathname <- file.path(path, "TASC3_medianRT_all_conditions.mat")
rawRT[['3']] <- readMat(pathname)$medianRT
# TASC4
pathname <- file.path(path, "TASC4_medianRT_all_conditions.mat")
rawRT[['4']] <- readMat(pathname)$medianRT
# TASC6
pathname <- file.path(path, "TASC6_medianRT_all_conditions.mat")
rawRT[['6']] <- readMat(pathname)$medianRT

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


TASC <- data.frame("RT"=RT,"block_labels"=block_labels_all,"distractor_labels"=distractor_labels_all,"position_labels"=position_labels_all,"novelty_labels"=novelty_labels_all,
                   "subject_labels"=subject_labels_all,"switch_labels"=switch_labels_all,"set_labels"=set_labels_all)
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

##################################################################
#switch cost for novel(N) and repeated(N) in block 5 and 6 in switch
TASC.switch.normal <- TASC.switch[TASC.switch$distractor_labels=='normal' & (TASC.switch$block_labels=='b5' | TASC.switch$block_labels=='b6'),]
TASC.switch.normal <- TASC.4[TASC.4$distractor_labels=='normal' & (TASC.4$block_labels=='b5' | TASC.4$block_labels=='b6'),]
TASC.switch.normal <- TASC.6[TASC.6$distractor_labels=='normal' & (TASC.6$block_labels=='b5' | TASC.6$block_labels=='b6'),]

TASC.switch.normal$noveltyAndDistractor <- ifelse(TASC.switch.normal$novelty_labels == 'repeated' & TASC.switch.normal$distractor_labels == 'normal' & TASC.switch.normal$set_labels == '16 items', '16 repeated nondistractor',
                                                       ifelse(TASC.switch.normal$novelty_labels == 'repeated' & TASC.switch.normal$distractor_labels == 'normal' & TASC.switch.normal$set_labels == '8 items', '8 repeated nondistractor',
                                                              ifelse(TASC.switch.normal$novelty_labels == 'novel' & TASC.switch.normal$distractor_labels == 'normal' & TASC.switch.normal$set_labels == '16 items', '16 novel nondistractor',
                                                                     ifelse(TASC.switch.normal$novelty_labels == 'novel' & TASC.switch.normal$distractor_labels == 'normal' & TASC.switch.normal$set_labels == '8 items', '8 novel nondistractor','others'))))
TASC.switch.normal$noveltyAndDistractor <- ifelse(TASC.switch.normal$novelty_labels == 'repeated' & TASC.switch.normal$distractor_labels == 'normal', 'repeated nondistractor',
                                                       ifelse(TASC.switch.normal$novelty_labels == 'novel' & TASC.switch.normal$distractor_labels == 'normal', 'novel nondistractor','others'))
# ggplot(TASC,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.switch.normal,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)

#t-test between blocks
print('paired t-test for novel nondistractor RT between block 5 and 6')
t.test(TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='novel nondistractor' & TASC.switch.normal$block_labels == 'b5'],TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='novel nondistractor' & TASC.switch.normal$block_labels == 'b6'],paired=TRUE)
print('paired t-test for novel distractor RT between block 5 and 6')
t.test(TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='repeated nondistractor' & TASC.switch.normal$block_labels == 'b5'],TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='repeated nondistractor' & TASC.switch.normal$block_labels == 'b6'],paired=TRUE)
# no repeated measure
anova(gls(RT ~ novelty_labels*block_labels*set_labels,data = TASC.switch.normal))
#repeated measure with only subject as random effect
anova(lme(RT ~ novelty_labels * block_labels, data = TASC.switch.normal, random = ~1|subject_labels, method = 'ML'))
# anova(lme(RT ~ novelty_labels * block_labels, random=list(subject_labels=pdBlocked(list(~1, pdIdent(~novelty_labels -1), pdIdent(~block_labels-1)))), data=TASC.switch.normal))
# summary(aov(RT ~ novelty_labels*block_labels + Error(subject_labels/(novelty_labels*block_labels)), data=TASC.switch.normal))
# anova(lmer(RT ~ novelty_labels * block_labels + (1|subject_labels), data = TASC.switch.normal))
#repeated measure with both set labels and subject labels as random effect
anova(lme(RT ~ novelty_labels * block_labels, data = TASC.switch.normal, random = list(~1|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim")))
#get random effect http://r.789695.n4.nabble.com/Lme-does-not-work-without-a-random-effect-UNCLASSIFIED-td850392.html
#to test random effect significance 
fit1 <- lm(RT ~ novelty_labels * block_labels, data = TASC.switch.normal)
fit2 <- lme(RT ~ novelty_labels * block_labels, data = TASC.switch.normal, random = list(~1|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)
#to test random and fixed effect interaction significance 
fit1 <- lme(RT ~ novelty_labels * block_labels, data = TASC.switch.normal, random = list(~1|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
fit2 <- lme(RT ~ novelty_labels * block_labels, data = TASC.switch.normal, random = list(~novelty_labels*block_labels|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)

##################################################################
#Persistance of distraction effect for novel and repeated in block 5-10 in switch
TASC.switch.novelty <- TASC.switch[TASC.switch$novelty_labels=='novel' & (TASC.switch$block_labels=='b6' | TASC.switch$block_labels=='b7' | TASC.switch$block_labels=='b8' | TASC.switch$block_labels=='b9' | TASC.switch$block_labels=='b10'),]
# TASC.switch.novelty <- TASC.switch[TASC.switch$novelty_labels=='repeated' & (TASC.switch$block_labels=='b6' | TASC.switch$block_labels=='b7' | TASC.switch$block_labels=='b8' | TASC.switch$block_labels=='b9' | TASC.switch$block_labels=='b10'),]

TASC.switch.novelty$noveltyAndDistractor <- ifelse(TASC.switch.novelty$novelty_labels == 'repeated' & TASC.switch.novelty$distractor_labels == 'normal', 'repeated nondistractor',
                                                  ifelse(TASC.switch.novelty$novelty_labels == 'novel' & TASC.switch.novelty$distractor_labels == 'normal', 'novel nondistractor',
                                                         ifelse(TASC.switch.novelty$novelty_labels == 'repeated' & TASC.switch.novelty$distractor_labels == 'distractor', 'repeated distractor',
                                                                ifelse(TASC.switch.novelty$novelty_labels == 'novel' & TASC.switch.novelty$distractor_labels == 'distractor', 'novel distractor','others'))))
# ggplot(TASC,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.switch.novelty,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)

# no repeated measure
anova(gls(RT ~ distractor_labels*block_labels,data = TASC.switch.novelty))
#repeated measure with only subject as random effect
anova(lme(RT ~ distractor_labels * block_labels, data = TASC.switch.novelty, random = ~1|subject_labels, method = 'ML'))
#repeated measure with both set labels and subject labels as random effect
anova(lme(RT ~ distractor_labels * block_labels, data = TASC.switch.novelty, random = list(~distractor_labels|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim")))
#get random effect http://r.789695.n4.nabble.com/Lme-does-not-work-without-a-random-effect-UNCLASSIFIED-td850392.html
#to test random effect significance 
fit1 <- lm(RT ~ distractor_labels * block_labels, data = TASC.switch.novelty)
fit2 <- lme(RT ~ distractor_labels * block_labels, data = TASC.switch.novelty, random = list(~1|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)
#to test random*fixed effect interaction significance 
fit1 <- lme(RT ~ distractor_labels * block_labels, data = TASC.switch.novelty, random = list(~1|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
fit2 <- lme(RT ~ distractor_labels * block_labels, data = TASC.switch.novelty, random = list(~distractor_labels|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)

##################################################################
#Do repeated trials with surprised distractor turn to novel trials in switch
# TASC.switch.normal <- TASC.switch[TASC.switch$block_labels=='b6' & ((TASC.switch$novelty_labels == 'novel' & TASC.switch$distractor_labels == 'normal')| (TASC.switch$novelty_labels == 'repeated' & TASC.switch$distractor_labels == 'distractor')),]
TASC.switch.normal <- TASC.switch[TASC.switch$block_labels=='b6',]
# TASC.switch.normal <- TASC.4[TASC.4$block_labels=='b6',]
# TASC.switch.normal <- TASC.6[TASC.6$block_labels=='b6',]

TASC.switch.normal$noveltyAndDistractor <- ifelse(TASC.switch.normal$novelty_labels == 'novel' & TASC.switch.normal$distractor_labels == 'distractor', 'novel distractor',
                                                 ifelse(TASC.switch.normal$novelty_labels == 'repeated' & TASC.switch.normal$distractor_labels == 'distractor', 'repeated distractor',
                                                        ifelse(TASC.switch.normal$novelty_labels == 'novel' & TASC.switch.normal$distractor_labels == 'normal', 'novel nondistractor',
                                                               ifelse(TASC.switch.normal$novelty_labels == 'repeated' & TASC.switch.normal$distractor_labels == 'normal', 'repeated nondistractor','others'))))
# ggplot(TASC,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.switch.normal,aes(distractor_labels,RT,group=novelty_labels,col=novelty_labels))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="errorbar",width=0.08,  size = 0.3)+
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)
#t-test between blocks
print('t-test for repeated distractor and novel distractor')
t.test(TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='repeated distractor'],TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='novel distractor'],paired=FALSE)
print('t-test for repeated distractor and novel nondistractor')
t.test(TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='repeated distractor'],TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='novel nondistractor'],paired=FALSE)
# no repeated measure
anova(gls(RT ~ novelty_labels*distractor_labels,data = TASC.switch.normal))
#repeated measure with only subject as random effect
anova(lme(RT ~ novelty_labels * distractor_labels, data = TASC.switch.normal, random = ~1|subject_labels, method = 'ML'))
#repeated measure with both set labels and subject labels as random effect
anova(lme(RT ~ novelty_labels * distractor_labels, data = TASC.switch.normal, random = list(~1|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim")))
#get random effect http://r.789695.n4.nabble.com/Lme-does-not-work-without-a-random-effect-UNCLASSIFIED-td850392.html
#to test random effect significance 
fit1 <- lm(RT ~ novelty_labels * distractor_labels, data = TASC.switch.normal)
fit2 <- lme(RT ~ novelty_labels * distractor_labels, data = TASC.switch.normal, random = list(~1|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)
#to test random*fixed effect interaction significance 
fit1 <- lme(RT ~ novelty_labels * distractor_labels, data = TASC.switch.normal, random = list(~1|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
fit2 <- lme(RT ~ novelty_labels * distractor_labels, data = TASC.switch.normal, random = list(~novelty_labels * distractor_labels|set_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)

##################################################################
#Distraction effect in 16/stay (TASC2)
TASC.2$noveltyAndDistractor <- ifelse(TASC.2$novelty_labels == 'repeated' & TASC.2$distractor_labels == 'normal', 'repeated nondistractor',
                                            ifelse(TASC.2$novelty_labels == 'novel' & TASC.2$distractor_labels == 'normal', 'novel nondistractor',
                                                   ifelse(TASC.2$novelty_labels == 'repeated' & TASC.2$distractor_labels == 'distractor', 'repeated distractor',
                                                          ifelse(TASC.2$novelty_labels == 'novel' & TASC.2$distractor_labels == 'distractor', 'novel distractor','others'))))
ggplot(TASC.2,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.2,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)
ggplot(TASC.2,aes(block_labels,RT,group=distractor_labels,col=distractor_labels))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)

# no repeated measure
anova(gls(RT ~ distractor_labels*block_labels,data = TASC.2))
#repeated measure with only subject as random effect
anova(lme(RT ~ distractor_labels * block_labels, data = TASC.2, random = ~1|subject_labels, method = 'ML'))
anova(lme(RT ~ distractor_labels * block_labels, random=list(subject_labels=pdBlocked(list(~1, pdIdent(~distractor_labels -1), pdIdent(~block_labels-1)))), data=TASC.2))
summary(aov(RT ~ distractor_labels*block_labels + Error(subject_labels/(distractor_labels*block_labels)), data=TASC.2))
anova(lmer(RT ~ distractor_labels * block_labels + (1|subject_labels), data = TASC.2))

##################################################################
#Distraction effect and SL repeated trials in 16/stay (TASC2)
TASC.2.repeat <- TASC.2[TASC.2$novelty_labels=='repeated',]
TASC.2.repeat$noveltyAndDistractor <- ifelse(TASC.2.repeat$position_labels == '1st' & TASC.2.repeat$distractor_labels == 'normal', '1st nondistractor',
                                          ifelse(TASC.2.repeat$position_labels == '2nd' & TASC.2.repeat$distractor_labels == 'normal', '2nd nondistractor',
                                             ifelse(TASC.2.repeat$position_labels == '1st' & TASC.2.repeat$distractor_labels == 'distractor', '1st distractor',
                                                ifelse(TASC.2.repeat$position_labels == '2nd' & TASC.2.repeat$distractor_labels == 'distractor', '2nd distractor','others'))))
ggplot(TASC.2.repeat,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.2.repeat,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)

# no repeated measure
anova(gls(RT ~ distractor_labels*position_labels*block_labels,data = TASC.2))
#repeated measure with only subject as random effect
anova(lme(RT ~ distractor_labels * position_labels * block_labels, data = TASC.2, random = ~1|subject_labels, method = 'ML'))

##################################################################
#Distraction effect in 16/stay (TASC2) block 1-10 AND 16/switch (TASC6) block 6-10
TASC.16items.distract <- TASC.16items[(TASC.16items$switch_labels=='stay' & (TASC.16items$block_labels=='b1' | TASC.16items$block_labels=='b2' | TASC.16items$block_labels=='b3' | TASC.16items$block_labels=='b4' | TASC.16items$block_labels=='b5')) | (TASC.16items$switch_labels=='switch' & (TASC.16items$block_labels=='b6' | TASC.16items$block_labels=='b7' | TASC.16items$block_labels=='b8' | TASC.16items$block_labels=='b9' | TASC.16items$block_labels=='b10')),]
TASC.16items.distract <- TASC.16items[(TASC.16items$switch_labels=='stay' ),]
TASC.16items.distract <- TASC.16items[(TASC.16items$switch_labels=='switch' & (TASC.16items$block_labels=='b6' | TASC.16items$block_labels=='b7' | TASC.16items$block_labels=='b8' | TASC.16items$block_labels=='b9' | TASC.16items$block_labels=='b10')),]
TASC.16items.distract$block_labels[which(TASC.16items.distract$block_labels=='b6')]<-'b1'
TASC.16items.distract$block_labels[which(TASC.16items.distract$block_labels=='b7')]<-'b2'
TASC.16items.distract$block_labels[which(TASC.16items.distract$block_labels=='b8')]<-'b3'
TASC.16items.distract$block_labels[which(TASC.16items.distract$block_labels=='b9')]<-'b4'
TASC.16items.distract$block_labels[which(TASC.16items.distract$block_labels=='b10')]<-'b5'

ggplot(TASC.16items.distract,aes(block_labels,RT,group=distractor_labels,col=distractor_labels)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.16items.distract,aes(block_labels,RT,group=distractor_labels,col=distractor_labels))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)

TASC.16items.distract$noveltyAndDistractor <- ifelse(TASC.16items.distract$novelty_labels == 'repeated' & TASC.16items.distract$distractor_labels == 'normal', 'repeated nondistractor',
                                                     ifelse(TASC.16items.distract$novelty_labels == 'novel' & TASC.16items.distract$distractor_labels == 'normal', 'novel nondistractor',
                                                            ifelse(TASC.16items.distract$novelty_labels == 'repeated' & TASC.16items.distract$distractor_labels == 'distractor', 'repeated distractor',
                                                                   ifelse(TASC.16items.distract$novelty_labels == 'novel' & TASC.16items.distract$distractor_labels == 'distractor', 'novel distractor','others'))))
ggplot(TASC.16items.distract,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.16items.distract,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)

# no repeated measure
anova(gls(RT ~ distractor_labels * novelty_labels * block_labels,data = TASC.16items.distract))
#repeated measure with only subject as random effect
anova(lme(RT ~ distractor_labels * novelty_labels * block_labels, data = TASC.16items.distract, random = ~1|subject_labels, method = 'ML'))
#repeated measure with both set labels and subject labels as random effect
anova(lme(RT ~ distractor_labels * novelty_labels * block_labels, data = TASC.16items.distract, random = list(~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim")))
#get random effect http://r.789695.n4.nabble.com/Lme-does-not-work-without-a-random-effect-UNCLASSIFIED-td850392.html
#to test random effect significance 
fit1 <- lm(RT ~ distractor_labels * novelty_labels * block_labels, data = TASC.16items.distract)
fit2 <- lme(RT ~ distractor_labels * novelty_labels * block_labels, data = TASC.16items.distract, random = list(~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)
#to test random effect significance 
fit1 <- lme(RT ~ distractor_labels * novelty_labels * block_labels, data = TASC.16items.distract, random = list(~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
fit2 <- lme(RT ~ distractor_labels * novelty_labels * block_labels, data = TASC.16items.distract, random = list(~distractor_labels * novelty_labels |switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)

##################################################################
#SL effect for repeated trials only in block 1-5  in 16/stay (TASC2) AND 16/switch (TASC6) 
TASC.16items.distract <- TASC.16items[(TASC.16items$switch_labels=='stay' & TASC.16items$novelty_labels=='repeated' & (TASC.16items$block_labels=='b1' | TASC.16items$block_labels=='b2' | TASC.16items$block_labels=='b3' | TASC.16items$block_labels=='b4' | TASC.16items$block_labels=='b5')) | (TASC.16items$switch_labels=='switch' & TASC.16items$novelty_labels=='repeated' & (TASC.16items$block_labels=='b1' | TASC.16items$block_labels=='b2' | TASC.16items$block_labels=='b3' | TASC.16items$block_labels=='b4' | TASC.16items$block_labels=='b5')),]

TASC.16items.distract$noveltyAndDistractor <- ifelse(TASC.16items.distract$position_labels == '1st' & TASC.16items.distract$distractor_labels == 'normal', '1st nondistractor',
                                                     ifelse(TASC.16items.distract$position_labels == '2nd' & TASC.16items.distract$distractor_labels == 'normal', '2nd nondistractor',
                                                            ifelse(TASC.16items.distract$position_labels == '1st' & TASC.16items.distract$distractor_labels == 'distractor', '1st distractor',
                                                                   ifelse(TASC.16items.distract$position_labels == '2nd' & TASC.16items.distract$distractor_labels == 'distractor', '2nd distractor','others'))))
ggplot(TASC.16items.distract,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.16items.distract,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)

# no repeated measure
anova(gls(RT ~ position_labels*block_labels,data = TASC.16items.distract))
#repeated measure with only subject as random effect
anova(lme(RT ~ position_labels * distractor_labels * block_labels, data = TASC.16items.distract, random = ~1|subject_labels, method = 'ML'))
#repeated measure with both set labels and subject labels as random effect
anova(lme(RT ~ position_labels * distractor_labels * block_labels, data = TASC.16items.distract, random = list(~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim")))
#get random effect http://r.789695.n4.nabble.com/Lme-does-not-work-without-a-random-effect-UNCLASSIFIED-td850392.html
#to test random effect significance 
fit1 <- lm(RT ~ position_labels * distractor_labels * block_labels, data = TASC.16items.distract)
fit2 <- lme(RT ~ position_labels * distractor_labels * block_labels, data = TASC.16items.distract, random = list(~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)
#to test random effect significance 
fit1 <- lme(RT ~ position_labels * distractor_labels * block_labels, data = TASC.16items.distract, random = list(~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
fit2 <- lme(RT ~ position_labels * distractor_labels * block_labels, data = TASC.16items.distract, random = list(~position_labels|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)

##################################################################
#Contextual cueing effects
TASC.cc <- TASC
TASC.cc <- TASC.16items
TASC.cc <- TASC.8items
TASC.cc <- TASC.switch
TASC.cc <- TASC.stay
TASC.cc$noveltyAndDistractor <- ifelse(TASC.cc$novelty_labels == 'repeated' & TASC.cc$distractor_labels == 'normal', 'repeated nondistractor',
                                                     ifelse(TASC.cc$novelty_labels == 'novel' & TASC.cc$distractor_labels == 'normal', 'novel nondistractor',
                                                            ifelse(TASC.cc$novelty_labels == 'repeated' & TASC.cc$distractor_labels == 'distractor', 'repeated distractor',
                                                                   ifelse(TASC.cc$novelty_labels == 'novel' & TASC.cc$distractor_labels == 'distractor', 'novel distractor','others'))))
ggplot(TASC.cc,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)
# no repeated measure
anova(gls(RT ~ novelty_labels * distractor_labels*block_labels,data = TASC.cc))
#repeated measure with only subject as random effect
anova(lme(RT ~ novelty_labels * distractor_labels * block_labels, data = TASC.cc, random = ~1|subject_labels, method = 'ML'))
anova(lme(RT ~ novelty_labels * distractor_labels * block_labels, data = TASC.cc, random=list(subject_labels=pdBlocked(list(~1, pdIdent(~novelty_labels-1), pdIdent(~distractor_labels-1), pdIdent(~block_labels-1)))), method = 'ML'))

#repeated measure with both set labels and subject labels as random effect
anova(lme(RT ~ novelty_labels * distractor_labels * block_labels, data = TASC.cc, random = list(~novelty_labels|set_labels,~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim")))
anova(lme(RT ~ novelty_labels * distractor_labels * block_labels, data = TASC.cc, random=list(subject_labels=pdBlocked(list(~1, pdIdent(~novelty_labels-1), pdIdent(~distractor_labels-1), pdIdent(~block_labels-1))),~novelty_labels|set_labels,~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim")))
#to test random effect significance 
fit1 <- lm(RT ~ novelty_labels * distractor_labels * block_labels, data = TASC.cc)
fit2 <- lme(RT ~ novelty_labels * distractor_labels * block_labels, data = TASC.cc, random = list(~1|set_labels,~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)
#to test random effect significance 
fit1 <- lme(RT ~ novelty_labels * distractor_labels * block_labels, data = TASC.cc, random = list(~1|set_labels,~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
fit2 <- lme(RT ~ novelty_labels * distractor_labels * block_labels, data = TASC.cc, random = list(~novelty_labels|set_labels,~1|switch_labels,~1|subject_labels), method = 'ML',control = list(opt = "optim"))
anova(fit2,fit1)

#


random=list(subject_labels=pdBlocked(list(~1, pdIdent(~novelty_labels-1), pdIdent(~distractor_labels-1), pdIdent(~block_labels-1))),~novelty_labels|set_labels,~1|switch_labels,~1|subject_labels)














# 
# 
# 
# 
# #assuming non-hierarchical structure
# #1.one way anova
# summary(aov(RT ~ block_labels,data = TASC))
# TukeyHSD(aov(RT ~ block_labels,data = TASC))
# #the same result can be seen in here when we ignore the hierarchical structure
# summary(lm(RT ~ block_labels,data = TASC))
# #2.ANCOVA
# summary(aov(RT ~ block_labels + distractor_labels, data = TASC))
# #same as
# summary(lm(RT ~ block_labels + distractor_labels,data = TASC))
# 
# #1. test the need for multilevel model
# #fitting a baseline model that includes only the intercept, 
# # RT~1 means it predicts RT from only the intercept
# summary(gls(RT ~ 1, data = TASC, method = 'ML', na.action = na.exclude))
# #2. test the same model but allowing the intercepts to vary for one predictor
# #here our experiment should assume random intercept because people differ in their
# #specify the random part of the model using random = x|y, 1 denotes the intercept
# #I think this is how we determin whether the random effect is sig or not -- otherwise we won't include them into the model -- confirmed, this is true
# summary(lme(RT ~ 1, data = TASC, random = ~1|set_labels, method = 'ML'))
# summary(lme(RT ~ 1, data = TASC, random = ~1|switch_labels, method = 'ML'))
# #then we can (1)compare AIC and BIC between models for which the smaller the better that the model can fit
# #or (2)compute the difference between logLik(model)*-2 and difference in df, then look up chi-square with that df to see whether the difference is sig
# #or (3)anova(model1, model2) to see whether the difference is sig
# #and in our case both makes a sig change to the original model -- it's necessary for us to allow intercept to vary for both variables
# anova(gls(RT ~ 1, data = TASC, method = 'ML'),lme(RT ~ 1, data = TASC, random = ~1|set_labels, method = 'ML'))
# anova(gls(RT ~ 1, data = TASC, method = 'ML'),lme(RT ~ 1, data = TASC, random = ~1|switch_labels, method = 'ML'))
# #then we can add more predictor
# summary(lme(RT ~ distractor_labels, data = TASC, random = ~1|switch_labels, method = 'ML'))
# summary(lme(RT ~ distractor_labels + block_labels+novelty_labels, data = TASC, 
#             random = ~1|switch_labels, method = 'ML'))
# #to read the fixed effects table from the equation above:
# #nondistractor, block 1, repeated trials on average have RT 0.893 (sig)
# #distractor is slower by 0.0177
# #when block is 2 RT is faster by -0.057 compare with when block is 1 (the effect is sig)
# #novel has RT sig slower by 0.08 compare with old
# #then we allow the slope to vary: random = ~distractor_labels|switch_labels means we allow the effect of distractor to vary across switch labels -- cross level interaction
# #not significant: block_labels:distractor_labels, block_labels:distractor_labels:novelty_labels 
# #cross level interactions: http://www.rensenieuwenhuis.nl/r-sessions-21-multilevel-model-specification-nlme/
# finalModel <- lme(RT ~ distractor_labels + block_labels + novelty_labels + block_labels:novelty_labels + novelty_labels:distractor_labels, data = TASC, 
#                   random = list(~(distractor_labels+novelty_labels)|switch_labels, ~distractor_labels|set_labels), method = 'ML',control = list(opt = "optim"))
# summary(finalModel)
# anova(finalModel)
# #eventually we want something like this to demonstrate the mixed effects https://stackoverflow.com/questions/4798343/convert-factor-to-integer
# #NOTE: here we can't include position AND novelty both in the model because they're confounded fixed effects as novelty"novel" is the same as position"novel"
# #Note: we have to include the two-way interactions for the three-way interaction to work
# #And somehow intervals(finalModel) doesn't run so we can't check the fit
# 
# #to run a subset of the dataset
# summary(lme(RT ~ distractor_labels + block_labels + novelty_labels + block_labels:novelty_labels+novelty_labels:distractor_labels, data = TASC, 
#                   random = ~distractor_labels|set_labels, subset = TASC$set_labels=='16 items', method = 'ML',control = list(opt = "optim")))
# 
# # #if we wnat to model the growth curve:
# # summary(lme(RT ~ block_labels, data = TASC, random = ~1|switch_labels, method = 'ML'))
# # #in growth curve models we need to model covariance structures
# # summary(lme(RT ~ block_labels, data = TASC, random = ~1|switch_labels, method = 'ML',correlation = corAR1()))
# # summary(lme(RT ~ poly(as.numeric(as.character(block_labels,3))), data = TASC, random = ~1|switch_labels, method = 'ML',correlation = corAR1()))
# 
# # plot lme resutls: https://www.rdocumentation.org/packages/nlme/versions/3.1-137/topics/plot.lme
# 
# distractorModel <- lme(RT ~ distractor_labels , data = TASC, random = list(~1|switch_labels, ~1|set_labels), method = 'ML',control = list(opt = "optim"))
# summary(distractorModel)
# anova(distractorModel)
# 
# distractorModel <- lme(RT ~ distractor_labels , data = TASC, random = list(~1|switch_labels), subset = TASC$set_labels=='8 items', method = 'ML',control = list(opt = "optim"))
# summary(distractorModel)
# anova(distractorModel)
# 
# positionModel <- lme(RT ~ position_labels , data = TASC, random = list(~1|switch_labels), subset = TASC$position_labels=='1st', method = 'ML',control = list(opt = "optim"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #(1 | ) are random effects and others are fixed
# if (identical(TASC_files,c('2','3','4','6')) ){
#   # summary(aov(RT ~ block_labels*distractor_labels*position_labels + Error(switch_labels*set_labels/(block_labels*distractor_labels*position_labels)), data=TASC))
#   # TASC_mdl <- lme(RT~block_labels * distractor_labels * position_labels,random=list(~1|switch_labels, ~1|set_labels),data=TASC)
#   # TASC_mdl <- lme(RT ~ block_labels * distractor_labels * position_labels,random=list(~1, pdIdent(~switch_labels-1), pdIdent(~set_labels-1)),method="ML", data=TASC)
#   TASC_mdl <- lmer(RT ~ block_labels * distractor_labels * position_labels * (1 | switch_labels) * (1 | set_labels), data = TASC, REML = FALSE)
#   summary(TASC_mdl)
#   anova(lm(RT ~ block_labels*distractor_labels*position_labels*switch_labels*set_labels, data = TASC))
# }else if ( identical(TASC_files,c('2','3')) || identical(TASC_files,c('4','6')) ){
#   TASC_mdl <- lmer(RT ~ block_labels * distractor_labels * position_labels * (1 | set_labels), data = TASC, REML = FALSE)
#   summary(TASC_mdl)
# #  TASC_mdl <- lme(RT~block_labels * distractor_labels * position_labels, random=~1|set_labels,data=TASC)
#   anova(lm(RT ~ block_labels*distractor_labels*position_labels*set_labels, data = TASC))
# }else if ( identical(TASC_files,c('2','6')) || identical(TASC_files,c('3','4')) ){
#   TASC_mdl<- lmer(RT ~ block_labels * distractor_labels * position_labels * (1 | switch_labels), data = TASC, REML = FALSE)
#   anova(lm(RT ~ block_labels*distractor_labels*position_labels*switch_labels, data = TASC))
# }else if (identical(TASC_files,c('2')) || identical(TASC_files,c('3')) || identical(TASC_files,c('4')) || identical(TASC_files,c('6'))){
#   anova(lm(RT ~ block_labels*distractor_labels*position_labels, data = TASC))
# }  
# #TASC_mdl<- lme(TASC$RT ~ TASC$block_labels*TASC$distractor_labels*TASC$novelty_labels*TASC$position_labels, random= list(~1|TASC$switch_labels, ~1|TASC$set_labels), data = TASC)
# #display(TASC_mdl)
# #https://stats.stackexchange.com/questions/22988/how-to-obtain-the-p-value-check-significance-of-an-effect-in-a-lme4-mixed-mode
# anova(TASC_mdl)
# ranova(TASC_mdl, reduce.terms = FALSE)
# #difflsmeans(TASC_mdl)
# 
# 
# #multilevel modeling
# #http://www.bristol.ac.uk/cmm/learning/multilevel-models/what-why.html
# #difference between anova(lmer) and summary(lmer)
# #https://stats.stackexchange.com/questions/271488/differences-between-summary-and-anova-function-for-multilevel-lmer-model
# 
# repeated measure ANOVA
#http://rcompanion.org/handbook/I_09.html

