
##################################################################
#4-way ANOVA for each experiment
#TASC.2
# summary(aov(RT ~ position_labels * novelty_labels * distractor_labels * block_labels + Error(subject_labels/(position_labels * novelty_labels * distractor_labels * block_labels)), data=TASC.2))
# anova(fit2<-lmer(RT ~ position_labels * novelty_labels * distractor_labels * block_labels + (1|subject_labels), data=TASC.2))
#TASC.3
# summary(aov(RT ~ position_labels * novelty_labels * distractor_labels * block_labels + Error(subject_labels/(position_labels * novelty_labels * distractor_labels * block_labels)), data=TASC.3))
# anova(fit2<-lmer(RT ~ position_labels * novelty_labels * distractor_labels * block_labels + (1|subject_labels), data=TASC.3))
#TASC.4
# summary(aov(RT ~ position_labels * novelty_labels * distractor_labels * block_labels + Error(subject_labels/(position_labels * novelty_labels * distractor_labels * block_labels)), data=TASC.4))
# anova(fit2<-lmer(RT ~ position_labels * novelty_labels * distractor_labels * block_labels + (1|subject_labels), data=TASC.4))
#TASC.6
# summary(aov(RT ~ position_labels * novelty_labels * distractor_labels * block_labels + Error(subject_labels/(position_labels * novelty_labels * distractor_labels * block_labels)), data=TASC.6))
# anova(fit2<-lmer(RT ~ position_labels * novelty_labels * distractor_labels * block_labels + (1|subject_labels), data=TASC.6))

##################################################################
#switch cost for novel(N) and repeated(N) in block 5 and 6 in switch
TASC.switch.normal <- TASC.4[TASC.4$distractor_labels=='normal' & (TASC.4$block_labels=='b5' | TASC.4$block_labels=='b6'),]
TASC.switch.normal <- TASC.6[TASC.6$distractor_labels=='normal' & (TASC.6$block_labels=='b5' | TASC.6$block_labels=='b6'),]
# TASC.switch.normal <- TASC.switch[TASC.switch$distractor_labels=='normal' & (TASC.switch$block_labels=='b5' | TASC.switch$block_labels=='b6'),]

# TASC.switch.normal$noveltyAndDistractor <- ifelse(TASC.switch.normal$novelty_labels == 'repeated' & TASC.switch.normal$distractor_labels == 'normal' & TASC.switch.normal$set_labels == '16 items', '16 repeated nondistractor',
#                                                   ifelse(TASC.switch.normal$novelty_labels == 'repeated' & TASC.switch.normal$distractor_labels == 'normal' & TASC.switch.normal$set_labels == '8 items', '8 repeated nondistractor',
#                                                          ifelse(TASC.switch.normal$novelty_labels == 'novel' & TASC.switch.normal$distractor_labels == 'normal' & TASC.switch.normal$set_labels == '16 items', '16 novel nondistractor',
#                                                                 ifelse(TASC.switch.normal$novelty_labels == 'novel' & TASC.switch.normal$distractor_labels == 'normal' & TASC.switch.normal$set_labels == '8 items', '8 novel nondistractor','others'))))
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
anova(gls(RT ~ novelty_labels*block_labels,data = TASC.switch.normal))
#repeated measure ANOVA https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/
summary(aov(RT ~ novelty_labels*block_labels + Error(subject_labels/(novelty_labels*block_labels)), data=TASC.switch.normal))
# anova(fit2<-lmer(RT ~ novelty_labels*block_labels + (1|subject_labels), data=TASC.switch.normal))
#post-hoc
#https://stats.stackexchange.com/questions/186319/repeated-measures-anova-how-to-locate-the-significant-differences-by-r
#https://stats.stackexchange.com/questions/14078/post-hoc-test-after-anova-with-repeated-measures-using-r

##################################################################
#Persistance of distraction effect for novel and repeated in block 5-10 in switch experiments
TASC.switch.novelty <- TASC.6[TASC.6$block_labels=='b1' | TASC.6$block_labels=='b2' | TASC.6$block_labels=='b3' | TASC.6$block_labels=='b4' | TASC.6$block_labels=='b5',]
TASC.switch.novelty <- TASC.6[TASC.6$block_labels=='b6' | TASC.6$block_labels=='b7' | TASC.6$block_labels=='b8' | TASC.6$block_labels=='b9' | TASC.6$block_labels=='b10',]
TASC.switch.novelty <- TASC.6[TASC.6$novelty_labels=='repeated' & (TASC.6$block_labels=='b1' | TASC.6$block_labels=='b2' | TASC.6$block_labels=='b3' | TASC.6$block_labels=='b4' | TASC.6$block_labels=='b5'),]
# TASC.switch.novelty <- TASC.6[TASC.6$novelty_labels=='repeated' & (TASC.6$block_labels=='b6' | TASC.6$block_labels=='b7' | TASC.6$block_labels=='b8' | TASC.6$block_labels=='b9' | TASC.6$block_labels=='b10'),]
# TASC.switch.novelty <- TASC.6[TASC.6$novelty_labels=='novel' & (TASC.6$block_labels=='b6' | TASC.6$block_labels=='b7' | TASC.6$block_labels=='b8' | TASC.6$block_labels=='b9' | TASC.6$block_labels=='b10'),]
TASC.switch.novelty <- TASC.4[TASC.4$block_labels=='b1' | TASC.4$block_labels=='b2' | TASC.4$block_labels=='b3' | TASC.4$block_labels=='b4' | TASC.4$block_labels=='b5',]
TASC.switch.novelty <- TASC.4[TASC.4$block_labels=='b6' | TASC.4$block_labels=='b7' | TASC.4$block_labels=='b8' | TASC.4$block_labels=='b9' | TASC.4$block_labels=='b10',]
# TASC.switch.novelty <- TASC.4[TASC.4$novelty_labels=='repeated' & (TASC.4$block_labels=='b1' | TASC.4$block_labels=='b2' | TASC.4$block_labels=='b3' | TASC.4$block_labels=='b4' | TASC.4$block_labels=='b5'),]
# TASC.switch.novelty <- TASC.4[TASC.4$novelty_labels=='repeated' & (TASC.4$block_labels=='b6' | TASC.4$block_labels=='b7' | TASC.4$block_labels=='b8' | TASC.4$block_labels=='b9' | TASC.4$block_labels=='b10'),]
# TASC.switch.novelty <- TASC.4[TASC.4$novelty_labels=='novel' & (TASC.4$block_labels=='b6' | TASC.4$block_labels=='b7' | TASC.4$block_labels=='b8' | TASC.4$block_labels=='b9' | TASC.4$block_labels=='b10'),]

# ggplot(TASC,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
# ggplot(TASC.switch.novelty,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor))+
ggplot(TASC.switch.novelty,aes(block_labels,RT,group=position_labels,col=position_labels))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)
# no repeated measure
anova(gls(RT ~ novelty_labels * distractor_labels * block_labels,data = TASC.switch.novelty))
#repeated measure ANOVA https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/
# summary(aov(RT ~ distractor_labels * block_labels + Error(subject_labels/(distractor_labels * block_labels)), data=TASC.switch.novelty))
summary(aov(RT ~  position_labels * block_labels + Error(subject_labels/(position_labels * block_labels)), data=TASC.switch.novelty))
# anova(fit2<-lmer(RT ~ novelty_labels * distractor_labels * block_labels + (1|subject_labels), data=TASC.switch.novelty))

##################################################################
#Do repeated trials with surprised distractor turn to novel trials in switch
TASC.switch.normal <- TASC.4[TASC.4$block_labels=='b6',]
TASC.switch.normal <- TASC.6[TASC.6$block_labels=='b6',]

ggplot(TASC.switch.normal,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) +
# ggplot(TASC.switch.normal,aes(distractor_labels,RT,group=novelty_labels,col=novelty_labels))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="errorbar",width=0.08,  size = 0.3)+
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)
#t-test between blocks TWO-SAMPLE
print('t-test for repeated distractor and novel distractor')
t.test(TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='repeated distractor'],TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='novel distractor'],paired=FALSE)
print('t-test for repeated distractor and novel nondistractor')
t.test(TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='repeated distractor'],TASC.switch.normal$RT[TASC.switch.normal$noveltyAndDistractor=='novel nondistractor'],paired=FALSE)
#t-test between blocks PAIRED
TASC.switch.normal_new = aggregate(TASC.switch.normal$RT, by=list(TASC.switch.normal$subject_labels,TASC.switch.normal$noveltyAndDistractor), FUN=mean)
colnames(TASC.switch.normal_new) <- c("subject_labels", "noveltyAndDistractor", "RT")
print('t-test for repeated distractor and novel distractor')
t.test(TASC.switch.normal_new$RT[TASC.switch.normal_new$noveltyAndDistractor=='repeated distractor'],TASC.switch.normal_new$RT[TASC.switch.normal_new$noveltyAndDistractor=='novel distractor'],paired=TRUE)
print('t-test for repeated distractor and novel nondistractor')
t.test(TASC.switch.normal_new$RT[TASC.switch.normal_new$noveltyAndDistractor=='repeated distractor'],TASC.switch.normal_new$RT[TASC.switch.normal_new$noveltyAndDistractor=='novel nondistractor'],paired=TRUE)
# no repeated measure
anova(gls(RT ~ distractor_labels*novelty_labels,data = TASC.switch.normal))
#repeated measure ANOVA https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/
summary(aov(RT ~ novelty_labels * distractor_labels + Error(subject_labels/(novelty_labels * distractor_labels)), data=TASC.switch.normal))
# anova(fit2<-lmer(RT ~ novelty_labels * distractor_labels + (1|subject_labels), data=TASC.switch.normal))

##################################################################
#Distraction effect in stay block 1-10 AND switch block 6-10
TASC.distract <- TASC.16items[(TASC.16items$switch_labels=='stay' ),]
TASC.distract <- TASC.16items[(TASC.16items$switch_labels=='switch' & (TASC.16items$block_labels=='b6' | TASC.16items$block_labels=='b7' | TASC.16items$block_labels=='b8' | TASC.16items$block_labels=='b9' | TASC.16items$block_labels=='b10')),]
TASC.distract <- TASC.8items[(TASC.8items$switch_labels=='stay' ),]
TASC.distract <- TASC.8items[(TASC.8items$switch_labels=='switch' & (TASC.8items$block_labels=='b6' | TASC.8items$block_labels=='b7' | TASC.8items$block_labels=='b8' | TASC.8items$block_labels=='b9' | TASC.8items$block_labels=='b10')),]

# ggplot(TASC.16items.distract,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.distract,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)
# no repeated measure
anova(gls(RT ~ novelty_labels * distractor_labels * block_labels,data = TASC.distract))
#repeated measure ANOVA https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/
summary(aov(RT ~ novelty_labels * distractor_labels * block_labels + Error(subject_labels/(novelty_labels * distractor_labels * block_labels)), data=TASC.distract))
# anova(fit2<-lmer(RT ~ novelty_labels * distractor_labels * block_labels + (1|subject_labels), data=TASC.distract))

##################################################################
#SL effect 
TASC.SL <- TASC.2[(TASC.2$novelty_labels=='repeated' ),]
TASC.SL <- TASC.6[(TASC.6$novelty_labels=='repeated' ),]
TASC.SL <- TASC.6[(TASC.6$novelty_labels=='repeated' & (TASC.6$block_labels=='b6' | TASC.6$block_labels=='b7' | TASC.6$block_labels=='b8' | TASC.6$block_labels=='b9' | TASC.6$block_labels=='b10')),]
TASC.SL <- TASC.3[(TASC.3$novelty_labels=='repeated' ),]
TASC.SL <- TASC.4[(TASC.4$novelty_labels=='repeated' ),]
TASC.SL <- TASC.4[(TASC.4$novelty_labels=='repeated' & (TASC.4$block_labels=='b6' | TASC.4$block_labels=='b7' | TASC.4$block_labels=='b8' | TASC.4$block_labels=='b9' | TASC.4$block_labels=='b10')),]

# ggplot(TASC.16items.distract,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.SL,aes(block_labels,RT,group=positionAndDistractor,col=positionAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)
# no repeated measure
anova(gls(RT ~ position_labels * distractor_labels * block_labels,data = TASC.SL))
#repeated measure ANOVA https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/
summary(aov(RT ~ position_labels * distractor_labels * block_labels + Error(subject_labels/(position_labels * distractor_labels * block_labels)), data=TASC.SL))
# anova(fit2<-lmer(RT ~ position_labels * distractor_labels * block_labels + (1|subject_labels), data=TASC.SL))

##################################################################
#SL effect in no distractor blocks
TASC.SL <- TASC.6[(TASC.6$novelty_labels=='repeated' & (TASC.6$block_labels=='b1' | TASC.6$block_labels=='b2' | TASC.6$block_labels=='b3' | TASC.6$block_labels=='b4' | TASC.6$block_labels=='b5')),]
TASC.SL <- TASC.4[(TASC.4$novelty_labels=='repeated' & (TASC.4$block_labels=='b1' | TASC.4$block_labels=='b2' | TASC.4$block_labels=='b3' | TASC.4$block_labels=='b4' | TASC.4$block_labels=='b5')),]

# ggplot(TASC.16items.distract,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor)) + geom_smooth(se=T,size=1) +geom_point(alpha = 0.3)
ggplot(TASC.SL,aes(block_labels,RT,group=position_labels,col=position_labels))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)
# no repeated measure
anova(gls(RT ~ position_labels * block_labels,data = TASC.SL))
#repeated measure ANOVA https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/
summary(aov(RT ~ position_labels * block_labels + Error(subject_labels/(position_labels * block_labels)), data=TASC.SL))
# anova(fit2<-lmer(RT ~ position_labels * block_labels + (1|subject_labels), data=TASC.SL))

##################################################################
#CC effect 
TASC.CC <- TASC.2
TASC.CC <- TASC.3
TASC.CC <- TASC.4
TASC.CC <- TASC.6

aggregate(TASC.CC$RT, by=list(TASC.CC$noveltyAndDistractor), FUN=mean)
switch.rect <- data.frame (xmin=5.5, xmax=10.5, ymin=-Inf, ymax=Inf)
p<-ggplot(TASC.CC,aes(block_labels,RT,group=noveltyAndDistractor,col=noveltyAndDistractor, fill = noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.3, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 2)+ 
  geom_rect(data=switch.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="darkgrey", alpha=0.1, inherit.aes = FALSE)+ 
  xlab("Block") + ylab("RT (ms)")+ theme(legend.title = element_blank())+ 
  scale_fill_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle1')),
                    breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
                    labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
  scale_colour_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle2')),
                      breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
                      labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
  scale_shape_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle2')),
                     breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
                     labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"))+
  theme(legend.position="none") + coord_cartesian(ylim=c(650,1070))
# scale_fill_manual(values = alpha(c("indianred3", "indianred2","darkolivegreen3",'darkolivegreen1')),
#                   breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
#                   labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
# scale_colour_manual(values = alpha(c("indianred4", "indianred2","darkolivegreen3",'darkolivegreen3')),
#                     breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
#                     labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
# scale_shape_manual(values = alpha(c("indianred4", "indianred2","darkolivegreen3",'darkolivegreen3')),
#                     breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
#                     labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+
# scale_fill_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle1')))+
# scale_colour_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle2')))
p
ggsave("/Users/chendanlei/Google Drive/Manuscripts/Predictive_Attention_Learn&Mem_manuscript/figures/Fig2a.png", width = 140, height = 120, units = c("mm"), p, bg = "transparent")

ggplot(TASC.CC,aes(block_labels,RT,group=distractor_labels,col=distractor_labels))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)

# no repeated measure
anova(gls(RT ~ novelty_labels * distractor_labels * block_labels,data = TASC.CC))
#repeated measure ANOVA https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/
summary(aov(RT ~ novelty_labels * distractor_labels * block_labels + Error(subject_labels/(novelty_labels * distractor_labels * block_labels)), data=TASC.CC,na.action = na.exclude))
# anova(fit2<-lmer(RT ~ novelty_labels * distractor_labels * block_labels + (1|subject_labels), data=TASC.CC,na.action = na.exclude))
summary(aov(RT ~ novelty_labels * block_labels + Error(subject_labels/(novelty_labels * block_labels)), data=TASC.CC,na.action = na.exclude))
# anova(fit2<-lmer(RT ~ novelty_labels * block_labels + (1|subject_labels), data=TASC.CC))

##################################################################
##CC effect overrides switch effect in repetition
pathname <- file.path(path, "TASC4_block6_repetition.mat")
pathname <- file.path(path, "TASC6_block6_repetition.mat")
mtx <- readMat(pathname)$mtx
TASC<- data.frame("RT"=mtx[,1],'subject_labels'=mtx[,2],'repetition'=mtx[,3],'novelty_labels'=mtx[,4],'distractor_labels'=mtx[,5])
TASC$RT=TASC$RT*1000
TASC$novelty_labels[TASC$novelty_labels==0] = 'repeated'
TASC$novelty_labels[TASC$novelty_labels==1] = 'novel'
TASC$distractor_labels[TASC$distractor_labels==0] = 'normal'
TASC$distractor_labels[TASC$distractor_labels==1] = 'distractor'
TASC$noveltyAndDistractor <- ifelse(TASC$novelty_labels == 'repeated' & TASC$distractor_labels == 'normal', 'repeated nondistractor',
                                    ifelse(TASC$novelty_labels == 'novel' & TASC$distractor_labels == 'normal', 'novel nondistractor',
                                           ifelse(TASC$novelty_labels == 'repeated' & TASC$distractor_labels == 'distractor', 'repeated distractor',
                                                  ifelse(TASC$novelty_labels == 'novel' & TASC$distractor_labels == 'distractor', 'novel distractor','others'))))
switch.rect <- data.frame (xmin=0.5, xmax=4.5, ymin=-Inf, ymax=Inf)
p<-ggplot(TASC,aes(repetition,RT,group=noveltyAndDistractor,col=noveltyAndDistractor, fill = noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.3, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 2)+
  geom_rect(data=switch.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="darkgrey", alpha=0.1, inherit.aes = FALSE)+ 
  xlab("repetition") + ylab("RT (ms)")+ theme(legend.title = element_blank())+ 
  scale_fill_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle1')),
                    breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
                    labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
  scale_colour_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle2')),
                      breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
                      labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
  scale_shape_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle2')),
                     breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
                     labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"))+
  theme(legend.position="none") + coord_cartesian(ylim=c(660,1150))
# scale_fill_manual(values = alpha(c("indianred3", "indianred2","darkolivegreen3",'darkolivegreen1')),
#                   breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
#                   labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
# scale_colour_manual(values = alpha(c("indianred4", "indianred2","darkolivegreen3",'darkolivegreen3')),
#                     breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
#                     labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+ 
# scale_shape_manual(values = alpha(c("indianred4", "indianred2","darkolivegreen3",'darkolivegreen3')),
#                     breaks=c("novel distractor", "novel nondistractor", "repeated distractor", "repeated nondistractor"),
#                     labels=c("Novel Singleton Present", "Novel Singleton Absent", "Repeated Singleton Present", "Repeated Singleton Absent"))+
# scale_fill_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle1')))+
# scale_colour_manual(values = alpha(c("dodgerblue4", "dodgerblue3","plum2",'thistle2')))
p
# ggsave("/Users/chendanlei/Google Drive/Manuscripts/Predictive_Attention_Learn&Mem_manuscript/figures/Fig3b1.png", width = 140, height = 120, units = c("mm"), p, bg = "transparent")

#repeated measure ANOVA https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/
summary(aov(RT ~ novelty_labels * distractor_labels * repetition + Error(subject_labels/(novelty_labels * distractor_labels * repetition)), data=TASC,na.action = na.exclude))
# anova(fit2<-lmer(RT ~ novelty_labels * distractor_labels * repetition + (1|subject_labels), data=TASC))

ggplot(TASC[TASC$distractor_labels=='distractor',],aes(repetition,RT,group=noveltyAndDistractor,col=noveltyAndDistractor, fill = noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.data=mean_se, geom="ribbon", alpha = 0.15, colour = NA)+
  stat_summary(fun.data=mean_se, geom="line", size = 0.5)+
  stat_summary(fun.y=mean, geom="point", size = 1.5)
#repeated measure ANOVA https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/
summary(aov(RT ~ novelty_labels * repetition + Error(subject_labels/(novelty_labels * repetition)), data=TASC[TASC$distractor_labels=='distractor',],na.action = na.exclude))
# anova(fit2<-lmer(RT ~ novelty_labels * repetition + (1|subject_labels), data=TASC[TASC$distractor_labels=='distractor',]))

switch.rect <- data.frame (xmin=0.25, xmax=1.75, ymin=-Inf, ymax=Inf)
p<-ggplot(TASC[(TASC$distractor_labels=='distractor' & TASC$repetition==1),],aes(repetition,RT,group=noveltyAndDistractor, fill = noveltyAndDistractor))+
  # geom_point(alpha = 0.1, size = 1.5) +
  stat_summary(fun.y=mean,position=position_dodge(width=0.90), alpha = 0.3, geom="bar", size = 1.5)+
  stat_summary(fun.data=mean_se,position=position_dodge(width=0.90), geom="errorbar",width=0.1,  size = 0.5) +
  scale_x_continuous(breaks=c(0,1,2))+
  geom_rect(data=switch.rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="darkgrey", alpha=0.1, inherit.aes = FALSE)+ 
  xlab("repetition") + ylab("RT (ms)")+ theme(legend.title = element_blank())+ 
  scale_fill_manual(values = alpha(c("dodgerblue4", "plum2")),
                    breaks=c("novel distractor", "repeated distractor"),
                    labels=c("Novel Singleton Present", "Repeated Singleton Present"))+ 
  scale_colour_manual(values = alpha(c("dodgerblue4", "plum2")),
                      breaks=c("novel distractor", "repeated distractor"),
                      labels=c("Novel Singleton Present", "Repeated Singleton Present"))+ 
  scale_shape_manual(values = alpha(c("dodgerblue4", "plum2")),
                     breaks=c("novel distractor", "repeated distractor"),
                     labels=c("Novel Singleton Present", "Repeated Singleton Present"))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold")) + coord_cartesian(ylim=c(660,1150))+
  theme(legend.position="none")
p
# ggsave("/Users/chendanlei/Google Drive/Manuscripts/Predictive_Attention_Learn&Mem_manuscript/figures/Fig3b2.png", width = 80, height = 120, units = c("mm"), p, bg = "transparent")

#t-test
print('t-test for repeated distractor and novel distractor')
t.test(TASC$RT[TASC$noveltyAndDistractor=='novel distractor'],TASC$RT[TASC$noveltyAndDistractor=='repeated distractor'],paired=TRUE)

