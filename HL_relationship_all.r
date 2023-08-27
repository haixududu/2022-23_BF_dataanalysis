################################################
# Moderation relationship
################################################


## read files ##

Avoid_seperate<- read_sav(

################################################
# Relationship between attention and avoidance #
################################################


library(ggeffects)

library(rsq)

#------ h4: relationship-----------
##############################
#total fixation duration (tfd)
##############################

# clean all

test_data <- split(Avoid_seperate, Avoid_seperate$cstrial)

# ----------- read data----------------
tffavo_mod1 <- glmer(avoidance ~  1 + logcsp_tff + logcsm_tff + (1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tffavo_mod2 <- glmer(avoidance ~  1 + logcsp_tff  + cstrial +  logcsp_tff*cstrial + (1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tffavo_mod3 <- glmer(avoidance ~  1 + logcsm_tff + cstrial+ logcsm_tff*cstrial + (1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )


tffavo_mod4 <- glmer(avoidance ~  1 + logcsp_tff + logcsm_tff +  cstrial + logcsm_tff*cstrial + logcsp_tff*cstrial + (1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )


tffavo_mod5 <- glmer(avoidance ~  1 + logtff + (1|Subject_id), data = Avoid,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tffavo_mod6 <- glmer(avoidance ~  1 + logtff + cstype + (1|Subject_id), data = Avoid,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tffavo_mod7 <- glmer(avoidance ~  1 + logtff + cstype + cstrial +  (1|Subject_id), data = Avoid,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )



 tffavo_mod8 <- glmer(avoidance ~  1 + logcsp_tff +  (1|Subject_id), data = test_data$`cs+_trial`,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

 tffavo_mod9 <- glmer(avoidance ~  1 + logcsp_tff + logcsm_tff + (1|Subject_id), data = test_data$`cs+_trial`,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tffavo_mod10 <- glmer(avoidance ~  1 +  logcsp_tff + (1|Subject_id), data = test_data$`cs-_trial`,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tffavo_mod11 <- glmer(avoidance ~  1 +  logcsp_tff + logcsm_tff + (1|Subject_id), data = test_data$`cs-_trial`,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tffavo_mod7 <- glmer(avoidance ~  1 + logtff + cstype + cstrial +  (1|Subject_id), data = Avoid,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

# longer tff predicts higher possibility of avodiance 




#----- Model selection ---------------------
anova(tffavo_mod1,tffavo_mod2)

anova(tffavo_mod1,tffavo_mod2,tffavo_mod4)

anova(tffavo_mod5,tffavo_mod6,tffavo_mod7)

#--------- fixed effect ----------------------

summary(tffavo_mod1)



summary (tffavo_mod2)
summary (tffavo_mod5)
summary (tffavo_mod6)
summary (tffavo_mod7)

summary (tffavo_mod8)
summary (tffavo_mod9)
summary (tffavo_mod10)
summary (tffavo_mod11)
#---------odd ratio--------------------
# odd ratio 
# standard error 
se <- sqrt(diag(vcov(tffavo_mod11)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(tffavo_mod11), LL = fixef(tffavo_mod11) - 1.96 * se, UL = fixef(tffavo_mod11) + 1.96 *
                se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)


performance::r2(tffavo_mod11)


se <- sqrt(diag(vcov(tffavo_mod5)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(tffavo_mod5), LL = fixef(tffavo_mod5) - 1.96 * se, UL = fixef(tffavo_mod5) + 1.96 *
                se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)

se <- sqrt(diag(vcov(tffavo_mod6)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(tffavo_mod6), LL = fixef(tffavo_mod6) - 1.96 * se, UL = fixef(tffavo_mod6) + 1.96 *
                se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)
#------- fix effect -------------------



plot(ggpredict(tffavo_mod2, c("logcsp_tff")),add.data = TRUE, colors = "#9496C4" ) + 
  labs(x = 'log(time to first fixation durtion_cs+ (seconds))', y = 'Probability of avoidance (%)')
plot(ggpredict(tffavo_mod1, terms  = c('logcsp_tff','cstrial')),add.data = TRUE,facet = TRUE, colors = c('#d9a9ec','#56717f')) +  labs(x = ('log(time to first fixation durtion_cs+ (seconds))'), y = 'Probability of avoidance (%)')

plot(ggpredict(tffavo_mod3, terms  = c('logcsp_tff','cstrial')),add.data = TRUE,facet = TRUE, colors = c('#d9a9ec','#56717f')) +  labs(x = ('log(time to first fixation durtion_cs+ (seconds))'), y = 'Probability of avoidance (%)')

plot(ggpredict(tffavo_mod5, terms  = c('logtff')),add.data = TRUE, colors = c('#d9a9ec','#56717f')) +  labs(x = ('log(time to first fixation durtion(seconds)'), y = 'Probability of avoidance (%)')


plot(ggpredict(tffavo_mod1, c("logcsm_tff")),add.data = TRUE, colors = '#ecd09c' ) + 
  labs(x = 'log(time to first fixation durtion_cs- (seconds))', y = 'Probability of avoidance (%)')
plot(ggpredict(tffavo_mod1, terms  = c('logcsm_tff','cstrial')),add.data = TRUE,facet = TRUE, colors = c('#ed434f','#377db7')) +  labs(x = 'log(time to first fixation durtion_cs- (seconds))', y = 'Probability of avoidance (%)') 


p1<- plot(ggpredict(tffavo_mod5, terms  = c('logtff')),add.data = TRUE, colors = c('#d9a9ec','#56717f')) +  labs(x = ('log(time to first fixation durtion(seconds)'), y = 'Probability of avoidance (%)', title = 'predictor: logtff')

p2<-plot(ggpredict(tffavo_mod6, terms  = c('logtff','cstype')),add.data = TRUE, facet = TRUE,colors = c('#d9a9ec','#56717f')) +  labs(x = ('log(time to first fixation durtion(seconds)'), y = 'Probability of avoidance (%)',  title = 'predictor: logtff + cstye')


p3<-plot(ggpredict(tffavo_mod7, terms  = c('logtff','cstype', 'cstrial')),add.data = TRUE, facet = TRUE,colors = c('#d9a9ec','#56717f')) +  labs(x = ('log(time to first fixation durtion(seconds)'), y = 'Probability of avoidance (%)', title = 'predictor: logtff + cstype + cstrial')

ggarrange(p1,p2,p3,ncol = 2,nrow = 2)



## plot log tff of cstype and cs trials 
tff1<- plot(ggpredict(tffavo_mod9, terms  = c('logcsp_tff')),add.data = TRUE, colors = c('#ed434f')) +
  labs(x = 'log(Time to First Fixation of CS+ type)(seconds)', 
       y = 'Probability of Avoidance (%)',title = 'A.Time to First Fixation of CS+ type predicted Probaility of Avoidance\n during CS+ trial')+ 
  scale_x_continuous(limits = c(-2, 3)) + scale_y_continuous(limits = c(0, 1)) + theme(plot.title = element_text(size = 11))

tff2<- plot(ggpredict(tffavo_mod9, terms  = c('logcsm_tff')),add.data = TRUE,colors = c('#377db7')) + 
  labs(x = 'log(Time to First Fixation of CS- type)(seconds)',  
       y = 'Probability of Avoidance(%)',title = 'B.Time to First Fixation of CS- type predicted Probaility of Avoidance\n during CS+ trial')+ 
  scale_x_continuous(limits = c(-2, 3)) + scale_y_continuous(limits = c(0, 1))+ theme(plot.title = element_text(size = 11))


#tff3<- plot(ggpredict(tffavo_mod11, terms  = c('logcsp_tff')),add.data = TRUE,colors = c('#d9a9ec')) + 
  labs(x = 'log(time to first fixation of CS+ type)(seconds)', 
       y = 'Probability of Avoidance (%)',title = 'Time to First Fixation of CS+ type predicted PA during CS- trial')+ 
  scale_x_continuous(limits = c(-2, 4)) +scale_y_continuous(limits = c(0, 1))

#tff4<- plot(ggpredict(tffavo_mod11, terms  = c('logcsm_tff')),add.data = TRUE,colors = c('#56717f')) + 
  labs(x = 'log(time to first fixation of CS- type)(seconds)', 
       y = 'Probability of Avoidance (%)',title = 'Time to First Fixation of CS+ type predicted PA during CS- trial')+ 
  scale_x_continuous(limits = c(-2, 4))

  

###################################
#probability of first fixation (pff)
####################################

# ---------- Model selection -------------

# longer tff of cs+ predicts higher possibility of avoidance 
pffavo_mod1 <- glmer(avoidance ~  1 + cs_pff + (1|Subject_id), data = test_dat$`cs`,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )
pffavo_mod2 <- glmer(avoidance ~  1 + cs_pff + cstrial+ (1|trial_number) + (1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 0 )
anova(pffavo_mod1,pffavo_mod2)


pffavo_mod3 <- glmer(avoidance ~  1 + cs_pff + (1|Subject_id), 
                     data = test_data$`cs+_trial`,family = binomial (link = "logit"), 
                     control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )
pffavo_mod4 <- glmer(avoidance ~  1 + cs_pff + (1|Subject_id), 
                     data = test_data$`cs-_trial`,family = binomial (link = "logit"), 
                     control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

#------------ Model results ------------------

summary(pffavo_mod3)

# odd ratio 
# standard error 
se <- sqrt(diag(vcov(pffavo_mod4)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(pffavo_mod4), LL = fixef(pffavo_mod4) - 1.96 * se, UL = fixef(pffavo_mod4) + 1.96 *se))
# exponentiating the coefficients ：
exp(tab)

performance::r2(pffavo_mod4)
#------- fix effect -------------------
library(scales)

layout(mat = matrix(c(2, 1, 0, 3), nrow = 2,ncol = 2),       
       heights = c(1, 2),    # Heights of the two rows
       widths = c(2, 1)) 
par(mar = c(5, 4, 0, 0))
plot(ggpredict(pffavo_mod2, c('cs_pff')), colors = c("#9496C4"),limits = c(0,1)) +
  labs(x = 'Probability of first fixation (%)', y = 'Probability of avoidance (%)') +
  scale_x_continuous(limits = c(0, 1))+
  scale_x_continuous(labels = percent_format()) +labs(colour = "pff")
par(mar = c(0, 4, 0, 0))
plot(ggpredict(pffavo_mod2, terms  = c('cs_pff','cstrial')),facet = TRUE, colors = c('#377db7','#ed434f'),limits = c(0,1) ) +
  labs(x = 'Probability of first fixation (%)', y = 'Probability of avoidance (%)') +
  scale_x_continuous(limits = c(0, 1))+
  scale_x_continuous(labels = percent_format()) +labs(colour = "cstrial")


pff1<- plot(ggpredict(pffavo_mod3, terms  = c('cs_pff')), colors = c('#ed434f'),limits = c(0,1) ) +
  labs(x = 'Probability of First Fixation (%)', y = 'Probability of Avoidance (%)', title = 'C.Probability of First Fixation predicted Probability of Avodiance during CS+ trial') +
  scale_x_continuous(limits = c(0, 1))+ scale_y_continuous(limits = c(0, 1)) + theme(plot.title = element_text(size = 11))

#pff2<- plot(ggpredict(pffavo_mod4, terms  = c('cs_pff')), colors = c("#9496C4"),limits = c(0,1) ) +
  labs(x = 'Probability of First Fixation (%)', y = 'Probability of avoidance (%)', title = 'PFF predicted PA during CS+ trial') +
  scale_x_continuous(limits = c(0, 1))+
  scale_x_continuous(labels = percent_format()) +labs(colour = "cstrial")

ggarrange(pff1,pff2,ncol = 2,nrow = 1)
###################################
# Total fixation duration
####################################

# ---------- Model selection -------------

# longer tfd of cs+ predicts higher possibility of avoidance 
tfdavo_mod1 <- glmer(avoidance ~  1 + logcsp_tfd  + logcsm_tfd  + (1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1 )
tfdavo_mod2 <- glmer(avoidance ~  1 + logcsp_tfd  + logcsm_tfd  + cstrial+ (1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1 )

anova(tfdavo_mod1,tfdavo_mod2)



tfdavo_mod8 <- glmer(avoidance ~  1 + logcsp_tfd +  
                       (1|Subject_id), 
                     data = test_data$`cs+_trial`,family = binomial (link = "logit"), 
                     control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tfdavo_mod9 <- glmer(avoidance ~  1 + logcsp_tfd + logcsm_tfd + (1|Subject_id), 
                     data = test_data$`cs+_trial`,family = binomial (link = "logit"), 
                     control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tfdavo_mod10 <- glmer(avoidance ~  1 +  logcsp_tfd + (1|Subject_id), 
                      data = test_data$`cs-_trial`,family = binomial (link = "logit"), 
                      control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

tfdavo_mod11 <- glmer(avoidance ~  1 +  logcsp_tfd + logcsm_tfd + (1|Subject_id), 
                      data = test_data$`cs-_trial`,family = binomial (link = "logit"), 
                      control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )

# model results

summary (tfdavo_mod11) 


# fixed effect 

# odd ratio 
# standard error 
se <- sqrt(diag(vcov(tfdavo_mod10)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(tfdavo_mod10), LL = fixef(tfdavo_mod10) - 1.96 * se, 
              UL = fixef(tfdavo_mod10) + 1.96 *se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)

performance::r2(tfdavo_mod10)

plot(ggpredict(tfdavo_mod2, c("logcsp_tfd")),add.data = TRUE, colors = "#9496C4")+
  labs(x = 'log(total fixation durtion_cs+ (seconds))', y = 'Probability of avoidance (%)')
  
plot(ggpredict(tfdavo_mod2, c("logcsm_tfd")),add.data = TRUE, colors = '#ecd09c' )+
  labs(x = 'log(total fixation durtion_cs- (seconds))', y = 'Probability of avoidance (%)') 

plot(ggpredict(tfdavo_mod2, terms  = c('logcsp_tfd','cstrial')),add.data = TRUE,facet = TRUE, colors = c('#d9a9ec','#56717f'))+ labs(x = 'total fixation durtion_cs+ (seconds)', y = 'Probability of avoidance (%)')
plot(ggpredict(tfdavo_mod2, terms  = c('logcsm_tfd','cstrial')),add.data = TRUE,facet = TRUE, colors = c('#ed434f','#377db7'))+ labs(x = 'total fixation durtion_cs- (seconds)', y = 'Probability of avoidance (%)')



## plot log tff of cstype and cs trials 
tfd1<- plot(ggpredict(tfdavo_mod9, terms  = c('logcsp_tfd')),add.data = TRUE, colors = c('#ed434f')) +
  labs(x = 'log(Total Fixation Duration of CS+ type)(seconds)', 
       y = 'Probability of avoidance (%)',title = 'D. Total fixation duration of CS+ type predicted Probability of Avoidance\n during CS+ trial')+ 
  scale_x_continuous(limits = c(-2, 2)) +  scale_y_continuous(limits = c(0, 1))+ theme(plot.title = element_text(size = 11))

tfd2<- plot(ggpredict(tfdavo_mod9, terms  = c('logcsm_tfd')),add.data = TRUE,colors = c('#377db7')) + 
  labs(x = 'log(Total Fixation Duration of CS- type)(seconds)',  
       y = 'Probability of avoidance(%)',title = 'E.Total fixation duration of CS- type predicted Probability of Avoidance\n during CS+ trial')+ 
  scale_x_continuous(limits = c(-2, 2))+  scale_y_continuous(limits = c(0, 1))+ theme(plot.title = element_text(size = 11))


tfd3<- plot(ggpredict(tfdavo_mod10, terms  = c('logcsp_tfd')),add.data = TRUE,colors = c('#d9a9ec')) + 
  labs(x = 'log(Total Fixation Duration of CS+ type)(seconds)', 
       y = 'Probability of avoidance (%)',
       title = 'Total fixation duration of CS- type 
       predicted Probability of Avoidance during CS- trial')+ 
  scale_x_continuous(limits = c(-2, 4)) + theme(plot.title = element_text(size = 11))




hlay <- rbind(c(1,2),
              c(3,NA),
              c(4,5))
select_grobs <- function(lay) {
  id <- unique(c(t(lay))) 
  id[!is.na(id)]
} 
gs<- list(tff1,tff2,pff1,tfd1,tfd2)
grid.arrange(grobs=gs[select_grobs(hlay)], layout_matrix=hlay)

######################
# moderation effects
######################

Avoid_relation <- read.csv('/Users/mac/Documents/Handylibrarian/HL_results_baseline/Avoid_relationship.csv', 
                           header = TRUE, sep = ",", fill = TRUE)


#--------- convert pd to binary splot ------------------------


# standardized data: PD has negative value thus standarization is better than logistic 
# ignore the individual difference 


Avoid_seperate$standardized_pd_csp <- scale(Avoid_seperate$csp_pd)
Avoid_seperate$standardized_pd_csm <- scale(Avoid_seperate$csm_pd)


# pd_csm
mean_pd_csm <- mean(Avoid_seperate$standardized_pd_csm, na.rm = TRUE)
sd_pd_csm <- sd(Avoid_seperate$standardized_pd_csm, na.rm = TRUE)
cutoff_high_pd_csm<- mean_pd_csm + sd_pd_csm 
cutoff_low_pd_csm<- mean_pd_csm - sd_pd_csm




# seperate CS+ trial 
test_data$'cs+_trial'$'standardized_pd_csp' <- scale(test_data$'cs+_trial'$'csp_pd')

test_data$'cs+_trial'$'standardized_pd_csp' <- scale(test_data$'cs+_trial'$'csm_pd')

# Calculate the mean while ignoring NA values
mean_pd_csp <- mean(test_data$'cs+_trial'$'standardized_pd_csp', na.rm = TRUE)
sd_pd_csp <- sd(test_data$'cs+_trial'$'standardized_pd_csp', na.rm = TRUE)
cutoff_high_pd_csp<- mean_pd_csp + sd_pd_csp 
cutoff_low_pd_csp<- mean_pd_csp - sd_pd_csp



 # dummy pd 
 # dichotomous data 
 test_data$'cs+_trial'$'dummypd_csp'<- ifelse(test_data$'cs+_trial'$'standardized_pd_csp' < cutoff_high_pd_csp, "low", "high")
 
 Avoid_seperate$dummypd_csm <- ifelse(Avoid_seperate$standardized_pd_csm < cutoff_high_pd_csm, "low", "high")
 
 # three level data ( high, low, and media)
 Avoid_seperate$dummycsp_pd_three <- cut(Avoid_seperate$standardized_pd_csp,
                                   breaks = c(-Inf, cutoff_low_pd_csp, cutoff_low_pd_csp, Inf),
                                   labels = c("m-sd", "m", "m+sd"),
                                   include.lowest = TRUE)
 
 

 # standardized data: tff and tfd 
 test_data$'cs+_trial'$'standardized_tfd_csp'<- scale( test_data$'cs+_trial'$'logcsp_tff')
 
 test_data$'cs+_trial'$'standardized_tfd_csm'<- scale(test_data$'cs+_trial'$'logcsm_tff')
 
 # standardized data: tff and tfd 
 
 test_data$'cs+_trial'$'standardized_tfd_csp'<- scale( test_data$'cs+_trial'$'logcsp_tfd')
 
 test_data$'cs+_trial'$'standardized_tfd_csm'<- scale(test_data$'cs+_trial'$'logcsm_tfd')

 

####################################################
# moderate model-- PD*tff (trial_number and cstrial) 
####################################################


#----model selection cs+------


pdtff_mod1 <- glmer(avoidance ~  1 + standardized_tff_csp + dummypd_csp + standardized_tff_csp*dummypd_csp+(1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1)

pdtff_mod2 <- glmer(avoidance ~  1 + standardized_tff_csp + dummypd_csp + standardized_tff_csp*dummypd_csp + cstrial+(1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1)
 
 
pdtff_mod1 <- glmer(avoidance ~  1 + standardized_tff_csp + dummypd_csp + standardized_tff_csp*dummypd_csp+(1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1)

pdtff_mod2 <- glmer(avoidance ~  1 + standardized_tff_csp + dummypd_csp + standardized_tff_csp*dummypd_csp + cstrial+(1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1)



anova(pdtff_mod1,pdtff_mod2)

#----- model results -----

summary(pdtff_mod2)

outlierTest(pdtff_mod2)

## effect size and expect 

# odd ratio 
# standard error 
se <- sqrt(diag(vcov(pdtff_mod2)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(pdtff_mod2), LL = fixef(pdtff_mod2) - 1.96 * se, UL = fixef(pdtff_mod2) + 1.96 *
                se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)

## effect size

#--------- figure ---------



p2<- plot(ggpredict(pdtff_mod2, terms  = c('standardized_tff_csp','dummypd_csp','cstrial')),add.data = TRUE,facet = TRUE,colors = c('#ed434f','#377db7'))  + 
  labs(x = 'log(time to first fixation_cs+ (seconds))', y = 'Probability of Avoidance (%)')
p1<- plot(ggpredict(pdtff_mod2, terms  = c('standardized_tff_csp','dummypd_csp')),add.data = TRUE,colors = c('#ed434f','#377db7'))  + 
  labs(x = 'log(time to first fixation_cs+ (seconds))', y = 'Probability of Avoidance (%)')


library(ggpubr)
library(tibble)


#----model selection cs-------


pdtff_mod2_csm <- glmer(avoidance ~  1 + standardized_tff_csm + dummypd_csm + standardized_tff_csp*dummypd_csm + cstrial+(1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1)


summary(pdtff_mod2_csm)



#Odd ratio 
se <- sqrt(diag(vcov(pdtff_mod2_csm)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(pdtff_mod2_csm), LL = fixef(pdtff_mod2_csm) - 1.96 * se, UL = fixef(pdtff_mod2_csm) + 1.96 *
                se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)

# plot data 
p4<- plot(ggpredict(pdtff_mod2_csm, terms  = c('standardized_tff_csm','dummypd_csm','cstrial')),add.data = TRUE,facet = TRUE,colors = c('#ed434f','#377db7'))  + 
  labs(x = 'log(time to first fixation_csm (seconds))', y = 'Probability of Avoidance (%)')
p3<- plot(ggpredict(pdtff_mod2_csm, terms  = c('standardized_tff_csm','dummypd_csm')),add.data = TRUE,colors = c('#ed434f','#377db7'))  + 
  labs(x = 'log(time to first fixation_cs- (seconds))', y = 'Probability of Avoidance (%)')

ggarrange(p1,p2,p3,p4,ncol = 2,nrow =2)

################################################
# PD*tff (cs+_trial and individual )
################################################

mean_pd_csp<- aggregate(csp_pd ~ Subject_id + cstrial, data = Avoid_seperate , FUN = mean)


################################
# moderate model-- PD*tfd
################################


pdtfd_mod1 <- glmer(avoidance ~  1 + standardized_tfd_csp + dummypd_csp + standardized_tfd_csp*dummypd_csp+(1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1)

pdtfd_mod2 <- glmer(avoidance ~  1 + standardized_tfd_csp + dummypd_csp + standardized_tfd_csp*dummypd_csp + cstrial+(1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1)



summary(pdtfd_mod2)

##-------- model results ------------

# odd ratio 
# standard error 
se <- sqrt(diag(vcov(pdtfd_mod2)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(pdtfd_mod2), LL = fixef(pdtfd_mod2) - 1.96 * se, UL = fixef(pdtfd_mod2) + 1.96 *
                se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)


#------- figure --------------------------

p2<- plot(ggpredict(pdtfd_mod2, terms  = c('standardized_tfd_csp','dummypd_csp','cstrial')),add.data = TRUE,facet = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(total fixation duration_cs+ (seconds))', y = 'Probability of Avoidance (%)')

p1<- plot(ggpredict(pdtfd_mod2, terms  = c('standardized_tfd_csp','dummypd_csp')),add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(total fixation duration_cs+ (seconds))', y = 'Probability of Avoidance (%)')


#------------

pdtfd_mod3 <- glmer(avoidance ~  1 + standardized_tfd_csm + dummypd_csm+ standardized_tfd_csp*dummypd_csm +(1|Subject_id), 
                    data = Avoid_seperate,family = binomial (link = "logit"), 
                    control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1)
pdtfd_mod4 <- glmer(avoidance ~  1 + standardized_tfd_csm + dummypd_csm + standardized_tfd_csp*dummypd_csm + 
                      cstrial+(1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), 
                    control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1 )


summary(pdtfd_mod4) # significant on cs- trial

##-------- model results ------------

# odd ratio 
# standard error 
se <- sqrt(diag(vcov(pdtfd_mod4)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(pdtfd_mod4), LL = fixef(pdtfd_mod4) - 1.96 * se, UL = fixef(pdtfd_mod4) + 1.96 *
                se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)

##-------------- figure----------

p4<- plot(ggpredict(pdtfd_mod4, terms  = c('standardized_tfd_csm','dummypd_csm','cstrial')),add.data = TRUE,facet = TRUE, colors = c('#56717f','#d9a9ec')) + labs(x = 'total fixation duration_cs- (seconds)', y = 'Probability of avoidance (%)')

p3<- plot(ggpredict(pdtfd_mod4, terms  = c('standardized_tfd_csm','dummypd_csm')),add.data = TRUE, colors = c('#56717f','#d9a9ec')) + labs(x = 'total fixation duration_cs- (seconds)', y = 'Probability of avoidance (%)')


ggarrange(p1,p2,p3,p4,ncol = 2,nrow =2)


################################
# moderate model-- PD*pff
################################

pdpff_mod1 <- glmer(avoidance ~  1 + cs_pff + dummypd_csp + cstrial+cs_pff*dummypd_csp*cstrial + (1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1)
pdpff_mod2 <- glmer(avoidance ~  1 + cs_pff + dummypd_csm + cs_pff*dummypd_csm+cstrial + (1|Subject_id), data = Avoid_seperate,family = binomial (link = "logit"), control = glmerControl(optimizer = "bobyqa"),  nAGQ = 1 )


summary(pdpff_mod1) ## significant 


# odd ratio 
# standard error 
se <- sqrt(diag(vcov(pdpff_mod2)))
# tabl of estimates with 95% CI
(tab <- cbind(Est = fixef(pdpff_mod2), LL = fixef(pdpff_mod2) - 1.96 * se, UL = fixef(pdpff_mod1) + 1.96 *
                se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)

summary(pdpff_mod2)
se <- sqrt(diag(vcov(pdpff_mod2)))
# tabl of estimates with 95% CI
(tab <- cbind(Est = fixef(pdpff_mod2), LL = fixef(pdpff_mod2) - 1.96 * se, UL = fixef(pdpff_mod1) + 1.96 *
                se))
## exponentiating the coefficients ：
#If we wanted odds ratios instead of coefficients on the logit scale, 
# we could exponentiate the estimates and CIs.
exp(tab)


#--------- figure ------------------
plot(ggpredict(pdpff_mod1, terms  = c('cs_pff','dummypd_csp'))) +labs(x = 'probability of first fixation (%)', y = 'Probability of avoidance (%)') +
  scale_x_continuous(limits = c(0, 1))+
  scale_x_continuous(labels = percent_format()) +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = "dummypd_cs+")
 plot(ggpredict(pdpff_mod1, terms  = c('cs_pff','dummypd_csp','cstrial')), 
     facet = TRUE,  colors = c('#377db7','#ed434f')) +
  labs(x = 'probability of first fixation (%)', y = 'Probability of avoidance (%)') +
  scale_x_continuous(limits = c(0, 1))+
  scale_x_continuous(labels = percent_format()) + 
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = "dummypd_cs+")


p3<- plot(ggpredict(pdpff_mod2, terms  = c('cs_pff','dummypd_csm')), 
          colors = c('#377db7','#ed434f')) +labs(x = 'probability of first fixation (%)', y = 'Probability of avoidance (%)') +scale_x_continuous(limits = c(0, 1))+
  scale_x_continuous(labels = percent_format()) +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = "dummypd_cs-")
  
p4<- plot(ggpredict(pdpff_mod2, terms  = c('cs_pff','dummypd_csm','cstrial')), 
          facet = TRUE,  colors = c('#377db7','#ed434f')) +labs(x = 'probability of first fixation (%)', y = 'Probability of avoidance (%)') +scale_x_continuous(limits = c(0, 1))+
  scale_x_continuous(labels = percent_format()) +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = "dummypd_cs-")


ggarrange(p1,p2,p3,p4,ncol = 2,nrow =2)



#########################################
# moderate model-- fear of pain and avoid 
#########################################



Avoid_relation <- read.csv('/Users/mac/Documents/Handylibrarian/HL_results_baseline/HL_relation_fearaall.csv', 
                           header = TRUE, sep = ",", fill = TRUE)

Avoid_relation$cstrial<- factor(Avoid_relation$cstrial,levels=c('cs+_trial','cs-_trial'))

#----- convert fear of pain------

# fear of pain across cstrial

Avoid_relation$csp_fear_new <- c(scale(Avoid_relation$fear_pain, center=TRUE, scale= TRUE)) #Centering IV;

mean_fearcsp<- mean(Avoid_relation$csp_fear_new)
sd_fearcsp<- sd(Avoid_relation$csp_fear_new)

cutoff_high <-mean_fearcsp + sd_fearcsp
cutoff_low <- mean_fearcsp - sd_fearcsp

# Create dummy-coded variable
Avoid_relation$dummycsp_fear <- cut(Avoid_relation$csp_fear,
                                    breaks = c(-Inf, cutoff_low, cutoff_high, Inf),
                                    labels = c('m-sd', "hi", "m+sd"),
                                    include.lowest = TRUE)




csp_avoid_relation<- subset(Avoid_relation,Avoid_relation$cstrial == 'cs+_trial')

csp_avoid_relation$csp_fear_new <- c(scale(csp_avoid_relation$fear_pain, center=TRUE, scale= TRUE)) #Centering IV;



mean_fear_csp<- mean(csp_avoid_relation$csp_fear_new)
sd_fear_csp<- sd(csp_avoid_relation$csp_fear_new)


cutoff_high_csp <-mean_fear_csp + sd_fear_csp
cutoff_low_csp <- mean_fear_csp - sd_fear_csp



csp_avoid_relation$dummyfear_csp <- ifelse(csp_avoid_relation$csp_fear< cutoff_high_csp, " low_rate", 'high_rate')

fear_tff_csp <- lm(Pro_avoid ~  1 + csp_tff+ dummyfear_csp + csp_tff*dummyfear_csp, data = csp_avoid_relation)

summary(fear_tff_csp)

anova(fear_tff_csp)  

performance::r2(fear_tff_csp)


# 
fear_tff <- lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp+ cstrial + csp_tff*dummyfear_csp + (1|Subject_id), data = Avoid_relation)

fear_tff_trial <- lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp+ cstrial + csp_tff*dummyfear_csp*cstrial + (1|Subject_id), data = Avoid_relation)

summary(fear_tff)

anova(fear_tff)  

emmeans(fear_tff,pairwise~ csp_tff*dummyfear_csp, adjust="Holm",pbkrtest.limit = 4257)

performance::r2(fear_tff)

# moderate by cstrial and fear
fear_tff_trial <- lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp+ cstrial + csp_tff*dummyfear_csp*cstrial + (1|Subject_id), data = Avoid_relation)

summary(fear_tff_trial)

anova(fear_tff_trial)  

emmeans(fear_tff_trial,pairwise~ csp_tff*dummyfear_csp*cstrial, adjust="Holm",pbkrtest.limit = 4257)

performance::r2(fear_tff_trial)

# cs+ trial 
csp_avoid_relation$dummyfear_csp <- ifelse(csp_avoid_relation$csp_fear< cutoff_high_csp, " low_rate", 'high_rate')

fear_pff_csp <- lm(Pro_avoid ~  1 + pff+ dummyfear_csp + pff*dummyfear_csp + (1|Subject_id), data = csp_avoid_relation)

summary(fear_pff_csp)

anova(fear_pff_csp)  

performance::r2(fear_pff_csp)



# plot---------------------
tt1 <- "p<0.001, R^2_marginal = 0.641 "

p1<- plot(ggeffect(fear_tff, terms  = c('csp_tff','dummyfear_csp'),ci.lvl = 0.95),add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(time to first fixation(seconds)', y = 'Probability of avoidance (%)', 
  title ='lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp +
  cstrial + csp_tff*dummyfear_csp + (1|Subject_id)') +
   geom_text(aes(x=0,y= -1,label= tt1),
            color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  scale_y_continuous(limits = c(0, 1))+
  scale_y_continuous(labels = percent_format()) +labs(colour = "moderator fear of pain")
  

p2<- plot(ggeffect(fear_tff, terms  = c('csp_tff','dummyfear_csp','cstrial'),ci.lvl = 0.95),add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(time to first fixation(seconds)', y = 'Probability of avoidance (%)', 
       title = 'lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp 
       + cstrial + csp_tff*dummyfear_csp + (1|Subject_id)') +
  scale_y_continuous(limits = c(0, 1))+
  scale_y_continuous(labels = percent_format()) +labs(colour = "moderator fear of pain")

#plot------------------------

  p3<- plot(ggeffect(fear_tff, terms  = c('csp_tff','dummyfear_csp'),ci.lvl = 0.95),add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(time to first fixation(seconds)', y = 'Probability of avoidance (%)',
       title = 'lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp 
       + cstrial + csp_tff*dummyfear_csp + (1|Subject_id))')+
  geom_text(aes(x=0,y= -1,label= tt),
           color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  
  scale_y_continuous(limits = c(0, 1))+
  scale_y_continuous(limits = c(0,1),labels = percent_format()) +labs(colour = "moderator fear of pain")

# plot -----------
tt2<-  "p= 0.8754, R^2_marginal = 0.700 "

p4<- plot(ggeffect(fear_tff_trial, terms  = c('csp_tff','dummyfear_csp'),ci.lvl = 0.95),add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(time to first fixation(seconds)', y = 'Probability of avoidance (%)',
  title = 'lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp+ cstrial 
  + csp_tff*dummyfear_csp*cstrial + (1|Subject_id)') +
  geom_text(aes(x=0.5,y= -1,label= tt2),
            color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  scale_y_continuous(labels = percent_format()) +labs(colour = "moderator fear of pain")

p5<- plot(ggeffect(fear_tff_trial, terms  = c('csp_tff','dummyfear_csp','cstrial'),ci.lvl = 0.95),add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
    labs(x = 'log(time to first fixation(seconds)', y = 'Probability of avoidance (%)',
         title = 'lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp+ cstrial 
         + csp_tff*dummyfear_csp*cstrial + (1|Subject_id)') + 
  scale_y_continuous(limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format()) +labs(colour = "moderator fear of pain")

# plot-------

tt3<- "p = 0.7032, R^2_adjust = 0.482"

p6<- plot(ggeffect(fear_tff_csp, terms  = c('csp_tff','dummyfear_csp'),ci.lvl = 0.95)
          ,add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(time to first fixation_cs+_trial(seconds)', y = 'Probability of avoidance_cs+_trial (%)',
       title = 'lm(Pro_avoid ~  1 + csp_tff+ dummyfear_csp+ cstrial 
       + csp_tff*dummyfear_csp,data = csp_avoid_relation') + 
  geom_text(aes(x=-0.5,y= -1,label= tt3),
            color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  scale_y_continuous(limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format()) +labs(colour = "moderator fear of pain_cs+")



ggarrange(p1,p2,p3,p4, p5,p6,ncol = 3,nrow =2)


# pff_fearofpain 


fear_pff <- lmer(Pro_avoid ~  1 + Pff+ dummyfear_csp+ cstrial + Pff*dummyfear_csp + (1|Subject_id), data = Avoid_relation)

summary(fear_pff)

anova(fear_pff)  

emmeans(fear_pff,pairwise~ Pff*dummyfear_csp, adjust="Holm",pbkrtest.limit = 4257)

performance::r2(fear_pff)

# moderate by cstrial and fear # is singular
fear_pff_trial <- lmer(Pro_avoid ~  1 + Pff+ dummyfear_csp + cstrial + Pff*dummyfear_csp*cstrial + (1|Subject_id), data = Avoid_relation)

summary(fear_pff_trial)

anova(fear_pff_trial)  

emmeans(fear_pff_trial,pairwise~ Pff*dummyfear_csp*cstrial, adjust="Holm",pbkrtest.limit = 4257)

performance::r2(fear_pff_trial)


# only cstrial 


fear_pff_csp <- lm(Pro_avoid ~  1 + Pff+ dummyfear_csp + Pff*dummyfear_csp + (1|Subject_id), data = csp_avoid_relation)

summary(fear_pff_csp)

anova(fear_pff_csp)  

performance::r2(fear_pff_csp)



# plot---------------------
tt1 <- "p = 0.0368, R^2_marginal = 0.517 "

p1<- plot(ggeffect(fear_pff, terms  = c('Pff','dummyfear_csp'),ci.lvl = 0.95),
          add.data = TRUE, colors = c('#377db7','#ed434f')) + 
  labs(x = 'probability of first fixation', y = 'Probability of avoidance (%)', 
       title ='lmer(Pro_avoid ~  1 + pff + dummyfear_cs +
  cstrial + pff*dummyfear_cs + (1|Subject_id)') +
  geom_text(aes(x=0.25,y= -1,label= tt1),
            color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(limits = c(0,1),labels = percent_format())
  labs(colour = "fear of pain")


p2<- plot(ggeffect(fear_pff, terms  = c('Pff','dummyfear_csp','cstrial'),ci.lvl = 0.95),add.data = TRUE, colors = c('#377db7','#ed434f')) + 
  labs(x = 'probability of first fixation', y = 'Probability of avoidance (%)', 
       title = 'lmer(Pro_avoid ~  1 + pff+ dummyfear_csp 
       + cstrial + pff*dummyfear_csp + (1|Subject_id)') +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(limits = c(0,1),labels = percent_format())+
  labs(colour = "fear of pain")

#plot------------------------

p3<- plot(ggeffect(fear_pff, terms  = c('Pff','dummyfear_csp'),ci.lvl = 0.95),
          add.data = TRUE, colors = c('#377db7','#ed434f')) + 
  labs(x = 'probability of first fixation', y = 'Probability of avoidance (%)',
       title = 'lmer(Pro_avoid ~  1 + pff+ dummyfear_csp 
 trial + pff*dummyfear_csp + (1|Subject_id))')+
  scale_y_continuous(limits = c(0,1),labels = percent_format()) +
  scale_x_continuous(limits = c(0,1),labels = percent_format())+
  labs(colour = "fear of pain")

# plot-------

tt3<- "p = 0.3114, R^2_adjust = 0.062"

p4<- plot(ggeffect(fear_pff_csp, terms  = c('Pff','dummyfear_csp'),ci.lvl = 0.95),
          add.data = TRUE, colors = c('#377db7','#ed434f')) + 
  labs(x = 'probability of first fixation', y = 'Probability of avoidance_cs+_trial (%)',
       title = 'lm(Pro_avoid ~  1 + pff+ dummyfear_csp+ cstrial 
       + pff*dummyfear_csp,data = csp_avoid_relation') + 
  geom_text(aes(x=0.25,y= -1,label= tt3),
            color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(limits = c(0,1),labels = percent_format())+
    labs(colour = "fear of pain_cs+")



ggarrange(p1,p2,p3,p4,ncol = 2,nrow =2)


# fear_tfd



# 
fear_tfd <- lmer(Pro_avoid ~  1 + csp_tfd+ dummyfear_csp+ csp_tfd*dummyfear_csp + (1|Subject_id), data = Avoid_relation)

summary(fear_tfd)

anova(fear_tfd)  

emmeans(fear_tff,pairwise~ csp_tff*dummyfear_csp, adjust="Holm",pbkrtest.limit = 4257)

performance::r2(fear_tfd)

# moderate by cstrial and fear
fear_tfd_trial <- lmer(Pro_avoid ~  1 + csp_tfd+ dummyfear_csp+ cstrial + csp_tfd*dummyfear_csp*cstrial + (1|Subject_id), data = Avoid_relation)

summary(fear_tfd_trial)

anova(fear_tfd_trial)  

emmeans(fear_tfd_trial,pairwise~ csp_tff*dummyfear_csp*cstrial, adjust="Holm",pbkrtest.limit = 4257)

performance::r2(fear_tfd_trial)

# cs+ trial 


fear_tfd_csp <- lm(Pro_avoid ~  1 + csp_tfd+ dummyfear_csp + csp_tfd*dummyfear_csp, data = csp_avoid_relation)

summary(fear_tfd_csp)

anova(fear_tfd_csp)  

performance::r2(fear_tfd_csp)



# plot---------------------
tt1 <- "p<0.001, R^2_marginal = 0.641 "
plot(ggeffect(fear_tfd, terms  = c('csp_tfd','dummyfear_csp'),ci.lvl = 0.95),add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(Total fixation duration(seconds)', y = 'Probability of avoidance (%)', 
       title ='lmer(Pro_avoid ~  1 + csp_tfd+ dummyfear_csp +
  cstrial + csp_tfd*dummyfear_csp + (1|Subject_id)') +
  geom_text(aes(x=0,y= -1,label= tt1),
            color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  scale_y_continuous(limits = c(0, 1))+
  scale_y_continuous(labels = percent_format()) +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = "moderator fear of pain")

plot(ggeffect(fear_tfd, terms  = c('csp_tfd','dummyfear_csp','cstrial'),ci.lvl = 0.95),
     add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(time to first fixation(seconds)', y = 'Probability of avoidance (%)', 
       title = 'lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp 
       + cstrial + csp_tff*dummyfear_csp + (1|Subject_id)') +
  scale_y_continuous(limits = c(0, 1))+
  scale_y_continuous(labels = percent_format()) +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = "moderator fear of pain")

#plot------------------------

p3<- plot(ggeffect(fear_tfd, terms  = c('csp_tfd','dummyfear_csp'),ci.lvl = 0.95),
          add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(Total fixation duration(seconds)', y = 'Probability of avoidance (%)',
       title = 'lmer(Pro_avoid ~  1 + csp_tfd+ dummyfear_csp 
       + cstrial + csp_tdf*dummyfear_csp + (1|Subject_id))')+
  geom_text(aes(x=0,y= -1,label= tt),
            color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  scale_y_continuous(limits = c(0, 1))+
  scale_y_continuous(limits = c(0,1),labels = percent_format()) +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = " fear of pain")

# plot -----------
tt2<-  "p= 0.475, R^2_marginal = 0.87 "

p4<- plot(ggeffect(fear_tfd_trial, terms  = c('csp_tfd','dummyfear_csp'),ci.lvl = 0.95),add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(Total fixation duration(seconds)', y = 'Probability of avoidance (%)',
       title = 'lmer(Pro_avoid ~  1 + csp_tfd+ dummyfear_csp+ cstrial 
  + csp_tfd*dummyfear_csp*cstrial + (1|Subject_id)') +
  geom_text(aes(x=0.5,y= -1,label= tt2),
            color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  scale_y_continuous(labels = percent_format()) +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = " fear of pain")

p5<- plot(ggeffect(fear_tfd_trial, terms  = c('csp_tff','dummyfear_csp','cstrial'),ci.lvl = 0.95),
          add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(time to first fixation(seconds)', y = 'Probability of avoidance (%)',
       title = 'lmer(Pro_avoid ~  1 + csp_tff+ dummyfear_csp+ cstrial 
         + csp_tff*dummyfear_csp*cstrial + (1|Subject_id)') + 
  scale_y_continuous(limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format()) +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = " fear of pain")

# plot-------

tt3<- "p = 0.475, R^2adjust = 0.87"

 plot(ggeffect(fear_tfd_csp, terms  = c('csp_tfd','dummyfear_csp'),ci.lvl = 0.95),
      add.data = TRUE, colors = c('#56717f','#d9a9ec')) + 
  labs(x = 'log(Total fixation duration(seconds)', y = 'Probability of avoidance_cs+_trial (%)',
       title = 'lm(Pro_avoid ~  1 + csp_tfd+ dummyfear_csp+ cstrial 
       + csp_tfd*dummyfear_csp,data = csp_avoid_relation') + 
  geom_text(aes(x=-0.5,y= -1,label= tt3),
            color="darkgrey",family = "serif",fontface = "plain",size = 3)+
  scale_y_continuous(limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format()) +
  scale_colour_brewer(palette = "Accent") +
  scale_fill_brewer(palette = 'Accent')+
  labs(colour = "fear of pain_cs+")



ggarrange(p1,p2,p3,p4, p5,p6,ncol = 3,nrow =2)
