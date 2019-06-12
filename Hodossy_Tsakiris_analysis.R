# load packages
library(papaja)
library(readxl)
library(lme4)
library(ggeffects)
library(sjPlot)
library(sjmisc)
library(effects)
library(phia) 
library(ggplot2)
library(lattice)
library(MASS)
library(fitdistrplus)
library(logspline)
library(MuMIn)
library(dplyr)
library(buildmer)
library(zoo)
library(cowplot)
library(dplyr)
library(readr)
library(gridExtra)
library(imager)
library(utils)

# Seed for random number generation
set.seed(42)
# disable scientific notation (this causes long numbers to appear in full rather than being truncated)
options(scipen = 999)

###requires data from git hub
#url:https://github.com/HoLilla/Hodossy-Tsakiris-2019

###Experiment 1 

#explore baselines under different conditions
ALL_TRIALS_BASELINE <- read_excel("EXP1_BL_CV_main.xlsx")
names(ALL_TRIALS_BASELINE)[1]<-'PPT'

#scale covariates, create copy of baseline as a DV
ALL_TRIALS_BASELINE$CV_RR<-scale(ALL_TRIALS_BASELINE$RR,center=TRUE,scale=TRUE)
ALL_TRIALS_BASELINE$DV_BL_HF<-ALL_TRIALS_BASELINE$BL_HF
ALL_TRIALS_BASELINE$CV_BL_HF<-scale(ALL_TRIALS_BASELINE$BL_HF,center=TRUE,scale=TRUE)
ALL_TRIALS_BASELINE$CV_BL_HR<-scale(ALL_TRIALS_BASELINE$BL_HR,center=TRUE,scale=TRUE)
ALL_TRIALS_BASELINE$CV_BL_RR<-scale(ALL_TRIALS_BASELINE$BL_RR,center=TRUE,scale=TRUE)

#declare factors
ALL_TRIALS_BASELINE$Congruency<-factor(ALL_TRIALS_BASELINE$Congruency)
ALL_TRIALS_BASELINE$BF<-factor(ALL_TRIALS_BASELINE$BF)
ALL_TRIALS_BASELINE$Regulation_order<-factor(ALL_TRIALS_BASELINE$Regulation_order)

#test if baselines under different conditions are different from each other significantly
B1<-lmer(DV_BL_HF~ 1 + (1|PPT), data=ALL_TRIALS_BASELINE, REML=FALSE)
B2<-lmer(DV_BL_HF~ BF*Congruency + (1|PPT), data=ALL_TRIALS_BASELINE, REML=FALSE)
anova(B1,B2)
summary(B2)

#In the bit below average baselines are used as they are not different

#show order effects
ALL_TRIALS_av_BL<-read_excel("EXP1_order_effect.xlsx")

#declare factors
ALL_TRIALS_av_BL$Congruency<-factor(ALL_TRIALS_av_BL$Congruency)
ALL_TRIALS_av_BL$BF<-factor(ALL_TRIALS_av_BL$BF)
ALL_TRIALS_av_BL$Regulation_order<-factor(ALL_TRIALS_av_BL$Regulation_order)
ALL_TRIALS_av_BL$Time<-factor(ALL_TRIALS_av_BL$Time)

#rename levels-for plots later
levels(ALL_TRIALS_av_BL$Regulation_order)[levels(ALL_TRIALS_av_BL$Regulation_order)=="1"] <- "Regulation in first half"
levels(ALL_TRIALS_av_BL$Regulation_order)[levels(ALL_TRIALS_av_BL$Regulation_order)=="2"] <- "Regulation in second half"

#centering covariates, but they are already centered in the baseline spreadsheet

#reveals BF task order effect
order_effect0<-lmer(CH_DV_HF~ 1 + (1|PPT), data=ALL_TRIALS_av_BL, REML=FALSE)
order_effect1<-lmer(CH_DV_HF~ Regulation_order + (1|PPT), data=ALL_TRIALS_av_BL, REML= FALSE)

anova(order_effect0,order_effect1)

order_effect1<-lmer(CH_DV_HF~ Regulation_order + (1|PPT), data=ALL_TRIALS_av_BL, REML= TRUE)

#get effects 
order_all_effects<-allEffects(order_effect1)
order_all_effects1<-order_all_effects[[1]]
order_all_effects.df<-as.data.frame(order_all_effects1)

order_statsDF<-get_model_data(order_effect1)
order_R2<-r.squaredGLMM(order_effect1)

desc_order<-psych::describeBy(ALL_TRIALS_av_BL$CH_DV_HF, ALL_TRIALS_av_BL$Regulation_order)

#Main analysis for Experiment 1
#using first half only due to order effects
HALF_TRIALS_ST1<-read_excel("HALF_TRIALS_EXP1.xlsx")
#declarefactors
HALF_TRIALS_ST1$Congruency<-factor(HALF_TRIALS_ST1$Congruency)
HALF_TRIALS_ST1$Time<-factor(HALF_TRIALS_ST1$Time)
HALF_TRIALS_ST1$BF<-factor(HALF_TRIALS_ST1$BF)
HALF_TRIALS_ST1$recognition_accuracy<-factor(HALF_TRIALS_ST1$recognition_accuracy)
#rename 1st coloumn PPT
names(HALF_TRIALS_ST1)[1]<-'PPT'
#name levels here for graph
levels(HALF_TRIALS_ST1$Congruency)[levels(HALF_TRIALS_ST1$Congruency)=="1"] <- "Congruent"
levels(HALF_TRIALS_ST1$Congruency)[levels(HALF_TRIALS_ST1$Congruency)=="2"] <- "Incongruent"
levels(HALF_TRIALS_ST1$BF)[levels(HALF_TRIALS_ST1$BF)=="1"] <- "Attention"
levels(HALF_TRIALS_ST1$BF)[levels(HALF_TRIALS_ST1$BF)=="2"] <- "Regulate"
levels(HALF_TRIALS_ST1$Time)[levels(HALF_TRIALS_ST1$Time)=="1"] <- "Baseline"
levels(HALF_TRIALS_ST1$Time)[levels(HALF_TRIALS_ST1$Time)=="2"] <- "Task"

#explore transformations
HALF_TRIALS_ST1$SQRT_DV_HF<-sqrt(HALF_TRIALS_ST1$DV_HF)
HALF_TRIALS_ST1$LN_DV_HF<-log(HALF_TRIALS_ST1$DV_HF)

normal_HF<- fitdist(HALF_TRIALS_ST1$DV_HF, "norm")
normal_SQRT_HF<- fitdist(HALF_TRIALS_ST1$SQRT_DV_HF, "norm")
normal_LN_HF<-fitdist(HALF_TRIALS_ST1$LN_DV_HF, "norm")

plot(normal_HF)
plot(normal_SQRT_HF)
plot(normal_LN_HF)

shTest<-shapiro.test(HALF_TRIALS_ST1$DV_HF)
shapiro.test(HALF_TRIALS_ST1$SQRT_DV_HF)
shapiro.test(HALF_TRIALS_ST1$LN_DV_HF)
#sqrt transform would make it good 
#in supplementary material:use link=sqrt and keep raw data

#create dataframe
HALF_TRIALS_ST1B<-HALF_TRIALS_ST1 %>%
  mutate(CV_BL_HF1 = rep(HALF_TRIALS_ST1$CV_BL_HF[HALF_TRIALS_ST1$Time=="Baseline"],each=2))%>%
  mutate(CH_HF = rep(HALF_TRIALS_ST1$DV_HF[HALF_TRIALS_ST1$Time=="Task"]-
                       HALF_TRIALS_ST1$DV_HF[HALF_TRIALS_ST1$Time=="Baseline"],each=2))%>%
  filter(Time == "Task")

HALF_TRIALS_ST1B$Congruency<-factor(HALF_TRIALS_ST1B$Congruency)
HALF_TRIALS_ST1B$BF<-factor(HALF_TRIALS_ST1B$BF)

#optimal model selection based on AIC values
st1.opt.mod<-buildmer(SQRT_DV_HF~ CV_BL_HF1+CV_RR+Congruency*BF + (1|PPT), data=HALF_TRIALS_ST1B,crit='AIC')
summary(st1.opt.mod)

st1<-lm(SQRT_DV_HF ~ CV_BL_HF1 + Congruency + BF, data = HALF_TRIALS_ST1B)
tab_model(st1)
abs_scores<-st1
#create dataframe from the observed effects/estimates for ggplot later
st1_abs<-allEffects(abs_scores)
st1_abs1<-st1_abs$Congruency
st1_abs1.df<-as.data.frame(st1_abs1)
st1_abs1.df$Congruency<-factor(st1_abs1.df$Congruency)
st1_abs_desc<-psych::describeBy(HALF_TRIALS_ST1B$DV_HF, HALF_TRIALS_ST1B$Congruency)
#create dataframe from observed stats
st1_abs_statsdf<-get_model_data(abs_scores)
#get a measure of effect size with R squared from summary lm
st1_abs_sum<-summary(abs_scores)

#cardiac recognition accuracy
HALF_TRIALS_accuracy<-read_excel("HALF_ACCURACY1.xlsx")

HALF_TRIALS_accuracy$recognition_accuracy<-as.integer(HALF_TRIALS_accuracy$recognition_accuracy)
HALF_TRIALS_accuracy$Congruency<-factor(HALF_TRIALS_accuracy$Congruency)
HALF_TRIALS_accuracy$BF<-factor(HALF_TRIALS_accuracy$BF)

levels(HALF_TRIALS_accuracy$Congruency)[levels(HALF_TRIALS_accuracy$Congruency)=="1"] <- "Congruent"
levels(HALF_TRIALS_accuracy$Congruency)[levels(HALF_TRIALS_accuracy$Congruency)=="2"] <- "Incongruent"
levels(HALF_TRIALS_accuracy$BF)[levels(HALF_TRIALS_accuracy$BF)=="1"] <- "Attention"
levels(HALF_TRIALS_accuracy$BF)[levels(HALF_TRIALS_accuracy$BF)=="2"] <- "Regulate"

#logistic regression on accuracy scors
accuracy<-glm(recognition_accuracy ~BF*Congruency,family=binomial(link='logit'),data=HALF_TRIALS_accuracy)
#get effects 
accuracy_statsDF<-get_model_data(accuracy)
tab_model(accuracy)
accuracy_R2<-r.squaredGLMM(accuracy)

task_acc<-plyr::count(HALF_TRIALS_accuracy, c("recognition_accuracy"))
acc_prob<-task_acc[[2,2]]/sum(task_acc[[2]])*100

#plot for Experiment 1

source("R_rainclouds.R")
source("summarySE.R")
source("simulateData.R")

# width and height variables for saved plots
w = 6
h = 3

low_abs<-ggplot(HALF_TRIALS_ST1B, aes(x = Congruency, y = SQRT_DV_HF, fill=Congruency)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(aes(x = as.numeric(Congruency)-.15, y = SQRT_DV_HF, colour=Congruency),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = Congruency, y = SQRT_DV_HF, fill=Congruency),outlier.shape = NA, alpha = .5, width = .1)+
  geom_line(data = st1_abs1.df, aes(x = as.numeric(Congruency)+.1, y = fit, colour=Congruency), linetype = 3)+
  geom_point(data = st1_abs1.df, aes(x = as.numeric(Congruency)+.1, y = fit,colour=Congruency), shape = 18) +
  geom_errorbar(data = st1_abs1.df, aes(x = as.numeric(Congruency)+.1, y = fit, ymin = lower, ymax = upper), width = .05)+
  coord_flip()+scale_colour_brewer(palette = "Set1")+xlab("")+
  scale_fill_brewer(palette = "Set1")+  ylab("HF-HRV (nu)")+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(plot.title = element_text(size=12))+
  theme(legend.position="bottom")

###Experiment 2
ALL_TRIALS_ST2 <- read_excel("EXP2.xlsx")
missing_rr<-length(which(ALL_TRIALS_ST2$CV_RR=="NA"))/length(ALL_TRIALS_ST2)

#to have BL measures to check BL differences
ALL_TRIALS_ST2$DV_BL_HF<-ALL_TRIALS_ST2$CV_BL_HF
#change score
ALL_TRIALS_ST2$CH_HF<-ALL_TRIALS_ST2$DV_HF-ALL_TRIALS_ST2$DV_BL_HF

#make NA recognisable for R
ALL_TRIALS_ST2$CV_RR<-as.numeric(ALL_TRIALS_ST2$CV_RR)
ALL_TRIALS_ST2$CV_BL_HF<-as.numeric(ALL_TRIALS_ST2$CV_BL_HF)
ALL_TRIALS_ST2$Water<-as.numeric(ALL_TRIALS_ST2$Water)
ALL_TRIALS_ST2$Attention<-as.numeric(ALL_TRIALS_ST2$Attention)

#centering the CVs 
ALL_TRIALS_ST2$CV_RR<-scale(ALL_TRIALS_ST2$CV_RR,center=TRUE,scale=TRUE)
ALL_TRIALS_ST2$CV_BL_HF<-scale(ALL_TRIALS_ST2$CV_BL_HF,center=TRUE,scale=TRUE)
ALL_TRIALS_ST2$CV_Water<-scale(ALL_TRIALS_ST2$Water,center=TRUE,scale=TRUE)
ALL_TRIALS_ST2$CV_Attention<-scale(ALL_TRIALS_ST2$Attention,center=TRUE,scale=TRUE)

#create factors
ALL_TRIALS_ST2$Repetition<-factor(ALL_TRIALS_ST2$Repetition)
ALL_TRIALS_ST2$Time<-factor(ALL_TRIALS_ST2$Time)
ALL_TRIALS_ST2$Congruency<-factor(ALL_TRIALS_ST2$Congruency)
ALL_TRIALS_ST2$Belief<-factor(ALL_TRIALS_ST2$Belief)
ALL_TRIALS_ST2$Order<-factor(ALL_TRIALS_ST2$Order)

baseline<-subset(ALL_TRIALS_ST2,Time==1)
task<-subset(ALL_TRIALS_ST2,Time==2)

#interpolate the missing values
task$CV_RR<-na.locf(task$CV_RR)

#test if baselines under different conditions are different from each other significantly
B1<-lmer(DV_HF~ Congruency*Belief +(1|PPT), data=baseline)
summary(B1)
#they are not

#explore distributions 

fn_ch <- fitdist(task$CH_HF, "norm")
plot(fn_ch)

#you can also play around with the normal distribution fit after different types of transformations and see if that makes it better
summary(fn_ch) # look for AIC, BIC scores for model fit - anova doesn't work unfortunatelly
shTest2<-shapiro.test(task$CH_HF)

#interpret it as higher level congruency 
#prepare space, create empty coloumn for this new variable 
task[ncol(task)+1]<-"NaN"
names(task)[ncol(task)]<-"BeliefFeedback_Congruency"
#create codes for higher level congruency

for (i in 1:nrow(task)) {
  if(task[i,5] == task[i,6])
  { task[[i,18]] <- 1
  } else {
    task[[i,18]] <- 0
  }
}

task$BeliefFeedback_Congruency<-factor(task$BeliefFeedback_Congruency)

#rename levels for ggplot later
levels(task$Congruency)[levels(task$Congruency)=="1"] <- "Congruent"
levels(task$Congruency)[levels(task$Congruency)=="2"] <- "Incongruent"
levels(task$Belief)[levels(task$Belief)=="1"] <- "Self"
levels(task$Belief)[levels(task$Belief)=="2"] <- "Other"

levels(task$BeliefFeedback_Congruency)[levels(task$BeliefFeedback_Congruency)=="1"] <- "Congruent"
levels(task$BeliefFeedback_Congruency)[levels(task$BeliefFeedback_Congruency)=="0"] <- "Incongruent"


#check for order and repetition effects
order_effectST2<-lmer(CH_HF~ Order + (1|PPT), data=task, REML=TRUE)
repetition_effectST2<-lmer(CH_HF~ Repetition + (1|PPT), data=task, REML=TRUE)
order_stats_ST2<-tab_model(order_effectST2)
rep_stats_ST2<-tab_model(repetition_effectST2)
#no effect of repetion or order

#after review
#model selection based on AIC
st2.opt.mod<-buildmer(CH_HF~ Congruency*Belief+CV_Attention+CV_RR+CV_BL_HF+CV_Water+
                        (1|PPT), data=task,crit='AIC',quiet= TRUE)
summary(st2.opt.mod)

CH2<-lmer(CH_HF ~ CV_BL_HF+Congruency*Belief+ CV_RR+(1|PPT), data=task)
tab_model(CH2)
#explore simple effects
simple_effect_congruency2<-testInteractions(CH2, fixed="Belief", across="Congruency",adjustment="bonferroni" )
simple_effect_belief<-testInteractions(CH2, fixed="Congruency", across="Belief",adjustment="bonferroni" )


#save interaction belief congruency effect in dataframe
ch_eff<-allEffects(CH2)
ch_eff
ch_eff1<-ch_eff$"Congruency:Belief"
ch.df<-as.data.frame(ch_eff1)
#save output of analysis into variable
html.CH2<-tab_model(CH2)
#save stats in dataframe
CH3_statsDF<-get_model_data(CH2)
#effect size
CH3_R2<-r.squaredGLMM(CH2)

#alternative analysis on the higher level congruency codes
CH2A<-lmer(CH_HF~ CV_BL_HF+BeliefFeedback_Congruency+CV_RR+ (1|PPT), data=task)
#save outcome into variable
html.CH3A<-tab_model(CH2A)
#save into data frame
CH3A_statsDF<-get_model_data(CH2A)
#effect size
CH3A_R2<-r.squaredGLMM(CH2A)

#save effects of higher level congruency coded as a main effect into dataframe
chA_eff<-allEffects(CH2A)
chA_eff1<-chA_eff$BeliefFeedback_Congruency
chA.df<-as.data.frame(chA_eff1)
#describe dataset -means, standard deviation
desc_ST2<-psych::describeBy(task$CH_HF, task$Belief:task$Congruency)
desc_ST2_alt<-psych::describeBy(task$CH_HF, task$BeliefFeedback_Congruency)
#sample descriptives
malesST=length(which(ALL_TRIALS_ST2$GENDER == "MALE"))/16
age=mean(ALL_TRIALS_ST2$Age)
ageSD=sd(ALL_TRIALS_ST2$Age)

##model on absolute scores
st2.abs<-lmer(DV_HF ~ CV_BL_HF + Congruency*Belief + CV_RR + (1 | PPT), data =task)
get_model_data(st2.abs)
tab_model(st2.abs)

#Plot for Experiment 2
#create combined plots
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
ch.df$Congruency <- factor(ch.df$Congruency, levels = unique(ch.df$Congruency), ordered=TRUE)
ch.df$Belief <- factor(ch.df$Belief, levels = unique(ch.df$Belief), ordered=TRUE)


#check if levels are the same for every df - to make sure it is well plotted
task$BeliefFeedback_Congruency <- factor(task$BeliefFeedback_Congruency,levels = unique(ch.df$Congruency), ordered=TRUE)
chA.df$BeliefFeedback_Congruency<-factor(chA.df$BeliefFeedback_Congruency,as.character(rev(chA.df$BeliefFeedback_Congruency)))
##to double-check levels:
#levels(task$Congruency)
#levels(ch.df$Congruency)
#levels(task$BeliefFeedback_Congruency)
#levels(chA.df$BeliefFeedback_Congruency)

belief_cong_int<-ggplot(task, aes(x = Belief, y = CH_HF, fill = Congruency)) +
  geom_flat_violin(aes(fill = Congruency),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Belief)-.15, y = CH_HF, colour = Congruency),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = Belief, y = CH_HF, fill = Congruency),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = ch.df, aes(x = as.numeric(Belief)+.1, y = fit, group = Congruency, colour = Congruency), linetype = 3)+
  geom_point(data = ch.df, aes(x = as.numeric(Belief)+.1, y = fit, group = Congruency, colour = Congruency), shape = 18) +
  geom_errorbar(data = ch.df, aes(x = as.numeric(Belief)+.1, y = fit, group = Congruency, colour = Congruency, ymin = lower, ymax = upper), width = .05)+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  ylab("") + ggtitle("A") +
  #theme(plot.title = element_text(hjust = 0))+
  #theme_apa(base_size = 12, base_family = "",box = TRUE)+
  theme(plot.title = element_text(size=12))+coord_flip()+
  theme(legend.position="bottom")

belief_cong_main<-ggplot(task, aes(x = BeliefFeedback_Congruency, y = CH_HF, fill=BeliefFeedback_Congruency)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(aes(x = as.numeric(BeliefFeedback_Congruency)-.15, y = CH_HF, colour=BeliefFeedback_Congruency),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = BeliefFeedback_Congruency, y = CH_HF, fill=BeliefFeedback_Congruency),outlier.shape = NA, alpha = .5, width = .1)+
  geom_line(data = chA.df, aes(x = as.numeric(BeliefFeedback_Congruency)+.1, y = fit, colour=BeliefFeedback_Congruency), linetype = 3)+
  geom_point(data = chA.df, aes(x = as.numeric(BeliefFeedback_Congruency)+.1, y = fit,colour=BeliefFeedback_Congruency), shape = 18) +
  geom_errorbar(data = chA.df, aes(x = as.numeric(BeliefFeedback_Congruency)+.1, y = fit, ymin = lower, ymax = upper), width = .05)+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+  xlab("")+
  ggtitle("B") +
  #theme(plot.title = element_text(hjust = 0))+
  # theme_apa(base_size = 12, base_family = "",box = TRUE)+
  xlab("")+ ylab("")+coord_flip()+scale_x_discrete(labels = c('','')) +theme( axis.ticks.y = element_blank(),legend.position="none")+theme(plot.title = element_text(size=12))

legend <- get_legend(belief_cong_int)
belief_cong_int <- belief_cong_int + theme(legend.position="none")

# Create a blank plot
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

grid.arrange(belief_cong_int, belief_cong_main, legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.3))



