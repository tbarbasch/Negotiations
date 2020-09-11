library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)

# Import spreadsheet for parental care
Pcare<-read.csv('Pcare.csv')

# Import spreadsheet for egg counts
laid_SL<-read.csv('laid_SL.csv')

# Import spreadsheet for monthly egg counts
monthly_FSL<-read.csv('monthly_FSL.csv')

# Import spreadsheet for hatch counts/proportion
prophatch_Mtend<-read.csv('prophatch_Mtend.csv')

# Import spreadsheet for monthly hatch counts
monthhatch_Mtend2<-read.csv('monthhatch_Mtend2.csv')


## model for number of eggs laid per clutch
mod.m1<-glmer(Laid~Treatment*as.factor(Month)+FSL+(1|ID)+(1|OLRE),data=na.omit(laid_SL),family="poisson")
summary(mod.m1)
laid_SL<-na.omit(laid_SL)
laid_SL$predictlaid<-predict(mod.m1,type='response')

# model for number of eggs laid per month
mod3a<-glmer(Laid~Treatment*as.factor(Month)+FSL+(1|OLRE)+(1|ID),data=na.omit(monthly_FSL),family=poisson)
summary(mod3a)
monthly_FSL<-na.omit(monthly_FSL)
monthly_FSL$predictlaid<-predict(mod3a,type='response')

# Models for female and male time tending with and without eggs laid as covariate
# fit model with eggs laid
modMeggs<-lmer(M.tend~Month*Treatment+Age+(1|Anemone.ID),data=na.omit(Pcare))
modMeggsLaid<-lmer(M.tend~Month*Treatment+Age+Laid+(1|Anemone.ID),data=na.omit(Pcare))
modFeggs<-lmer(F.tend~Month*Treatment+Age+(1|Anemone.ID),data=na.omit(Pcare))
modFeggsLaid<-lmer(F.tend~Month*Treatment+Age+Laid+(1|Anemone.ID),data=na.omit(Pcare))
summary(modMeggs)
summary(modMeggsLaid)
summary(modFeggs)
summary(modFeggsLaid)
Pcare<-na.omit(Pcare)
# predicted values from the models
Pcare$predictMtend<-predict(modMeggsLaid)
Pcare$predictFtend<-predict(modFeggsLaid)

## proportion of eggs hatched with and without male time tending as covariate
mod_prophatchMtend<-glmer(Prophatch~Treatment*as.factor(Month)+M.tend+(1|ID),data=na.omit(prophatch_Mtend),family="binomial")
summary(mod_prophatchMtend)

mod_prophatch<-glmer(Prophatch~Treatment*as.factor(Month)+(1|ID),data=na.omit(prophatch_Mtend),family="binomial")
summary(mod_prophatch)

# predicted values from the model
prophatch_Mtend<-na.omit(prophatch_Mtend)
prophatch_Mtend$predictprophatch<-predict(mod_prophatchMtend,type='response')


## Number of embryos hatched per clutch with and without male tending and eggs laid
mod_prophatch2<-glmer(Hatched~Treatment*as.factor(Month)+Laid+M.tend+(1|ID)+(1|OLRE),data=na.omit(prophatch_Mtend),family=poisson)
summary(mod_prophatch2)

mod_prophatch3<-glmer(Hatched~Treatment*as.factor(Month)+(1|ID)+(1|OLRE),data=na.omit(prophatch_Mtend),family=poisson)
summary(mod_prophatch3)
# predicted values from the model
prophatch_Mtend$predicthatch<-predict(mod_prophatch2,type='response')

## Number of embryos hatched per month with and without male tending and eggs laid
mod5<-glmer(Hatch~Treatment*as.factor(Month)+(1|ID)+(1|OLRE),data=na.omit(monthhatch_Mtend2),family=poisson)
summary(mod5)
mod5.1<-glmer(Hatch~Treatment*as.factor(Month)+Laid+M.tend+(1|ID)+(1|OLRE),data=na.omit(monthhatch_Mtend2),family=poisson)
summary(mod5.1)

# predicted values from the model
monthhatch_Mtend2<-na.omit(monthhatch_Mtend2)
monthhatch_Mtend2$predicthatch<-predict(mod5.1,type='response')

## Figure 1 - Female Effort
laid2<-laid_SL
laid2$Treatment <- factor(laid2$Treatment, levels = c("C", "F","M"),
                          labels = c("Control", "FH","MH"))
laid3<-monthly_FSL
laid3$Treatment <- factor(laid3$Treatment, levels = c("C", "F","M"),
                          labels = c("Control", "FH","MH"))
Pcare$Treatment <- factor(Pcare$Treatment, levels = c("C", "F","M"),
                          labels = c("Control", "FH","MH"))
prophatch_Mtend$Treatment <- factor(prophatch_Mtend$Treatment, levels = c("C", "F","M"),
                                    labels = c("Control", "FH","MH"))

hatch2<-monthhatch_Mtend
hatch2$Treatment <- factor(hatch2$Treatment, levels = c("C", "F","M"),
                           labels = c("Control", "FH","MH"))
hatch3<-monthhatch_Mtend2
hatch3$Treatment <- factor(hatch3$Treatment, levels = c("C", "F","M"),
                           labels = c("Control", "FH","MH"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

bp <- ggplot(hatch4, aes(x=Laid, y=predicthatch, lty=Month)) +
  #geom_boxplot()+
  geom_point()+
  geom_smooth(size=1,method="lm",se=F,col="black")+scale_linetype_discrete(labels=c("Pre","Post"))+theme_classic()
legend <- get_legend(bp)

(f1<-ggplot(laid2, aes(x = as.factor(Month), y = Laid, lty=Treatment)) +
    #geom_point(aes(y = Laid,shape= Treatment),size=2,alpha=0.2,position=position_dodge(width=0.5))+
    #geom_point(aes(y = Laid,shape= Treatment),size=2,position=position_jitter(h=0,w=0.2),alpha=0.2) +
    geom_boxplot(fill=NA,colour=alpha("black",0.4),width=0.8,size=0.2)+
    #geom_pointrange(aes(ymin=predictEggslower,ymax=predictEggsupper,shape=Treatment,position='dodge'),alpha=0.2)+
    geom_smooth(size = 1,method="lm",se=F, col="black",aes(group=Treatment,Month,predictlaid)) +
    #geom_line(aes(y=meaneggs,group=Treatment),size=.75,position=position_dodge(width=0.2))+
    facet_grid(.~Treatment)+
    labs(x = "Manipulation",
         y = "Number of Eggs Laid per Clutch",tag = "(a)")+ scale_color_discrete(name = "Manipulation")+ 
    scale_x_discrete(labels = c('pre','post'))+theme_classic())


(f2<-ggplot(laid3, aes(x = as.factor(Month), y = Laid, lty=Treatment)) +
    #geom_point(aes(y = Laid,shape= Treatment),size=2,alpha=0.2,position=position_dodge(width=0.5))+
    #geom_point(aes(y = Laid,shape= Treatment),size=2,position=position_jitter(h=0,w=0.2),alpha=0.2) +
    geom_boxplot(fill=NA,colour=alpha("black",0.4),width=0.8,size=0.2)+
    #geom_pointrange(aes(ymin=predictEggslower,ymax=predictEggsupper,shape=Treatment,position='dodge'),alpha=0.2)+
    geom_smooth(size = 1,method="lm",se=F, col="black",aes(group=Treatment,Month,predictlaid)) +
    #geom_line(aes(y=meaneggs,group=Treatment),size=.75,position=position_dodge(width=0.2))+
    facet_grid(.~Treatment)+
    labs(x = "Manipulation",
         y = "Number of Eggs Laid per Month",tag = "(b)")+ scale_color_discrete(name = "Manipulation")+ 
    scale_x_discrete(labels = c('pre','post'))+theme_classic())

(f3<-ggplot(Pcare, aes(x = as.factor(Month), y = F.tend, lty=Treatment)) +
    #geom_point(aes(y = Laid,shape= Treatment),size=2,alpha=0.2,position=position_dodge(width=0.5))+
    #geom_point(aes(y = Laid,shape= Treatment),size=2,position=position_jitter(h=0,w=0.2),alpha=0.2) +
    geom_boxplot(fill=NA,colour=alpha("black",0.4),width=0.8,size=0.2)+
    #geom_pointrange(aes(ymin=predictEggslower,ymax=predictEggsupper,shape=Treatment,position='dodge'),alpha=0.2)+
    geom_smooth(size = 1,method="lm",se=F, col="black",aes(group=Treatment,Month,predictFtend)) +
    #geom_line(aes(y=meaneggs,group=Treatment),size=.75,position=position_dodge(width=0.2))+
    facet_grid(.~Treatment)+
    labs(x = "Manipulation",
         y = "Female Time Spent Tending (mins)",tag = "(c)")+ scale_color_discrete(name = "Manipulation")+ 
    scale_x_discrete(labels = c('pre','post'))+theme_classic())

grid.arrange(f1,f2,f3,ncol=1)



(p1<-ggplot(Pcare, aes(x = as.factor(Month), y = M.tend, lty=Treatment)) +
    #geom_point(aes(y = Laid,shape= Treatment),size=2,alpha=0.2,position=position_dodge(width=0.5))+
    #geom_point(aes(y = Laid,shape= Treatment),size=2,position=position_jitter(h=0,w=0.2),alpha=0.2) +
    geom_boxplot(fill=NA,colour=alpha("black",0.4),width=0.8,size=0.2)+
    #geom_pointrange(aes(ymin=predictEggslower,ymax=predictEggsupper,shape=Treatment,position='dodge'),alpha=0.2)+
    geom_smooth(size = 1,method="lm",se=F, col="black",aes(group=Treatment,Month,predictMtend)) +
    #geom_line(aes(y=meaneggs,group=Treatment),size=.75,position=position_dodge(width=0.2))+
    facet_grid(.~Treatment)+
    labs(x = "Manipulation",
         y = "Male Time Spent Tending (mins)",tag = "(a)")+ scale_color_discrete(name = "Manipulation")+ 
    scale_x_discrete(labels = c('pre','post'))+theme_classic())

(p2<-ggplot(prophatch_Mtend, aes(x = as.factor(Month), y = Prophatch, lty=Treatment)) +
    #geom_point(aes(y = Laid,shape= Treatment),size=2,alpha=0.2,position=position_dodge(width=0.5))+
    #geom_point(aes(y = Laid,shape= Treatment),size=2,position=position_jitter(h=0,w=0.2),alpha=0.2) +
    geom_boxplot(fill=NA,colour=alpha("black",0.4),width=0.8,size=0.2)+
    #geom_pointrange(aes(ymin=predictEggslower,ymax=predictEggsupper,shape=Treatment,position='dodge'),alpha=0.2)+
    geom_smooth(size = 1,method="lm",se=F, col="black",aes(group=Treatment,Month,predictprophatch)) +
    #geom_line(aes(y=meaneggs,group=Treatment),size=.75,position=position_dodge(width=0.2))+
    facet_grid(.~Treatment)+
    labs(x = "Manipulation",
         y = "Proportion of Embryos Surviving per Clutch",tag = "(b)")+ scale_color_discrete(name = "Manipulation")+ 
    scale_x_discrete(labels = c('pre','post'))+theme_classic())

grid.arrange(p1,p2,nrow=2)

monthhatch_Mtend2$Treatment <- factor(monthhatch_Mtend2$Treatment, levels = c("C", "F","M"),
                                      labels = c("Control", "FH","MH"))
(g1<-ggplot(prophatch_Mtend, aes(x = as.factor(Month), y = Hatched, lty=Treatment)) +
    #geom_point(aes(y = Laid,shape= Treatment),size=2,alpha=0.2,position=position_dodge(width=0.5))+
    #geom_point(aes(y = Laid,shape= Treatment),size=2,position=position_jitter(h=0,w=0.2),alpha=0.2) +
    geom_boxplot(fill=NA,colour=alpha("black",0.4),width=0.8,size=0.2)+
    #geom_pointrange(aes(ymin=predictEggslower,ymax=predictEggsupper,shape=Treatment,position='dodge'),alpha=0.2)+
    geom_smooth(size = 1,method="lm",se=F, col="black",aes(group=Treatment,Month,predicthatch)) +
    #geom_line(aes(y=meaneggs,group=Treatment),size=.75,position=position_dodge(width=0.2))+
    facet_grid(.~Treatment)+
    labs(x = "Manipulation",
         y = "Number of Embryos Maturing per Clutch",tag = "(a)")+ scale_color_discrete(name = "Manipulation")+ 
    scale_x_discrete(labels = c('pre','post'))+theme_classic())

(g2<-ggplot(monthhatch_Mtend2, aes(x = as.factor(Month), y = Hatch, lty=Treatment)) +
    #geom_point(aes(y = Laid,shape= Treatment),size=2,alpha=0.2,position=position_dodge(width=0.5))+
    #geom_point(aes(y = Laid,shape= Treatment),size=2,position=position_jitter(h=0,w=0.2),alpha=0.2) +
    geom_boxplot(fill=NA,colour=alpha("black",0.4),width=0.8,size=0.2)+
    #geom_pointrange(aes(ymin=predictEggslower,ymax=predictEggsupper,shape=Treatment,position='dodge'),alpha=0.2)+
    geom_smooth(size = 1,method="lm",se=F, col="black",aes(group=Treatment,Month,predicthatch)) +
    #geom_line(aes(y=meaneggs,group=Treatment),size=.75,position=position_dodge(width=0.2))+
    facet_grid(.~Treatment)+
    labs(x = "Manipulation",
         y = "Number of Embryos Maturing per Month",tag = "(b)")+ scale_color_discrete(name = "Manipulation")+ 
    scale_x_discrete(labels = c('pre','post'))+theme_classic())
grid.arrange(g1,g2,nrow=2)

