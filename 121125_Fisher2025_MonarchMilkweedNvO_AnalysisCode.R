setwd("C:/Users/FisherKe/OneDrive - State of Connecticut/Publications/Monarch Milkweed New v. Old")

library(ggplot2)
library(emmeans)
library(lattice)
library(Rmisc)
library(grid)
library(dplyr)
library(lubridate)
library(circular)
library(multcomp)
library(gridExtra)
library(multcompView)


#######################################################################################
##milkweed quality
leaf<-read.csv("LeafCharacteristics.csv",header=TRUE)

######leaf length
g<-glm(Length~Leaf,data=leaf,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Leaf"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(leaf, measurevar="Length", groupvars=c("Leaf"))
gse


######leaf width
g<-glm(Width~Leaf,data=leaf,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Leaf"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(leaf, measurevar="Length", groupvars=c("Leaf"))
gse


#####leaf toughness
g<-glm(Force~Leaf,data=leaf,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Leaf"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(leaf, measurevar="Length", groupvars=c("Leaf"))
gse


#####total nitrogen analyses
N<-read.csv("PercentNitrogen.csv",header=TRUE)
names(N)
boxplot(NitrogenPerc~Leaf, data=N)

#GLM
g<-glm(NitrogenPerc~Leaf,data=N,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Leaf"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(N, measurevar="NitrogenPerc", groupvars=c("Leaf"))
gse


#######################################################################################
####larval survival, growth, and development

##proportion survival
sur<-read.csv("SurviveToPupation.csv", header=TRUE)
chisq.test(c(74.2,75.4))#6 days
chisq.test(c(48.8,60.8))#pupation
#no difference in survival


#mass at day 6
day<-read.csv("2324_GD_6DayMass.csv", header=TRUE)
names(day)
day$Trial<-as.character(day$Trial)
day$Treatment<-as.character(day$Treatment)
day$YearTrial<-as.character(day$YearTrial)
day
#pupal2<-pupal[!(pupal$Trial=="3"),]

boxplot(SixDayMass~Treatment*Year, data=day)

g<-glm(SixDayMass~Treatment*YearTrial,data=day,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Treatment","YearTrial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("YearTrial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("Treatment"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(day, measurevar="SixDayMass", groupvars=c("Treatment"))
gse



#pupal mass
pupal<-read.csv("2324_GD_PupalMass.csv", header=TRUE)
names(pupal)
unique(pupal$Trial)
unique(pupal$Year)
pupal$YearTrial<-as.character(pupal$YearTrial)
pupal$Treatment<-as.character(pupal$Treatment)
#pupal2<-pupal[!(pupal$Trial=="3"),]

g<-glm(PupalMass~Treatment*YearTrial,data=pupal,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Treatment","YearTrial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("YearTrial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("Treatment"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

boxplot(PupalMass~Treatment*Year, data=pupal)

gse<-summarySE(pupal, measurevar="PupalMass", groupvars=c("Treatment"))
gse


####mass in one figure
choice<-read.csv("2324_GD_AllMass.csv",header=TRUE)
choice$Trial<-as.character(choice$Trial)
names(choice)
head(choice)

boxplot(Mass~Treatment*Stage, data=choice)

names(choice)

desired_order<-c("Top","Bottom")
choice$Treatment<-factor(choice$Treatment,levels=desired_order)

stage<-c("Day 6","Day 6","Pupa","Pupa")
treat<-c("Top","Bottom","Top","Bottom")
sig<-c("b","a","b","a")
data_label<-data.frame(Stage=stage,Treatment=treat,Sig=sig)

# Step 1: Get max Mass per group
label_positions <- choice %>%
  group_by(Stage, Treatment) %>%
  summarise(y_pos = max(Mass, na.rm = TRUE) + 0.05, .groups = "drop")

# Step 2: Merge with your label data
data_label_pos <- merge(data_label, label_positions, by = c("Stage", "Treatment"))

ggplot(choice, aes(x = Stage, y = Mass, fill = Treatment)) +
  geom_boxplot()+
  xlab("Lifestage") +
  ylab("Mass (g)")+
  theme_bw()+
  scale_fill_grey(start=0.4, end=0.8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.75))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #geom_text(aes(label=sig, y=(1.6)),colour="black",vjust=0.3, size=6, position=position_dodge(.8),fontface="plain")+
  #geom_text(aes(label=sigexit, y=(count+2)),colour="deepskyblue3",vjust=-1.5, size=5, position=position_dodge(.8),fontface="italic")+
  #geom_text(aes(label=sigenter, y=(count+2)),colour="darkslategray",vjust=-1.5, size=5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  #scale_y_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 14))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #ggtitle("Comparing Top Leaves Only")+
  #theme(aspect.ratio = 4.5/10)
  #theme(legend.position = "bottom")+
  guides(fill=guide_legend(title="Leaf Location"))+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14))+
  theme(legend.box.background=element_rect(colour = "black"),
        legend.background = element_blank())+
  #geom_vline(xintercept=1.5,linetype="dotdash")+
  #annotate("text", x = 1, y = 10, label = "p = < 0.0001")+
  #annotate("text", x = 2, y = 24, label = "p < 0.0001")
  theme(legend.position="bottom")+
  geom_text(data = data_label_pos,
            aes(x = Stage, y = y_pos, label = Sig, group = Treatment, fill = NULL),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 6)


#############
#days until pupation
day<-read.csv("2324_GD_PupalDays.csv", header=TRUE)
names(day)
day$Trial<-as.character(day$Trial)
day$Treatment<-as.character(day$Treatment)
day$YearTrial<-as.character(day$YearTrial)
day
#pupal2<-pupal[!(pupal$Trial=="3"),]

boxplot(PupaDays~Treatment*Year, data=day)

g<-glm(PupaDays~Treatment*YearTrial,data=day,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Treatment","YearTrial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("YearTrial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("Treatment"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(day, measurevar="PupaDays", groupvars=c("Treatment"))
gse



#######################################################################################
####feeding behavior assays

#############
#no choice neonate
nochoiceneo<-read.csv("NoChoice_neonate2.csv",header=TRUE)
nochoiceneo$Trial<-as.character(nochoiceneo$Trial)
names(nochoiceneo)
head(nochoiceneo)

boxplot(BiomassConsumed~Treatment, data=nochoiceneo)

g<-glm(BiomassConsumed~Treatment*Trial,data=nochoiceneo,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Treatment","Trial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("Trial"),type="response")
joint_tests(emm)
pairs(emm)
cld(emm)
emm<-emmeans(g,c("Treatment"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(nochoiceneo, measurevar="BiomassConsumed", groupvars=c("Treatment"))
gse

boxplot(MassChange~Treatment, data=nochoiceneo)

g<-glm(MassChange~Treatment*Trial,data=nochoiceneo,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Treatment","Trial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("Trial"),type="response")
joint_tests(emm)
pairs(emm)
cld(emm)
emm<-emmeans(g,c("Treatment"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(nochoiceneo, measurevar="MassChange", groupvars=c("Treatment"))
gse


#no choice third
nochoice3<-read.csv("NoChoice_third.csv",header=TRUE)
nochoice3$Trial<-as.character(nochoice3$Trial)
names(nochoice3)
head(nochoice3)

boxplot(BiomassConsumed~Treatment, data=nochoice3)

g<-glm(BiomassConsumed~Treatment*Trial,data=nochoice3,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Treatment","Trial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("Trial"),type="response")
joint_tests(emm)
pairs(emm)
cld(emm)
emm<-emmeans(g,c("Treatment"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(nochoice3, measurevar="BiomassConsumed", groupvars=c("Treatment"))
gse

boxplot(MassChange~Treatment, data=nochoice3)

g<-glm(MassChange~Treatment*Trial,data=nochoice3,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Treatment","Trial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("Trial"),type="response")
joint_tests(emm)
pairs(emm)
cld(emm)
emm<-emmeans(g,c("Treatment"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(nochoice3, measurevar="MassChange", groupvars=c("Treatment"))
gse


###############################################################
#choice data

choice<-read.csv("Choice_Neonate&Third.csv",header=TRUE)
choice$Trial<-as.character(choice$Trial)
names(choice)
head(choice)

#neonates
neonate<-choice[(choice$Lifestage=="Neonate"),]
boxplot(BiomassConsumed~Leaf, data=neonate)

g<-glm(BiomassConsumed~Leaf*Trial,data=neonate,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Leaf","Trial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("Trial"),type="response")
joint_tests(emm)
pairs(emm)
cld(emm)
emm<-emmeans(g,c("Leaf"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(neonate, measurevar="BiomassConsumed", groupvars=c("Leaf"))
gse

#third instar
third<-choice[(choice$Lifestage=="Third"),]
boxplot(BiomassConsumed~Leaf, data=third)

g<-glm(BiomassConsumed~Leaf*Trial,data=third,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("Leaf","Trial"),type="response")
joint_tests(emm)
pairs(emm)
emm<-emmeans(g,c("Trial"),type="response")
joint_tests(emm)
pairs(emm)
cld(emm)
emm<-emmeans(g,c("Leaf"),type="response")
joint_tests(emm)
pairs(emm)
emm
cld(emm)

gse<-summarySE(third, measurevar="BiomassConsumed", groupvars=c("Leaf"))
gse


####figure 2
boxplot(BiomassConsumed~Leaf*Lifestage, data=choice)

names(choice)

g<-glm(BiomassConsumed~LifestageLeaf,data=choice,family = gaussian(link = "identity"))
summary(g)
emm<-emmeans(g,c("LifestageLeaf"),type="response")
joint_tests(emm)
pairs(emm)
cld(emm)

gse<-summarySE(choice, measurevar="BiomassConsumed", groupvars=c("Lifestage","Leaf"))
gse
gse$BiomassConsumed2<-gse$BiomassConsumed*1000
gse$se2<-gse$se*1000
gse
#gse$below<-c(0.39,0.23,0.19,0.19,0.36,0.19)
gse$sig<-c("b","a","b","a")
#gse$name<-c("Clip Bottom","Clip Bottom","Clip Bottom","Undisturb Bottom","Undisturb Bottom","Undisturb Bottom",
#           "Clip Top","Clip Top","Clip Top","Undisturb Top","Undisturb Top","Undisturb Top")
gse$order<-c(1,1,2,2)
gse$order2<-c(2,1,2,1)
gse$Lifestage2<-c("Neonate","Neonate","Third Instar","Third Instar")
#gse<-gse[-c(3),]
gse
#gse$Force<-c(0.9211111 ,0.8757407 ,0,0.9216667,0.9746296 ,0.7198148  )
#gse$sd<-c(0.1843977 ,0.3283096 ,0,0.1859169 ,0.2586665 ,0.2181871 )
#gse$sig<-c("b","b"," ","b","b","a")
#gse

ggplot(data = gse, aes(x = reorder(Lifestage2,order), y = BiomassConsumed2, fill = reorder(Leaf,order2))) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75,color="black")  +
  #ggplot(gse, aes(x=reorder (Leaf,order), y=Force, width=.8))+
  # geom_bar(stat="identity", color="black",fill="grey",
  #         position=position_dodge())+
  xlab("Lifestage") +
  ylab("Biomass Consumed (mg)")+
  theme_bw()+
  scale_fill_grey()+
  #scale_fill_brewer(palette="PuBuGn")+
  scale_y_continuous(expand=c(0,0), limits=c(0,26))+
  geom_errorbar(aes(ymin=BiomassConsumed2-se2, ymax=BiomassConsumed2+se2), width=.2, 
                position=position_dodge(.8))+
  geom_text(aes(label=sig, y=(BiomassConsumed2+se2+2)),colour="black",vjust=0.3, size=6, position=position_dodge(.8),fontface="plain")+
  #geom_text(aes(label=sigexit, y=(count+2)),colour="deepskyblue3",vjust=-1.5, size=5, position=position_dodge(.8),fontface="italic")+
  #geom_text(aes(label=sigenter, y=(count+2)),colour="darkslategray",vjust=-1.5, size=5, position=position_dodge(.8),fontface="plain")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  #scale_y_discrete(labels=label_wrap_gen(width=10))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title = element_text(size = 14))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #ggtitle("Comparing Top Leaves Only")+
  #theme(aspect.ratio = 4.5/10)
  #theme(legend.position = "bottom")+
  guides(fill=guide_legend(title="Leaf Location"))+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14))+
  theme(legend.box.background=element_rect(colour = "black"),
        legend.background = element_blank())+
  geom_vline(xintercept=1.5,linetype="dotdash")+
  #annotate("text", x = 1, y = 10, label = "p = < 0.0001")+
  #annotate("text", x = 2, y = 24, label = "p < 0.0001")
  theme(legend.position="bottom")



