

library(lme4)
library(lmerTest) 
library(ggplot2)
library(ggthemes)
library(car)
library(phia) 
library(factoextra)
library(nFactors)
library(rmeta)
library(metafor)
library(lavaan)
library(rstudioapi) 
library(meta)

#set working directory to source folder
rm(list=ls())
setwd(dirname(getActiveDocumentContext()$path))

#### read in data #### 

s <- read.csv("Processed Societal Data.csv")
m <- read.csv("Processed Multilevel Data.csv")

#creating a datafile with only cases that have tightness data

s<-s[is.na(s$General_TL5_Final)==F,]

#### structure of tightness-looseness #### 

domains <- data.frame("Soc_TL5_Final"=s$Soc_TL5_Final,
                      "Mar_TL5_Final"=s$Mar_TL5_Final,
                      "Sex_TL5_Final"=s$Sex_TL5_Final,
                      "FM_TL5_Final"=s$FM_TL5_Final,
                      "Gender_TL5_Final"=s$Gender_TL5_Final,
                      "Law_TL5_Final"=s$General_TL5_Final)

## means of the domains
mean(domains$FM_TL5_Final,na.rm=T) ##Mean = 3.37
mean(domains$Law_TL5_Final,na.rm=T) ##Mean = 2.99
mean(domains$Gender_TL5_Final,na.rm=T) ##Mean = 2.85
mean(domains$Sex_TL5_Final,na.rm=T) ##Mean = 2.81
mean(domains$Mar_TL5_Final,na.rm=T) ##Mean = 2.77
mean(domains$Soc_TL5_Final,na.rm=T) ##Mean = 2.65

# exploratory factor analysis
ev <- eigen(cor(domains,use="complete.obs")) # get eigenvalues
ap <- parallel(subject=nrow(domains),var=ncol(domains),
               rep=100,cent=.05)
nS <- nScree(x=ev$values)
plotnScree(nS) 

##PCA 
res.pca <- prcomp(na.omit(domains), scale = TRUE)
summary(res.pca)
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
get_pca(res.pca, "ind")

##ICC
summary(lmer(TL1~(1|sccsn),data=m)) #ICC = 0.3170/(0.3170+0.3618) = .47/47%

##correlate the domains
cor.test(domains$Law_TL5_Final,domains$Soc_TL5_Final)
cor.test(domains$Law_TL5_Final,domains$Gender_TL5_Final)
cor.test(domains$Law_TL5_Final,domains$Mar_TL5_Final)
cor.test(domains$Law_TL5_Final,domains$Sex_TL5_Final)
cor.test(domains$Law_TL5_Final,domains$FM_TL5_Final)

cor.test(domains$Gender_TL5_Final,domains$Soc_TL5_Final)
cor.test(domains$Gender_TL5_Final,domains$Mar_TL5_Final)
cor.test(domains$Gender_TL5_Final,domains$Sex_TL5_Final)
cor.test(domains$Gender_TL5_Final,domains$FM_TL5_Final)

cor.test(domains$Soc_TL5_Final,domains$Mar_TL5_Final)
cor.test(domains$Soc_TL5_Final,domains$Sex_TL5_Final)
cor.test(domains$Soc_TL5_Final,domains$FM_TL5_Final)

cor.test(domains$Mar_TL5_Final,domains$Sex_TL5_Final)
cor.test(domains$Mar_TL5_Final,domains$FM_TL5_Final)

cor.test(domains$Sex_TL5_Final,domains$FM_TL5_Final)

#### correlates of tightness-looseness ####

###Pathogen Prevalence
summary(lmer(ZTL1~Zpathogensum+(1|sccsn),data=m))
summary(lmer(ZTL1~Zpathogensum+(1|sccsn)+(1|Family),data=m))

summary(lmer(ZTL1~Zpathogendummy+(1|sccsn),data=m))
summary(lmer(ZTL1~Zpathogendummy+(1|sccsn)+(1|Family),data=m))

####Warfare 
summary(lmer(ZTL1~Zv773_rev+(1|sccsn),data=m)) #intra-polity
summary(lmer(ZTL1~Zv773_rev+(1|sccsn)+(1|Family),data=m)) #intra-polity | singular

summary(lmer(ZTL1~Zv774_rev+(1|sccsn),data=m)) #inter-polity
summary(lmer(ZTL1~Zv774_rev+(1|sccsn)+(1|Family),data=m)) #inter-polity | singular

##Famine/Scarcity
summary(lmer(ZTL1~famine_index+(1|sccsn),data=m)) 
summary(lmer(ZTL1~famine_index+(1|sccsn)+(1|Family),data=m)) 

#Natural Disasters
summary(lmer(ZTL1~Zoth+(1|sccsn),data=m)) 
summary(lmer(ZTL1~Zoth+(1|sccsn)+(1|Family),data=m)) 

##Population Density
summary(lmer(ZTL1~Zv1720_poppress+(1|sccsn),data=m))
summary(lmer(ZTL1~Zv1720_poppress+(1|sccsn)+(1|Family),data=m))

##Complexity
summary(lmer(ZTL1~complexity+(1|sccsn),data=m)) #complexity
summary(lmer(ZTL1~complexity+(1|sccsn)+(1|Family),data=m)) #complexity

#Individual indicators
summary(lmer(ZTL1~v149+(1|sccsn)+(1|Family),data=m)) #writing and records
summary(lmer(ZTL1~v150+(1|sccsn)+(1|Family),data=m)) #fixidity of residence
summary(lmer(ZTL1~v151+(1|sccsn)+(1|Family),data=m)) #agriculture
summary(lmer(ZTL1~v152+(1|sccsn)+(1|Family),data=m)) #urbanization
summary(lmer(ZTL1~v153+(1|sccsn)+(1|Family),data=m)) #technological specialization - null
summary(lmer(ZTL1~v154+(1|sccsn)+(1|Family),data=m)) #land transport - null
summary(lmer(ZTL1~v155+(1|sccsn)+(1|Family),data=m)) #money
summary(lmer(ZTL1~v156+(1|sccsn)+(1|Family),data=m)) #density of population
summary(lmer(ZTL1~v157+(1|sccsn)+(1|Family),data=m)) #political integration
summary(lmer(ZTL1~v158+(1|sccsn)+(1|Family),data=m)) #social stratification

###Residence heterogeneity
summary(lmer(ZTL1~scale(Res_Hetero)+(1|sccsn),data=m))
summary(lmer(ZTL1~scale(Res_Hetero)+(1|sccsn)+(1|Family),data=m))

##Mobility and intergroup contact
summary(lmer(ZTL1~contact2+(1|sccsn),data=m))
summary(lmer(ZTL1~contact2+(1|sccsn)+(1|Family),data=m))

##Political Decision-Making
summary(lmer(ZTL1~Zv761_rev+(1|sccsn),data=m)) 
summary(lmer(ZTL1~Zv761_rev+(1|sccsn)+(1|Family),data=m)) 

##moralizing high gods
summary(lmer(ZTL1~Zv238_rec+(1|sccsn),data=m)) 
summary(lmer(ZTL1~Zv238_rec+(1|sccsn)+(1|Family),data=m)) ##did not converge

####Forest plot of correlations
meta<-read.csv("forest.csv")

analysis1<-rma(Beta,VAR,method="REML",data=meta)  ##
summary(analysis1)

cols <- c("red", "blue")[match(meta$Nesting, c("Society", "Family"))]

f1<-meta::forest(analysis1, 
                 slab=paste(meta$Variable),  ##what is the label on the x axis?
                 psize = meta$Societies/40,   ##how large do you want the points?
                 top = 0,   ##how much space do you want above the graph
                 at = c(-.75,-.50,-.25,0,.25,.50,.75),  ##sets reference numbers at the bottom
                 refline = 0,  ##adds a dashed reference line
                 pch = 16)#,

#single ecological threats with controls
summary(lmer(ZTL1~Zpathogensum+longitude+latitude+horticulture+pastoralism + huntergatherer + (1|sccsn),data=m))
summary(lmer(ZTL1~Zv773_rev+longitude+latitude+horticulture+pastoralism + huntergatherer + (1|sccsn),data=m))
summary(lmer(ZTL1~Zv774_rev+longitude+latitude+horticulture+pastoralism + huntergatherer + (1|sccsn),data=m))
summary(lmer(ZTL1~famine_index+longitude+latitude+horticulture+pastoralism + huntergatherer + (1|sccsn),data=m))
summary(lmer(ZTL1~Zoth+longitude+latitude+horticulture+pastoralism + huntergatherer + (1|sccsn),data=m))
summary(lmer(ZTL1~Zv1720_poppress+longitude+latitude+horticulture+pastoralism + huntergatherer + (1|sccsn),data=m))

#### structural equation model #####

##SEM
model1 <- "TL =~ Soc_TL5_Final + Mar_TL5_Final + Sex_TL5_Final + FM_TL5_Final + Gender_TL5_Final + General_TL5_Final
          
          TL ~ threat_fam + threat_path + complexity + Res_Hetero + pastoralism + huntergatherer + horticulture + agriculture + MatriDescent + PatriDescent + polygamy + latitude + longitude
          Contact.OtherSocieties ~ TL + pastoralism + huntergatherer + horticulture + agriculture + latitude + longitude
          Zv761_rev ~ TL + pastoralism + huntergatherer + horticulture + agriculture + latitude + longitude
          Zv238_rec ~ TL + pastoralism + huntergatherer + horticulture + agriculture + latitude + longitude"

fit1 <- cfa(model = model1, data = s, missing = "ML")
summary(fit1, fit.measures = T, standardized = T,rsquare=T)

semPlot::semPaths(fit1)

#### supplement : tightness-looseness scores in Table S1 ####

tightness_scores <- data.frame("sccsn" = s$sccsn,
                               "society" = s$socname,
                               "tightness" = rowMeans(domains,na.rm=T))

#### supplement : additional models ####

##Removing TL from equations predicting theorized consequences
model2 <- "TL =~ Soc_TL5_Final + Mar_TL5_Final + Sex_TL5_Final + FM_TL5_Final + Gender_TL5_Final + General_TL5_Final
          
          TL ~ threat_fam + threat_path + complexity + pastoralism + huntergatherer + horticulture + agriculture + Res_Hetero + MatriDescent + PatriDescent + polygamy + latitude + longitude
          Contact.OtherSocieties ~ pastoralism + huntergatherer + horticulture + latitude + longitude
          Zv761_rev ~ pastoralism + huntergatherer + horticulture + latitude + longitude
          Zv238_rec ~ pastoralism + huntergatherer + horticulture + latitude + longitude"

fit2 <- cfa(model = model2, data = s, missing = "ML")
summary(fit2, fit.measures = T, standardized = T,rsquare=T)

lavTestLRT(fit1,fit2) ##model 2 is significantly worse

##Removing controls from regressions predicting theorized antecedents
model3 <- "TL =~ Soc_TL5_Final + Mar_TL5_Final + Sex_TL5_Final + FM_TL5_Final + Gender_TL5_Final + General_TL5_Final
          
          TL ~ threat_fam + threat_path + complexity + Res_Hetero
          Contact.OtherSocieties ~ TL + pastoralism + huntergatherer + horticulture + agriculture + latitude + longitude
          Zv761_rev ~ TL + pastoralism + huntergatherer + horticulture + latitude + longitude
          Zv238_rec ~ TL + pastoralism + huntergatherer + horticulture + latitude + longitude

          MatriDescent~~PatriDescent
          polygamy ~~ MatriDescent
          polygamy ~~ PatriDescent"

fit3 <- cfa(model = model3, data = s, missing = "ML")
summary(fit3, fit.measures = T, standardized = T,rsquare=T)

lavTestLRT(fit1,fit3) ##model 3 is significantly worse

##Removing controls from regressions predicting theorized consequences
model4 <- "TL =~ Soc_TL5_Final + Mar_TL5_Final + Sex_TL5_Final + FM_TL5_Final + Gender_TL5_Final + General_TL5_Final
          
          TL ~ threat_fam + threat_path + complexity + pastoralism + huntergatherer + horticulture + agriculture + Res_Hetero + MatriDescent + PatriDescent + polygamy + latitude + longitude
          Contact.OtherSocieties ~ TL
          Zv761_rev ~ TL
          Zv238_rec ~ TL"

fit4 <- cfa(model = model4, data = s, missing = "ML")
summary(fit4, fit.measures = T, standardized = T,rsquare=T)

lavTestLRT(fit1,fit4) ##model 4 is significantly worse

##Adding regressions for association between theorized consequences
model5 <- "TL =~ Soc_TL5_Final + Mar_TL5_Final + Sex_TL5_Final + FM_TL5_Final + Gender_TL5_Final + General_TL5_Final
          
          TL ~ threat_fam + threat_path + complexity + pastoralism + huntergatherer + horticulture + agriculture + Res_Hetero + MatriDescent + PatriDescent + polygamy + latitude + longitude
          Contact.OtherSocieties ~ TL + pastoralism + huntergatherer + horticulture + agriculture + latitude + longitude
          Zv761_rev ~ TL + pastoralism + huntergatherer + horticulture + latitude + longitude
          Zv238_rec ~ TL + pastoralism + huntergatherer + horticulture + latitude + longitude

          Contact.OtherSocieties~Zv761_rev
          Zv761_rev~Zv238_rec
          Contact.OtherSocieties~Zv238_rec"

fit5 <- cfa(model = model5, data = s, missing = "ML")
summary(fit5, fit.measures = T, standardized = T,rsquare=T)

lavTestLRT(fit1,fit5) ##model 5 is significantly worse

##Adding regressions for association between theorized antecedents
model6 <- "TL =~ Soc_TL5_Final + Mar_TL5_Final + Sex_TL5_Final + FM_TL5_Final + Gender_TL5_Final + General_TL5_Final
          
          TL ~ threat_fam + threat_path + complexity + pastoralism + huntergatherer + horticulture + agriculture + Res_Hetero + MatriDescent + PatriDescent + polygamy + latitude + longitude
          Contact.OtherSocieties ~ TL + pastoralism + huntergatherer + horticulture + agriculture + latitude + longitude
          Zv761_rev ~ TL + pastoralism + huntergatherer + horticulture + latitude + longitude
          Zv238_rec ~ TL + pastoralism + huntergatherer + horticulture + latitude + longitude

          threat_fam~threat_path
          complexity~threat_fam
          complexity~threat_path
          Res_Hetero~threat_fam
          Res_Hetero~threat_path"

fit6 <- cfa(model = model6, data = s, missing = "ML")
summary(fit6, fit.measures = T, standardized = T,rsquare=T)

lavTestLRT(fit1,fit6) ##model 6 is significantly worse

##Switching antecedents and consequences
model7 <- "TL =~ Soc_TL5_Final + Mar_TL5_Final + Sex_TL5_Final + FM_TL5_Final + Gender_TL5_Final + General_TL5_Final
          
          TL ~ Contact.OtherSocieties + Zv761_rev + Zv238_rec + pastoralism + huntergatherer + horticulture + agriculture + MatriDescent + PatriDescent + polygamy + latitude + longitude
          threat_path ~ TL + pastoralism + huntergatherer + horticulture + agriculture + latitude + longitude
          threat_fam ~ TL + pastoralism + huntergatherer + horticulture + agriculture + latitude + longitude
          complexity ~ TL + pastoralism + huntergatherer + horticulture + latitude + longitude
          Res_Hetero ~ TL + pastoralism + huntergatherer + horticulture + latitude + longitude"

fit7 <- cfa(model = model7, data = s, missing = "ML")
summary(fit7, fit.measures = T, standardized = T,rsquare=T) ##This model did NOT converge

#### supplement : domain-specific analyses ####

##Domain-Specific Analyses
cor.test(s$Soc_TL5_Final,s$pathogensum)
cor.test(s$Soc_TL5_Final,s$Zv773_rev)
cor.test(s$Soc_TL5_Final,s$Zv774_rev)
cor.test(s$Soc_TL5_Final,s$famine_index)
cor.test(s$Soc_TL5_Final,s$Zoth)
cor.test(s$Soc_TL5_Final,s$v1720_poppress)
cor.test(s$Soc_TL5_Final,s$complexity)
cor.test(s$Soc_TL5_Final,s$Res_Hetero)
cor.test(s$Soc_TL5_Final,s$Contact.OtherSocieties)
cor.test(s$Soc_TL5_Final,s$Zv761_rev) #authoritarian leader
cor.test(s$Soc_TL5_Final,s$Zv238_rec) ##high god

cor.test(s$General_TL5_Final,s$pathogensum)
cor.test(s$General_TL5_Final,s$Zv773_rev)
cor.test(s$General_TL5_Final,s$Zv774_rev)
cor.test(s$General_TL5_Final,s$famine_index)
cor.test(s$General_TL5_Final,s$Zoth)
cor.test(s$General_TL5_Final,s$v1720_poppress)
cor.test(s$General_TL5_Final,s$complexity)
cor.test(s$General_TL5_Final,s$Res_Hetero)
cor.test(s$General_TL5_Final,s$Contact.OtherSocieties)
cor.test(s$General_TL5_Final,s$Zv761_rev) #authoritarian leader
cor.test(s$General_TL5_Final,s$Zv238_rec) ##high god

cor.test(s$Gender_TL5_Final,s$pathogensum)
cor.test(s$Gender_TL5_Final,s$Zv773_rev)
cor.test(s$Gender_TL5_Final,s$Zv774_rev)
cor.test(s$Gender_TL5_Final,s$famine_index)
cor.test(s$Gender_TL5_Final,s$Zoth)
cor.test(s$Gender_TL5_Final,s$v1720_poppress)
cor.test(s$Gender_TL5_Final,s$complexity)
cor.test(s$Gender_TL5_Final,s$Res_Hetero)
cor.test(s$Gender_TL5_Final,s$Contact.OtherSocieties)
cor.test(s$Gender_TL5_Final,s$Zv761_rev) #authoritarian leader
cor.test(s$Gender_TL5_Final,s$Zv238_rec) ##high god

cor.test(s$Sex_TL5_Final,s$pathogensum)
cor.test(s$Sex_TL5_Final,s$Zv773_rev)
cor.test(s$Sex_TL5_Final,s$Zv774_rev)
cor.test(s$Sex_TL5_Final,s$famine_index)
cor.test(s$Sex_TL5_Final,s$Zoth)
cor.test(s$Sex_TL5_Final,s$v1720_poppress)
cor.test(s$Sex_TL5_Final,s$complexity)
cor.test(s$Sex_TL5_Final,s$Res_Hetero)
cor.test(s$Sex_TL5_Final,s$Contact.OtherSocieties)
cor.test(s$Sex_TL5_Final,s$Zv761_rev) #authoritarian leader
cor.test(s$Sex_TL5_Final,s$Zv238_rec) ##high god

cor.test(s$Mar_TL5_Final,s$pathogensum)
cor.test(s$Mar_TL5_Final,s$Zv773_rev)
cor.test(s$Mar_TL5_Final,s$Zv774_rev)
cor.test(s$Mar_TL5_Final,s$famine_index)
cor.test(s$Mar_TL5_Final,s$Zoth)
cor.test(s$Mar_TL5_Final,s$v1720_poppress)
cor.test(s$Mar_TL5_Final,s$complexity)
cor.test(s$Mar_TL5_Final,s$Res_Hetero)
cor.test(s$Mar_TL5_Final,s$Contact.OtherSocieties)
cor.test(s$Mar_TL5_Final,s$Zv761_rev) #authoritarian leader
cor.test(s$Mar_TL5_Final,s$Zv238_rec) ##high god

cor.test(s$FM_TL5_Final,s$pathogensum)
cor.test(s$FM_TL5_Final,s$Zv773_rev)
cor.test(s$FM_TL5_Final,s$Zv774_rev)
cor.test(s$FM_TL5_Final,s$famine_index)
cor.test(s$FM_TL5_Final,s$Zoth)
cor.test(s$FM_TL5_Final,s$v1720_poppress)
cor.test(s$FM_TL5_Final,s$complexity)
cor.test(s$FM_TL5_Final,s$Res_Hetero)
cor.test(s$FM_TL5_Final,s$Contact.OtherSocieties)
cor.test(s$FM_TL5_Final,s$Zv761_rev) #authoritarian leader
cor.test(s$FM_TL5_Final,s$Zv238_rec) ##high god




