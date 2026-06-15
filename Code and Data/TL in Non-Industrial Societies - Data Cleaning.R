
library(rstudioapi) 

#set working directory to source folder
rm(list=ls())
setwd(dirname(getActiveDocumentContext()$path))

#### Read in Data Files ####

geo <- read.csv("geography.csv") #this has elevation, latitude, and longitude 
codes <- read.csv("tightness_codes.csv") #this is the file that contains our tightness codes
tl1 <- read.csv("overall_code.csv") #this file has the overall tightness scores
meta <- read.csv("sccs_meta.csv") #this file has a bunch of sccs meta-data
resid <- read.csv("social_organization.csv") #this file has the patri- and matri-locality variables
subsistence<-read.csv("subsistence.csv")

#### Creating the Society-Level Master File #### 

d <- merge(codes,geo,by="sccsn",all=T)
d <- merge(d,meta,by="sccsn",all=T)
d <- merge(d,resid,by="sccsn",all=T)
d <- merge(d,subsistence,by="sccsn",all=T)

#Pathogen prevalence
path<-data.frame("1"=d$v1253_rec,
                 "2"=d$v1254_rec,
                 "3"=d$v1255_rec,
                 "4"=d$v1256_rec,
                 "5"=d$v1257_rec,
                 "6"=d$v1258_rec,
                 "7"=d$v1259_rec)

d$pathogensum<-rowSums(path,na.rm=T)
d$pathogendummy<-ifelse(d$pathogensum>0,1,0)
d$Zpathogensum<-scale(d$pathogensum)
d$Zpathogendummy<-scale(d$pathogendummy)

####Warfare (Ross Variables)
d$v774_rev<-(0-d$v774)+5
d$v773_rev<-(0-d$v773)+5
d$Zv773_rev<-scale(d$v773_rev)
d$Zv774_rev<-scale(d$v774_rev)

#Warfare (Ember Variables)
d$extfreq_rec<-ifelse(d$extfreq==0,NA,d$extfreq)
d$extfreq_rec<-ifelse(d$extfreq_rec==8,NA,d$extfreq_rec)
d$Zextfreq_rec<-scale(d$extfreq_rec)

d$intfreq_rec<-ifelse(d$intfreq==0,NA,d$intfreq)
d$intfreq_rec<-ifelse(d$intfreq_rec==8,NA,d$intfreq_rec)
d$Zintfreq_rec<-scale(d$intfreq_rec)

##Famine/Scarcity
d$v1265_dum<-ifelse(d$v1265>2,1,0)
d$Zfam<-scale(d$fam)
d$Zchrs<-scale(d$chrs)
d$Zv1265_dum<-scale(d$v1265_dum)
d$Zv1267<-scale(d$v1267)

famine<-data.frame("Zfam"=d$Zfam,
                   "Zchrs"=d$Zchrs,
                   "Zv1265_dum"=d$Zv1265_dum,
                   "Zv1267"=d$Zv1267)
psych::alpha(famine)

d$famine_index<-rowMeans(famine,na.rm=T)

#Natural Disasters
d$Zoth<-scale(d$oth)

##Population Density
d$Zv1720_poppress<-scale(d$v1720_poppress)

#Social Complexity
d$v151_2<-ifelse(d$v151==3,2,
                  ifelse(d$v151==4,3,
                         ifelse(d$v151==5,4,
                                ifelse(d$v151==1,1,
                                       ifelse(d$v151==2,2,
                                              NA)))))
d$v153_2<-ifelse(d$v153==3,2,
                  ifelse(d$v153==4,2,
                         ifelse(d$v153==5,3,
                                ifelse(d$v153==1,1,
                                       ifelse(d$v153==2,2,
                                              NA)))))
d$v155_2<-ifelse(d$v155==3,2,
                  ifelse(d$v155==4,3,
                         ifelse(d$v155==5,4,
                                ifelse(d$v155==1,1,
                                       ifelse(d$v155==2,2,
                                              NA)))))

complexity<-data.frame("writingrecords"=scale(d$v149),
                       "fixedresidence"=scale(d$v150),
                       "agriculture"=scale(d$v151),
                       "urbanization"=scale(d$v152),
                       "technology"=scale(d$v153_2),
                       "landtransport"=scale(d$v154),
                       "money"=scale(d$v155_2),
                       "popdensity"=scale(d$v156),
                       "polintegration"=scale(d$v157),
                       "stratification"=scale(d$v158))
psych::alpha(complexity)
d$complexity<-rowMeans(complexity,na.rm=T)

###Patrilocality
d$Res_Hetero<-ifelse(d$Predom_Bilocal>0,1,
                      ifelse(d$Predom_M>0,1,
                             ifelse(d$Predom_Bilocal==0&d$Predom_M==0,0,
                                    NA)))

##Mobility and intergroup contact
d$mobility<-d$Adult.Mobility
d$contact2<-0-d$Contact.OtherSocieties

##Political Authoritarianism
d$v761_rev<-(0-d$v761)+5
d$Zv761<-scale(d$v761)
d$Zv761_rev<-0-scale(d$v761)

##moralizing high gods
d$Zv238_rec<-scale(ifelse(d$v238<4,0,
                           ifelse(d$v238==4,1,
                                  NA)))

##Polygyny (control variable)
d$polygamy<-ifelse(d$v861==1,0,
                    ifelse(d$v861==2,0,
                           ifelse(d$v861==3,1,
                                  ifelse(d$v861==4,1,
                                         NA))))

# factor analysis of threat

threats<-data.frame("famine"=d$famine_index,
                    "populationpressure"=d$Zv1720_poppress,
                    "naturalhazards"=d$Zoth,
                    "externalwar"=d$Zv774_rev,
                    "internalwar"=d$Zv773_rev,
                    "pathogens"=d$Zpathogensum)

threats2<-na.omit(threats)

library(nFactors)
ev <- eigen(cor(threats2)) # get eigenvalues
ap <- parallel(subject=nrow(threats2),var=ncol(threats2),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qpea)
plotnScree(nS)

# Maximum Likelihood Factor Analysis
fit <- factanal(threats2, factors=2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(threats),cex=.7) # add variable names

d$threat_fam<-rowMeans(threats[,1:3],na.rm=T)
d$threat_path<-rowMeans(threats[,4:6],na.rm=T)

#write out the society-level file

write.csv(d,"Processed Societal Data.csv",na = "",row.names = F)

#### create the multi-level file ####

long<-merge(d,tl1,by="sccsn",all=T)

#Creating domain dummy-variables
long$Law<-ifelse(long$Domain==1,1,0)
long$Socialization<-ifelse(long$Domain==2,1,0)
long$Gender<-ifelse(long$Domain==3,1,0)
long$Marriage<-ifelse(long$Domain==4,1,0)
long$Sexuality<-ifelse(long$Domain==5,1,0)
long$Funerals<-ifelse(long$Domain==6,1,0)

#standardize the TL1 variable

long$ZTL1 <- scale(long$TL1)

# write out the file

write.csv(long,"Processed Multilevel Data.csv",na = "",row.names = F)
