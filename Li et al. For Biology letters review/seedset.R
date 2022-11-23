
####################################################################################
### Data and code for Dongbo Li,Christopher F. Clements, Jane Memmott (2022) #######
### 'Isolation limits pollination in a UK fragmented landscape'              #######
####################################################################################


##load package
library(tidyverse)
library(glmmTMB)
library(DHARMa)


###read in data
df<- read_csv('C:\\Users\\ek18186\\OneDrive - University of Bristol\\Desktop\\My phd research outputs\\Papers\\Bluebell paper\\drafts\\Submittion\\data repository/Bluebell_seedset.csv')

##wild as logical
df$wild <- as.logical(df$wild )

## scale x data
dfs <- transform(df,area=scale(area,center=FALSE),proximity=scale(proximity,center=FALSE))

###fit quasi Poisson GLMMs
##number of seeds
t.glmm.s <- glmmTMB(No.S~ area+proximity+area:proximity
                  +(1|id)+(1|wild),
                  family=nbinom1,
                  data=dfs)

summary(t.glmm.s )

#check model fit
plot(simulateResiduals(t.glmm.s ,n=1000))


##number of seed capusles
t.glmm.f <- glmmTMB(No.F~ area+proximity+area:proximity
                  +(1|id)+(1|wild),
                  family=nbinom1,
                  data=dfs)

summary(t.glmm.f)

##check model fit
plot(simulateResiduals(t.glmm.f,n=1000))

##number of undeveloped flowers
t.glmm.uf <- glmmTMB(No.UF~ area+proximity+area:proximity
                    +(1|id)+(1|wild),
                    family=nbinom1,
                    data=dfs)

summary(t.glmm.uf)


##check model fit
plot(simulateResiduals(t.glmm.uf,n=1000))








