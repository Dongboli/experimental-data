#############################################################################################################
####this code are available for peer-review of the paper'Habitat connectivity buffer extinction under ######
#### extreme droughts in experimental metapopulations'                                                 ######
#### Authors: Dongbo Li, Jane Memmott, Chris Clements                                              #########
########################################################################################################

##load packages
library(glmmTMB)
library(DHARMa)

## Q1: How experimental factors affect time to extinction
##read in data
df1 <- read.csv('/Extinction_time.csv')


str(df1)

##change to factor variables
df1$quality <- factor(df1$quality,levels = c("poor", "good"))
df1$severity <- factor(df1$severity,levels = c("slowdrying", "fastdrying"))
df1$patch <- factor(df1$patch,levels = c("slowincrease", "fastincrease"))

###GLM
glm.tte <-glm(lget~ quality+severity+patch+quality:severity+quality:patch+severity:patch，
              family =gaussian(link='log'),
              data=df1)

summary(glm.tte)

##check residuls
plot(glm.tte,2)


##Q2: fit a zero-inflated GLMM using glmmTMB
##read in data
df2 <- read.csv('/Varability_abun.csv')

##fit a zero-inflated GLMM 
m2 <- glmmTMB(varability~ 
                     week + 
                     quality+
                     severity+
                     patch+
                     quality:severity+
                     quality:patch+
                     patch:severity+
                     week:quality+
                     week:patch+
                     week:severity+
                     (1|id),
                   ziformula = ~week+severity+quality+patch,
                   data=df2, 
                   family="nbinom2")

summary(m2)

##check residuals
plot(simulateResiduals(m_mod13,n=1000))

##Q2: fit a non-linear regression curves
##read in data
df3 <- read.csv('/total_abun.csv')

##load drc package
library(drc)

##generate new dataframe for predict values
new.df <- unique(df3[,c("id",
                         "quality",
                         "severity",
                         "patch",
                         "week")])


###fit curves
est.log <- data.frame() 
fpred <- NULL
N <- 60

##for each microcosm fit a different model
for (i in (1:60)) {
  m <- drm(number ~ week,
              fct = L.3(), 
              data =subset(df3, id==i))
  
  est.log <- rbind(est.log,coef(m))
  fpred <- c(fpred, predict(pkmi, newdata=new.df))
}

##rename them 
names(est.log)<- c("b","d","e")

##then join with treatments
est.log$id <- as.numeric(rownames(est.log))

##attch with treatments
df4 <- unique(df3[,c("id",
                         "quality",
                         "severity",
                         "patch")]) %>% 
  left_join(est.log) %>% 
  subset(patch != "consistent") ##remove constant treatment


##glm the parameter b
##log transform data
df4$lgb <- log(df4$b)

str(df4)
##transfer to factor
df4$quality <- factor(df4$quality,levels = c("poor", "good"))
df4$severity <- factor(df4$severity,levels = c("slowdrying", "fastdrying"))
df4$patch <- factor(df4$patch,levels = c("slowincrease", "fastincrease"))

###GLM
detach(package:drc,unload=TRUE)

glm.b <-glm(lgb~ quality+severity+patch+quality:severity+quality:patch+severity:patch,
              family ='gaussian',
              data=df4)

summary(glm.b)

##check residuals
plot(glm.b,2)






