
# Seed set analysis

library(tidyverse)
library(ggplot2)
library(ARTool)
##

# Borage
# read in data
dat.BO <- read.csv("C:\\Users\\97679\\OneDrive - University of Leeds\\Research\\Unpublished papers\\2022-Meadow paper\\Sub drafts\\New revision 2023\\Re-written\\Data respository\\BO.csv")

head(dat.BO)
str(dat.BO)

# FACTORS
dat.BO2 <- dat.BO |> 
  mutate_at(vars(Hedge, Habitats), as.factor)
  
###one way anova of starting density
aov.BO <- aov(Starting~Hedge,data =dat.BO2 |> subset(Hedge !='UM'))

Anova(aov.BO,type =3)

# alternative welch's ANOVA
oneway.test(Starting~Hedge,data =dat.BO2|> subset(Hedge != 'UM')) 

# Manny U test on open polliantion
wilcox.test(subset(dat.BO2, Hedge !='UM')$No.seeds,subset(dat.BO2, Hedge =='UM')$No.seeds,alternative = "two.sided")

# how much open pollination increases
(mean(subset(dat.BO2, Hedge !='UM')$No.seeds, na.rm = TRUE)-mean(subset(dat.BO2, Hedge =='UM')$No.seeds, na.rm = TRUE))/mean(subset(dat.BO2, Hedge !='UM')$No.seeds, na.rm = TRUE)

# Alight rank transformed Anova
art.bo <- art(No.seeds ~  Habitats* Hedge,
              data = dat.BO2  |> subset(Hedge !='UM') |> subset(!is.na(No.seeds)) ) # remove bagging and missing

# type III anova
anova(art.bo)

# post hoc 
art.con(art.bo, "Hedge")
art.con(art.bo, "Habitats")




# Salva
# read in data 
dat.SV <- read.csv("C:\\Users\\97679\\OneDrive - University of Leeds\\Research\\Unpublished papers\\2022-Meadow paper\\Sub drafts\\New revision 2023\\Re-written\\Data respository\\SV.csv")

head(dat.SV)
str(dat.SV)

# FACTORS
dat.SV2 <- dat.SV |> 
  mutate_at(vars(Hedge, Habitats), as.factor) |> 
  subset(!is.na(WPT)) # USE wpt as pollination measures

###one way anova of starting density
aov.SV <- aov(Starting~Hedge,data =dat.SV2 |> subset(Hedge !='UM'))

Anova(aov.SV,type =3)

# alternative welch's ANOVA
oneway.test(Starting~Hedge,data =dat.SV2|> subset(Hedge != 'UM')) 

# Manny U test on open pollination 
wilcox.test(subset(dat.SV2, Hedge !='UM')$WPT,subset(dat.SV2, Hedge =='UM')$WPT,alternative = "two.sided")

# how much open pollination increases
(mean(subset(dat.SV2, Hedge !='UM')$WPT, na.rm = TRUE)-mean(subset(dat.SV2, Hedge =='UM')$WPT, na.rm = TRUE))/mean(subset(dat.SV2, Hedge !='UM')$WPT, na.rm = TRUE)

# Alight rank transformed Anova
art.sv <- art(WPT~  Habitats* Hedge,
              data = dat.SV2  |> subset(Hedge !='UM') ) # remove bagging 

# type III anova
anova(art.sv)

# post hoc 
art.con(art.sv, "Hedge")
art.con(art.sv, "Habitats")



# Lotus
# read in data 
dat.LC <- read.csv("C:\\Users\\97679\\OneDrive - University of Leeds\\Research\\Unpublished papers\\2022-Meadow paper\\Sub drafts\\New revision 2023\\Re-written\\Data respository\\LC.csv")

head(dat.LC)
str(dat.LC)

# FACTORS
dat.LC2 <- dat.LC |> 
  mutate_at(vars(Hedge, Habitats), as.factor) |> 
  subset(!is.na(No.seeds))  # USE seed numbers

###one way anova of starting density
aov.LC <- aov(Starting~Hedge,data =dat.LC2 )

Anova(aov.LC,type =3)

# alternative welch's ANOVA
oneway.test(Starting~Hedge,data =dat.LC2) 

# There was no bagging experiments for LC, see methods
# Alight rank transformed Anova
art.lc<- art(No.seeds~  Habitats* Hedge,
              data = dat.LC2   ) 

# type III anova
anova(art.lc)

# post hoc 
art.con(art.lc, "Hedge")
art.con(art.lc, "Habitats")

# Fly pollinated
# CE
# read in data 
dat.CE <- read.csv("C:\\Users\\97679\\OneDrive - University of Leeds\\Research\\Unpublished papers\\2022-Meadow paper\\Sub drafts\\New revision 2023\\Re-written\\Data respository\\CE.csv")

head(dat.CE)
str(dat.CE)

# FACTORS
dat.CE2 <- dat.CE |> 
  mutate_at(vars(Hedge, Habitats), as.factor) |> 
  subset(!is.na(Weight)) # USE  weight as pollination measures

###one way anova of starting density
aov.CE <- aov(Starting~Hedge,data =dat.CE2 |> subset(Hedge !='UM'))

Anova(aov.CE,type =3)

# alternative welch's ANOVA
oneway.test(Starting~Hedge,data =dat.CE2|> subset(Hedge != 'UM')) 

# Manny U test on open polliantion
wilcox.test(subset(dat.CE2, Hedge !='UM')$Weight,subset(dat.CE2, Hedge =='UM')$Weight,alternative = "two.sided")

# how much open pollination increases
(mean(subset(dat.CE2, Hedge !='UM')$Weight)-mean(subset(dat.CE2, Hedge =='UM')$Weight))/mean(subset(dat.CE2, Hedge !='UM')$Weight)

# Alight rank transformed Anova
art.ce <- art(Weight ~  Habitats* Hedge,
              data = dat.CE2  |> subset(Hedge !='UM')) # remove bagging 

# type III anova
anova(art.ce)

# post hoc 
art.con(art.ce, "Hedge")
art.con(art.ce, "Habitats")


# Fly pollinated: AA
# read in data 
dat.AA<- read.csv("C:\\Users\\97679\\OneDrive - University of Leeds\\Research\\Unpublished papers\\2022-Meadow paper\\Sub drafts\\New revision 2023\\Re-written\\Data respository\\AA.csv")

head(dat.AA)
str(dat.AA)

# FACTORS
dat.AA2 <- dat.AA |> 
  mutate_at(vars(Hedge, Habitats), as.factor) |> 
  subset(!is.na(Weight)) # USE  weight as pollination measures

###one way anova of starting density
aov.AA <- aov(Starting~Hedge,data =dat.AA2 )

Anova(aov.AA,type =3)

# alternative welch's ANOVA
oneway.test(Starting~Hedge,data =dat.AA2) 

# Alight rank transformed Anova
art.aa<- art(Weight ~  Habitats* Hedge,
              data = dat.AA2  ) 

# type III anova
anova(art.aa)

# post hoc 
art.con(art.aa, "Hedge")
art.con(art.aa, "Habitats")


# Moth pollinated
# ZC
# read in data 
dat.ZC<- read.csv("C:\\Users\\97679\\OneDrive - University of Leeds\\Research\\Unpublished papers\\2022-Meadow paper\\Sub drafts\\New revision 2023\\Re-written\\Data respository\\ZC.csv")

head(dat.ZC)
str(dat.ZC)

# FACTORS
dat.ZC2 <- dat.ZC |> 
  mutate_at(vars(Hedge, Habitats), as.factor) |> 
  subset(!is.na(No.seeds)) # USE  No.seeds  as pollination measures

###one way anova of starting density
aov.ZC <- aov(Starting~Hedge,data =dat.ZC2  |> subset(Hedge != 'UM')) #exclude bagging

Anova(aov.ZC,type =3)

# alternative welch's ANOVA
oneway.test(Starting~Hedge,data =dat.ZC2|> subset(Hedge != 'UM')) 

# Manny U test on open polliantion
wilcox.test(subset(dat.ZC2, Hedge !='UM')$No.seeds,subset(dat.ZC2, Hedge =='UM')$No.seeds,alternative = "two.sided")

# how much open pollination increases
(mean(subset(dat.ZC2, Hedge !='UM')$No.seeds)-mean(subset(dat.ZC2, Hedge =='UM')$No.seeds))/mean(subset(dat.ZC2, Hedge !='UM')$No.seeds)

# Alight rank transformed Anova
art.zc <- art(No.seeds ~  Habitats* Hedge,
              data = dat.ZC2  |> subset(Hedge !='UM')) # remove bagging 

# type III anova
anova(art.zc)

# post hoc 
art.con(art.zc, "Hedge")
art.con(art.zc, "Habitats")

# SN
# read in data 
dat.SN<- read.csv("C:\\Users\\97679\\OneDrive - University of Leeds\\Research\\Unpublished papers\\2022-Meadow paper\\Sub drafts\\New revision 2023\\Re-written\\Data respository\\SN.csv")

head(dat.SN)
str(dat.SN)

# FACTORS
dat.SN2 <- dat.SN |> 
  mutate_at(vars(Hedge, Habitats), as.factor) |> 
  subset(!is.na(No.seeds)) # USE  No.seeds  as pollination measures

###one way anova of starting density
aov.SN <- aov(Starting~Hedge,data =dat.SN2  |> subset(Hedge != 'UM')) #exclude bagging

Anova(aov.SN,type =3)

# alternative welch's ANOVA
oneway.test(Starting~Hedge,data =dat.SN2 |> subset(Hedge != 'UM')) 

# Manny U test on open polliantion
wilcox.test(subset(dat.SN2, Hedge !='UM')$No.seeds,subset(dat.SN2, Hedge =='UM')$No.seeds,alternative = "two.sided")

##### No significant effect of open pollination

# Alight rank transformed Anova
art.sn <- art(No.seeds ~  Habitats* Hedge,
              data = dat.SN2  |> subset(Hedge !='UM')) # remove bagging 

# type III anova
anova(art.sn)

# post hoc 
art.con(art.sn, "Hedge")
art.con(art.sn, "Habitats")

###################################################################################
####################################################################################
library(vegan)
# pollination contribution
# BO
p_BO <- dat.BO |> 
  mutate_at(vars(Hedge, Habitats), as.factor)  |>  mutate(var = No.seeds ) |> 
  dplyr::select(Round, Hedge,Habitats, Replicate, No.pc, var) |> 
  filter(!Round == 'UM') |> 
  filter(!is.na(Replicate)) |> 
  pivot_wider(names_from = Round, values_from = var ) |> 
  rename( round1='1',round2='2') |> 
  rowwise() |> 
    mutate(BO = mean(c(round1,round2), na.rm = T, na.action = na.pass)/mean( subset(dat.BO2, Round == 'UM')$No.seeds), # Divided by the mean in pollination exclusion
         Habitats = factor(Habitats),
         Replicate = factor(Replicate)) |> 
  ungroup() |> 
  mutate(uid = row_number()) |> 
  dplyr::select(Hedge,Habitats, BO,uid) |> 
  mutate(uid =factor(uid))

# SV
p_SV <- dat.SV |> 
  mutate_at(vars(Hedge, Habitats), as.factor) |> filter(!Round == 'UM') |> 
  dplyr::select(Round, Hedge, Replicate,Habitats, No.pc, WPT) |>
  mutate(SV = WPT/mean( subset(dat.SV2, Round == 'UM')$WPT),
         uid = row_number(),
         Habitats = factor(Habitats),
         Replicate = factor(Replicate))|> 
  dplyr::select(Hedge,Habitats, SV,uid)|> 
  mutate(uid =factor(uid))

# LC
p_LC <- dat.LC |> 
  mutate_at(vars(Hedge, Habitats), as.factor)   |> 
  dplyr::select(Round, Hedge,Habitats, Replicate, No.pc, var=No.seeds) |>
  pivot_wider(names_from = Round, values_from = var ) |> 
  rename( round1='1',round2='2') |> 
  rowwise() |> 
  mutate(LC = mean(c(round1,round2), na.rm = T, na.action = na.pass)/ mean(subset(dat.LC2, Hedge =='C')$No.seeds, na.rm = T) # use no hedge as baseline, see methods
  ) |> 
  ungroup() |> 
  mutate(uid = row_number(),
         Habitats = factor(Habitats),
         Replicate = factor(Replicate))|> 
  dplyr::select(Hedge,Habitats, LC,uid)|> 
  mutate(uid =factor(uid))

# CE
p_CE <-dat.CE |> 
  mutate_at(vars(Hedge, Habitats), as.factor)  |> filter(!Round == 'UM') |>
  dplyr::select(Round, Hedge,Habitats, Replicate, No.pc, var=Weight) |>
  pivot_wider(names_from = Round, values_from = var ) |> 
  rename( round1='1',round2='2') |> 
  rowwise() |> 
  mutate(CE = mean(c(round1,round2), na.rm = T, na.action = na.pass)/mean( subset(dat.CE2, Round == 'UM')$Weight)
  ) |> 
  ungroup() |> 
  mutate(uid = row_number(),
         Habitats = factor(Habitats),
         Replicate = factor(Replicate))|> 
  dplyr::select(Hedge,Habitats, CE,uid)|> 
  mutate(uid =factor(uid))

# AA
p_AA <-dat.AA |> 
  mutate_at(vars(Hedge, Habitats), as.factor)  |> 
  dplyr::select(Round, Hedge,Habitats, Replicate, No.pc=No.plant, var=Weight) |>
  pivot_wider(names_from = Round, values_from = var ) |> 
  rename( round1='1',round2='2') |> 
  rowwise() |> 
  mutate(AA = mean(c(round1,round2), na.rm = T, na.action = na.pass)/mean(subset(dat.AA2, Hedge =='C')$Weight, na.rm = T) # use no hedge as baseline, see methods
  ) |> 
  ungroup() |> 
  mutate(uid = row_number(),
         Habitats = factor(Habitats),
         Replicate = factor(Replicate))|> 
  dplyr::select(Hedge,Habitats,AA,uid)|> 
  mutate(uid =factor(uid))

# ZC
p_ZC <-dat.ZC |> 
  mutate_at(vars(Hedge, Habitats), as.factor) |> filter(!Round == 'UM') |>
  dplyr::select(Round, Hedge, Habitats, Replicate, No.pc, var=No.seeds) |>
  pivot_wider(names_from = Round, values_from = var ) |> 
  rename( round1='1',round2='2') |> 
  rowwise() |> 
  mutate(ZC = mean(c(round1,round2), na.rm = T, na.action = na.pass)*(2/3)/mean(subset(dat.ZC2, Hedge == 'UM')$No.seeds) 
  ) |> 
  ungroup() |> 
  mutate(uid = row_number(),
         Habitats = factor(Habitats),
         Replicate = factor(Replicate))|> 
  dplyr::select(Hedge,Habitats, ZC,uid)|> 
  mutate(uid =factor(uid))

# SN
p_SN <-dat.SN |> 
  mutate_at(vars(Hedge, Habitats), as.factor) |> filter(!Round == 'UM') |>
  dplyr::select(Round, Hedge, Replicate,Habitats, No.pc, No.seeds) |>
  mutate(SN = No.seeds/mean( subset(dat.SN2, Round == 'UM')$No.seeds) ,
         uid = row_number(),
         Habitats = factor(Habitats),
         Replicate = factor(Replicate)) |> 
  dplyr::select(Hedge,Habitats, SN,uid)|> 
  mutate(uid =factor(uid))

# join with data
dat1 <- left_join(p_BO,p_SV, by = c("Hedge" , "Habitats",  "uid") )

dat2 <- left_join(dat1, p_LC |> dplyr::select(uid, LC), by = c( "uid") )

dat3 <- left_join(dat2,p_CE  |> dplyr::select(uid, CE), by = c( "uid") )

dat4 <- left_join(dat3,p_AA  |> dplyr::select(uid, AA), by = c("uid") )

dat5 <- left_join(dat4,p_ZC |> dplyr::select(uid, ZC) , by = c( "uid") )

# Not join with SN
dat <- dat5

# remove community with NA to allow distance metric
nmds_dat <- dat |> 
  na.omit()

# Normalize the data to [0, 1]
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# select columns to generate new data
data_normalized <- nmds_dat |> 
  dplyr::select(BO,SV,LC,CE, AA, ZC)


# distance matrix
dissimilarity_matrix <- vegdist(data_normalized, 
                                method = "bray")

# nmds
nmds_res <- metaMDS(dissimilarity_matrix, k = 2, maxit = 999,trymax = 500)

nmds_res

# plot
plot(nmds_res, type = "t")

#stress plot
stressplot(nmds_res)

# extract NMDS scores
nmds_scores <- as.data.frame(scores(nmds_res))

## combine with data set
data_plot <- cbind(nmds_dat,nmds_scores)


## fit species vectors for draw arrows
species_fit <- envfit(nmds_res, data_normalized, permutations = 999)

## 
data_plot |> mutate(Hedge = if_else(Hedge == 'C', 'Control','Artificial linear feature'),
                    Habitats = if_else(Habitats =='urban','Urban','Rural')) -> data_plot


## draw polygons
# Function to find convex hull points for a given group
find_hull <- function(df) df[chull(df$NMDS1, df$NMDS2), ]

# Find convex hull points for each group
hulls <- cbind(data_plot[,1:5],nmds_scores) |> 
  group_by(Hedge, Habitats) |> 
  do(find_hull(.))

##
hulls_ur <- hulls |> subset(Habitats == 'Urban')
hulls_ru <- hulls |> subset(Habitats == 'Rural')


# Figure 2
ggplot() +
  geom_point(data = data_plot, aes(x = NMDS1, y = NMDS2, shape = Habitats, col = Hedge),size = 2) +
  geom_polygon(data = hulls_ur, aes(x = NMDS1, y = NMDS2, fill = Hedge), alpha = 0.4)+
  geom_polygon(data = hulls_ru, aes(x = NMDS1, y = NMDS2, fill = Hedge),linetype = 'dashed' ,alpha = 0.4)+
  theme_bw() +
  labs( x = "NMDS1", y = "NMDS2") +
  scale_fill_manual(values = c("Artificial linear feature" = "#FAAB18", "Control" = "#1380A1"), name = 'Connection')+
  scale_color_manual(values = c("Artificial linear feature" = "#FAAB18", "Control" = "#1380A1"), name = 'Connection')+
  # geom_segment(data = species_scores, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
  #  arrow = arrow(length = unit(0.3, "cm")), color = "black")+
  # geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = Species2), vjust = 0.7, hjust = 1.2)+
  annotate('text', x = 0.4, y = -1, label = 'Stress = 0.130') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'top',
        axis.title.y = element_text(size =16),
        axis.title.x = element_text(size =16),
        legend.title = element_blank()) 


ggsave("Figure2.jpg", dpi = 300, width = 7.7, height = 6.5, units = 'in')




## transfer factors
data_plot$Hedge <- as.factor(data_plot$Hedge )
data_plot$Habitats <- as.factor(data_plot$Habitats )
data_plot$inter <- interaction(data_plot$Hedge,data_plot$Habitats)


#PERMANOVA (Permutational Multivariate Analysis of Variance) 
set.seed(20230410)  # Set a seed for reproducibility

adonis_habitat <- adonis2(dissimilarity_matrix ~ inter, data = data_plot, permutations = 9999,method = "bray", by = "terms")

adonis_habitat



##
## plot pollinator contribution
cn <- colnames(new_dat)

pc <- new_dat |> left_join(p_SN |> dplyr::select(uid, SN), by ='uid') |> 
  pivot_longer(cols = c(3,5:10), names_to = 'Species', values_to = 'Pc') 

## summmarise data for plot
pc2 <- pc |> 
  group_by(Hedge, Habitats, Species) |> 
  summarise(meanpc = mean(Pc, na.rm = T),
            sdpc = sd(Pc, na.rm = T))

## Change content
pc2 |> mutate(Hedge = if_else(Hedge == 'C', 'Control','Artificial linear feature'),
              Habitats = if_else(Habitats =='urban','Urban','Rural')) -> pc2


# Figure 3
pc2 |> 
  ggplot() +
  geom_pointrange(aes(x=factor(Species, levels = c('BO','LC','SV','CE','AA','ZC','SN')), y= meanpc, ymin=meanpc-sdpc, ymax=meanpc+sdpc,col = Hedge, shape = Habitats),
                  position = position_dodge(width = 0.5)) +
  theme_classic() +
  scale_colour_manual(values = c("Artificial linear feature" = "#FAAB18", "Control" = "#1380A1"), name = 'Connection') +
  ylab('Pollination contribution') + xlab('') +
  geom_hline(yintercept  = 1, linetype="dashed") +
  theme( axis.title=element_text(size=16,face="bold"),
         axis.text.x =element_text(size=11,face="bold"))

