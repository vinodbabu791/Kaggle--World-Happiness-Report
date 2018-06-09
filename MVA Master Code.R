# Importing Libraries
library(ggplot2)
library(ggord)
library(MVA)
library(plyr)

# Importing data
happy_data <- read.csv('C:/Users/Universe/Desktop/DataScience/Spring 2018/MVA/Project/Data/2016_Master Dat_cln.csv')

row.names(happy_data) <- happy_data[,24]
# Replacing redundant ranks
happy_data['KOR','Happiness.Rank'] <- 58
happy_data['UGA','Happiness.Rank'] <- 146
row.names(happy_data) <- happy_data$Happiness.Rank

# Replacing life expectancy of 'Sierra Leone from 0 to mean of column 
happy_data['111',c('Health..Life.Expectancy.')] <- mean(happy_data$Health..Life.Expectancy.)

# Checking correlation of data
cor(happy_data[,-c(1,2,24)])

# removing columns that does not share good correlation with happiness score.
happy_data_y <- happy_data[,c(1:6,13,16,17,18,19,21,22,23,24)]
happy_data_x <- happy_data[,-c(1:6,13,16,17,18,19,21,22,23,24)]

# Renaming column names
colnames(happy_data_x) <- c('GDPPerCapita','Family','LifeExpectancy','Freedom','TrustOnGovernment','Generosity','OverweightFemale','OverweightMale','UrbanPopulation')
head(happy_data_x)

#### Method 1: Principal Component analysis ####
happy_data_scaled <- scale(happy_data_x)
model_pca = princomp(happy_data_scaled,cor = TRUE)
summary(model_pca,loadings=TRUE)
pca_scores <- as.data.frame(model_pca$scores)
loadings <- as.data.frame(model_pca$loadings[,1:9])

## Plot of variance
variance <- data.frame(model_pca$sdev^2)
colnames(variance) <- 'var'

ggplot(data=variance,aes(x=row.names(variance),y=var,group=1))+
  geom_line(color='blue',size=1.3)+
  geom_point(color='black',size=4)+
  labs(x='Principal Components',y='Proportion of Original Variance explained',title='Scree plot: Proportion of Variance explained by PCs')+
  theme_set(theme_gray(base_size=24))

ggplot(data=variance,aes(x=row.names(variance),y=cumsum(var)*100/nrow(variance),group=1))+
  geom_line(color='darkgreen',size=1.3)+
  geom_point(color='black',size=4)+
  labs(x='Principal Components',y='% of Original Variance explained',title='Cummulative % of Variance explained by PCs')+
  theme_set(theme_gray(base_size=24))

## Biplot

# grouping data based on happiness ranking to identify distinct factors that separates happiest, happier and not so happy countries
happy_data[happy_data$Happiness.Rank<51,'happy_cat'] <- 'Happy'  
happy_data[happy_data$Happiness.Rank>101,'happy_cat'] <- 'Not so Happy'
happy_data[is.na(happy_data$happy_cat),'happy_cat'] <- 'Moderately Happy'

# with data points
ggord(model_pca,grp_in = happy_data$happy_cat,vec_ext=6,veclsz=1.5,veccol='black',txt=5,ext=1.5,alpha=0.75)+
  geom_vline(xintercept=0,color='orange',size=1.5)+
  geom_hline(yintercept=0,color='orange',size=1.5)+
  theme(legend.text=element_text(size=15))+
  labs(title='Biplot')
  
# plotting With hull for each group 
ggord(model_pca,grp_in = happy_data$happy_cat,cols=c('darkgreen','red','blue'),obslab=TRUE,vec_ext=6,veclsz=1.5,veccol='black',txt=5,ext=1.5,alpha=1,hull=T,ellipse=F)+
  geom_vline(xintercept=0,color='orange',size=1.5)+
  geom_hline(yintercept=0,color='orange',size=1.5)+
  theme(legend.text=element_text(size=15))+
  labs(title='Biplot')

## Plotting graphs separately for each group of countries

# Happy Countries
ggord(obs=pca_scores[,1:2][happy_data$happy_cat=='Happy',],cols='darkgreen',vecs=loadings,grp_in = happy_data[happy_data$happy_cat=='Happy',]$happy_cat,obslab=TRUE,vec_ext=6,veclsz=1.5,veccol='black',txt=5,ext=1.5,alpha=1,alpha_el=0.35,hull=T,ellipse=F)+
  geom_vline(xintercept=0,color='orange',size=1.5)+
  geom_hline(yintercept=0,color='orange',size=1.5)+
  theme(legend.text=element_text(size=15))+
  labs(title='Biplot',subtitle='Happy Countries')+
  xlim(-5,2)

# Moderately Happy Countries
ggord(obs=pca_scores[,1:2][happy_data$happy_cat=='Moderately Happy',],cols='red',vecs=loadings,grp_in = happy_data[happy_data$happy_cat=='Moderately Happy',]$happy_cat,obslab=TRUE,vec_ext=6,veclsz=1.5,veccol='black',txt=5,ext=1.5,alpha=1,alpha_el=0.25,hull=T,ellipse=F)+
  geom_vline(xintercept=0,color='orange',size=1.5)+
  geom_hline(yintercept=0,color='orange',size=1.5)+
  theme(legend.text=element_text(size=15))+
  labs(title='Biplot',subtitle='Moderately Happy Countries')+
  xlim(-5,4)

# Not So happy countries
ggord(obs=pca_scores[,1:2][happy_data$happy_cat=='Not so Happy',],cols='blue',vecs=loadings,grp_in = happy_data[happy_data$happy_cat=='Not so Happy',]$happy_cat,obslab=TRUE,vec_ext=6,veclsz=1.5,veccol='black',txt=5,ext=1.5,alpha=1,alpha_el=0.35,hull=T,ellipse=F)+
  geom_vline(xintercept=0,color='orange',size=1.5)+
  geom_hline(yintercept=0,color='orange',size=1.5)+
  theme(legend.text=element_text(size=15))+
  labs(title='Biplot',subtitle='Not so happy Countries')+
  xlim(-5,5)

## Comparing only happy and not so happy countries
ggord(obs=pca_scores[,1:2][happy_data$happy_cat!='Moderately Happy',],cols=c('darkgreen','blue'),vecs=loadings,grp_in = happy_data[happy_data$happy_cat!='Moderately Happy',]$happy_cat,obslab=TRUE,vec_ext=6,veclsz=1.5,veccol='black',txt=5,ext=1.5,alpha=1,alpha_el=0.35,hull=T,ellipse=F)+
  geom_vline(xintercept=0,color='orange',size=1.5)+
  geom_hline(yintercept=0,color='orange',size=1.5)+
  theme(legend.text=element_text(size=15))+
  labs(title='Biplot',subtitle='Happy Countries vs Not so happy Countries')+
  xlim(-5,5)


## plotting with country labels
row.names(pca_scores) <- happy_data$Country.Code
ggord(obs=pca_scores[,1:2],cols=c('darkgreen','red','blue'),vecs=loadings,grp_in = happy_data$happy_cat,obslab=TRUE,vec_ext=6,veclsz=1.5,veccol='black',txt=5,ext=1.5,alpha=1,alpha_el=0.35,hull=T,ellipse=F)+
  geom_vline(xintercept=0,color='orange',size=1.5)+
  geom_hline(yintercept=0,color='orange',size=1.5)+
  theme(legend.text=element_text(size=15))+
  labs(title='Biplot',subtitle='With Country labels')+
  xlim(-5,5)


