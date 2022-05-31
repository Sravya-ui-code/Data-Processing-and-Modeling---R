# Project 

# DATA-PREPROCESSING 

df <- read.csv(file='Creditcard.csv', stringsAsFactors = FALSE) # Load the dataset 

# Summary of the dataset 
s <- summary(df); s;   

# 
install.packages("reshape")
library(reshape)
meltData <- melt(df)
--boxplot(data=meltData, value~variable)

library(ggplot2)
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

install.packages("Hmisc")
library(Hmisc)
hist.data.frame(df)

# install.packages("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
d <- df[,2:18]
head(d)
rquery.cormat(d) # Correlation matrix graph 


colSums(is.na(df)) # Number of na values in each column 
df$MINIMUM_PAYMENTS[is.na(df$MINIMUM_PAYMENTS)]<- median(df$MINIMUM_PAYMENTS,na.rm=TRUE)
df$CREDIT_LIMIT [is.na(df$CREDIT_LIMIT)]<-median(df$CREDIT_LIMIT ,na.rm=TRUE)
colSums(is.na(df))

#Clustering 

######## PCA + Clustering Analysis ########  
d <- df[,c(2:18)]
pca.out = prcomp(d) # leave out demographics
# summary report
summary(pca.out)

# now let us look at the scores for the observations
scores = as.data.frame(predict(pca.out, d))
# alternative ways to get scores
# scores2 <- as.data.frame(pca.out$x)

# now we perform cluster analysis
# instead of using Xs in df.nona, we want to using these PCs in scores
# lets use the first 2 components and make 4 clusters
clus.out <- kmeans(d,  centers = 4, nstart = 10)
# ratio of between-cluster variation to within-cluster variation: 11.75074 => much higher than using all the variables in the raw data
clus.out$betweenss/mean(clus.out$withinss)  

# visualize the cluster
scores$cluster <- as.character(clus.out$cluster)
g <- ggplot(scores, aes(PC1, PC7)) 
g + geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0)  # label dot using the cluster name


###### choosing k using the "elbow plot" ###### 
# create an empty dataframe with two columns
# create an empty dataframe with two columns
choosek <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(choosek) <- c("numClusters", "avgWithinSS")
for (k in 2:10) {
  tempkm <- kmeans(scores[,1:2],  centers = k, nstart = 10)
  tempdf <- data.frame(numClusters = k, avgWithinSS = mean(tempkm$withinss))
  choosek <- rbind(choosek, tempdf)
}
g <- ggplot(choosek, aes(numClusters, avgWithinSS))  
g + geom_line() + labs(x="Number of Clusters (k)", y="Average Within-Cluster Squared Distance")
rm(choosek, tempdf, tempkm)

##### Directly apply Kmeans to the data ##### 
# remove NA observations, no need to scale the data b/c all ratings from columns 5:36 are on the same scale

# for replication
set.seed(123)
# kmeans(data, #clusters, nstart = 1)
#       data: data matrix or dataframe
#       #clusters: the number of clusters k
#       nstart: the number of different starting random seeds to use, recommended to set nstart value greater than 1
clus.out <- kmeans(df.nona[,2:18], centers = 4, nstart = 10)

clus.out$size  # number of obs. in each cluster
clus.out$betweenss/mean(clus.out$withinss)  # ratio of between-cluster variation to within-cluster variation: 1.408905
# check centroids for each cluster 
clus.out$centers
rm(choosek, tempdf, tempkm)

install.packages("M3C")
library(M3C)
tsne(d,labels=as.factor(clus.out$centers))

#-------------------------------------------------------------

##### Interpreting Clusters ##### 
# now let us make a new data set from df.nona
# with only demographic, overall evaluation, and a cluster column
# start by adding a column titled cluster into survey
# Nationality: US (1) Foreign (2)
# Gender: Male (1) Female (2)
# Age: age in years
# Have_offer: have a job or internship offer? Yes (1) No (2)
# Overall_Satisfaction
# visualize the cluster

survey = cbind(df.nona[,c(1,2,3,4,34)], cluster = clus.out$cluster)
survey$isUS = as.numeric(survey$Nationality==1)
survey$isMale = as.numeric(survey$Gender==1)
survey$isOffer = as.numeric(survey$Have_offer==1)

# cluster 2 has the highest overall satisfaction & cluster 3 has the lowest
survey$cluster <- as.factor(survey$cluster)
ggplot(survey, aes(cluster, Overall_Satisfaction)) + geom_boxplot() 

# when we further check students' demographics in each cluster
aggdf <- aggregate(cbind(isUS,isMale,isOffer,Age, Overall_Satisfaction) ~ cluster, data=survey, mean )
aggdf
# cluster 2: highest ratio of job & internship offers, highest female%
# cluster 3: lowest female%, youngest 

# alternative visualization 
ggplot(aggdf, aes(cluster, Overall_Satisfaction)) + 
  geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0)

ggplot(aggdf, aes(cluster, isOffer)) + 
  geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0)

ggplot(aggdf, aes(cluster, isMale)) + 
  geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0)

ggplot(aggdf, aes(cluster, Age)) + 
  geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0)


#Findings 


# ON AND OFF INSTALLMENT 

d$purchase_type 
d$purchase_type <- ifelse((d$ONEOFF_PURCHASES == 0 & df$INSTALLMENTS_PURCHASES == 0), "none",
                          ifelse((d$ONEOFF_PURCHASES > 0 & df$INSTALLMENTS_PURCHASES == 0), "ON OFF",
                          ifelse((d$ONEOFF_PURCHASES == 0 & df$INSTALLMENTS_PURCHASES > 0), "INSTALLMENTS","BOTH")));


# Marketing strategies --- 

# Purchase and tenure 

g <- ggplot(d, aes(TENURE,PURCHASES))
g + geom_point()  


#----

install.packages("Rtsne")
install.packages("M3C")

require(Rtsne)
library(M3C)
tsne(pollen$data,labels=as.factor(pollen$celltypes))


#-------------------------------------------------------------
plot(d$PURCHASES,type = "b", pch = 19, 
     col = "red", xlab = "x", ylab = "y")

installr:
install.packages("installr") 
library(installr)


x <- ggplot(aes(purchase_type,MINIMUM_PAYMENTS),data= d) +
  geom_line(lwd = 10)

ggplot2.barplot(data=df, xName="time", yName='total_bill')

#- 


boxplot(CREDIT_LIMIT ~ purchase_type,
        data= d ,
        main="",
        xlab="Month Number",
        ylab="Degree Fahrenheit",
        col="orange",
        border="brown"
)

install.packages("openintro")
library(openintro)
install.packages("lattice")
library(lattice)
library(ggplot2)
(d$purchase_type,d$credit_limit)

#------------------------------------------------------------------
install.packages("gganimate")
install.packages('devtools')
devtools::install_github('thomasp85/gganimate')
install.packages("gapminder")
library(ggplot2)
library(gganimate)
library(devtools)
theme_set(theme_bw())
library(gapminder)
head(gapminder)

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")

p

p + transition_time(year) +
  labs(title = "Year: {frame_time}")
file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)



















