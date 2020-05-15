##############################################################
# Klusteranalys Medelkostnad + medeldagar
##############################################################

#Euclidean distance matrix 1
distance1 <- dist(df1)
print(distance1, digits=4)

#Scatter plot
plot(df1)
with(df, text(df1, labels=lannr, pos=2, cex=.6))

ggplot(df1, aes(x=Medelkostnad, y=Medeldagar)) +
  geom_text(
    label=df$lannr, 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )


#Scree plot
screeplot1 <- (nrow(df1)-1)*sum(apply(df1,2,var))
for (i in 2:17) screeplot1[i] <- sum(kmeans(df1, centers = i)$withinss)
plot(1:17, screeplot1, type = "b", xlab="Antal kluster", ylab="Within group SS", xlim=c(1,8))

#Snyggare Scree plot
fviz_nbclust(df1, kmeans, method = "wss")

#Dendrogram COMPLETE
dend1 <- hclust(distance1)
plot(dend1)
fviz_dend(x=dend1, cex=0.7, lwd=0.7)
labels(dend1) <- c("7","5","17","14","25","19","20","10","12","21","22","3","4","24","6","13","8","18")
fviz_dend(x=dend1, cex=0.8, lwd=0.1, k=3, k_colors = c("gray75","gray52","gray32"), rect=T, rect_border = c("gray85","gray52","gray42"), rect_fill = T, main="Vårdkostnad med vårddagar, medelvärden", xlab = "Län", ylab="Avstånd")

#Dendrogram AVERAGE
dend1.average <- hclust(distance1, method = "average")
plot(dend1.average)
fviz_dend(x=dend1.average, cex=0.7, lwd=0.7)

#Cluster membership
Cluster.Complete <- cutree(dend1,3)
Cluster.Average <- cutree(dend1.average,3)

#Table of cluster membership
table(Cluster.Complete, Cluster.Average)

#Cluster means
aggregate(df1, list(Cluster.Complete), mean) 
aggregate(df1, list(Cluster.Average), mean) 

### K-means clustering
kc1 <- kmeans(df1,3, 100)
kc1
plot(Medelkostnad~Medeldagar, df1, col=kc1$cluster)

#K-means snyggare graf
df1 %>%
  as_tibble() %>%
  mutate(cluster = kc1$cluster,
         state = row.names(df)) %>%
  ggplot(aes(Medelkostnad, Medeldagar, color = factor(cluster), label = df$lannr)) +
  geom_text()



##############################################################
# Klusteranalys Medelkostnad + medeldrgikr
##############################################################

#Euclidean distance matrix 1
distance2 <- dist(df2)
print(distance2, digits=4)

#Scatter plot
plot(df2)
with(df, text(df2, labels=lannr, pos=2, cex=.6))

ggplot(df2, aes(x=Medelkostnad, y=Medeldrgikr)) +
  geom_text(
    label=df$lannr, 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = TRUE
  )

#Scree plot
screeplot2 <- (nrow(df2)-1)*sum(apply(df2,2,var))
for (i in 2:17) screeplot2[i] <- sum(kmeans(df2, centers = i)$withinss)
plot(1:17, screeplot2, type = "b", xlab="Antal kluster", ylab="Within group SS", xlim=c(1,8))

#Snyggare Scree plot
fviz_nbclust(df2, kmeans, method = "wss")


#Dendrogram COMPLETE
dend2 <- hclust(distance2)
plot(dend2)
fviz_dend(x=dend2, cex=0.7, lwd=0.7)
labels(dend2) <- c("7","5","17","14","25","19","20","3","4","24","13","6","8","18","12","21","10","22")
fviz_dend(x=dend2, cex=0.8, lwd=0.1, k=3, k_colors = c("gray75","gray52","gray32"), rect=T, rect_border = c("gray85","gray52","gray42"), rect_fill = T, main="Faktisk kostnad med förväntad kostnad, medelvärden", xlab = "Län", ylab="Avstånd")

#Dendrogram AVERAGE
dend2.average <- hclust(distance2, method = "average")
plot(dend2.average)
fviz_dend(x=dend2.average, cex=0.7, lwd=0.7)

#Cluster membership
Cluster.Complete2 <- cutree(dend2,3)
Cluster.Average2 <- cutree(dend2.average,3)

#Table of cluster membership
table(Cluster.Complete2, Cluster.Average2)

#Cluster means
aggregate(df2, list(Cluster.Complete2), mean) 
aggregate(df2, list(Cluster.Average2), mean)


### K-means clustering
kc2 <- kmeans(df2,3)
kc2
plot(Medelkostnad~Medeldrgikr, df2, col=kc2$cluster)

#K-means snyggare graf
df2 %>%
  as_tibble() %>%
  mutate(cluster = kc2$cluster,
         state = row.names(df)) %>%
  ggplot(aes(Medelkostnad, Medeldrgikr, color = factor(cluster), label = df$lannr)) +
  geom_text()