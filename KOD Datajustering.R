#Ändrar structure						
data$MVO <- factor(data$MVO)						
data$lannr <- factor(data$lannr)						
data$mdc <- factor(data$mdc)						
data$yfall <- factor(data$yfall)						

df$Medelkostnad <- as.numeric(df$Medelkostnad)						
df$Medeldagar <- as.numeric(df$Medeldagar)						
df$Medeldrgikr <- as.numeric(df$Medeldrgikr)						
df$Medelskillnad <- as.numeric(df$Medelskillnad)						


#Tar bort saknad data (Missing values)						
colSums(is.na(data))#Endast 8 NA's i variablen Vardkostnad						
is.na(data)						
sum(is.na(data)) #Enda variabel med NA = Vardkostnad. 8 totalt. Raderar dessa ur dataset						
data <- na.omit(data)						

#Nya kolumner med kostnadsskillnader i ln, diff, Z + outliers						
a <- mutate(data, drgikr = vikt*55042)						
data <- mutate(a, kostnadsskillnad = Vardkostnad - drgikr)						
data <- mutate(data, sqkostnads = kostnadsskillnad*kostnadsskillnad)						
data <- mutate(data, logsqkostnads = log(sqkostnads))						
data <- mutate(data, Zkostnadsskillnad = (kostnadsskillnad - -31906.66)/225009)						
data$outliers <- data$Zkostnadsskillnad > abs(3) #Ny kolumn med outliers						
data <- mutate(data, logvardkostnad = log(Vardkostnad))						
data$logvardkostnad[data$logvardkostnad == -Inf] <- 0						

#Data med endast innerfall						
data1 <- filter(data, yfall == 0)						
data1 <- na.omit(data1)						

#Data med endast innerfall, mdc=19						
data2 <- filter(data, mdc == 19, yfall == 0)						
data2 <- na.omit(data2)						

#Data med både innerfall och ytterfall, mdc=19						
data3 <- filter(data, mdc == 19)						
data3 <- na.omit(data3)						

#HUVUDDATA utan ytterfall och endast MVO 900						
data4 <- filter(data, yfall == 0)						
data4 <- na.omit(data4)					

#Data utan outliers för kostnadsskillnad						
data4.WO.Outliers <- filter(data, yfall == 0, outliers == FALSE)						

#Data till kluster						
cdata1 <- data4[c("kostnadsskillnad", "lannr", "varddagar", "Vardkostnad", "drgikr")]						
summavarddagar <- aggregate(cdata1$varddagar, by=list(lannr=data4$lannr), FUN=sum)						
summavardkostnad <- aggregate(cdata1$Vardkostnad, by=list(lannr=data4$lannr), FUN=sum)						
summakostnadsskillnad <- aggregate(cdata1$kostnadsskillnad, by=list(lannr=data4$lannr), FUN=sum)						
summadrgikr <- aggregate(cdata1$drgikr, by=list(lannr=data4$lannr), FUN=sum)						
cdata1 <- cbind(summavardkostnad, summavarddagar, summakostnadsskillnad, summadrgikr)						
colnames(cdata1) <- c("lannr", "summavardkostnad", "e", "summavarddagar", "er", "summakostnadsskillnad", "et", "summadrgikr")						
cdata1$e <- NULL						
cdata1$er <- NULL						
cdata1$et <- NULL						
cdata1 <- mutate(cdata1, lnsummadrgikr = log(summadrgikr))						
cdata1 <- mutate(cdata1, lnsummavardkostnad = log(summavardkostnad))						
cdata1 <- mutate(cdata1, lnsummavarddagar = log(summavarddagar))						

df1 <- df[c("Medelkostnad", "Medeldagar")]						
df2 <- df[c("Medelkostnad", "Medeldrgikr")]						
df3 <- df[c("Medelskillnad", "Medeldrgikr", "Medelkostnad")]		
df.medel <- df[c("Medelkostnad", "lannr")]
df.skillnad <- df[c("Medelskillnad", "lannr")]
dfsumma <- cdata1[,c("lannr","summavardkostnad","summadrgikr")]
dfmedel <- df[,c("lannr","Medelkostnad","Medeldrgikr")]

#Data till regression
df <- data4
df$outliers <- NULL
df$yfall <- NULL
df$sqkostnads <- NULL
df$Zkostnadsskillnad <- NULL
df$drg <- as.factor(df$drg)
df.WO.Outliers <- filter(data, yfall == 0, outliers == FALSE)

df1 <- mutate(df[, c("Vardkostnad", "kostnadsskillnad", "varddagar", "lannr", "mdc", "MVO")])
df1 <- mutate(df1, lnvarddagar = log(varddagar))
df1 <- mutate(df1, lnVardkostnad = log(Vardkostnad))
df1 <- mutate(df1, y2 = (kostnadsskillnad))
df1$lnVardkostnad[df1$lnVardkostnad == -Inf] <- 0


df1 <- mutate(df[, c("Vardkostnad", "kostnadsskillnad", "varddagar", "lannr", "mdc", "MVO")])
df1 <- mutate(df1, lnvarddagar = log(varddagar))
df1 <- mutate(df1, lnVardkostnad = log(Vardkostnad))
df1 <- mutate(df1, y2 = (kostnadsskillnad))
df1$lnVardkostnad[df1$lnVardkostnad == -Inf] <- 0
df1 <- filter(df1, Vardkostnad > 1)


