#######################################
# Boxplot av medelvärden med errors
#######################################

Boxplot1 <- data4 %>% 
  group_by(lannr) %>%   
  summarise(mean_PL = mean(Vardkostnad),  
            sd_PL = sd(Vardkostnad), 
            n_PL = n(), 
            SE_PL = sd(Vardkostnad)/sqrt(n()))

Plotsummary <- ggplot(Boxplot1, aes(lannr, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2)

Plotsummary + labs(y="Genomsnittlig vårdkostnad (kr) ± s.d.", x = "Län") + theme_classic()

### Loggade versionen
test <- data4[,c("Vardkostnad","lannr")]
test <- mutate(test, logkostnad = log(Vardkostnad))
test <- test[,c("lannr","logkostnad")]
test$logkostnad[test$logkostnad == -Inf] <- 0	

Boxplot2 <- test %>% 
  group_by(lannr) %>%   
  summarise(mean_PL = mean(logkostnad),  
            sd_PL = sd(logkostnad), 
            n_PL = n(),  
            SE_PL = sd(logkostnad)/sqrt(n())) 

Plotsummary2 <- ggplot(Boxplot2, aes(lannr, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2)

Plotsummary2 + labs(y="Ln av genomsnittlig vårdkostnad (kr) ± s.d.", x = "Län") + theme_classic()

#### Bubbpleplot av snittkostnader
df.medel$Urvalsstorlek <- c("3904", "2639", "3210", "3986", "1905", "2665", "1436", "12314", "2554", "16338", "1749", "3290", "1874", "2529", "2276", "2580", "2860", "2806")
df.medel$Urvalsstorlek <- as.numeric(df.medel$Urvalsstorlek)
df.medel$Antalinvanare <- c("376354", "294695", "461583", "360825", "199886", "244670", "159684", "1362000", "329352", "1710000", "281482", "302252", "273929", "287191", "286547", "245453", "270154", "250497")
df.medel$Antalinvanare <- as.numeric(df.medel$Antalinvanare)

ggplot(df.medel, aes(x=Urvalsstorlek, y=Medelkostnad, size = Antalinvanare, color=Medelkostnad, label = rownames(lannr))) +
  geom_point(alpha=0.3) +
  scale_size(range = c(2, 24), name="Antal invånare per län") +
  geom_hline(yintercept=87488, linetype="dashed", color="red", size=1) +
  geom_text(data=df.medel, aes(Urvalsstorlek, Medelkostnad, label = lannr), colour = I(alpha("Black", 1)), size = 4 );



#### Bubbpleplot av snittskillnader
df.skillnad$Urvalsstorlek <- c("3904", "2639", "3210", "3986", "1905", "2665", "1436", "12314", "2554", "16338", "1749", "3290", "1874", "2529", "2276", "2580", "2860", "2806")
df.skillnad$Urvalsstorlek <- as.numeric(df.skillnad$Urvalsstorlek)
df.skillnad$Antalinvanare <- c("376354", "294695", "461583", "360825", "199886", "244670", "159684", "1362000", "329352", "1710000", "281482", "302252", "273929", "287191", "286547", "245453", "270154", "250497")
df.skillnad$Antalinvanare <- as.numeric(df.medel$Antalinvanare)

ggplot(df.skillnad, aes(x=Urvalsstorlek, y=Medelskillnad, size = Antalinvanare, color=Medelskillnad, label = rownames(lannr))) +
  geom_point(alpha=0.3) +
  scale_size(range = c(2, 24), name="Antal invånare per län (2019)") +
  geom_hline(yintercept=12188, linetype="dashed", color="red", size=1) +
  geom_text(data=df.skillnad, aes(Urvalsstorlek, Medelskillnad, label = lannr), colour = I(alpha("Black", 1)), size = 4 ); 
summary(data4$kostnadsskillnad)

#### Bubbleplot av snittdrgikr
df$Urvalsstorlek <- c("3904", "2639", "3210", "3986", "1905", "2665", "1436", "12314", "2554", "16338", "1749", "3290", "1874", "2529", "2276", "2580", "2860", "2806")
df$Urvalsstorlek <- as.numeric(df.medel$Urvalsstorlek)
df$Antalinvanare <- c("376354", "294695", "461583", "360825", "199886", "244670", "159684", "1362000", "329352", "1710000", "281482", "302252", "273929", "287191", "286547", "245453", "270154", "250497")
df$Antalinvanare <- as.numeric(df.medel$Antalinvanare)

ggplot(df, aes(x=Urvalsstorlek, y=Medeldrgikr, size = Antalinvanare, color="skyblue3", label = rownames(lannr))) +
  geom_point(alpha=0.3) +
  scale_size(range = c(2, 24), name="Antal invånare per län") +
  geom_text(data=df, aes(Urvalsstorlek, Medeldrgikr, label = lannr), colour = I(alpha("Black", 1)), size = 4 );



#### Barplot av Medelvärden
Kostnad<-ggplot(data=df.medel, aes(x=as.factor(lannr), y=Medelkostnad)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_hline(yintercept=87488, linetype="dashed", color="red", size=1) +
  theme_minimal()
Kostnad

Skillnad<-ggplot(data=df, aes(x=as.factor(lannr), y=Medelskillnad)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_hline(yintercept=12188, linetype="dashed", color="red", size=1) +
  theme_minimal()
Skillnad

Dagar<-ggplot(data=df, aes(x=as.factor(lannr), y=Medeldagar)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_hline(yintercept=12.42, linetype="dashed", color="red", size=1) +
  theme_minimal()
Dagar

drgikr<-ggplot(data=df, aes(x=as.factor(lannr), y=Medeldrgikr)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_hline(yintercept=75301, linetype="dashed", color="red", size=1) +
  theme_minimal()
drgikr


######################################################
# Snitt estimerad kostnad vs snitt faktisk kostnad
######################################################

ggplot(data = dfmedel %>% gather(Variable, Medeldrgikr, -lannr), 
       aes(x = as.factor(lannr), y = Medeldrgikr, fill = Variable)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(y= "Kronor", x = "Län") +
  geom_hline(yintercept=73604, linetype="dashed", color="red", size=1) +
  geom_hline(yintercept=87488, linetype="dashed", color="skyblue3", size=1)
facet_grid(~lannr, scales = 'free_x', space = 'free')

summary(data4$Vardkostnad)
