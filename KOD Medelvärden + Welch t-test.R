##########################
# Medelvärdesdata
#########################
medeldata<- data4[,c("lannr","Vardkostnad","varddagar","kostnadsskillnad","drgikr")]

medeldata.03 <- subset(medeldata, lannr == "03",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.04 <- subset(medeldata, lannr == "04",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.05 <- subset(medeldata, lannr == "05",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.06 <- subset(medeldata, lannr == "06",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.07 <- subset(medeldata, lannr == "07",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.08 <- subset(medeldata, lannr == "08",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.10 <- subset(medeldata, lannr == "10",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.12 <- subset(medeldata, lannr == "12",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.13 <- subset(medeldata, lannr == "13",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.14 <- subset(medeldata, lannr == "14",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.17 <- subset(medeldata, lannr == "17",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.18 <- subset(medeldata, lannr == "18",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.19 <- subset(medeldata, lannr == "19",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.20 <- subset(medeldata, lannr == "20",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.21 <- subset(medeldata, lannr == "21",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.22 <- subset(medeldata, lannr == "22",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.24 <- subset(medeldata, lannr == "24",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))
medeldata.25 <- subset(medeldata, lannr == "25",
                       select=c(lannr, Vardkostnad, varddagar, kostnadsskillnad, drgikr ))

### Medelvärden vårdkostnad
mean03 <- mean(medeldata.03$Vardkostnad)
mean04 <- mean(medeldata.04$Vardkostnad)
mean05 <- mean(medeldata.05$Vardkostnad)
mean06 <- mean(medeldata.06$Vardkostnad)
mean07 <- mean(medeldata.07$Vardkostnad)
mean08 <- mean(medeldata.08$Vardkostnad)
mean10 <- mean(medeldata.10$Vardkostnad)
mean12 <- mean(medeldata.12$Vardkostnad)
mean13 <- mean(medeldata.13$Vardkostnad)
mean14 <- mean(medeldata.14$Vardkostnad)
mean17 <- mean(medeldata.17$Vardkostnad)
mean18 <- mean(medeldata.18$Vardkostnad)
mean19 <- mean(medeldata.19$Vardkostnad)
mean20 <- mean(medeldata.20$Vardkostnad)
mean21 <- mean(medeldata.21$Vardkostnad)
mean22 <- mean(medeldata.22$Vardkostnad)
mean24 <- mean(medeldata.24$Vardkostnad)
mean25 <- mean(medeldata.25$Vardkostnad)

medelvärden.kostnad <- cbind(mean03, mean04, mean05, mean06, mean07, mean08, mean10, mean12, mean13, mean14, mean17, mean18, mean19, mean20, mean21, mean22, mean24, mean25)
medelkostnad <- t(medelvärden.kostnad)
colnames(medelkostnad) <- "Medelkostnad"
View(medelkostnad)

### Medelvärden vårddagar
mean03.dag <- mean(medeldata.03$varddagar)
mean04.dag <- mean(medeldata.04$varddagar)
mean05.dag <- mean(medeldata.05$varddagar)
mean06.dag <- mean(medeldata.06$varddagar)
mean07.dag <- mean(medeldata.07$varddagar)
mean08.dag <- mean(medeldata.08$varddagar)
mean10.dag <- mean(medeldata.10$varddagar)
mean12.dag <- mean(medeldata.12$varddagar)
mean13.dag <- mean(medeldata.13$varddagar)
mean14.dag <- mean(medeldata.14$varddagar)
mean17.dag <- mean(medeldata.17$varddagar)
mean18.dag <- mean(medeldata.18$varddagar)
mean19.dag <- mean(medeldata.19$varddagar)
mean20.dag <- mean(medeldata.20$varddagar)
mean21.dag <- mean(medeldata.21$varddagar)
mean22.dag <- mean(medeldata.22$varddagar)
mean24.dag <- mean(medeldata.24$varddagar)
mean25.dag <- mean(medeldata.25$varddagar)

medelvärden.dagar <- cbind(mean03.dag, mean04.dag, mean05.dag, mean06.dag, mean07.dag, mean08.dag, mean10.dag, mean12.dag, mean13.dag, mean14.dag, mean17.dag, mean18.dag, mean19.dag, mean20.dag, mean21.dag, mean22.dag, mean24.dag, mean25.dag)
medeldagar <- t(medelvärden.dagar)
colnames(medeldagar) <- "Medeldagar"
View(medeldagar)

### Medelvärden skillnad
mean03.skillnad <- mean(medeldata.03$kostnadsskillnad)
mean04.skillnad <- mean(medeldata.04$kostnadsskillnad)
mean05.skillnad <- mean(medeldata.05$kostnadsskillnad)
mean06.skillnad <- mean(medeldata.06$kostnadsskillnad)
mean07.skillnad <- mean(medeldata.07$kostnadsskillnad)
mean08.skillnad <- mean(medeldata.08$kostnadsskillnad)
mean10.skillnad <- mean(medeldata.10$kostnadsskillnad)
mean12.skillnad <- mean(medeldata.12$kostnadsskillnad)
mean13.skillnad <- mean(medeldata.13$kostnadsskillnad)
mean14.skillnad <- mean(medeldata.14$kostnadsskillnad)
mean17.skillnad <- mean(medeldata.17$kostnadsskillnad)
mean18.skillnad <- mean(medeldata.18$kostnadsskillnad)
mean19.skillnad <- mean(medeldata.19$kostnadsskillnad)
mean20.skillnad <- mean(medeldata.20$kostnadsskillnad)
mean21.skillnad <- mean(medeldata.21$kostnadsskillnad)
mean22.skillnad <- mean(medeldata.22$kostnadsskillnad)
mean24.skillnad <- mean(medeldata.24$kostnadsskillnad)
mean25.skillnad <- mean(medeldata.25$kostnadsskillnad)

medelvärden.skillnad <- cbind(mean03.skillnad, mean04.skillnad, mean05.skillnad, mean06.skillnad, mean07.skillnad, mean08.skillnad, mean10.skillnad, mean12.skillnad, mean13.skillnad, mean14.skillnad, mean17.skillnad, mean18.skillnad, mean19.skillnad, mean20.skillnad, mean21.skillnad, mean22.skillnad, mean24.skillnad, mean25.skillnad)
medelskillnad <- t(medelvärden.skillnad)
colnames(medelskillnad) <- "Medelskillnad"
View(medelskillnad)

### Medelvärden skillnad
mean03.drgikr <- mean(medeldata.03$drgikr)
mean04.drgikr <- mean(medeldata.04$drgikr)
mean05.drgikr <- mean(medeldata.05$drgikr)
mean06.drgikr <- mean(medeldata.06$drgikr)
mean07.drgikr <- mean(medeldata.07$drgikr)
mean08.drgikr <- mean(medeldata.08$drgikr)
mean10.drgikr <- mean(medeldata.10$drgikr)
mean12.drgikr <- mean(medeldata.12$drgikr)
mean13.drgikr <- mean(medeldata.13$drgikr)
mean14.drgikr <- mean(medeldata.14$drgikr)
mean17.drgikr <- mean(medeldata.17$drgikr)
mean18.drgikr <- mean(medeldata.18$drgikr)
mean19.drgikr <- mean(medeldata.19$drgikr)
mean20.drgikr <- mean(medeldata.20$drgikr)
mean21.drgikr <- mean(medeldata.21$drgikr)
mean22.drgikr <- mean(medeldata.22$drgikr)
mean24.drgikr <- mean(medeldata.24$drgikr)
mean25.drgikr <- mean(medeldata.25$drgikr)

medelvärden.drgikr <- cbind(mean03.drgikr, mean04.drgikr, mean05.drgikr, mean06.drgikr, mean07.drgikr, mean08.drgikr, mean10.drgikr, mean12.drgikr, mean13.drgikr, mean14.drgikr, mean17.drgikr, mean18.drgikr, mean19.drgikr, mean20.drgikr, mean21.drgikr, mean22.drgikr, mean24.drgikr, mean25.drgikr)
medeldrgikr <- t(medelvärden.drgikr)
colnames(medeldrgikr) <- "Medeldrgikr"
View(medeldrgikr)


t.test.03 <- t.test(medeldata.03$Vardkostnad, medeldata.03$drgikr)
t.test.04 <- t.test(medeldata.04$Vardkostnad, medeldata.04$drgikr)
t.test.05 <- t.test(medeldata.05$Vardkostnad, medeldata.05$drgikr)
t.test.06 <- t.test(medeldata.06$Vardkostnad, medeldata.06$drgikr)
t.test.07 <- t.test(medeldata.07$Vardkostnad, medeldata.07$drgikr)
t.test.08 <- t.test(medeldata.08$Vardkostnad, medeldata.08$drgikr)
t.test.10 <- t.test(medeldata.10$Vardkostnad, medeldata.10$drgikr)
t.test.12 <- t.test(medeldata.12$Vardkostnad, medeldata.12$drgikr)
t.test.13 <- t.test(medeldata.13$Vardkostnad, medeldata.13$drgikr)
t.test.14 <- t.test(medeldata.14$Vardkostnad, medeldata.14$drgikr)
t.test.17 <- t.test(medeldata.17$Vardkostnad, medeldata.17$drgikr)
t.test.18 <- t.test(medeldata.18$Vardkostnad, medeldata.18$drgikr)
t.test.19 <- t.test(medeldata.19$Vardkostnad, medeldata.19$drgikr)
t.test.20 <- t.test(medeldata.20$Vardkostnad, medeldata.20$drgikr)
t.test.21 <- t.test(medeldata.21$Vardkostnad, medeldata.21$drgikr)
t.test.22 <- t.test(medeldata.22$Vardkostnad, medeldata.22$drgikr)
t.test.24 <- t.test(medeldata.24$Vardkostnad, medeldata.24$drgikr)
t.test.25 <- t.test(medeldata.25$Vardkostnad, medeldata.25$drgikr)

t.test.03
t.test.04
t.test.05
t.test.06
t.test.07
t.test.08
t.test.10
t.test.12
t.test.13
t.test.14
t.test.17
t.test.18
t.test.19
t.test.20
t.test.21
t.test.22
t.test.24
t.test.25