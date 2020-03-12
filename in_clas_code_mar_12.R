# In-class coding examples for March 12, 2020 - NRC290B




sals = read.csv("mander_anova.csv")
file.exists("data/mander_anova.csv")
dir()


head(sals)


# subsetting the salamder data to pull out only the "female" rows
sals
subset(sals, sals$Sex == "female")

# How could I tell how many collectors there are, and their names?
sals$Collector
unique(sals$Collector)


sals$Site



boxplot(sals$SVL ~ sals$Sex)

par(mfrow = c(3, 1))
hist(subset(sals, Sex == "female")$SVL)
hist(subset(sals, Sex == "male")$SVL)
hist(subset(sals, Sex == "unknown")$SVL)


dev.off()








head(sals)

names(sals)

boxplot(sals$SVL)
plot(sals$SVL, sals$Total_length)
fit_svl_tot = lm(sals$SVL ~ sals$Total_length)
summary(fit_svl_tot)
anova(fit_svl_tot)