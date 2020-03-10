# import data into a variable called 'dat'
dat <- read.csv("data/week_04_pre_data.csv")

dat

# Extract column named 'RevengeOfNumbers'
dat$RevengeOfNumbers

# extract column 3:
dat[ , 3]
dat[3 , 3]


# this fails.  Why?
mean(dat$RevengeOfNumbers)

# This works.  Why?
mean(dat$RevengeOfNumbers, na.rm = TRUE)

# You can check out the help file to find the answer:
?mean
# Also, Google is a great resource for R questions when the help entries are hard to understand.







dat2 = read.csv("data/cedar_creek_survey.csv")
head(dat2)
nrow(dat2)
summary(dat2)

# we want to know the mean number of distinct species
mean(dat2$Distinct.Species.Count)
mean(dat2$Distinct.Species.Count, na.rm = TRUE)

max(dat2$Distinct.Species.Count, na.rm = TRUE)
min(dat2$Distinct.Species.Count, na.rm = TRUE)









