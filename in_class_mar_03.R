# I need to tell R that the 'Spp' column contains names.
# I want these row names of the data frame rather than a column of data.
# The 'row.names = "Spp"' is needed to tell r that it should be row names and not data

# on my computer, the data file is in a subdirectory called 'data' so I have to
# tell R to look there using "data/" before the filename.
dat = read.csv("data/butterfly-table.csv", row.names = "Spp")

# The data should be in a data frame:
class(dat)
head(dat)

# I can view the contents of the column called "X1998" using:
dat$X1998

# I can view the third column with:
dat[, 3]

# I can subset the 3rd and 4th columns with:
dat[, 3:4]

# I can get the first, third, and 5th colums with:
dat[, c(1, 3, 5)]


# barplot doesn't work directly:
barplot(dat)

# This works but the legend is not in a useful position:
barplot(as.matrix(dat), legend.text=rownames(dat))

# This separates each species into its own own bar per year.
# The legend is still covering up part of the plot.
barplot(as.matrix(dat), beside = TRUE, legend.text=rownames(dat))

# I can make a bar plot of all species for a single year (1998):
barplot(dat$X1998, names.arg = rownames(dat), main = "butterflies, year 1998")

# I can make a bar plot of a single species for all years:
barplot(as.matrix(dat[1, ]))


# I know I can use the 'legend()' function:
legend(x = "top", rownames(dat), horiz = TRUE, cex = 0.7)

# I could probably tinker with the legend arguments to make something that looks nicer.
# I might look up the following arguments in the help entry:
# col = 
# fill =
# x = 
# legend = 
# bty =
# cex = 
# ncol = 



# I can use the square brackets to select a row or column by its name:
dat["M.bro", ]
dat[1, ]


c(dat[1, ])

plot(dat[1, ])
plot(dat)



# What about making a line plot?
# Here's one row:
plot(as.matrix(dat)[1, ], type = "l", ylim = c(0, 100))

# I can add row 2 using lines()
lines(as.matrix(dat)[2, ])



# I happen to know about a function called 'matplot', short for matrix plot.
# This looks sketchy, but I might be able to work with it:
matplot(as.matrix(dat))

# This is a little better:
matplot(as.matrix(dat), type = "l")


# I can make all of the lines solid, or different varieties of dashed:
matplot(as.matrix(dat), type = "l", lty = 1)
matplot(as.matrix(dat), type = "l", lty = 2)
matplot(as.matrix(dat), type = "l", lty = 3)


# I can specify the color for all lines using a color code, or using a named color:
matplot(as.matrix(dat), type = "l", col = 2)
matplot(as.matrix(dat), type = "l", col = "blue")


# I can also specify a vector of colors.
# Here's an ugly and difficult-to-read set:
matplot(as.matrix(dat), type = "l", col = 6:11)

# Here is a better version using thicker lines (lwd = ).
# My line of code is getting a little long, so I'll separate it into 
# several lines so I can read it more easily:
matplot(
  as.matrix(dat),
  type = "l", 
  lwd = 3,
  col = c(1, 2, 4, "darkgreen", "darkblue", "turquoise"))

# A legend, axis labels, and main titles would be helpful
# Your code here: