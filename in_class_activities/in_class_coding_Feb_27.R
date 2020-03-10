
# I can use read.csv() to load a data.frame object from a file
voles = read.csv("data/vole.trapping.csv")

rarefact = read.csv("data/rarefaction_data_2020.csv")


my_x_label = "number of individuals"




plot(
 y = rarefact$n_individuals, 
 x = rarefact$site, 
  xlab = my_x_label)

data(mtcars)
class(mtcars)


class(voles)


voles = read.csv("vole.trapping.csv")
voles <- read.csv("vole.trapping.csv")



