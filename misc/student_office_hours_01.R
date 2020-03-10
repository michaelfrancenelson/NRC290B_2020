butterfly = read.csv("data/butterfly-table.csv", row.names = 1)
butterfly = read.csv("data/butterfly-table.csv")


as.matrix(butterfly[, -1])


butterfly_mat = as.matrix(butterfly)

class(butterfly)
class(butterfly_mat)

data.frame(butterfly)
barplot(butterfly_mat)
barplot(butterfly_mat, beside = TRUE)
barplot(butterfly_mat, beside = FALSE, names.arg = 1:5)
barplot(
  butterfly_mat, 
  beside = TRUE, 
  names.arg = c(1996:2000),
  ylim = c(0, 300))




butterfly[1, ]