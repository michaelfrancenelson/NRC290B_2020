
random_groups = function(names, n_groups, group_name_prefix = "Group ", grouping = "grouping 1", seed = NA)
{
  if (!is.na(seed)) set.seed(seed)
  
  require(data.table)
  suppressWarnings(
    {
      tmp1 = split(sample(names), 1:n_groups)
    }
  )
  
  vvv = vector(mode = "character", length = 0)
  out = data.table(name = vvv, group = vvv, grouping = vvv)
  
  names(tmp1) = paste0(group_name_prefix, 1:n_groups)
  for (i in 1:length(tmp1))
    out =  rbind(data.table(name = as.character(tmp1[[i]]), group = as.character(names(tmp1[i])), grouping = grouping), out)
  
  out = out[order(out$group, out$name), ]
  
  return(out)
}


dat = read.csv("roster_feb_4.csv")
head(dat)
nrow(dat)
names = as.character(sample(dat$FullName))
names_1 = strsplit(names, ",")
names_final = sapply(names_1, function(x) paste(x[2], x[1]))

random_groups(names_final, 5, "Bear Group ", "Bear Groups", seed = 1)

