require(data.table)
require(ggplot2)

rm(list = ls())


# dat = fread("in_class_activities/quab_master.csv")
dat = fread("in_class_activities/quab.csv")


# take a look at the data:
dat



# Normalize by plot area ----
# Salanamders per hectare
dat[, density_sal := salamander_counts / plot_area]

# pines per hectare
dat[, density_pin := pine_counts / plot_area]

# g1 = ggplot(dat, aes(x = density_pin, y = density_sal, colour = plot_id))
g1 = ggplot(dat, aes(x = density_pin, y = density_sal))

# Simple scatterplot
g1 + geom_point()


# fit a slr ----
fit1 = lm(density_sal ~ density_pin, data = dat)
summary(fit1)


g1 + geom_point() + geom_smooth(method = "lm", se = FALSE)

# With standard errors
g1 + geom_point() + geom_smooth(method = "lm")













# Blue-spotted salamander Ambystoma laterale
# https://www.mass.gov/files/documents/2017/09/29/quabbinnaturalcommunities.pdf
# http://www.conservewildlifenj.org/species/spotlight/bluespotted/

# oak_density  = c(55, 12)
# oak_density  = runif(n_tables, 10, 30)
# beta_oak = -0.03
# data[, plot_area := rpois(n_tables, mean_plot_size)
# data[, oak_counts := rpois(n_tables, oak_density)]
# data[, oak_density := oak_counts / plot_area]                      
# data[, salamander_pop := round(runif(n_tables, 0.9, 1.1) * (beta_oak * oak_density + beta_pine * pine_density) * plot_area)]
# 
# n_tables = 20
# min_plot_size = 3
# max_plot_size = 7
# 
# 
# set.seed(12345)
# 
# pine_density = runif(n_tables, 10, 30)
# beta_pine = 2.5
# 
# quabbin_total_area = 2.9e4
# 
# data = data.table()
# 
# 
# data[, plot_area := round(runif(n_tables, min_plot_size, max_plot_size))]
# data[, pine_counts := rpois(n_tables, pine_density)]
# data[, pine_density := pine_counts / plot_area]                      
# data[, salamander_counts := round(runif(n_tables, 0.4, 1.6) * (beta_pine * pine_density) * plot_area)]
# data[, salamander_dens := salamander_counts / plot_area]
# data[, plot_id := 1:n_tables]
# 
# data[, hist(pine_counts)]
# data[, hist(pine_density)]
# data[, dotchart(pine_counts, xlab = "Number of pines", ylab = "Sample Plot ID", labels = 1:nrow(data))]
# data[, dotchart(pine_density, xlab = "Pines per hectare", ylab = "Sample Plot ID", labels = 1:nrow(data))]
# 
# head(data)
# data2 = data[, .(plot_id, plot_area, salamander_counts, pine_counts, salamander_dens, pine_density)]
# 
# 
# fit1 = data[, lm(salamander_dens ~ pine_density)]
# 
# data[, plot(pine_density, salamander_dens, ylim = c(0, max(salamander_dens)))]
# summary(fit1)
# abline(fit1)





























# fwrite(data2, "in_class_activities/quabbin_salamanders.csv")






# data[, plot(pine_counts, salamander_pop, ylim = c(0, max(salamander_pop)))]
# summary(fit2)
# summary(fit3)
# fit1 = data[, lm(salamander_dens ~ oak_density )]
# fit3 = data[, lm(salamander_dens ~ oak_density + pine_density)]
# data[, plot(oak_counts, salamander_pop, ylim = c(0, max(salamander_pop)))]
# data[, plot(oak_density, salamander_dens, ylim = c(0, max(salamander_dens)))]
# 
# 
