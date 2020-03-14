{
  require(here)
  require(ggplot2)
  require(grid)
  
  source(here("slides", "sutherland_plot_functions.R"))
  if (!dir.exists(here(tmp_img_dir <- file.path("slides", "tmp_images")))) dir.create(here(tmp_img_dir))
  if (!dir.exists((img_dir <- file.path(tmp_img_dir, "09")))) dir.create(here(img_dir))
}



# Graphical parameters ----
{
  bg_col = adjustcolor(4, 0.4)
  cex = 3
  cex_main = 2
  pch_plots = 21
  
  g_point = geom_point(pch = pch_plots, cex = cex, bg = bg_col)
  aes_1 = aes(x = x, y = y)
  t_1 = theme_minimal() + 
    theme(
      axis.title = element_blank(),
      rect = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = 1, fill = "transparent")
    )
  t_2 = theme_minimal() + 
    theme(
      axis.title = element_blank(),
      rect = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank()
    )
  
  
  grid_row_plot = function(grobs_list)
  {
    vp = viewport(layout = grid.layout(nrow = 1, ncol = length(grobs_list)))
    grid.newpage()
    for (i in 1:length(grobs_list))
    {
      pushViewport(vp)
      pushViewport(viewport(layout.pos.col = i, layout.pos.row = 1))
      grid.draw(grobs_list[[i]])      
      popViewport()
    }
  }
}


if (FALSE) { # Correlation quiz 1 ----
  g1 = ggplotGrob(ggplot(data.frame(x = log(1:20), y = rlnorm(20, seq(0, 0.7, length = 20)*2, 0.1)), aes_1) +
                    g_point + t_1 + ggtitle("A"))
  g2 = ggplotGrob(ggplot(data.frame(x = 1:20, y = rnorm(20, seq(0, 0.7, length = 20)*2, 0.1)), aes_1) +
                    g_point + t_1 + ggtitle("B"))
  g3 = ggplotGrob(ggplot(data.frame(x = 1:20, y = rnorm(20, seq(0, 0.7, length = 20)*2, 0.1)), aes_1) +
                    g_point + t_1 + ggtitle("C"))
  pdf(here(img_dir, "corQuiz1.pdf"), height = 3, width = 8.5)
  grid_row_plot(list(g1, g2, g3))
  dev.off()
}


if (FALSE) { # Means density/box plots ----
  
  x1 <- rnorm(100000, 50, 10)
  x2 <- rnorm(100000, 150, 10)
  x3 <- rnorm(100000, 52, 10)
  x4 <- rnorm(100000, 60, 10)
  
  
  
  pdf(here(img_dir, "meansA.pdf"), height = 4, width = 5)
  chris_means_plot(x1, x2, c(0, 200), c(0, 0.05))
  dev.off()
  
  pdf(here(img_dir, "meansB.pdf"), height = 4, width = 5)
  chris_means_plot(x1, x3, c(0, 110), c(0, 0.05))
  dev.off()
  
  pdf(here(img_dir, "meansC.pdf"), height = 4, width = 5)
  chris_means_plot(x1, x4, c(0, 120), c(0, 0.05))
  dev.off()
  
  
  
  
  # ggplot(data.frame(x = x1), aes(x = x)) + 
  #   geom_density() +
  #   geom_vline(xintercept = mean(x1), lwd = lwd_mean, col = col_1) +
  #   geom_density(data = data.frame(x = x2)) +
  #   geom_vline(xintercept = mean(x2), lwd = lwd_mean, col = col_2) +
  #   t_2
  # ggplot(data.frame(x = x1), aes(y = x)) + geom_boxplot() + t_2  
  
}




if (FALSE) { # Species - area curves -----
  
  noisy_line = function(n, seed, x_min, x_max, slope, y_adj = 10, y_sd = 2)
  {
    set.seed(seed)
    x = runif(n, x_min, x_max)
    y = rnorm(n, y_adj + slope * x, y_sd)
    return(data.frame(x, y))
  }
  
  sr_1 = noisy_line(n = 25, seed = 12, x_min = 0, x_max = 10, y_adj = 10, slope = 2, y_sd = 2)
  sr_2 = noisy_line(n = 25, seed = 1,  x_min = 0, x_max = 10, y_adj = 10, slope = 5, y_sd = 6)
  
  pdf(here(img_dir, "species_area_A.pdf"), height = 3, width = 4)
  ggplot(sr_1, aes_1) + g_point + ylab("Dependent Variable") + xlab("")
  dev.off()
  
  pdf(here(img_dir, "species_area_with.pdf"), height = 3, width = 4)
  ggplot(sr_2, aes_1) + g_point + ylab("Species Richness") + xlab("")
  dev.off()
  
  pdf(here(img_dir, "species_area_without.pdf"), height = 3, width = 4)
  ggplot(sr_2, aes_1) + g_point + ylab("Species Richness") + xlab("Area (ha)")
  dev.off()
  
  pdf(here(img_dir, "species_area_B.pdf"), height = 3, width = 4)
  ggplot(sr_1, aes_1) + g_point + ylab("Dependent Variable") + xlab("Independent Variable")
  dev.off()
  
}

{ # Correlation direction plots
  
  
  noisy_y = function(x, seed, slope, y_adj = 10, y_sd = 2)
  {
    set.seed(seed)
    y = rnorm(length(x), y_adj + slope * x, y_sd)
    return (y)
  }
  
  x <- runif(25, 0, 10)
  data.frame(x, 
             pos = noisy_y(x, seed = 123, y_adj = 10, y_sd = 2, slope = 2),
             none = noisy_y(x, seed = 123, y_adj = 10, y_sd = 2, slope = 0),
             neg = noisy_y(x, seed = 123, y_adj = 10, y_sd = 2, slope = -4))
  
  X <- runif(25, 0, 10)
  Pos <- rnorm(25, 10 + 2*X, 2)
  No <- rnorm(25, 10 + 0*X, 2)
  Neg <- rnorm(25, 10 - 2*X, 2)
  
  # pdf(here(img_dir, "signA.pdf"), height = 2, width = 6)
  par(mfrow = c(1, 3), oma = c(0, 0, 0, 0), mar = c(4, 4, 3, 1), mgp = c(1, 1, 1))
  plot(X, Pos, type = "p", bg = adjustcolor(1, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, axes = F)
  box(bty = "l")
  plot(X, No, type = "p", bg = adjustcolor(1, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, axes = F)
  box(bty = "l")
  plot(X, Neg, type = "p", bg = adjustcolor(1, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, axes = F)
  box(bty = "l")
  dev.off()
  
  
  pdf(here(img_dir, "signB.pdf"), height = 2, width = 6)
  par(mfrow = c(1, 3), oma = c(0, 0, 0, 0), mar = c(4, 4, 3, 1), mgp = c(1, 1, 1))
  plot(X, Pos, type = "p", bg = adjustcolor(3, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, main = "Positive Correlation", axes = F)
  box(bty = "l")
  plot(X, No, type = "p", bg = adjustcolor(1, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, main = "No Correlation", axes = F)
  box(bty = "l")
  plot(X, Neg, type = "p", bg = adjustcolor(2, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, main = "Negative Correlation", axes = F)
  box(bty = "l")
  dev.off()
  
  
  
    
}




# 
# pdf(here(img_dir, "corQuiz1.pdf"), height = 3, width = 8.5)
# par(mfrow = c(1, 3), mar = c(2, 2, 4, 1), oma = c(0, 0, 0, 0))
# set.seed(3)
# plot(log(1:20), 
#      rlnorm(20, seq(0, 0.7, length = 20)*2, 0.1), 
#      main = "A", axes = F, 
#      xlab = "", ylab = "", 
#      pch = 21,
#      bg = adjustcolor(4, 0.4), 
#      cex = 2, cex.main = 2)
# box(bty = "l")
# 
# plot(1:20, 
#      rnorm(20, seq(0, 0.7, length = 20)*2, 0.1), 
#      main = "B", axes = F, 
#      xlab = "", ylab = "", 
#      pch = 21, 
#      bg = adjustcolor(4, 0.4), 
#      cex = 2, cex.main = 2)
# 
# 
# box(bty = "l")
# plot(
#   1:20, 
#   rnorm(20, seq(0, 0.7, length = 20)*3 - seq(0, 0.7, length = 20)^2*5, 0.1), 
#   main = "C", axes = F, xlab = "", ylab = "", 
#   pch = 21, cex = 2, cex.main = 2,
#   bg = adjustcolor(4, 0.4))
# box(bty = "l")
# dev.off()
# 
# 
# x1 <- rnorm(100000, 50, 10)
# x2 <- rnorm(100000, 150, 10)
# x3 <- rnorm(100000, 52, 10)
# x4 <- rnorm(100000, 60, 10)
# 
# pdf(here(img_dir, "meansA.pdf"), height = 4, width = 5)
# par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(2, 2, 1, 1))
# plot(density(x1), col = 1, lwd = 1, xlim = c(0, 200), ylim = c(0, 0.05), 
#      main = "", axes = F, xlab = "", ylab = "")
# abline(v = mean(x1), lwd = 2, col = 2)
# abline(v = mean(x2), lwd = 2, col = 4)
# lines(density(x1), col = 1, lwd = 1)
# lines(density(x2), col = 1, lwd = 1)
# axis(1)
# box(bty = "l")
# boxplot(cbind(x1, x2), ylim = c(0, 200), horizontal = TRUE, pch = "", 
#         col = adjustcolor(c(2, 4), 0.3), axes = F)
# axis(1)
# abline(v = mean(x1), lwd = 2, col = 2)
# abline(v = mean(x2), lwd = 2, col = 4)
# boxplot(cbind(x1, x2), ylim = c(0, 200), horizontal = TRUE, pch = "", 
#         col = adjustcolor(c(2, 4), 0.3), axes = F, add = T)
# box(bty = "l")
# dev.off()
# 
# pdf(here(img_dir, "means2.pdf"), height = 4, width = 5)
# par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(2, 2, 1, 1))
# plot(density(x1), col = 1, lwd = 1, xlim = c(0, 200), ylim = c(0, 0.05), 
#      main = "", axes = F, xlab = "", ylab = "")
# abline(v = mean(x1), lwd = 2, col = 2)
# abline(v = mean(x3), lwd = 2, col = 4)
# lines(density(x1), col = 1, lwd = 1)
# lines(density(x3), col = 1, lwd = 1)
# axis(1)
# box(bty = "l")
# boxplot(cbind(x1, x3), ylim = c(0, 200), horizontal = TRUE, pch = "", 
#         col = adjustcolor(c(2, 4), 0.3), axes = F)
# axis(1)
# abline(v = mean(x1), lwd = 2, col = 2)
# abline(v = mean(x3), lwd = 2, col = 4)
# boxplot(cbind(x1, x3), ylim = c(0, 200), horizontal = TRUE, pch = "", 
#         col = adjustcolor(c(2, 4), 0.3), axes = F, add = T)
# box(bty = "l")
# dev.off()
# 
# pdf(here(img_dir, "means3.pdf"), height = 4, width = 5)
# par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(2, 2, 1, 1))
# plot(density(x1), col = 1, lwd = 1, xlim = c(0, 200), ylim = c(0, 0.05), 
#      main = "", axes = F, xlab = "", ylab = "")
# abline(v = mean(x1), lwd = 2, col = 2)
# abline(v = mean(x4), lwd = 2, col = 4)
# lines(density(x1), col = 1, lwd = 1)
# lines(density(x4), col = 1, lwd = 1)
# axis(1)
# box(bty = "l")
# boxplot(cbind(x1, x4), ylim = c(0, 200), horizontal = TRUE, pch = "", 
#         col = adjustcolor(c(2, 4), 0.3), axes = F)
# axis(1)
# abline(v = mean(x1), lwd = 2, col = 2)
# abline(v = mean(x4), lwd = 2, col = 4)
# boxplot(cbind(x1, x4), ylim = c(0, 200), horizontal = TRUE, pch = "", 
#         col = adjustcolor(c(2, 4), 0.3), axes = F, add = T)
# box(bty = "l")
# dev.off()
# 
# 
# 
# 
# set.seed(12)
# AR <- runif(25, 0, 10)
# SR <- rnorm(25, 10 + 2*AR, 2)
# pdf(here(img_dir, "specareaA.pdf"), height = 3, width = 4)
# par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1), mgp = c(1, 1, 1))
# plot(AR, SR, type = "p", bg = adjustcolor(4, 0.4), xlab = "", ylab = "Dependent variable", ylim = c(5, 30), las = 1, bty = "l", pch = 21, cex = 2, axes = F)
# box(bty = "l")
# dev.off()
# 
# set.seed(1)
# x1 <- runif(25, 0, 10)
# y1 <- rnorm(25, 10 + 5*x1, 6)
# pdf(here(img_dir, "specareaWith.pdf"), height = 3, width = 4)
# par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
# plot(x1, y1, type = "p", bg = adjustcolor(4, 0.4), ylab = "Species Richness", xlab = "Area (ha)", las = 1, bty = "l", pch = 21, cex = 2, axes = F, ylim = c(0, 70))
# box(bty = "l")
# axis(2, las = 1)
# axis(1, at = 1:10)
# dev.off()
# 
# pdf(here(img_dir, "specareaWithout.pdf"), height = 3, width = 4)
# par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
# plot(x1, y1, type = "p", col = adjustcolor(4, 0.05), ylab = "Species Richness", xlab = "Area (ha)", las = 1, bty = "l", pch = 16, cex = 2, axes = F, ylim = c(0, 70))
# box(bty = "l")
# axis(2, las = 1)
# axis(1, at = 1:10)
# dev.off()
# 
# pdf(here(img_dir, "specareaB.pdf"), height = 3, width = 4)
# par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1), mgp = c(1, 1, 1))
# plot(AR, SR, type = "p", bg = adjustcolor(4, 0.4), xlab = "Independent variable", ylab = "Dependent variable", ylim = c(5, 30), las = 1, bty = "l", pch = 21, cex = 2, axes = F)
# box(bty = "l")
# dev.off()




X <- runif(25, 0, 10)
Pos <- rnorm(25, 10 + 2*X, 2)
No <- rnorm(25, 10 + 0*X, 2)
Neg <- rnorm(25, 10 - 2*X, 2)

# pdf(here(img_dir, "signA.pdf"), height = 2, width = 6)
par(mfrow = c(1, 3), oma = c(0, 0, 0, 0), mar = c(4, 4, 3, 1), mgp = c(1, 1, 1))
plot(X, Pos, type = "p", bg = adjustcolor(1, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, axes = F)
box(bty = "l")
plot(X, No, type = "p", bg = adjustcolor(1, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, axes = F)
box(bty = "l")
plot(X, Neg, type = "p", bg = adjustcolor(1, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, axes = F)
box(bty = "l")
dev.off()


pdf(here(img_dir, "signB.pdf"), height = 2, width = 6)
par(mfrow = c(1, 3), oma = c(0, 0, 0, 0), mar = c(4, 4, 3, 1), mgp = c(1, 1, 1))
plot(X, Pos, type = "p", bg = adjustcolor(3, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, main = "Positive Correlation", axes = F)
box(bty = "l")
plot(X, No, type = "p", bg = adjustcolor(1, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, main = "No Correlation", axes = F)
box(bty = "l")
plot(X, Neg, type = "p", bg = adjustcolor(2, 0.3), xlab = "Independent", ylab = "Dependent", las = 1, bty = "l", pch = 21, cex = 1.8, main = "Negative Correlation", axes = F)
box(bty = "l")
dev.off()


mayfly <- data.frame(Speed = c(2, 3, 5, 9, 14, 24, 29, 34), 
                     Abundance = c(6, 3, 5, 23, 16, 12, 48, 43))
pdf(here(img_dir, "spearMayfly.pdf"), height = 3.5, width = 3.5, bg = "white")
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 3, 1))
with(mayfly, 
     plot(Speed, Abundance, pch = 21, bg = adjustcolor(4, 0.5), cex = 1.5, las = 1, ylim = c(0, 50))
)
dev.off()



head(mayfly)
mayfly$Diff <- mayfly$Abundance.rank - mayfly$Speed.rank
mayfly
mayfly$Diff.sq <- mayfly$Diff^2
mayfly
mayfly
mayfly <- data.frame(Speed = c(2, 3, 5, 9, 14, 24, 29, 34), 
                     Abundance = c(6, 3, 5, 23, 16, 12, 48, 43))
pdf(here(img_dir, "spearMayfly.pdf"), height = 3.5, width = 3.5, bg = "white")
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 3, 1))
with(mayfly, 
     plot(Speed, Abundance, pch = 21, bg = adjustcolor(4, 0.5), cex = 1.5, las = 1, ylim = c(0, 50))
)
dev.off()
mayfly <- mayfly[, 1:2]
mayfly

mayfly <- data.frame(Speed = c(2, 3, 5, 9, 14, 24, 29, 34), 
                     Abundance = c(6, 3, 5, 23, 16, 12, 48, 43))
pdf(here(img_dir, "spearMayfly.pdf"), height = 3.5, width = 3.5, bg = "white")
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 3, 1))
with(mayfly, 
     plot(Speed, Abundance, pch = 21, bg = adjustcolor(4, 0.5), cex = 1.5, las = 1, ylim = c(0, 50))
)
#make the mayfly 'data.frame' by hand:
mayfly <- data.frame(Speed = c(2, 3, 5, 9, 14, 24, 29, 34), 
                     Abundance = c(6, 3, 5, 23, 16, 12, 48, 43))
#make the mayfly 'data.frame' by hand:
mayfly <- data.frame(Speed = c(2, 3, 5, 9, 14, 24, 29, 34), 
                     Abundance = c(6, 3, 5, 23, 16, 12, 48, 43))
mayfly
cor(var1, var2, method = 'spearman')
cor.test(var1, var2, method = 'spearman')
cor(mayfly$Speed, mayfly$Abundance, method = 'spearman')
cor(mayfly$Speed, mayfly$Abundance, method = 'spearman')
cor(var1, var2, method = 'pearson')
cor.test(var1, var2, method = 'pearson')

cor(mayfly$Speed, mayfly$Abundance, method = 'pearson')
cor(mayfly$Speed, mayfly$Abundance, method = 'pearson')
cor(mayfly$Speed, mayfly$Abundance, method = 'pearson')
cor.test(mayfly$Speed, mayfly$Abundance, method = 'pearson')
cor(mayfly$Speed, mayfly$Abundance, method = 'pearson')
cor.test(mayfly$Speed, mayfly$Abundance, method = 'pearson')
lm(mayfly$Abundance ~ mayfly$Speed)
summary(lm(mayfly$Abundance ~ mayfly$Speed))
cor.test(mayfly$Speed, mayfly$Abundance, method = 'pearson')
summary(lm(mayfly$Abundance ~ mayfly$Speed))
cor.test(mayfly$Speed, mayfly$Abundance, method = 'spearman')
summary(lm(mayfly$Abundance ~ mayfly$Speed))
