require(here)
require(data.table)
source(here("data", "environment_vars.R"))

# load data ----
{
  classData <- read.csv(here(chris_dat_dir, "classData16.csv"), h = T)
  mander <- read.csv(here(chris_dat_dir, "mander.csv"), h = T)
  bp <- read.table(here(chris_dat_dir, "bearpoop.txt"), h = T)
  
  x1 <- rnorm(100000,  50, 10)
  x2 <- rnorm(100000, 150, 10)
  x3 <- rnorm(100000,  52, 10)
  x4 <- rnorm(100000,  60, 10)
}


# Means plots ----
{
  means_plot = function(x1, x2)
  {
    par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(2, 2, 1, 1))
    plot(
      density(x1), col = 1, lwd = 1, 
      xlim = c(0, 200), ylim = c(0, 0.05), 
      main = "", xlab = "", ylab = "", 
      axes = F)
    lines(density(x1), col = 1, lwd = 1); lines(density(x2), col = 1, lwd = 1)
    
    abline(v = c(mean(x1), mean(x2)), lwd = 2, col = c(2, 4))
    axis(1); box(bty = "l")
    
    boxplot(
      cbind(x1, x2), 
      ylim = c(0, 200), 
      horizontal = TRUE, 
      pch = "", 
      col = adjustcolor(c(2, 4), 0.5), 
      axes = F)
    
    abline(v = c(mean(x1), mean(x2)), lwd = 2, col = c(2, 4))
    axis(1); box(bty = "l")
  }
  
  save_mean_plot = function(x1, x2, filename)
  {
    pdf(here(slide_img_dir, filename), height = 4, width = 5)
    means_plot(x1, x2)
    dev.off()
  }
  
  save_mean_plot(x1, x2, "means1.pdf")
  save_mean_plot(x1, x3, "means2.pdf")
  save_mean_plot(x1, x4, "means3.pdf")
  means_plot(x1, x3)
  means_plot(x1, x4)
}


# Wyola/Quabbin example plots ----
{
  {
    # n_samples_wyola = 32
    # n_samples_quabbin = 120
    # mean_wyola = 50
    # mean_quabbin = 42
    # sd_wyola = 5
    # sd_quabbin = 5.7
    
    sim_fish = function(mean_wyola, mean_quabbin, sd_wyola, sd_quabbin, n_samples_wyola, n_samples_quabbin)
    {
      data.table(
        lake = c(rep("Wyola", n_samples_wyola), rep("Quabbin", n_samples_quabbin)), 
        mass = c(rnorm(n_samples_wyola, mean = mean_wyola, sd = sd_wyola), quabbin = rnorm(n_samples_quabbin, mean = mean_quabbin, sd = sd_quabbin))
      )
    }
    dat = sim_fish(mean_wyola, mean_quabbin, sd_wyola, sd_quabbin, n_samples_wyola, n_samples_quabbin)
  }
  
  # Differences: wyola and quabbin ----
  {
    plot_dif = function(dat, main = "Lake Trout Mass", xlim = NA)
    {
      col_wy = 2; col_qu = 4
      par(mfrow = c(2, 1), oma = c(0, 0, 2.2, 0), mar = c(3, 2, 1, 1))
      
      d1 = dat[lake == "Wyola"]
      d2 = dat[lake == "Quabbin"]
      
      x1 = d1$mass; x2 = d2$mass
      dens1 = density(x1); dens2 = density(x2)
      
      if(is.na(xlim)) xlim = c(min(c(x1, x2)), max(c(x1, x2)))
      ylim = c(min(c(dens1$y, dens2$y)), 1.35 * max(c(dens1$y, dens2$y)))
      
      plot(density(x1), 
           col = col_wy, lwd = 1, 
           xlim = xlim, ylim = ylim, axes = F, ann = T, 
           main = "", xlab = "", ylab = "", type = "n")
      
      mtext(main, cex = 1.8, line = 1)
      mtext(side = 1, text = "grams", line = 2)
      mtext(side = 2, text = "density", line = 0.7)
      legend(x = "topleft", legend = c(paste0("Wyola (n = ", length(x1), ")"), paste0("Quabbin (n = ", length(x2), ")")), 
             lwd = 3.4, col = c(col_wy, col_qu), lty = 1, bty = "n")
      
      abline(v = c(mean(x1), mean(x2)), lwd = 2, col = c(col_wy, col_qu))
      # abline(v = mean(x2), lwd = 2, col = col_qu)
      
      lines(dens1, col = col_wy, lwd = 1)
      lines(dens2, col = col_qu, lwd = 1)
      
      axis(1); box(bty = "l")
      
      par(oma = c(0, 0, 2.2, 0), mar = c(2, 2, 1, 1), new = T)
      boxplot(
        x1, x2, ylim = xlim, 
        horizontal = TRUE, pch = "", axes = F,
        col = adjustcolor(c(col_wy, col_qu), 0.3))
      axis(1); box(bty = "l")
      abline(v = c(mean(x1), mean(x2)), lwd = 2, col = c(2, 4))
      # abline(v = mean(x2), lwd = 2, col = 4)
    }
    
    plot_dif(dat)
    
    plot_dif(sim_fish(mean_wyola, mean_quabbin, sd_wyola, sd_quabbin, n_samples_wyola, n_samples_quabbin))
    
    pdf(here(slide_img_dir, "quab_wyola_1.pdf"), height = 4.8, width = 8)  
    plot_dif(sim_fish(34, 34.2, 3, 3.1, 45, 61))
    dev.off()
    pdf(here(slide_img_dir, "quab_wyola_2.pdf"), height = 4.8, width = 8)  
    plot_dif(sim_fish(34, 37.2, 3.7, 3.1, 45, 61))
    dev.off()
    pdf(here(slide_img_dir, "quab_wyola_3.pdf"), height = 4.8, width = 8)  
    plot_dif(sim_fish(36.9, 37.2, 0.7, 0.5, 145, 161), xlim = c(35, 39))
    dev.off()
    pdf(here(slide_img_dir, "quab_wyola_4.pdf"), height = 4.8, width = 8)  
    plot_dif(sim_fish(36.9, 37.2, 0.11, 0.19, 113, 91), xlim = c(35, 39))
    dev.off()
    
  }
}


# t-tests  ----
{
  tt <- seq(-3, 3, 0.1)
  
  pdf(here(slide_img_dir, "t5.pdf"), height = 4, width = 5)
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
  plot(tt, dt(tt, df = 1), type = "l", lwd = 2, col = 4, xlab = "", ylim = c(0, 0.4), ylab = "Probability", main = "Degrees of Freedom = 1", las = 1, bty = "l")
  abline(h = 0.05, lty = 2, col = 2)
  dev.off()
  
  pdf(here(slide_img_dir, "t25.pdf"), height = 4, width = 5)
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
  plot(tt, dt(tt, df = 1), type = "l", col = 1, lwd = 1, xlab = "", ylim = c(0, 0.4), ylab = "Probability", main = "Degrees of Freedom = 5", las = 1, bty = "l")
  lines(tt, dt(tt, df = 5), type = "l", lwd = 2, col = 4, xlab = "", ylab = "")
  abline(h = 0.05, lty = 2, col = 2)
  dev.off()
  
  pdf(here(slide_img_dir, "t50.pdf"), height = 4, width = 5)
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
  plot(tt, dt(tt, df = 1), type = "l", col = 1, lwd = 1, xlab = "", ylim = c(0, 0.4), ylab = "Probability", main = "Degrees of Freedom = 500", las = 1, bty = "l")
  lines(tt, dt(tt, df = 5), type = "l", col = 1, lwd = 1, xlab = "", ylab = "")
  lines(tt, dt(tt, df = 500), type = "l", lwd = 2, col = 4, xlab = "", ylab = "")
  abline(h = 0.05, lty = 2, col = 2)
  dev.off()
  
  df <- 1:50
  pdf(here(slide_img_dir, "samplesize.pdf"), height = 4, width = 5)
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
  plot(df, qt(0.975, df = df), type = "l", lwd = 2, xlab = "Degrees-of-Freedom", 
       ylab = "Critical t value", las = 1, bty = "l", ylim = c(0, 13))
  points(df, qt(0.975, df = df), pch = 16, col = 4)
  dev.off()
}