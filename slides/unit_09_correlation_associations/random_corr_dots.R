## random dots for correlation
## MGM 20191025

close <- matrix(c(1.1,1.0,1.9,3.5,5.4,4.8,9.1,8.9,4.5,4.4), ncol = 2, nrow = 5, byrow = TRUE)
close <- as.data.frame(close); colnames(close) <- c("Explanatory", "Response")
lm_close <- lm(Response ~ Explanatory, data = close); coef_lmclose <- lm_close$coefficients
pred_df <- data.frame(Response = predict(lm_close, close), Explanatory = close$Explanatory)
close_p <- ggplot(data = close, aes(Explanatory, Response)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE, fullrange = TRUE) +
  xlim(0,10.5) + ylim(0,10.5) +
  theme_classic(); close_p


far <- matrix(c(1.1,0.5,1.9,4.8,5.4,1.5,9.1,9.2,4.5,9.5), ncol = 2, nrow = 5, byrow = TRUE)
far <- as.data.frame(far); colnames(far) <- c("Explanatory", "Response")
far_p <- ggplot(data = far, aes(Explanatory, Response)) +
  geom_point() + 
  geom_line(color = "blue", data = pred_df, size = 1) +
  xlim(0,10.5) + ylim(0,10.5) +
  theme_classic(); far_p

### correlation
close.cor <- cor.test(close$Explanatory, close$Response, method = "pearson")

far.cor <- cor.test(far$Explanatory, far$Response, method = "pearson")

### additional plots
twoexplan <- close[,c(2,1)]; colnames(twoexplan)[2] <- "Explanatory1"
twoexplan$Response <- twoexplan$Response/2
sam_size <- seq(-3, 3, 0.1)
for(n in 1:length(twoexplan$Explanatory2)){
  twoexplan$Explanatory2[n] <- twoexplan$Response[n]+sample(sam_size, 1)
  twoexplan$Explanatory2[n] <- ifelse(n%%2, twoexplan$Explanatory2[n], twoexplan$Explanatory2[n]*2)
}

expl1_r_p <- ggplot(data = twoexplan, aes(Explanatory1, Response)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE, fullrange = TRUE) +
  xlim(0,10) + ylim(0,5) +
  theme_classic(); expl1_r_p

expl2_r_p <- ggplot(data = twoexplan, aes(Explanatory2, Response)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE, fullrange = TRUE, color = "red") +
  xlim(0,10) + ylim(0,5) +
  theme_classic(); expl2_r_p

expl1_expl2_p <- ggplot(data = twoexplan, aes(Explanatory2, Explanatory1)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE, fullrange = TRUE, color = "purple") +
  xlim(0,10) + ylim(0,10) +
  theme_classic(); expl1_expl2_p

ex.lm <- 