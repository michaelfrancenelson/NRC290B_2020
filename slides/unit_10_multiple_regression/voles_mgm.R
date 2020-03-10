voles <- read.csv(file.choose())

voles.glm <- glm(Voles ~ PercVeg + Dist2Road, family = "gaussian", data = voles)

voles.glm.add <- lm(Voles ~ 1, data = voles)
add1(voles.glm.add, ~ PercVeg + Dist2Road)

plot(voles$Dist2Road, voles$Voles)