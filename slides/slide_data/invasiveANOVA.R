##### Invasive two-way ANOVA
### MGM 20191017

#### Bring in data ----
inv <- read.csv("C:/Users/megha/Box/Courses/NRC_290b_2019_Fall/08 ANOVA/invasiveANOVA.csv")
colnames(inv) <- c("disturbance", "elevation", "abundance")

#### two-way ANOVAs ----
## additive
twowayANOVA.additive <- aov(abundance ~ disturbance + elevation, data = inv)
summary(twowayANOVA.additive)

## interaction
twowayANOVA.interaction <- aov(abundance ~ disturbance * elevation, data = inv)
summary(twowayANOVA.interaction)
