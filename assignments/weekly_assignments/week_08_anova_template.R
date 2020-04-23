# Fill in any missing code in this template.


# sample format
upper  = c(23, 25, 27, 28, 19, 26, 27)
middle = c(32, 37, 31, 28, 26, 29    )
lower  = c(24, 26, 27, 28, 25        )

# recording format
sward_heights = 
  rbind(
    data.frame(site = "upper",  height = upper),
    data.frame(site = "middle", height = middle),
    data.frame(site = "lower",  height = lower)
  )

# view a sample of the recording format:
head(sward_heights)



# Within-group numbers
n_upper  = length(upper)
n_middle =
n_lower  =

# Number of groups (between-group n) 
n_between = 

#Total number of observations:
n_total = sum(n_upper, n_middle, n_lower)



# Degrees of freedom
  
df_upper  = n_upper - 1
df_middle =
df_lower  =
  
df_within = df_upper + df_middle + df_lower


df_total =

# Vector of DFs  
df = c(between = df_between,  within = df_within, total = df_total)




# Means
grand_mean = mean(c(upper, middle, lower)) # C12

mean_upper  = mean(upper)  # B10
mean_middle =
mean_lower  =
  

# Sums of Squares
  
# Within-group SS
ss_upper  = var(upper)  * (n_upper  - 1)
ss_middle =
ss_lower  =  

# all within-group
ss_within  = sum(ss_upper, ss_middle, ss_lower)


# Between-group SS
ss_between_upper  = ((mean_upper  - grand_mean) ^ 2) * n_upper  # B14
ss_between_middle =                                             # C14
ss_between_lower  =                                             # D14
  
ss_between = sum(ss_between_upper, ss_between_middle, ss_between_lower)


all    = c(upper, middle, lower)
ss_total_1 = var(all) * (n_total - 1)
ss_total_2 = (ss_between + ss_within)

# This should return true if your calculations above were correct
ss_total_1 == ss_total_2

ss_total = var(all) * (n_total - 1)


# ANOVA table components
source = c(between = ss_between, within = ss_within, total = ss_total)
ms     = c(source / df)[1:2]
f_val = ms[1] / ms[2]
p_crit = 0.05
f_crit = qf(p_crit, df_between, df_within, lower.tail = FALSE)
p_obs = pf(f_vl, df_between, df_within, lower.tail = FALSE)


# These variables must contain the correct quantities.
# Check your calculations against the html assignment page.
n_upper
n_middle
n_lower

source
ms
f_val
p_crit
f_crit
p_obs


grand_mean
df

