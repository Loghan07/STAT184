# Downloading the packages we need for this problem

install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
vehicles <- ggplot2::mpg # Full dataset
gems <- ggplot2::diamonds # Full dataset


# Problem 1 Part A --------------------

p_A <- vehicles |>
  filter(displ <= 5) |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~ drv) +
  labs(x = "Engine displacement (litres)",
       y = "Highway fuel economy (mpg)") +
  theme_bw()
p_A

#   WRITTEN ANSWER TO PART A
#   The reason is that color is only mapped in the point layer, so when 
#   geom_smooth is called, it is not grouped by color. The cause of the three 
#   lines, is because facet_wap( ~ drv) splits the data based on the drive
#   type. Then, the smooth function is applied to each different panel, giving
#   the three different lines that we see. 


# Problem 1 part B ----------------------


# Reproduce the same Plot as above using ggplot

# Left Bar Graph
p_stack <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(fill = "factor(cyl)") +
  theme_bw()

p_stack

# Right Bar Graph
p_fill <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion within drive type", fill = "factor(cyl)") +
  theme_bw()

p_fill

# Plotting the propotion as Y axis

cyl_props <- vehicles |>
  count(drv, cyl) |>
  group_by(drv) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()  

cyl_props

p_prop <- ggplot(cyl_props, aes(x = drv, y = proportion, fill = factor(cyl))) + 
  geom_col() + 
  labs(y = "PropoDtion within drive type", fill = "factor(cyl)") + 
  theme_bw()

p_prop

# Testing and finding values

cyl_props
vehicles |> count(drv)
 
# Explanation for B.2

# So each bar represents the drive type of each vehicle, 4, f, and r. And they
# all appear full. This is because the graph is set up to show the proportion 
# make up of each drive type based on the cylinders it has. So. for example, in 
# rear wheel drive, the proportion that has 8 cylinders is upward of 80%, but
# theres only 25 vehicles within R. Where as in 4 wheel drive, there is 103, and 
# a make up of js shy of 50% for 8 cylinders. So if you work out the math, there
# are more 8 cyl in 4 then R. To summarize, they have equal heights because the 
# graph is representing the proportion with in each drive type, its not reliant
# on sample size. However, different sample sizes can cause misunderstandings when
# comparing data, because the proportion can mean different numbers based off how
# large the sample size is. 


# Problem 1 Part C -----------------------------

#creating the smaller table
smaller <-gems |> 
  filter(carat < 3)
n_retained <- nrow(smaller)
n_excluded <- nrow(gems) - nrow(smaller)
c(retained = n_retained, excluded = n_excluded)

# 53900 were retained and 40 were excluded

p_fine <- ggplot(smaller, aes(x = carat)) + 
  geom_histogram(binwidth = .01, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) + 
  labs(x = "Diamond Weight (carats)", y = "Count",
       title = "Bin Width = .01") +
  theme_bw()

p_coarse <- ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) +
  labs(x = "Diamond Weight (carats)", y = "Count",
       title = "Bin Width = 0.5") +
  theme_bw()

p_fine
p_coarse

gems |> filter(carat %in% c(0.99, 1.00)) |> count(carat)

# .99 carat: 23 diamonds and 1.00 carat 1558 diamonds 


# With the narrower bins, it is much easier for someone to see the most common
# carat weights within the each interval. The higher bars represents the carats
# with higher counts, in which we can see how at 1.00 we have 1558 but .99 
# we have a shorter graph, only showing 23 With the wide bins, 
#you can only gather the amount of diamonds within each interval


