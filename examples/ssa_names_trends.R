# uncomment if these packages are not installed
# install.packages('tidyverse')

# analyzes naming trends in the social security names database (national).
# takes interest in frequency of hilary/hillary before and after clinton elections.

# load packages
library(tidyverse)

# clear environment
rm(list = ls())

# read in names dataset
ssa <- read_csv("https://github.com/tomvogl/econ121/raw/main/data/ssa_names.csv")

# summarize
summary(ssa)

# to understand data structure, look at first 10 rows
slice(ssa, 1:10)

# could have equivalently used the pipe operator
ssa |> slice(1:10)

# what are the most common names today?
# create a table of the top-ten boys and girls names in 2022
ssa |> 
  filter(year==2022) |>              # only look at 2022 names
  group_by(sex) |>                   # look separately within M and F
  slice_max(frequency, n=10)          # top 10 observations by sex

# what were the most common names in 1940?
# create a table of the top-ten boys and girls names in 1940
ssa |> 
  filter(year==1940) |>              
  group_by(sex) |>                  
  slice_max(frequency, n=10)       

# how has the number of babies changed over time?
# create a table of the total number of babies by year
# and then store the table under the name annual_totals
annual_totals_table <-                # assign to annual_totals name
  ssa |>                             
    group_by(year) |>                # group by year
    summarise(total = sum(frequency)) # generate total number of babies in each year

# let's have a look at the table
annual_totals_table

# too many rows to be interpretable!
# let's do it as a line graph using ggplot()
ggplot(annual_totals_table, aes(x = year, y=total)) +    # use annual_totals data frame, define x and y axes
  geom_line()                                      # line plot

# let's look at the girl names Hilary and Hillary
# how did their frequency change because of Hillary Clinton?
# let's graph counts of both names with vertical lines at 
# the election years involving Bill or Hilary Clinton,
# 1992, 2008, and 2016.

hillary_table <-  # create table of Hilary/Hillary names by year
  ssa |>
  filter(sex=="F" & (name=="Hilary" | name=="Hillary"))

hillary_graph <-                                                # store clinton_graph
  ggplot(hillary_table, aes(x = year, y = frequency, color = name)) +  # `color` draws separate line plots for each name
      geom_line() +                                             # line plot
      geom_vline(xintercept=c(1992, 2008, 2016))                # add vertical lines

# draw the graph
hillary_graph

# the drops in the 2000s are hard to assess because they are small relative
# to the drop in the 1990s.
# let's draw the plot with a log scale on the y axis.
hillary_graph +
  scale_y_continuous(trans = "log10")

# let's also add points to the line graph to make the time increment clearer
hillary_graph +
  geom_point() +
  scale_y_continuous(trans = "log10")

