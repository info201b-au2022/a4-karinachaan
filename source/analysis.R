library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function gets the jail population for year
get_year_jail_pop <- function() {
  df <- get_data()
  year_df <- aggregate(x=df$total_pop, by = list(year=df$year), FUN=sum)
  return(year_df)
}

# This function plots a bar graph for jail pop vs. year 
plot_jail_pop_for_us <- function() {
  # get df for US data 
  year_df <- get_year_jail_pop()
  p <- ggplot(data=year_df, mapping=aes(x=year, y=x)) + geom_bar(stat="identity")
  
  return(p)
}
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


