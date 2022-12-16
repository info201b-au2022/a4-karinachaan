library(tidyverse)
# library(tidyr)
# library(dplyr)
library(reshape2)

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
  df$total_jail_pop <- replace(df$total_jail_pop, is.na(df$total_jail_pop), 0)
  year_df <- aggregate(x=df$total_jail_pop, by = list(year=df$year), FUN=sum)
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
get_jail_pop_by_states <- function(states) {
  # returns a melted df
  df <- get_data()
  df$total_jail_pop <- replace(df$total_jail_pop, is.na(df$total_jail_pop), 0)
  year_df <- aggregate(x=df$total_jail_pop, by=list(year=df$year, state=df$state), FUN=sum)

  years = unique(year_df$year)
  df <- data.frame(matrix(ncol=0, nrow=length(years)))
  df$year = years

  # filter through given states
  for (state in states){
    year_df_statefilter <- year_df[year_df$state==state,]
    colnames(year_df_statefilter)[3] <- state
    df[,state] = year_df_statefilter[,state]
  }

  # melt df for correct formatting for ggplot
  df_long <- melt(df, id="year")

  return(df_long)
}

plot_jail_pop_by_states <- function(states) {
  state_df_trends <- get_jail_pop_by_states(states)
  p <- ggplot(state_df_trends, aes(x=year, y=value, color=variable))
  # make line and add labels
  p <- p + geom_line() + ylab("Population") + xlab("Year")
  return(p)
}
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- cs.
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# plot county population vs white proportion in jail in a certain year
get_jail_prop <- function(target_year, cutoff=1000000) {
  df <- get_data()
  df_year <- df[df$year==target_year,]
  
  # print(sort(unique(df_year$county_name)))
  
  # make empty df 
  df_props <- data.frame(matrix(ncol=0, nrow=nrow(df_year)))
  
  # get white population
  df_props$white_pop_prop = df_year$white_pop_15to64 / df_year$total_pop_15to64 
  # get white proportion in jail 
  df_props$white_jail_prop = df_year$white_jail_pop / df_year$total_jail_pop
  
  df_props <- df_props[df_props$white_pop_prop <= 1,]
  df_props <- df_props[df_props$white_jail_prop <= 1,]
  
  df_props$white_jail_diff = df_props$white_pop_prop - df_props$white_jail_prop
  
  # remove na
  return(df_props %>% na.omit())
}

plot_jail_props <- function(target_year) {
  df = get_jail_prop(target_year)
  p <- ggplot(df, aes(x=white_pop_prop,y=white_jail_diff)) + geom_point() +
    xlab("Proportion of white15t64 in population") + ylab("Proportion of white in pop - Proportion of white in jail")
  
  return(p)
}
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


