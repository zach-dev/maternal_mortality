# load necessary libraries
library(tidyverse)
library(stringr)
library(broom)

# read in and tidy up the maternal mortality data
mm <- read_csv("../data/xmart.csv")
names(mm) <- c("country", "year", "maternal_mortality", "skilled_health_percentage")

# remove spaces in maternal mortality numbers where commas should be
mm$maternal_mortality <- str_replace_all(mm$maternal_mortality, "[\\s]+", "")

# turn the numbers inside square brackets into lower bounds and upper bounds
# first grab the whole mm string when its there and remove the [, -, and ]
tmp <- str_split(mm$maternal_mortality, "[\\[\\-\\]]")

# get rid of all but the first number in the maternal mortality column
mm$maternal_mortality <- str_replace(mm$maternal_mortality, "[\\[][0-9]*[-][0-9]*[\\]]", "")

# create the upper and lower bound lists
mm_lower_bound <- vector(length = length(tmp), mode = "character")
mm_upper_bound <- vector(length = length(tmp), mode = "character")
i = 1
for (ss in tmp) {
  mm_lower_bound[i] = ss[2]
  mm_upper_bound[i] = ss[3]
  i <- i + 1
}

# add the maternal mortality lower and upper bounds columns to the tibble
mm$mm_lower_bound <- mm_lower_bound
mm$mm_upper_bound <- mm_upper_bound
# mm <- mutate(mm, mm_lower_bound)
# mm <- mutate(mm, mm_upper_bound)

# convert the maternal mortality columns to numeric
mm$maternal_mortality <- as.numeric(mm$maternal_mortality)
mm$mm_lower_bound <- as.numeric(mm$mm_lower_bound)
mm$mm_upper_bound <- as.numeric(mm$mm_upper_bound)

# create a seperate row for each year of skilled health care percentage data
year_range_loc = str_detect(mm$year, "-")
mm_new <- data.frame(country = character(), year = character(), maternal_mortality = double(),
                     mm_lower_bound = double(), mm_upper_bound = double())
i = 1
for (y in year_range_loc){
  if (year_range_loc[i]) {
    ys <- str_split(mm$year[i], "-")
    mm$year[i] <- ys[[1]][1]
    newrow <- mm[i,]
    newrow$year <- ys[[1]][2]
    mm_new <- rbind(mm_new, newrow)
  }
  i = i + 1
}
mm <- rbind(mm, mm_new) 

# make the year column numeric
mm$year <- as.integer(mm$year)

# read in and tidy up the gross national income data
gni <- read_csv("../data/WHS9_93.csv", skip = 1)

# get rid of the space where the comma should be in the 2013 column and convert column to integer
gni$`2013` <- str_replace_all(gni$`2013`, "[\\s]+", "")
gni$`2013` <- as.integer(gni$`2013`)


# reformat so that there is one entry per country per year
gni <- gather(gni, `2013`:`2012`:`2011`:`2010`:`2009`:`2008`:`2007`:`2006`:`2005`:`2004`:`2003`:
              `2002`:`2001`:`2000`:`1999`:`1998`:`1997`:`1996`:`1995`:`1994`:`1993`:`1992`:
              `1991`:`1990`, key = "year", value = gross_nat_income)

# make year a numeric column
gni$year <- as.integer(gni$year)

# get rid of any rows that gave NAs for gross national income
gni <- na.omit(gni)

# perform a join on the gni and mm tibbles using year and country as the keys
mm_gni_clean <- inner_join(mm, gni, by = c("country" = "Country", "year" = "year"))

# write a csv containing the final cleaned up data frame
write.csv(mm_gni_clean, file = "../data/mm_gni_clean.csv")
