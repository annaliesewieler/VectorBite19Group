rm(list=ls())
source("functions.R")
require(ggplot2)

# read in data
prefixed_data = read.csv("VecTraits.csv")

# remove prefixes
unconverted_data = prefix_remover(prefixed_data)

# convert
data = converter(unconverted_data)

# query for 
plotdata = query(dataframe = data, desired_traits = 
               c("Resource Consumption Rate",
                  "Resource Mass Consumption Rate"), 
             desired_species = c("Carcinus maenas",
                                 "Podisus maculiventris",
                                 "Podisus nigrispinus"))


temp_plot(plotdata)
dens_plot(plotdata)

# clean and merge the other data set with vec traits
otherdata = read.csv("otherdata.csv")

# convert other data
converted_other = converter(otherdata,
          trait.name.column = "trait.name", 
          temperature.column = "T",
          species.column = "mosquito.species", 
          trait.value.column = "trait")

# query other data
other_data = query(converted_other, desired_traits = "mdr")

# query vectraits
vectraits = query(data, desired_traits = 
                    c("Development Rate"))

# traits merger
merged = traits_merger(vectraits, other_data, 
                       "Development Rate",
              "mdr")

# plot
temp_plot(merged)

