# function that converts any data frame with certain (specified) 
# columns to a common data frame that can be used for temp-trait 
# analysis

# Assume VecTrait data by default, but allow user to specify the 
# columns in their data set that correspond


# this function removes any prefixes in your data
prefix_remover = function(dataframe, prefix = "published_data."){
  
  column.names = names(dataframe)
  
  # remove "published_data." from each string
  column.names = str_replace_all(column.names, prefix,
                                 "")
  
  names(dataframe) = column.names
  
  return(dataframe)
  
}

converter = function(dataframe, 
                     trait.name.column = "originaltraitname",  #
                     trait.value.column="originaltraitvalue", 
                     temperature.column="ambienttemp", 
                     species.column="interactor1"){
  dataframe %>%
    rename(trait_name = trait.name.column, 
           trait_value = trait.value.column,
           temp = temperature.column,
           species = species.column) %>%
    select(trait_name, trait_value, temp, species) %>%
    return()
}

# append function

query = function(dataframe, desired_species=NULL, 
                 desired_traits=NULL, na.rm = FALSE){
  if (is.null(desired_species)){
    output1 = dataframe
  } else {
    output1 = subset(dataframe, species %in% desired_species) 
  }
  if(is.null(desired_traits)){
    output2 = output1
  } else{
    output2 = subset(output1, trait_name %in% desired_traits) 
  }
  if(nrow(output2) == 0){
    warning("No Data Returned")
  }
   return(output2)
}

temp_plot = function(dataframe){
  if(any(is.na(dataframe$temp))){
    warning("NAs removed")
   dataframe = dataframe[!is.na(dataframe$temp),]
   dataframe = dataframe[!is.na(dataframe$trait_value),]
  }
  number.plots = length(unique(dataframe$trait_name))
  for (i in 1:number.plots){
    print(dataframe %>% filter(trait_name == 
          as.character(unique(dataframe$trait_name)[i])) %>%
      ggplot(aes(x=temp, y=trait_value,color=species))+
      geom_point() +
      xlab("Temperature (C)") +
      ylab(unique(dataframe$trait_name)[i]) +
      theme_minimal())
  }
}

dens_plot = function(dataframe){
  if(any(is.na(dataframe$temp))){
    warning("NAs removed")
    dataframe = dataframe[!is.na(dataframe$temp),]
    dataframe = dataframe[!is.na(dataframe$trait_value),]
  }
  number.plots = length(unique(dataframe$trait_name))
  for (i in 1:number.plots){
    print(dataframe %>% filter(trait_name == 
                                 as.character(unique(dataframe$trait_name)[i])) %>%
            ggplot(aes(x=trait_value,fill=species))+
            geom_density() +
            ylab("Density")+
            facet_wrap(~species)+
            xlab(unique(dataframe$trait_name)[i]) +
            theme_minimal())
  }
}

traits_merger = function(df1, df2, traits1, traits2){
  if(length(traits1) != length(traits2)){
    warning("Trait matching vectors different lengths")
    break
  }
  
  # change all values in df2$traits2 to equivalent in 
  # df1$traits1
  
  for(i in 1:length(traits1)){
    df2$trait_name = gsub(traits2[i], traits1[i], 
                          df2$trait_name)
  }
  
  return(rbind(df1, df2))
}







