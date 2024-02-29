#icc.data.mb <-
#dog.during.vig <-  
ind981.data <- Data %>%
  filter(sound_stimuli == 'dog',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '981') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis) 
  group_by(activation_number) %>%
  summarise()

join.data<-left_join(ind1143.data, indL31.data, "activation_number") 
join.data<-left_join(join.data, ind981.data, "activation_number") 

icc.data.mb<-join.data %>%
  select(c('1143', '981', 'L31')) %>%
  replace(is.na(.), 0)

t(icc.data.mb)
  
print(psych::ICC(icc.data.mb))

print(irr::icc(icc.data.mb, model = "twoway", type = "agreement", unit = "average")) 

#gives the variation between the 3 individuals in their response to the dog stimulus.
#Low ICC -> low degree of rater/measurement agreement; individuals not similar to each other --> indicates consistency in individual response? 

library(tidyverse)
Data <- as.data.frame(Data)

#silent.during.vig 
ind981.data <- Data %>%
  filter(sound_stimuli == 'silent',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '981') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)
  group_by(activation_number) %>%
  summarise()

ind1143.data <- Data %>%
    filter(sound_stimuli == 'silent',
           period == 'during',
           activity == 'vigilant', 
           individual_ID == '1143') %>%
    pivot_wider(names_from = individual_ID, values_from = proportionvis)
  
indL31.data <- Data %>%
  filter(sound_stimuli == 'silent',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == 'L31') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

join.data<-left_join(ind1143.data, indL31.data, "activation_number") 
join.data<-left_join(join.data, ind981.data, "activation_number") 

icc.data.silent<-join.data %>%
  select(c('1143', '981', 'L31')) %>%
  replace(is.na(.), 0)

t(icc.data.silent)

print(psych::ICC(icc.data.silent)) #ICC of 0? 
print(irr::icc(icc.data.silent, model = "twoway", type = "agreement", unit = "single")) #negative ICC value? 

#within-individual variation across stimuli --> expected to have high ICC 
#981
ind981.data <- Data %>%
  filter(period == 'during',
         activity == 'vigilant', 
         individual_ID == '981') %>%
  pivot_wider(names_from = sound_stimuli, values_from = proportionvis) 

icc.data.981<-ind981.data %>%
  select(c('human', 'roedeer', 'silent','dog','novel','bird')) %>%
  replace(is.na(.), 0)

t(icc.data.981)
print(psych::ICC(icc.data.981)) #getting negative ICC values 
print(irr::icc(icc.data.981, model = "twoway", type = "consistency", unit = "average")) 

