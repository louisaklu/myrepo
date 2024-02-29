## ICC compares reliability of rating of same subject to total variation across all subjects and ratings. 
## High ICC = individual scores highly similar, variation can be attributed to inter-individual variation (consistency)

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
  
  ind1143.data <- Data %>%
    filter(sound_stimuli == 'dog',
           period == 'during',
           activity == 'vigilant', 
           individual_ID == '1143') %>%
    pivot_wider(names_from = individual_ID, values_from = proportionvis) 
  
  indL31.data <- Data %>%
    filter(sound_stimuli == 'dog',
           period == 'during',
           activity == 'vigilant', 
           individual_ID == 'L31') %>%
    pivot_wider(names_from = individual_ID, values_from = proportionvis)

join.data<-full_join(ind1143.data, indL31.data, "activation_number") 
join.data<-full_join(join.data, ind981.data, "activation_number") 

icc.data.mb<-join.data %>%
  select(c('1143', '981', 'L31'))

transpose<- t(icc.data.mb) #swap rows and columns 
  
print(psych::ICC(transpose)) #2-way mixed effects, single measurement: ICC 0.196 

print(irr::icc(transpose, model = "twoway", type = "agreement", unit = "single")) 

library(tidyverse)
Data <- as.data.frame(Data)

#human.during.vig 
ind981.data <- Data %>%
  filter(sound_stimuli == 'human',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '981') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

ind1143.data <- Data %>%
  filter(sound_stimuli == 'human',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '1143') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

indL31.data <- Data %>%
  filter(sound_stimuli == 'human',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == 'L31') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

join.data<-full_join(ind1143.data, indL31.data, "activation_number") 
join.data<-full_join(join.data, ind981.data, "activation_number") 

icc.data.mb<-join.data %>%
  select(c('1143', '981', 'L31'))

transpose<- t(icc.data.mb) 

print(psych::ICC(transpose)) #2-way mixed effects, single measurement: ICC 0?  

#novel.during.vig
ind981.data <- Data %>%
  filter(sound_stimuli == 'novel',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '981') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

ind1143.data <- Data %>%
  filter(sound_stimuli == 'novel',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '1143') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

indL31.data <- Data %>%
  filter(sound_stimuli == 'novel',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == 'L31') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

join.data<-full_join(ind1143.data, indL31.data, "activation_number") 
join.data<-full_join(join.data, ind981.data, "activation_number") 

icc.data.mb<-join.data %>%
  select(c('1143', '981', 'L31'))
  
  transpose<- t(icc.data.mb) 

print(psych::ICC(transpose)) #ICC 0.22

#roedeer.during.vig
ind981.data <- Data %>%
  filter(sound_stimuli == 'roedeer',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '981') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

ind1143.data <- Data %>%
  filter(sound_stimuli == 'roedeer',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '1143') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

indL31.data <- Data %>%
  filter(sound_stimuli == 'roedeer',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == 'L31') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

join.data<-full_join(ind1143.data, indL31.data, "activation_number") 
join.data<-full_join(join.data, ind981.data, "activation_number") 

icc.data.mb<-join.data %>%
  select(c('1143', '981', 'L31'))

transpose<- t(icc.data.mb) 

print(psych::ICC(transpose)) #ICC 0.079 


#bird.during.vig
ind981.data <- Data %>%
  filter(sound_stimuli == 'bird',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '981') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

ind1143.data <- Data %>%
  filter(sound_stimuli == 'bird',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '1143') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

indL31.data <- Data %>%
  filter(sound_stimuli == 'bird',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == 'L31') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

join.data<-full_join(ind1143.data, indL31.data, "activation_number") 
join.data<-full_join(join.data, ind981.data, "activation_number") 

icc.data.mb<-join.data %>%
  select(c('1143', '981', 'L31'))

transpose<- t(icc.data.mb) 

print(psych::ICC(transpose)) #ICC 0.021 


#silent.during.vig
ind981.data <- Data %>%
  filter(sound_stimuli == 'silent',
         period == 'during',
         activity == 'vigilant', 
         individual_ID == '981') %>%
  pivot_wider(names_from = individual_ID, values_from = proportionvis)

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

join.data<-full_join(ind1143.data, indL31.data, "activation_number") 
join.data<-full_join(join.data, ind981.data, "activation_number") 

icc.data.mb<-join.data %>%
  select(c('1143', '981', 'L31')) %>%
  
  transpose<- t(icc.data.mb) 

print(psych::ICC(transpose)) #ICC 0

