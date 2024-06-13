library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
alife_updated_df <- read_csv('alife_updated.csv')
endpoints <- alife_updated_df %>% filter(update==100000)

new_endpoints <- endpoints %>% pivot_longer(c('HostValsmean_intval', 'SymValsmean_intval'),
                           names_to = 'Identification', values_to = 'Interaction_Val')
new_endpoints <- new_endpoints %>% mutate(Identification = case_when(
  Identification == 'HostValsmean_intval' ~'Hosts', 
  Identification == 'SymValsmean_intval' ~'Symbionts'))

# BOXPLOT
ggplot(new_endpoints %>% filter(SYM_INT==-0.5)) +
#ggplot(new_endpoints) + 
  geom_boxplot(aes(x=as.factor(MUTATION_RATE), 
                                     y=Interaction_Val,
                                     group = interaction(MUTATION_RATE, Identification),
                                     color= Identification)) + 
 facet_wrap(~MUTATION_SIZE) + 
  theme_bw() + ylab('Mean Interaction Value') + 
  xlab('Mutation Rate') + theme(legend.position = 'bottom')

# HEAT MAP 
new_heatmap_endpoints <- new_endpoints %>% group_by(MUTATION_RATE, MUTATION_SIZE, Identification) %>% 
  filter(!is.na(Interaction_Val)) %>%
  summarise(Interaction_Val=mean(Interaction_Val))

ggplot(new_heatmap_endpoints %>% filter(Identification == 'Symbionts',!is.na(Interaction_Val)), aes(x = as.factor(MUTATION_RATE), 
                          y = as.factor(MUTATION_SIZE), 
                          fill = Interaction_Val)) +
  geom_tile() +
  scale_fill_gradient2(limits=c(-1,1),low='green', high='purple', name = 'Interaction Value') +
  theme_bw() +
  ylab('Mutation Size') +
  xlab('Mutation Rate') +
  theme(legend.position = 'bottom')

## MAXES

new_endpoints <- endpoints %>% pivot_longer(c('HostValsmax_intval', 'SymValsmax_intval'),
                                            names_to = 'Identification', values_to = 'Interaction_Val')
new_endpoints <- new_endpoints %>% mutate(Identification = case_when(
  Identification == 'HostValsmax_intval' ~'Hosts', 
  Identification == 'SymValsmax_intval' ~'Symbionts'))

# BOXPLOT
ggplot(new_endpoints %>% filter(SYM_INT==-0.5)) +
  #ggplot(new_endpoints) + 
  geom_boxplot(aes(x=as.factor(MUTATION_RATE), 
                   y=Interaction_Val,
                   group = interaction(MUTATION_RATE, Identification),
                   color= Identification)) + 
  facet_wrap(~MUTATION_SIZE) + 
  theme_bw() + ylab('Mean Interaction Value') + 
  xlab('Mutation Rate') + theme(legend.position = 'bottom')

# HEAT MAP 
new_heatmap_endpoints <- new_endpoints %>% group_by(MUTATION_RATE, MUTATION_SIZE, Identification) %>% 
  filter(!is.na(Interaction_Val)) %>%
  summarise(Interaction_Val=mean(Interaction_Val))

ggplot(new_heatmap_endpoints %>% filter(Identification == 'Symbionts',!is.na(Interaction_Val)), aes(x = as.factor(MUTATION_RATE), 
                                                                                                    y = as.factor(MUTATION_SIZE), 
                                                                                                    fill = Interaction_Val)) +
  geom_tile() +
  scale_fill_gradient2(limits=c(-1,1),low='green', high='purple') +
  theme_bw() +
  ylab('Mutation Size') +
  xlab('Mutation Rate') +
  theme(legend.position = 'bottom')
