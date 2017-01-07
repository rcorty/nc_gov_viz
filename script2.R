library(tidyverse)


demog <- read_csv(file = 'PEP_2015_PEPANNRES_with_ann.csv')

demog2 <- demog %>% separate(col = Geography, into = c('county', 'state'), sep = ', ') %>% mutate(pop2015 = `Population Estimate (as of July 1) - 2015`) %>% select(county, state, pop2015)

# demog2 %>% group_by(state) %>% summarise(pop = sum(pop2015)) %>% ggplot(mapping = aes(x = state, y = pop)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
demog2 %>% .[['pop2015']] %>% sum

nc_demog <- demog2 %>% filter(state == "North Carolina")

nc_demog2 <- nc_demog %>% mutate(county = gsub(replacement = '', x = county, pattern = ' County'))

nc_demog3 <- nc_demog2 %>% mutate(huge = factor(x = pop2015 > 500000, labels = c('pop>500k', 'pop<500k')),
                                  vbig = factor(x = pop2015 > 200000, labels = c('pop>200k', 'pop<200k')),
                                  big = factor(x = pop2015 > 100000, labels = c('pop>100k', 'pop<100k')))



library(stringi)

votes_by_precinct <- read_delim(file = 'resultsPCT20161108_new.txt', delim = '\t') %>% mutate(county = stri_trans_totitle(County))

gov_votes <- votes_by_precinct%>% filter(`Contest Name` == 'NC GOVERNOR')

gov_votes %>% group_by(county, Choice) %>% summarise(total = sum(`Total Votes`))

gov_votes_by_county <- gov_votes %>% group_by(county, Choice) %>% summarise(votes = sum(`Total Votes`))

df <- inner_join(x = gov_votes_by_county, y = nc_demog3, by = c('county', 'county'))



# df %>% group_by(Choice, huge) %>% summarise(votes = sum(votes)) %>% ggplot(mapping = aes(x = Choice, y = votes)) + geom_bar(stat = 'identity') + facet_wrap(~huge)

# df %>% group_by(Choice, vbig) %>% summarise(votes = sum(votes)) %>% ggplot(mapping = aes(x = Choice, y = votes)) + geom_bar(stat = 'identity') + facet_wrap(~vbig)

# df %>% group_by(Choice, big) %>% summarise(votes = sum(votes)) %>% ggplot(mapping = aes(x = Choice, y = votes)) + geom_bar(stat = 'identity') + facet_wrap(~big)




cooper <- df %>% spread(key = Choice, value = votes) %>% mutate(cooper_frac = `Roy Cooper`/(`Lon Cecil` + `Pat McCrory` + `Roy Cooper`),
                                                                cooper_bi_frac = `Roy Cooper`/(`Pat McCrory` + `Roy Cooper`),
                                                                cooper_excess = (`Roy Cooper` - `Pat McCrory`)/1000,
                                                                pop2015 = pop2015/1000)

# lm(formula = cooper_frac ~ pop2015, data = cooper)


cooper %>%
  ggplot(mapping = aes(x = reorder(county, cooper_excess), y = cooper_excess, fill = pop2015)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab('county') +
  ylab('Cooper (winner) votes in excess of McCrory (loser) (in thousands)') +
  ggtitle(label = 'Roy Cooper won the NC governors race by winning the few big Counties by a lot') +
  scale_fill_continuous(low = 'yellow', high = 'blue', name = 'population\n(thousands)') +
  ggsave(filename = 'nc_gov_viz.png', height = 6, width = 16, dpi = 500)


library(ggrepel)

cooper %>%
  ggplot(mapping = aes(x = pop2015, y = cooper_excess)) +
  geom_point() +
  geom_label(data = cooper %>% filter(pop2015 > 300),
             mapping = aes(x = pop2015+35, y = cooper_excess-5, label = county, fill = cooper_bi_frac)) +
  scale_x_continuous(expand = c(.1, .1)) +
  theme_minimal() +
  scale_fill_gradient2(midpoint = 0.5, high = 'blue', low = 'red') +
  xlab('county population (in thousands)') +
  ylab('Cooper (winner) votes in excess of McCrory (loser) (in thousands)') +
  ggtitle(label = 'Roy Cooper won the NC governors race by winning the few big Counties by a lot') +
  ggsave(filename = 'nc_gov_viz2.png', height = 12, width = 12, dpi = 500)



