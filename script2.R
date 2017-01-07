library(tidyverse)

demog <- read_csv(file = 'PEP_2015_PEPANNRES_with_ann.csv')

demog2 <- demog %>% separate(col = Geography, into = c('county', 'state'), sep = ', ') %>% mutate(pop2015 = `Population Estimate (as of July 1) - 2015`) %>% select(county, state, pop2015)

# demog2 %>% group_by(state) %>% summarise(pop = sum(pop2015)) %>% ggplot(mapping = aes(x = state, y = pop)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
demog2 %>% .[['pop2015']] %>% sum

nc_demog <- demog2 %>% filter(state == "North Carolina")

nc_demog2 <- nc_demog %>% mutate(county = toupper(gsub(replacement = '', x = county, pattern = ' County')))

nc_demog3 <- nc_demog2 %>% mutate(huge = factor(x = pop2015 > 500000, labels = c('pop>500k', 'pop<500k')),
                                  vbig = factor(x = pop2015 > 200000, labels = c('pop>200k', 'pop<200k')),
                                  big = factor(x = pop2015 > 100000, labels = c('pop>100k', 'pop<100k')))

votes_by_precinct <- read_delim(file = 'resultsPCT20161108_new.txt', delim = '\t') %>% mutate(county = County)

gov_votes <- votes_by_precinct%>% filter(`Contest Name` == 'NC GOVERNOR') %>% glimpse

gov_votes %>% group_by(County, Choice) %>% summarise(total = sum(`Total Votes`))

gov_votes_by_county <- gov_votes %>% group_by(county, Choice) %>% summarise(votes = sum(`Total Votes`))

df <- inner_join(x = gov_votes_by_county, y = nc_demog3, by = c('county', 'county'))



# df %>% group_by(Choice, huge) %>% summarise(votes = sum(votes)) %>% ggplot(mapping = aes(x = Choice, y = votes)) + geom_bar(stat = 'identity') + facet_wrap(~huge)

# df %>% group_by(Choice, vbig) %>% summarise(votes = sum(votes)) %>% ggplot(mapping = aes(x = Choice, y = votes)) + geom_bar(stat = 'identity') + facet_wrap(~vbig)

# df %>% group_by(Choice, big) %>% summarise(votes = sum(votes)) %>% ggplot(mapping = aes(x = Choice, y = votes)) + geom_bar(stat = 'identity') + facet_wrap(~big)




cooper <- df %>% spread(key = Choice, value = votes) %>% mutate(cooper_frac = `Roy Cooper`/(`Lon Cecil` + `Pat McCrory` + `Roy Cooper`),
                                                                cooper_bi_frac = `Roy Cooper`/(`Pat McCrory` + `Roy Cooper`),
                                                                cooper_excess = `Roy Cooper` - `Pat McCrory`)


# lm(formula = cooper_frac ~ pop2015, data = cooper)


cooper %>%
    ggplot(mapping = aes(x = reorder(county, cooper_excess), y = cooper_excess, fill = pop2015/1000)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab('county') +
    ylab('Cooper (winner) votes in excess of McCrory (loser)') +
    ggtitle(label = 'Roy Cooper won the NC governors race by winning a few big Counties by a lot') +
    scale_fill_continuous(name = 'population\n(thousands)') +
    ggsave(filename = 'nc_gov_viz.png', height = 6, width = 16, dpi = 500)


cooper %>%
  ggplot(mapping = aes(x = pop2015, y = cooper_excess)) +
  geom_point() +
  geom_label(mapping = aes(label = county, fill = cooper_bi_frac)) +
  scale_x_continuous(expand = c(.1, .1)) +
  theme_minimal() +
  scale_fill_gradient2(midpoint = 0.5, high = 'lightblue') +
  ggsave(filename = 'nc_gov_viz2.png', height = 12, width = 12, dpi = 500)
