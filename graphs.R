library(ggplot2)
# quanti x dog ####
corr <- pns %>%
  select_if(is.numeric)

corr_plot <- cor(corr, use= 'complete.obs')
corrplot::corrplot(corr_plot,type = 'lower', order = 'hclust')

# quanti x dog_pres ####
corr_ass <- pns %>% 
  select_if(is.numeric) %>%
  select(-dogs) %>%
  mutate(dogs_pres = pns$dogs_pres) %>%
  melt(id.vars = 'dogs_pres')

ggplot(data = corr_ass, aes(x=variable, y=value, fill=dogs_pres)) + 
  geom_boxplot() +
  xlab('') +
  ylab('Number of') +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top", legend.title = element_blank()) +
  facet_wrap( ~ variable, scales="free")

# test: quanti X quali ####
n.testR <- pns$dogs %>% # regression
  createDataPartition(p = .8, list = FALSE)
n.testC <- pns$dogs_pres %>% # classification
  createDataPartition(p = .8, list = FALSE)

testR <- pns[-n.testR, ]
testC <- pns[-n.testC, ]

corrR <- testR %>% 
  select_if(is.numeric) %>%
  select(-dogs) %>%
  mutate(dogs_pres = testR$dogs_pres)

test1 <- pns.corr[-n.test, c(1:6, 29)]
test2 <- pns.corr[-n.test, c(7:12, 29)]
test3 <- pns.corr[-n.test, c(13:18, 29)]
test4 <- pns.corr[-n.test, c(19:24, 29)]
test5 <- pns.corr[-n.test, c(25:29)]
names(test)

m.corrR <- melt(corrR, id.vars = 'dogs_pres')
m.test1 <- melt(test1, id.vars = 'dogs_pres')
m.test2 <- melt(test2, id.vars = 'dogs_pres')
m.test3 <- melt(test3, id.vars = 'dogs_pres')
m.test4 <- melt(test4, id.vars = 'dogs_pres')
m.test5 <- melt(test5, id.vars = 'dogs_pres')
head(m.test)

apply(corrR[,-18], 2, summary)

ggplot(data = m.corrR, aes(x=variable, y=value, fill=dogs_pres)) + 
  geom_boxplot() +
  xlab('') +
  ylab('Number of') +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top", legend.title = element_blank()) +
  facet_wrap( ~ variable, scales="free") #+

