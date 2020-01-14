# dataviz
library(ggplot2)
n.test <- pns$dogs_pres %>%
  createDataPartition(p = .8, list = FALSE)
test <- pns[-n.test, ]

cor_plot <- pns %>%
  select_if(is.numeric) #%>%
cor_plot$dogs_pres <- pns$dogs_pres

cor_plot <- cor(pns.corr, use= 'complete.obs')
corrplot::corrplot(pns.corr, type = 'lower', order = 'hclust')

pns.corr <- pns %>%
  select_if(is.numeric) #%>%
select(-dogs)
pns.corr$dogs_pres <- pns$dogs_pres

test1 <- pns.corr[-n.test, c(1:6, 29)]
test2 <- pns.corr[-n.test, c(7:12, 29)]
test3 <- pns.corr[-n.test, c(13:18, 29)]
test4 <- pns.corr[-n.test, c(19:24, 29)]
test5 <- pns.corr[-n.test, c(25:29)]
names(test)

m.test <- melt(test, id.vars = 'dogs_pres')
m.test1 <- melt(test1, id.vars = 'dogs_pres')
m.test2 <- melt(test2, id.vars = 'dogs_pres')
m.test3 <- melt(test3, id.vars = 'dogs_pres')
m.test4 <- melt(test4, id.vars = 'dogs_pres')
m.test5 <- melt(test5, id.vars = 'dogs_pres')
head(m.test)

ggplot(data = m.test1, aes(x=variable, y=value)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill=dogs_pres)) +
  facet_wrap( ~ variable, scales="free")


ggplot(data = m.test3, aes(x=variable, y=value, fill=dogs_pres)) + 
  geom_boxplot() +
  xlab('') +
  ylab('Number of') +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top", legend.title = element_blank()) +
  facet_wrap( ~ variable, scales="free") #+
