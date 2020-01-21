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

# test: quali X dog ####
set.seed(5)
n.testR <- pns$dogs %>% # regression
  createDataPartition(p = .8, list = FALSE)
n.testC <- pns$dogs_pres %>% # classification
  createDataPartition(p = .8, list = FALSE)

testR <- pns[-n.testR, ]
testC <- pns[-n.testC, ]

assR <- testR %>% 
  select_if(is.character) %>%
  select(-dogs_pres) %>%
  mutate_all(~ as.factor(.)) %>%
  mutate(dogs = testR$dogs)

library(GGally)
a <- ggpairs(assR[,c('school', 'dogs')], ggplot2::aes(colour= school), legend = c(1, 2))
plot(assR$`outer walls`, assR$dogs)
a <- assR[,1:5] %>%
  mutate(dogs = assR$dogs)
df.list <- list()
for (i in 1:41) {
  df.list[[i]] <- melt(assR, id.vars = names(assR[,i]))
}

# multiple plots? - http://bit.ly/2tzsoli ####
var_list = combn(names(a), 1, simplify=FALSE)
ggplot(df.list[[1]], aes_(fill=df.list[[1]][1], y=df.list[[1]][3], x=df.list[[1]][2])) + 
  geom_bar()#position="dodge", stat="identity")

a <- melt(assR, id.vars = 'raca')
summary(assR[,-42])
ggplot(data = assR, aes(x=microwave_pres, y=dogs, fill=microwave_pres)) + 
  geom_boxplot() +
  xlab('') +
  ylab('Number of') +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top", legend.title = element_blank()) #+
  facet_wrap( ~ variable, scales="free") #+

