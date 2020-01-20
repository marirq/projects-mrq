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

# multiple plots - http://bit.ly/2tzsoli ###
# Plot separate ggplot figures in a loop.
library(ggplot2)

# Make list of variable names to loop over.
var_list = combn(names(iris)[1:3], 2, simplify=FALSE)

# Make plots.
plot_list = list()
for (i in 1:3) {
  p = ggplot(iris, aes_string(x=var_list[[i]][1], y=var_list[[i]][2])) +
    geom_point(size=3, aes(colour=Species))
  plot_list[[i]] = p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in 1:3) {
  file_name = paste("iris_plot_", i, ".tiff", sep="")
  tiff(file_name)
  print(plot_list[[i]])
  dev.off()
}

# Another option: create pdf where each page is a separate plot.
pdf("plots.pdf")
for (i in 1:3) {
  print(plot_list[[i]])
}
dev.off()


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

