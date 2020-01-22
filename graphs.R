library(reshape2); library(ggplot2)
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

# quali X dog ####
assR <- pns %>% 
  select_if(is.character) %>%
  select(-dogs_pres) %>%
  #mutate_all(~ as.factor(.)) %>%
  mutate(dogs = pns$dogs)


for (i in 1:41) {# 2 ways - 41 df or 1 list with 41df
  assign(paste0('df', i), df.list[[i]] <- melt(assR[,c(i, 42)], id.vars = names(assR[,i]), env = .GlobalEnv)) 
}


var_list = combn(names(assR), 1, simplify=FALSE)
plot_list <- list()
for (i in c(1:41)) {
  p <- ggplot(data = assR, aes_string(x=var_list[[i]][1], y=var_list[[42]][1], fill=var_list[[i]][1])) + 
    #geom_jitter() +
    geom_boxplot() +
    xlab('') +
    ylab('Number of dogs') +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position = "top", legend.title = element_text(var_list[[i]][1]))
  plot_list[[i]] <- p
}

names(plot_list) <- paste0('plot',1:41)

require(gridExtra)
grid.arrange(plot1, plot2, plot3,
             plot4, plot5, plot6, ncol=3, nrow=2)


# test: quali X dog_pres ####
set.seed(5)
n.testR <- pns$dogs %>% # regression
  createDataPartition(p = .8, list = FALSE)
n.testC <- pns$dogs_pres %>% # classification
  createDataPartition(p = .8, list = FALSE)

testR <- pns[-n.testR, ]
testC <- pns[-n.testC, ]

assC <- testC %>% 
  select_if(is.character)

names(assC)

for (i in c(1:36, 38:41)) {# 2 ways - 41 df or 1 list with 41df
  assign(paste0('df', i), df.list[[i]] <- melt(assC[,c(i, 37)], id.vars = names(assC[,i]), env = .GlobalEnv)) 
}
head(df37)
