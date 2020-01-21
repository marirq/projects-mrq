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

#library(GGally)
#a <- ggpairs(assR[,c('school', 'dogs')], ggplot2::aes(colour= school), legend = c(1, 2))
rm(df.list)
df.list <- list()
for (i in 1:41) {
  df.list[[i]] <- melt(assR[,c(i, 42)], id.vars = names(assR[,i]))
}

for (i in 1:41) {# 2 ways - 41 df or 1 list with 41df
  assign(paste0('df', i), df.list[[i]] <- melt(assR[,c(i, 42)], id.vars = names(assR[,i]), env = .GlobalEnv)) 
}
rm(list =ls(pattern="^plot"))
head(df1)
head(df2)
require(gridExtra)

head(df1)

df.l <- vector("list", 41)
# select 3,5,9th element from each list
list.2 <- lapply(mylist, function(x) {x[c(3,5,9)]})
plot.l(df.list,41)
lapply(df.list, plot.l(41))

plot.l <- function(df.l, i){
  #df.l <- vector("list", 41)
  assign(paste0('plot', 1:i), assign(paste0('df', 1:i), df.l[[i]]) %>%
    ggplot(aes_(x=df.l[[i]][1], y=df.l[[i]][3], fill=df.l[[i]][1])) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position = "top"))
}
plot.l(df.list, 2)
assign(paste0('n.df', 4), df.list[[4]])
assign(paste0('plot', 1:2), for (i in 1:2) {
  df.l[[i]] %>%
    ggplot(aes_(x=df.l[[i]][1], y=df.l[[i]][3], fill=df.l[[i]][1])) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position = "top")
  })  

plot1 <- df1 %>%
  ggplot(aes(x=outer.walls, y=value, fill=outer.walls)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top")
plot2 <- ggplot(data = df2, aes(x=roof, y=value)) +
  geom_boxplot(aes(fill=roof)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top")
plot3 <- ggplot(data = df3, aes(x=floor, y=value)) +
  geom_boxplot(aes(fill=floor)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top")
plot4 <- ggplot(data = df4, aes(x=water.supply, y=value)) +
  geom_boxplot(aes(fill=water.supply)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top")
plot5 <- ggplot(data = df5, aes(x=piped.water, y=value)) +
  geom_boxplot(aes(fill=piped.water)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top")
plot6 <- df6 %>% 
  ggplot(aes(x=water.drink, y=value)) +
  geom_boxplot(aes(fill=water.drink)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top")

grid.arrange(plot1, plot2, plot3,
             plot4, plot5, plot6, ncol=3, nrow=2)

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

