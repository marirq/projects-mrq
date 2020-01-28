library(reshape2); library(ggplot2); require(gridExtra); library(GGally)
# data visualization in different ways
# function to arrenge ~6 plots together ####
grid.graphs <- function(n.list, i){ # 'n.list' should be a list with plots at each element 
  grid.arrange(n.list[[i]], n.list[[i+1]], n.list[[i+2]], # ''i' should be any number when added from zero until six
               n.list[[i+3]], n.list[[i+4]], n.list[[i+5]], nrow= 2, ncol= 3) # will be the plot number at the list 
}

# many quantitative variables X specific quantitative variable ####
corr <- df %>% # grouping only quantitative data
  select_if(is.numeric)

# linear relationship ###
# building a correlation matrix
corr_plot <- cor(corr, use= 'complete.obs') 

# graph of a correlation matrix
corrplot::corrplot(corr_plot,type = 'lower', order = 'hclust', mar= c(0.1, 0.1, 0.1, 2.1)) 

# capturing other relationships ###
# capturing the variables names
var_list = combn(names(corr), 1, simplify=FALSE)

# making a list with the plots
plot_list = list()
for (i in 1:length(var_list)) {
  p = ggplot(corr, aes_string(x=var_list[[i]][1], y=var_list[[`specific-quanti-var`]][1])) + # `specific-quanti-var` variable number
    geom_point() +
    ylab('Your title') +
    theme_bw()
  plot_list[[i]] = p
}

# 6 plots together
grid.graphs(n.lis=plot_list, i=i)

# many quantitative variables X specific qualitative variable ####
reg_ass <- pns %>% 
  select_if(is.numeric) %>% # grouping quantitative data and adding the `specific-quali-var`
  select(-`specific-quanti-var`) %>%
  mutate(var.quali = df$`specific-quali-var`) %>%
  melt(id.vars = 'specific-quali-var') # transforming the data from "wide" to "long" format

# plotting
ggplot(data = reg_ass[1:length(`you-need`),], aes(x=variable, y=value, fill=`specific-quali-var`)) + 
  geom_boxplot() +
  xlab('') +
  ylab('Number of') +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "top", legend.title = element_blank()) +
  facet_wrap( ~ variable, scales="free", nrow = 2, ncol = 3) # splitting by var.name in 2 rows and 3 columns

# many qualitative variables X specific quantitative variable ####
assR <- pns %>% 
  select_if(is.character) %>% # grouping qualitative data and adding the `specific-quanti-var`
  select(-`specific-quali-var`) %>%
  mutate(`specific-quanti-var` = df$`specific-quanti-var`)

# 2 ways for build 41 dsf or 1 list with 41df
#df.list <- list()
#for (i in 1:41) {
#  assign(paste0('df', i), df.list[[i]] <- melt(assR[,c(i, `specific-quanti-var`)], id.vars = names(assR[,i]), env = .GlobalEnv)) 
#}

# capturing the variables names
var_listA = combn(names(assR), 1, simplify=FALSE)

# making a list with the plots
plot_listA <- list()
for (i in 1:length(var_listA)) {
  p <- ggplot(data = assR, aes_string(x=var_listA[[i]][1], y=var_listA[[`specific-quanti-var`]][1], fill=var_listA[[i]][1])) + # `specific-quanti-var` variable number
    geom_boxplot() +
    xlab('') +
    ylab('Number of specific-quant-var') +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position = "top", legend.title = element_text(var_listA[[i]][1]))
  plot_listA[[i]] <- p
}

# 6 plots together
grid.graphs(n.list=plot_listA, i=i)

# many qualitative variables X specific qualitative variable ####
assC <- pns %>%  
  select_if(is.character) # grouping only quantitative data

# capturing the variables names
var_listB = combn(names(assC), 1, simplify=FALSE)

# making a list with the plots
plot_listB <- list()
for (i in 1:length(var_list)) {
  p <- ggplot(assC, aes_string(x = var_listB[[i]][1], fill = var_listB[[`specific-quali-var`]][1])) + 
    geom_bar(position = position_dodge(preserve = "single")) + 
    theme_bw() +
    ylab('') +
    theme(legend.position = "top") +
    labs(fill = element_blank())
    coord_flip()
  plot_listB[[i]] <- p
}

# 6 plots together
grid.graphs(n.list=plot_listB, i=i)
