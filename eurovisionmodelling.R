library(reshape2)
library(dplyr)
library(ggplot2)
library(ggforce)
library(igraph)
library(gridExtra)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Deactivate warnings
#options(warn=-1)

dataset <- read.csv('./eurovision_song_contest_1975_2019v3.csv')
# keep only the finals
dataset <- dataset[(dataset$X.semi...final =='f') ,]
dataset <- dataset[(dataset$Year > as.integer(2000)), ]
rownames(dataset)=NULL


# Count the participation of each country
participations <- dataset %>% 
         group_by(From.country) %>%
          summarise(n = n_distinct(Year))


# filter out countries with less than 30  participations in finals
countries <- participations$From.country[participations$n >= 17]
dataset_final <- subset(dataset, To.country %in% countries)
rownames(dataset_final)=NULL

print(countries)

# generate a pivot table from country to country
#pivot <- reshape2::dcast(dataset_final, formula = Year + From.country ~ To.country, 
#                           fun.aggregate=function(x){ as.character(x)[1] }, 
#                           value.var = "Points")


print('Transform data to pivot table')
pivot1 <- reshape2::dcast(dataset_final, formula = From.country + To.country ~ Year  , 
                          fun.aggregate=function(x){ as.character(x)[1] }, 
                          value.var = "Points")
print(head(pivot1))
cat("\n")


# convert <NA> to NA and character to numeric
pivot1[is.na(pivot1)]=NA
pivot1[names(pivot1)[ 3:ncol(pivot1)]] <- sapply(pivot1[names(pivot1)[ 3:ncol(pivot1)]], as.numeric)

# calculate the mean per countries pair/ sort by avg
pivot1 <- cbind(pivot1, "avg"=rowMeans(pivot1[ 3:ncol(pivot1)], na.rm=TRUE)) %>% arrange(desc(avg))
# Standart deviation
pivot1$std = apply(pivot1[,names(pivot1)[ 3:ncol(pivot1)-1]], 1, sd, na.rm=TRUE)
pivot1$pair <- paste(pivot1$From.country, "_to_", pivot1$To.country)

pivot1$sq_n <-  sqrt(rowSums(!is.na(pivot1[3:21])))



# Plot the 5 highest score pairs
plt <- pivot1  %>% arrange(-avg, -std, -sq_n) %>% filter(std != 0) %>% subset( select = c(24, 3:21)) %>% head(12)
plt <- melt(plt , id.vars = "pair")


ggplot(plt, aes(x = variable, y = value, colour = pair)) +
  ggtitle('Sample of country votes') + 
  xlab('Contest Year') +
  ylab('Points') +
  geom_point() +
  facet_wrap( ~ pair)

cat('Plot the histogram of 12 pairs')


# Change line color by sex
ggplot(plt, aes(x =value),  type = 'h') +
  geom_histogram(aes(color = pair), fill = "white",
                 position = "identity", bins = 12) + 
  ggtitle('Frequency of votes') + 
  xlab('Points given') +
  ylab('Number of contests') +
  facet_wrap( ~ pair)

print('\n')

# TODO add range boxplot


print('Build table with statisics as prsented in Gatherer paper')
# Create table demonstrated in http://cfpm.org/jom-emit/2004/vol8/gatherer_d_letter.html#Table_1
gatherer_table <- merge( x = pivot1,  y = pivot1, by.x = c('From.country', 'To.country'),
                         by.y = c('To.country', 'From.country'), suffixes = c('_1to2', '_2to1'))


gatherer_table <- gatherer_table[, (names(gatherer_table) %in%  c('From.country', 'To.country', 
                                                                  'avg_1to2', 'std_1to2', 'sq_n_1to2', 'avg_2to1', 'std_2to1', 'sq_n_2to1', 'total_avg'  ))]

# Remove duplicated pairs
cols = c(1,2)
df = gatherer_table[,cols]
for (i in 1:nrow(gatherer_table)){
  df[i, ] = sort(gatherer_table[i,cols])
}

gatherer_table <- gatherer_table[!duplicated(df),]
gatherer_table$total_avg <- gatherer_table$avg_1to2 + gatherer_table$avg_2to1

print(head(gatherer_table))
cat("\n")

# order pairs by the total avg and keep the top 15 pairs
t15_gtable <- gatherer_table %>% arrange(-total_avg ) %>% top_n(15)

# Generate random votes per pair and create a new table/ calculate mean and std
sim_df <- subset(t15_gtable, select = c(1,2))
# Random votes to newly generated columns
for (i in 1:100){
  votes <- sample(0:12, 15, replace=TRUE)
  sim_df[[paste0('test_', i)]] <- votes
}
  

sim_df <- cbind(sim_df, "sim_avg"=rowMeans(sim_df[ 3:ncol(sim_df)], na.rm=TRUE))  
sim_df$sim_std <- apply(sim_df[,names(sim_df)[ 3:ncol(sim_df)-1]], 1, sd, na.rm=TRUE)


# Plot The random voting histogram
plt2 <-  sim_df %>% subset( select = c(1:100)) 
plt2$pair <- paste(plt2$From.country, '_', plt2$To.country) 
plt2 <- plt2 %>% subset(select = c(3:101))

plt2 <- melt(plt2 , id.vars = "pair")


ggplot(plt2, aes(x =value),  type = 'h') +
  geom_histogram(aes(color = pair), fill = "white",
                 position = "identity", bins = 100) + 
  ggtitle('Random voting Histograms') + 
  xlab('Random Points given') +
  ylab('Number of contests') +
  facet_wrap( ~ pair)


#print('Random voting:', subset(sim_df, select = c( 'From.country', 'To.country',  'sim_avg', 'sim_std')))
print(subset(sim_df, select = c( 'From.country', 'To.country',  'sim_avg', 'sim_std')))


# Create a dataframe that contains all the mean and std values for the chsen pairs\
result_df <-subset( merge( x = t15_gtable,  y = sim_df, by = c('From.country', 'To.country')), 
                    select = c( 'From.country', 'To.country', 'avg_1to2', 'std_1to2',  'sq_n_1to2','avg_2to1', 'std_2to1', 'total_avg', 'sim_avg', 'sq_n_2to1', 'sim_std'))


# Z-score function
zscore <- function(rmean, rstd, rsqn,  smean, sstd){

  return(rmean-smean)/sqrt(((rstd/rsqn)^2) + ((sstd/10)^2))
}

# 2 new columns with Zscores
result_df$z_1to2 <- zscore(result_df$avg_1to2, result_df$std_1to2,  result_df$sq_n_1to2, result_df$sim_avg, result_df$sim_std   )
result_df$z_2to1 <- zscore(result_df$avg_2to1, result_df$std_2to1,  result_df$sq_n_2to1, result_df$sim_avg, result_df$sim_std   )


# Arrange by Zscore and keep  zscore >=1.65
pairs_df <- result_df[(result_df$z_1to2>=1.65) | (result_df$z_2to1>=1.65),] %>% arrange(-z_1to2, -z_2to1) %>% subset(select = c( 'From.country', 'To.country', 'z_1to2', 'z_2to1'))

# calculate the p-value for the pairs
pairs_df$p_1to2 <- pnorm(-abs(pairs_df$z_1to2))
pairs_df$p_2to1 <- pnorm(-abs(pairs_df$z_2to1))

# Demonstrate the significant differences
pairs_df$p_1to2_significance <- ifelse(pairs_df$p_1to2 < 0.05, 1, 0)
pairs_df$p_2to1_significance <- ifelse(pairs_df$p_2to1 < 0.05, 1, 0)

# Extract the dataframe as png
#png("/home/andreas/Documents/EurovisionModelling/Captions/pairs_table.png", height=200, width=800)
# p<-tableGrob(pairs_df)
#grid.arrange(p)
#dev.off()

print('Table with zscore and p-values\n')
print(pairs_df)

cat("\n")
cat('Plot the coutry clusters\n')
# Plot a graph of country clusters
df.graph <- graph.data.frame(d = subset(pairs_df, select = c('From.country', 'To.country')), directed = FALSE)
plot(df.graph, vertex.label = V(df.graph)$name, main = 'Country Clusters', vertex.label.cex = 1.2, edge.curved=.15, vertex.label.color = "black" )



# Print on terminal
print(df.graph,full = igraph_opt("print.full"), graph.attributes = igraph_opt("print.graph.attributes"), 
                                              vertex.attributes = igraph_opt("print.vertex.attributes"), 
                                            edge.attributes = igraph_opt("print.edge.attributes"),names = TRUE)
summary(df.graph)



