# library(cheddar)
library(NetIndices)
library(magrittr)
library(igraph)

lakes=c('Alford','WEBAmundsenFL','Beaver','Bridge','Chub','Connery','LittleRock','WEBPrestonFL','Sierra','Skipwith','Stink','Tuesday',
      'WEB114','WEB116','WEB117','WEB118','WEB120','WEB126','WEB131','WEB132','WEB133','WEB134','WEB135','WEB137','WEB143',
      'WEB204','WEB250','WEB251','WEB263','WEB269','WEB272','WEB277','WEB278','WEB279','WEB289','WEB294','WEB296','WEB310',
      'WEB33','WEB334','WEB335','WEB336','WEB337','WEB343','WEB345','WEB355','WEB356','WEB38','WEB39','WEB46','WEB58','WEB71','WEB72','WEB75','WEB76','WEB78','WEB84')

result_table=matrix(nrow=length(lakes),ncol=4,data=1000)
colnames(result_table)=c("Lake","S","mean_TL","mean_chain")

for(n in 1:length(lakes)){
  lake=lakes[n]
  print(lake)
  preypred=read.table(paste0('../Lakefiles/',lake,'/trophic.links.csv'),sep=',',header=TRUE)
  # NetIndices likes it backwards
  preypred %>%
    graph.data.frame(directed=TRUE)  %>% # we build a graph with igraph
    get.adjacency(sparse = F) %>% # and compute its adjacency matrix (in a non sparse format)
    TrophInd -> TrophicLevels # and finally compute the trophic levels

  meanTL=mean(TrophicLevels[,1])
  result_table[n,1]=lake
  result_table[n,2]=length(TrophicLevels[,1])
  result_table[n,3]=meanTL

  preypred %>%
    graph.data.frame(directed=TRUE) %>%
    # get.adjacency(sparse = F) %>%
    shortest.paths(mode="out") -> Paths

  noninfinite=Paths[which(Paths!=Inf)]
  meanPath=mean(noninfinite)

  result_table[n,4]=meanPath
}
result_table=as.data.frame(result_table)
result_table[,2]=as.numeric(as.character(result_table[,2]))
result_table[,3]=as.numeric(as.character(result_table[,3]))
result_table[,4]=as.numeric(as.character(result_table[,4]))

write.table(result_table,file='TLs_and_Chains.csv',sep=',')

print('Huzzah')


#contMap(herbtree,herbtraits[,1]) #Look at the pretty plot
