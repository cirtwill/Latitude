import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *
import role_dictionary_maker as RD

#For new lists, calculate properties.
#For all webs, add site label to summary_properties.
#For FL-only webs, make a check.
#For new webs, get latitudes, sitenames.


#Now you can re-run the analysis, then re-write the paper. Aren't you happy?

def create_web(directory,item):
  predprey=set()
  spp=set()

  f=open(directory+item)

  for line in f:
    (pred,prey)=line.split()
    predprey.add((pred,prey))
    spp.add(pred)
    spp.add(prey)
  f.close()

  G=nx.DiGraph() #Creates directed graph.
  for sp in spp:
    G.add_node(sp)
  for pair in predprey:
    G.add_edge(*pair)

  return G

def trophic_species_web(directory,item):
  G=create_web(directory,item)
  TSlists={}
  allval=[]
  for node1 in sorted(G.nodes()):
    if node1 not in allval:
      TS=set()
      for node2 in G.nodes():
        if node2!=node1:
          if G.predecessors(node1)==G.predecessors(node2):
            if G.successors(node1)==G.successors(node2):
              TS.add(node2)
    if TS!=set([]):
      TSlists[node1]=TS
      for item in TS:
        allval.append(item)
  #print TSlists.values(), allval

  T=nx.DiGraph()
  for sp in G.nodes():
    if sp not in allval:
      T.add_node(sp)
  for (a,b) in G.edges():
    if a not in allval:
      if b not in allval:
        T.add_edge(*(a,b))

  #print 'lumped species',len(G.nodes())-len(T.nodes())

  return T

def clustering(directory,item):
  #The average fraction of pairs connected to the same node connected to each other
  G=trophic_species_web(directory,item)
  N=len(G.nodes())
  L=len(G.edges())
  LS=Decimal(L)/Decimal(N)

  ratios=[]   # List of % of pairs connected to a third node that are also connected
  for kingnode in G.nodes():
    connected=G.predecessors(kingnode)+G.successors(kingnode)
    for node1 in connected:
      npairs=len(connected)-1
      also_connected=set()
      remnants=[]
      for item in connected:
        if item!=node1:
          remnants.append(item)
      if npairs!=0:
        for node2 in remnants:
          if node1 in G.successors(node2):
            also_connected.add(node2)
          if node1 in G.predecessors(node2):
            also_connected.add(node2)
        ratio=Decimal(len(also_connected))/Decimal(npairs)
        ratios.append(ratio)

  Clus=float(sum(ratios)/len(ratios))
  return Clus

def food_web_properties(directory,item):
  Clus=clustering(directory,item)

  G=trophic_species_web(directory,item)
  N=len(G.nodes())
  L=len(G.edges())
  LS=Decimal(L)/Decimal(N)
  C=Decimal(L)/Decimal(N**2)

  links=[]
  number_predators=[]
  number_prey=[]
  top=set()
  inter=set()

  for node in G.nodes():
    pred=len(G.predecessors(node))
    number_predators.append(pred)
    prey=len(G.successors(node))
    number_prey.append(prey)
    links.append(pred+prey)

  nonzeroprey=[]
  for preycount in number_prey:
    if preycount!=0:
      nonzeroprey.append(preycount)

  Gen=Decimal(sum(nonzeroprey))/Decimal(len(nonzeroprey))
  Vul=Decimal(sum(number_predators))/Decimal(len(number_predators))

  genSDconts=[] #excluding basal resources
  for prey in nonzeroprey:
    genSDconts.append((Decimal(prey)-Gen)**2)
  GenSD=sqrt(sum(genSDconts)/len(genSDconts))

  vulSDconts=[] #including top predators
  for pred in number_predators:
    vulSDconts.append((Decimal(pred)-Vul)**2)
  VulSD=sqrt(sum(vulSDconts)/len(vulSDconts))

  linkSDconts=[]
  for link in links:
    linkSDconts.append((Decimal(link)-Decimal(LS))**2)
  LinkSD=sqrt(sum(linkSDconts)/len(linkSDconts))

  try:
    Path=nx.average_shortest_path_length(G,weight=None) #average shortest path length
  except nx.exception.NetworkXError:
    for g in nx.connected_component_subgraphs(G.to_undirected()):
      paths=[]
      try:
        paths.append(nx.average_shortest_path_length(g,weight=None))
      except:
        paths.append(0)
    print item, 'not connected'
    Path=sum(paths)/len(paths)

  stroutput=[]
  outputs = [int(N),int(L),float(C),float(LS),float(LinkSD),float(Gen),float(GenSD),float(Vul),float(VulSD),float(Path),float(Clus)] 
  for thing in outputs:
    stroutput.append(str(thing))
  return stroutput 

def websorter(metafile,directory,motifdir):
  roledictionary=RD.wrapper(motifdir)

  infodict={}
  uselist=[]
  f=open(metafile)
  print 'go!'
  for line in f:
    newline=line.split('\n')[0]
    items=newline.split('*')
    if items[17][:3] in ['Use','use']:
      webno=items[0]
      try:
        webfile='WEB'+str(int(webno))+'.web'
      except:
        webfile=webno+'.web'
      uselist.append(webfile)
      ecotype=items[3]
      Humans=items[19]
      Site=items[21]
      if ecotype in ['lake','stream']:
        ecotype2='freshwater'
      else:
        ecotype2=ecotype
      year=items[6]
      latitude=items[10]
      info=[ecotype,ecotype2,year,Humans,Site,latitude]
      infodict[webfile]=info
  f.close()

  header=['Web',
          'Ecotype',
          'Ecotype2',
          'Year_pub',
          'Humans',
          'Site',
          'Latitude',
          'Species',
          'Links',
          'Connectance',
          'LS',
          'LinkSD',
          'Gen',
          'GenSD',
          'Vul',
          'VulSD',
          'Path',
          'Clus']
  header=header

  outfile=open('../mod_data/summary-properties.tsv','w')
  outfile.write('\t'.join(header))
  outfile.write('\n')

  for item in uselist:
    outputs=food_web_properties(directory,item)
    info=infodict[item]
    outfile.write(item+'\t')
    outfile.write('\t'.join(info))
    outfile.write('\t')
    outfile.write('\t'.join(outputs))
    outfile.write('\n')

  outfile.close()
           # extfile.write('\t'.join(map(str,[eval(item) for item in extheader])))

def main():
  
  metafile = '../mod_data/sup_data/food_web_notes.csv'
  directory = '../mod_data/lists/pred-prey-lists-to-use/'
  motifdir = '../mod_data/Roles/'

  websorter(metafile,directory,motifdir)

if __name__ == '__main__':
  main()
