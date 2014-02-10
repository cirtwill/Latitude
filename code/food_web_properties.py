import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *

#look up tools to build webs.


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

def trophic_levels(G):
  N=len(G.nodes())
  L=len(G.edges())
  LS=Decimal(L)/Decimal(N)

  TLdict={}
  preydict={}

  for node in G.nodes():
    prey=G.successors(node)
    if prey==[]:
      TLdict[node]=set([1])
    preydict[node]=prey

  for i in range(0,10):
    for node in preydict:
      if node not in TLdict:
        for prey in preydict[node]:
          if prey in TLdict:
            try:
              TLdict[node].add(TLdict[prey]+1)
            except KeyError:
              try:
                TLdict[node]=set([TLdict[prey]+1])
              except TypeError:
                TLdict[node]=set([sorted(TLdict[prey])[0]+1])
            except TypeError:
              TLdict[node].add(sorted(TLdict[prey])[0]+1)

  return TLdict

def food_web_properties(directory,item):
  Clus=clustering(directory,item)

  G=trophic_species_web(directory,item)
  N=len(G.nodes())
  L=len(G.edges())
  LS=Decimal(L)/Decimal(N)
  C=Decimal(L)/Decimal(N**2)

  TLdict=trophic_levels(G)

  basal=set()
  herb=set()
  omni=set()

  for species in TLdict:
    if TLdict[species]==1:
      basal.add(species)
    elif TLdict[species]==2:
      herb.add(species)
    if len(TLdict[species])!=1:
      omni.add(species)

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
    if prey!=0:
      if pred==0:
        top.add(node)
      else:
        inter.add(node)

  pBas=Decimal(len(basal))/Decimal(N)
  pHerb=Decimal(len(herb))/Decimal(N)
  pInt=Decimal(len(inter))/Decimal(N)
  pTop=Decimal(len(top))/Decimal(N)
  pOmni=Decimal(len(omni))/Decimal(N)

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
      paths.append(nx.average_shortest_path_length(g,weight=None))
    print item, 'not connected'
    Path=sum(paths)/len(paths)

  SWTLdict={}
  for species in TLdict:
    SWTLdict[species]=sorted(TLdict[species])[0]
  max_SWTL=sorted(SWTLdict.values())[-1]
  mean_SWTL=Decimal(sum(SWTLdict.values()))/Decimal(len(SWTLdict.values()))

  stroutput=[]
  outputs = [N,L,C,LS,LinkSD,Gen,GenSD,Vul,VulSD,mean_SWTL,max_SWTL,Path,Clus,pBas,pInt,pTop,pHerb,pOmni] 
  for thing in outputs:
    stroutput.append(str(thing))
  return stroutput 

def websorter(metafile,directory):
  Ruzicka=['WEB320','WEB321','WEB322','WEB323','WEB324']
  Fryer=['WEB33','WEB38','WEB39','WEB204']
  Dexter=['WEB51','WEB110','WEB111','WEB112','WEB113']
  Alcorlo_Pinol=['WEB334','WEB336']
  Alcorlo_Muerta=['WEB335','WEB337']
  Closs = ['WEB297','WEB298','WEB299','WEB300','WEB301','WEB302','WEB303','WEB304','WEB305','WEB306','WEB307','WEB308']
  Parker_spring = ['WEB273','WEB275']
  Parker_stream = ['WEB274','WEB276']
  Kelleway_Gingham=['WEB327','WEB330']
  Kelleway_Gwydir=['WEB328','WEB331']
  Yanez=['WEB44','WEB57']
  Erichsen=['WEB34','WEB206']
  Minshall=['WEB35','WEB209']
  Summerhayes=['WEB61','WEB62']
  Walsh=['WEB14','WEB15','WEB36']

  aggregators=[Ruzicka,Fryer,Dexter,Alcorlo_Muerta,Alcorlo_Pinol,Closs,Parker_stream,Parker_spring,Kelleway_Gwydir,Kelleway_Gingham,Erichsen,Minshall,Summerhayes,Walsh]
  agglist=Ruzicka+Fryer+Dexter+Alcorlo_Pinol+Alcorlo_Muerta+Closs+Parker_spring+Parker_stream+Kelleway_Gingham+Kelleway_Gwydir+Erichsen+Minshall+Summerhayes+Walsh

  infodict={}
  uselist=[]
  f=open(metafile)
  print 'go!'
  for line in f:
    newline=line.split('\n')[0]
    items=newline.split('*')
    if items[17][:3] in ['Use','use']:
      webno=int(items[0])
      webfile='WEB'+str(webno)
      uselist.append(webfile)
      ecotype=items[3]
      year=items[6]
      latitude=items[10]
      info=[ecotype,year,latitude]
      infodict[webfile]=info
  f.close()

  header=['Web',
          'Ecotype',
          'Year_pub',
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
          'mean_SWTL',
          'max_SWTL',
          'Path',
          'Clus',
          'pBas',
          'pInt',
          'pTop',
          'pHerb',
          'pOmni'] 

  outfile=open('Food-web-database/summary-properties.tsv','w')
  outfile.write('\t'.join(header))
  outfile.write('\n')

  for item in uselist:
    if item not in agglist:
      outputs=food_web_properties(directory,item)
      info=infodict[item]
      outfile.write(item+'\t')
      outfile.write('\t'.join(info))
      outfile.write('\t')
      outfile.write('\t'.join(outputs))
      outfile.write('\n')
    else:
      pass

  for fillist in aggregators:
    for web in fillist:
      code=int(web[3:])+1000
      webcode='WEB'+str(code)
      if webcode in os.listdir(directory):
        outputs=food_web_properties(directory,webcode)
        info=infodict[web]
        outfile.write(web+'\t')
        outfile.write('\t'.join(info))
        outfile.write('\t')
        outfile.write('\t'.join(outputs))
        outfile.write('\n')

           # extfile.write('\t'.join(map(str,[eval(item) for item in extheader])))

def main():
  
  directory = 'Food-web-database/pred-prey-lists-to-use/'
  metafile = 'food_web_notes.csv'

  websorter(metafile,directory)

if __name__ == '__main__':
  main()
