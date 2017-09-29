import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *

from byauthor_jacknife_setup import *
# import role_dictionary_maker as RD

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
  try:
    LS=Decimal(L)/Decimal(N)
  except:
    print item, L, N
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

 
def food_web_properties(directory,item,G):

  Clus=clustering(directory,item)

  N=len(G.nodes())

  B=[]
  T=[]
  H=[]
  I=[]

  LinksperS=[]
  for node in G.nodes():
    links=[]
    for edge in G.edges():
      if node in edge:
        links.append(edge)
    ls=len(links)
    LinksperS.append(ls)

  LS=Decimal(sum(LinksperS))/Decimal(N)
  C=Decimal(sum(LinksperS))/Decimal(N**2)

  for node in G.nodes():
    if len(G.predecessors(node))==0:
      T.append(node) # Cannibals are intermediate.
    if len(G.successors(node))==0:
      B.append(node)

  for node in G.nodes():
    if node not in B and node not in T:
      if set(G.successors(node))&set(B)!=set(G.successors(node)):
        I.append(node)
      else:
        H.append(node)

  links=[]
  number_predators=[]
  number_prey=[]
  top=float(float(len(T))/N)
  inter=float(float(len(I))/N)
  herbs=float(float(len(H))/N)
  basal=float(float(len(B))/N)

  TLdict={} 
  for node in B:
    TLdict[node]=1
  for node in G.nodes():
    if node not in B:
      paths=[]
      for res in B:
        try:
          short=nx.shortest_path_length(G,source=res,target=node)
          paths.append(short)
        except nx.exception.NetworkXNoPath:
          try:
            short=nx.shortest_path_length(G,source=node,target=res)
            paths.append(short)
          except:
            pass
      if len(paths)>0:
        TLdict[node]=float(sorted(paths)[0]+1) # The shortest path, plus one
      else:
        pass # Ignoring unconnected nodes
  if len(TLdict.values())>0:
    SWTL=float(sum(TLdict.values()))/float(len(TLdict.values()))
  else:
    print B, I, T
    print item, ' has no connected nodes?'
    print G.edges()

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
    paths=[]
    for g in nx.connected_component_subgraphs(G.to_undirected()):
      try:
        paths.append(nx.average_shortest_path_length(g,weight=None))
      except:
        pass
    print item, 'not connected'
    Path=sum(paths)/len(paths)

  stroutput=[]
  outputs = [int(N),int(sum(LinksperS)),float(C),float(LS),float(LinkSD),float(Gen),float(GenSD),float(Vul),float(VulSD),float(Path),float(SWTL),float(Clus),basal,herbs,inter,top] 
  for thing in outputs:
    stroutput.append(str(thing))
  return stroutput 

def authors_by_web(bibfile,used_webs,webkeys):
  authorset,authors_by_key,authors_with_initials=bibreader(bibfile,used_webs)
  web_to_key=web_key_dict(webkeys)

  # The idea here will be to add columns for each author to the datafile so I can jacknife by authors.
  web_to_authors={}
  for web in web_to_key:
    authors=authors_by_key[web_to_key[web]]
    web_to_authors[web]=authors
  return web_to_authors

def websorter(metafile,directory,bibfile,webkeys):
  infodict={}
  uselist=[]
  f=open(metafile)
  print 'go!'
  for line in f:
    newline=line.split('\n')[0]
    items=newline.split('*')
    if items[9][:3] in ['Use','use']:
      webno=items[0]
      try:
        webfile='WEB'+str(int(webno))+'.web'
      except:
        webfile=webno+'.web'
      uselist.append(webfile)
      ecotype=items[2]
      Site=items[10]
      latitude=items[5]
      info=[ecotype,Site,latitude]
      infodict[webfile]=info
  f.close()

  header=['Web',
          'Ecotype',
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
          'SWTL',
          'Clus',
          'Basal',
          'Herbivores',
          'Intermediate',
          'Toppreds']

  authorset,authors_by_key,authors_with_initials=bibreader(bibfile,used_webs)
  web_to_authors=authors_by_web(bibfile,used_webs,webkeys)

  bigheader=header+list(sorted(authorset))
  # Header is expanded with a list of authors... now I need to fill those columns.

  outfile=open('../mod_data/summary-properties_extended_connected.tsv','w')
  outfile.write(u'\t'.join(header).encode('utf-8').strip())
  outfile.write('\n')

  for item in uselist:
    outputs=food_web_properties(directory,item,trophic_species_web(directory,item))
    info=infodict[item]
    outfile.write(item+'\t')
    outfile.write('\t'.join(info))
    outfile.write('\t')
    outfile.write('\t'.join(outputs))
    outfile.write('\n')

  outfile.close()
           # extfile.write('\t'.join(map(str,[eval(item) for item in extheader])))

  out2=open('../non_TS/summary-properties_extended_connected.tsv','w')
  out2.write(u'\t'.join(bigheader).encode('utf-8').strip())
  out2.write('\n')

  for item in uselist:
    snoutputs=food_web_properties(directory,item,create_web(directory,item))
    info=infodict[item]
    out2.write(item+'\t')
    out2.write('\t'.join(info))
    out2.write('\t')
    out2.write('\t'.join(snoutputs))

    try:
      shortitem=item.split('.web')[0].split('WEB')[1]
    except IndexError:
      shortitem=item.split('.web')[0]
    if item=='WEB294_rotated.web':
      shortitem='294'

    for author in sorted(authorset):
      if author in web_to_authors[shortitem]:
        out2.write('\t1')
      else:
        out2.write('\t0')

    out2.write('\n')

  outfile.close()


def main():
  
  metafile = '../mod_data/sup_data/food_web_notes_updated.csv'
  directory = '../mod_data/lists/pred-prey-lists-2017/'
  motifdir = '../mod_data/Roles/'
  bibfile = '../manuscript/noISN.bib'  
  webkeys = '../manuscript/webs_and_keys.csv'

  websorter(metafile,directory,bibfile,webkeys)

if __name__ == '__main__':
  main()
