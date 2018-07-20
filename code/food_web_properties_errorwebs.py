import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *
import numpy as np

import byauthor_jacknife_setup as aj
used_webs=aj.used_webs
dubious_webs=['WEB218','WEB220','WEB225','WEB236','WEB238','WEB240','WEB244','WEB246','WEB251','WEB256','WEB307','WEB33','WEB345','WEB350','WEB351','WEB357','WEB359','WEB41','WEB80','WEB214','WEB215','WEB216','WEB217','WEB219','WEB221','WEB222','WEB223','WEB224','WEB291','WEB292','WEB342','WEB114','WEB117','WEB119','WEB120','WEB153','WEB154','WEB18','WEB180','WEB181','WEB182','WEB183','WEB202','WEB213','WEB213','WEB22','WEB23','WEB23','WEB24','WEB249','WEB250','WEB252','WEB253','WEB254','WEB254','WEB255','WEB255','WEB257','WEB257','WEB258','WEB258','WEB259','WEB259','WEB26','WEB260','WEB260','WEB261','WEB261','WEB262','WEB262','WEB265','WEB265','WEB266','WEB267','WEB268','WEB269','WEB269','WEB288','WEB289','WEB290','WEB290','WEB293','WEB293','WEB294','WEB296','WEB296','WEB320','WEB320','WEB321','WEB321','WEB322','WEB322','WEB323','WEB323','WEB324','WEB324','WEB333','WEB333','WEB339','WEB339','WEB37','WEB38','WEB39','WEB39','WEB60','WEB60']

def convert_authnames(authorset):
  textfriendly_authorset=[]
  for author in authorset:
    if '\\' in author:
      author=''.join(author.split('\\'))
    if ' ' in author:
      author=''.join(author.split(' '))
    if '{' in author:
      author=''.join(author.split('{'))
    if '}' in author:
      author=''.join(author.split('}'))
    if "'" in author:
      author=''.join(author.split("'"))

    textfriendly_authorset.append(author)
      
  return textfriendly_authorset

# Using corrected versions of dubious webs
def create_web(directory,item):
  predprey=set()
  spp=set()

  preydict={} # Prey for each predator
  preddict={} # Predators for each prey
  linkdict={} # Links per species

  f=open(directory+item)

  for line in f:
    (pred,prey)=line.split('\t')
    if '\n' in prey:
      prey=prey.split('\n')[0]
    # print pred, prey
    try:
      preydict[pred].append(prey)
      linkdict[pred].append((pred,prey))
    except KeyError:
      preydict[pred]=[prey]
      linkdict[pred]=[(pred,prey)]
    try:
      preddict[prey].append(pred)
      linkdict[prey].append((pred,prey))
    except KeyError:
      preddict[prey]=[pred]
      linkdict[prey]=[(pred,prey)]
    predprey.add((pred,prey))
    spp.add(pred)
    spp.add(prey)
  f.close()

  for node in spp:
    if node not in preydict:
      preydict[node]=[]
    if node not in preddict:
      preddict[node]=[]

  G=nx.DiGraph() #Creates directed graph.
  for sp in spp:
    G.add_node(sp)
  for pair in predprey:
    G.add_edge(*pair)

  return G, predprey, spp, preddict,preydict,linkdict

def trophic_species_web(directory,item):
  G,predprey,spp,preddict,preydict,linkdict=create_web(directory,item)
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

  T_predprey=[]
  T_spp=[]
  T_preddict={}
  T_preydict={}
  T_linkdict={}
  for sp in T.nodes():
    T_spp.append(sp)
    T_preddict[sp]=T.predecessors(sp)
    T_preydict[sp]=T.successors(sp)
    T_linkdict[sp]=set(T.predecessors(sp)).union(set(T.successors(sp)))
  for edge in T.edges():
    T_predprey.append(edge)

  #print 'lumped species',len(G.nodes())-len(T.nodes())

  return T,T_predprey,T_spp,T_preddict,T_preydict,T_linkdict

 
def food_web_properties(directory,item,G,predprey,spp,preddict,preydict,linkdict):

  N=len(G.nodes())
  L=len(G.edges())

  altN=len(spp)
  altL=len(predprey)

  if altN!=N or L!=altL:
    print 'something is wrong with networkx', item
    print N, altN, L, altL
    sys.exit()
  # print N-altN, L-altL

  B=[]
  T=[]
  H=[]
  I=[]

  # Simplest way to get LS
  LS=Decimal(L)/Decimal(N)
  C=Decimal(L)/Decimal(N**2)

  a=0
  for node in G.nodes():
    if len(G.predecessors(node))==0:
      T.append(node) # Cannibals are intermediate.
      if preddict[node]!=[]:
        print 'conflict over a T'
        a=1
    if len(G.successors(node))==0:
      B.append(node)
      if preydict[node]!=[]:
        print 'conflict over a B'
        a=1
  if a==1:
    sys.exit()

  for node in G.nodes():
    if node not in B and node not in T:
      if set(G.successors(node))&set(B)!=set(G.successors(node)):
        I.append(node)
      else:
        H.append(node)

  number_predators=[]
  number_prey=[]
  top=float(float(len(T))/N)
  inter=float(float(len(I))/N)
  herbs=float(float(len(H))/N)
  basal=float(float(len(B))/N)

  LperS=[]
  for node in G.nodes():
    pred=len(G.predecessors(node))
    altpred=len(preddict[node])
    if pred!=altpred:
      print 'Cant agree how many preds'
      sys.exit()
    number_predators.append(pred)
    # ALERT! SUCCESSORS GIVES ALL NEIGHBOURS!!!
    prey=len(G.successors(node))
    altprey=len(preydict[node])
    if prey!=altprey:
      print 'cant agree how many preys'
      sys.exit()
    neighbours=G.successors(node)
    preys=[]
    for preyitem in neighbours:
      if node in G.predecessors(preyitem):
        preys.append(preyitem)
    prey=len(preys)
    # prey=len(G.successors(node))
    # print prey, len(G.edges(node))
    number_prey.append(prey)
    altlinks=len(preddict[node]+preydict[node])
    links=len(set(preddict[node]).union(set(preydict[node])))
    LperS.append(links)

  nonzeroprey=[]
  for preycount in number_prey:
    if preycount!=0:
      nonzeroprey.append(preycount)

  # Note that mean generality excludes basal resources, mean vul does not exclude top preds
  Gen=Decimal(sum(nonzeroprey))/Decimal(len(nonzeroprey))
  Vul=Decimal(sum(number_predators))/Decimal(len(number_predators))
  meanL=Decimal(sum(LperS))/Decimal(len(LperS))

  if meanL==Vul:
    print 'LS equals Vulnerability again'
    sys.exit()

  stroutput=[]
  outputs = [int(N),int(L),float(C),float(LS),float(Gen),float(Vul),basal,herbs,inter,top] 
  for thing in outputs:
    stroutput.append(str(thing))
  return stroutput 

def authors_by_web(bibfile,used_webs,webkeys):
  authorset,authors_by_key,authors_with_initials=aj.bibreader(bibfile,used_webs)
  web_to_key=aj.web_key_dict(webkeys)

  # The idea here will be to add columns for each author to the datafile so I can jacknife by authors.
  web_to_authors={}
  for web in web_to_key:
    authors=authors_by_key[web_to_key[web]]
    web_to_authors[web]=convert_authnames(authors)
  return web_to_authors

def websorter(metafile,directory,Edir,bibfile,webkeys):
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
      if webfile!='WEB294_rotated.web':
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
          'Gen',
          'Vul',
          'Basal',
          'Herbivores',
          'Intermediate',
          'Toppreds']

  authorset,authors_by_key,authors_with_initials=aj.bibreader(bibfile,used_webs)
  web_to_authors=authors_by_web(bibfile,used_webs,webkeys)
  textfriendly_authorset=convert_authnames(authorset)

  bigheader=header+list(sorted(textfriendly_authorset))
  # Header is expanded with a list of authors... now I need to fill those columns.

  outfile=open('../mod_data/summary-properties_corrected_webs.tsv','w')
  outfile.write(u'\t'.join(bigheader).encode('utf-8').strip())
  # outfile.write(u'\t'.join(header).encode('utf-8').strip())
  outfile.write('\n')

  for item in uselist:
    if item.split('.')[0] in dubious_webs:
      G,predprey,spp,preddict,preydict,linkdict=trophic_species_web(Edir,item)
    else:
      G,predprey,spp,preddict,preydict,linkdict=trophic_species_web(directory,item)
    print item
    if item.split('.')[0] in dubious_webs:
      outputs=food_web_properties(Edir,item,G,predprey,spp,preddict,preydict,linkdict)
    else:
      outputs=food_web_properties(directory,item,G,predprey,spp,preddict,preydict,linkdict)
    info=infodict[item]
    outfile.write(item+'\t')
    outfile.write('\t'.join(info))
    outfile.write('\t')
    outfile.write('\t'.join(outputs))
    # sys.exit()
    try:
      shortitem=item.split('.web')[0].split('WEB')[1]
    except IndexError:
      shortitem=item.split('.web')[0]

    for author in sorted(textfriendly_authorset):
      if author in web_to_authors[shortitem]:
        outfile.write('\t1')
      else:
        outfile.write('\t0')
    outfile.write('\n')
  outfile.close()


  out2=open('../non_TS/summary-properties_corrected_webs.tsv','w')
  out2.write(u'\t'.join(bigheader).encode('utf-8').strip())
  # out2.write(u'\t'.join(header).encode('utf-8').strip())
  out2.write('\n')

  for item in uselist:
    # print item
    if item.split('.')[0] in dubious_webs:
      G,predprey,spp,preddict,preydict,linkdict=create_web(Edir,item)
      snoutputs=food_web_properties(Edir,item,G,predprey,spp,preddict,preydict,linkdict)
    else:
      G,predprey,spp,preddict,preydict,linkdict=create_web(directory,item)
      snoutputs=food_web_properties(directory,item,G,predprey,spp,preddict,preydict,linkdict)

    info=infodict[item]
    out2.write(item+'\t')
    out2.write('\t'.join(info))
    out2.write('\t')
    out2.write('\t'.join(snoutputs))

    try:
      shortitem=item.split('.web')[0].split('WEB')[1]
    except IndexError:
      shortitem=item.split('.web')[0]
    for author in sorted(textfriendly_authorset):
      if author in web_to_authors[shortitem]:
        out2.write('\t1')
      else:
        out2.write('\t0')

    out2.write('\n')
  out2.close()

def main():
  
  metafile = '../mod_data/sup_data/food_web_notes_updated.csv'
  directory = '../mod_data/lists/pred-prey-lists-2017/'
  Edir='../mod_data/lists/pred-prey-lists-2018-errorwebs/'
  bibfile = '../manuscript/noISN.bib'  
  webkeys = '../manuscript/webs_and_keys.csv'

  websorter(metafile,directory,Edir,bibfile,webkeys)

if __name__ == '__main__':
  main()
