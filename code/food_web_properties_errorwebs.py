import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *

import byauthor_jacknife_setup as aj

dubious_webs=['WEB218','WEB220','WEB225','WEB236','WEB238','WEB240','WEB244','WEB246','WEB251','WEB256','WEB307','WEB33','WEB345','WEB350','WEB351','WEB357','WEB359','WEB41','WEB80','WEB214','WEB215','WEB216','WEB217','WEB219','WEB221','WEB222','WEB223','WEB224','WEB291','WEB292','WEB342','WEB114','WEB117','WEB119','WEB120','WEB153','WEB154','WEB18','WEB180','WEB181','WEB182','WEB183','WEB202','WEB213','WEB213','WEB22','WEB23','WEB23','WEB24','WEB249','WEB250','WEB252','WEB253','WEB254','WEB254','WEB255','WEB255','WEB257','WEB257','WEB258','WEB258','WEB259','WEB259','WEB26','WEB260','WEB260','WEB261','WEB261','WEB262','WEB262','WEB265','WEB265','WEB266','WEB267','WEB268','WEB269','WEB269','WEB288','WEB289','WEB290','WEB290','WEB293','WEB293','WEB294','WEB296','WEB296','WEB320','WEB320','WEB321','WEB321','WEB322','WEB322','WEB323','WEB323','WEB324','WEB324','WEB333','WEB333','WEB339','WEB339','WEB37','WEB38','WEB39','WEB39','WEB60','WEB60']
# # Taking too long to recalculate. Will be better to just
# # read in the datafile and trim these.


# Using corrected versions of dubious webs
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

# def clustering(G):
#   #The average fraction of pairs connected to the same node connected to each other
#   N=len(G.nodes())
#   L=len(G.edges())
#   try:
#     LS=Decimal(L)/Decimal(N)
#   except:
#     print item, L, N
#   ratios=[]   # List of % of pairs connected to a third node that are also connected
#   for kingnode in G.nodes():
#     connected=G.predecessors(kingnode)+G.successors(kingnode)
#     for node1 in connected:
#       npairs=len(connected)-1
#       also_connected=set()
#       remnants=[]
#       for item in connected:
#         if item!=node1:
#           remnants.append(item)
#       if npairs!=0:
#         for node2 in remnants:
#           if node1 in G.successors(node2):
#             also_connected.add(node2)
#           if node1 in G.predecessors(node2):
#             also_connected.add(node2)
#         ratio=Decimal(len(also_connected))/Decimal(npairs)
#         ratios.append(ratio)

#   Clus=float(sum(ratios)/len(ratios))
#   return Clus

 
def food_web_properties(directory,item,G):

  # Clus=clustering(G)

  N=len(G.nodes())
  L=len(G.edges())

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

  LS=Decimal(L)/Decimal(N)
  C=Decimal(L)/Decimal(N**2)

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
  maxTL=sorted(TLdict.values())[-1]
  if len(TLdict.values())>0:
    SWTL=float(sum(TLdict.values()))/float(len(TLdict.values()))
    TLdict={}
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

  genSDconts=[]
  vulSDconts=[]
  linkSDconts=[]
  # chains=[]
  # print 'beginning chains'
  # chains2=[]
  # for pred in G.nodes():
  #   if pred not in B:
  #     # print pred
  #     for resource in B:
  #       paths2=nx.all_simple_paths(G,source=pred,target=resource,cutoff=5*maxTL)
  #       # print list(paths2)
  #       # for path in paths:
  #       #   chains.append(len(path))
  #       for path in paths2:
  #         chains2.append(len(path))
  #       if len(list(paths2))!=0:
  #         print "This is not the way I thought it worked"
  #       if len(list(paths))==0 and len(list(paths2))==0:
  #         print "No paths"
  # sys.exit()
  # if  chains2!=[]:
  #   Chain=float(sum(chains2))/float(len(chains2))
  #   chains2=[]
  # # elif chains2==[] and chains!=[]:
  # #   Chain=float(sum(chains))/float(len(chains))
  # # elif chains==[] and chains2==[]:
  # #   print 'No chains'
  # #   Chain=0
  # else:
  #   print 'Too many chains'
  #   print len(chains2), len(chains)
  #   Chain=1000

  # print 'chains done', Chain
  # try:
  #   Path=nx.average_shortest_path_length(G,weight=None) #average shortest path length
  # except nx.exception.NetworkXError:
  #   paths=[]
  #   for g in nx.connected_component_subgraphs(G.to_undirected()):
  #     try:
  #       paths.append(nx.average_shortest_path_length(g,weight=None))
  #     except:
  #       pass
  #   print item, 'not connected'
  #   Path=sum(paths)/len(paths)

  stroutput=[]
  outputs = [int(N),int(sum(LinksperS)),float(C),float(LS),float(LinkSD),float(Gen),float(GenSD),float(Vul),float(VulSD),float(SWTL),basal,herbs,inter,top] 
  for thing in outputs:
    stroutput.append(str(thing))
  return stroutput 

# def authors_by_web(bibfile,used_webs,webkeys):
#   authorset,authors_by_key,authors_with_initials=aj.bibreader(bibfile,used_webs)
#   web_to_key=web_key_dict(webkeys)

#   # The idea here will be to add columns for each author to the datafile so I can jacknife by authors.
#   web_to_authors={}
#   for web in web_to_key:
#     authors=authors_by_key[web_to_key[web]]
#     web_to_authors[web]=authors
#   return web_to_authors

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
          'SWTL',
          'Basal',
          'Herbivores',
          'Intermediate',
          'Toppreds']

  # authorset,authors_by_key,authors_with_initials=bibreader(bibfile,used_webs)
  # web_to_authors=authors_by_web(bibfile,used_webs,webkeys)

  # bigheader=header+list(sorted(authorset))
  # Header is expanded with a list of authors... now I need to fill those columns.

  outfile=open('../mod_data/summary-properties_corrected_webs.tsv','w')
  # outfile.write(u'\t'.join(bigheader).encode('utf-8').strip())
  outfile.write(u'\t'.join(header).encode('utf-8').strip())
  outfile.write('\n')

  # for item in uselist:
  for item in ['WEB333.web']:
    print item
    if item.split('.')[0] in dubious_webs:
      outputs=food_web_properties(Edir,item,trophic_species_web(Edir,item))
    else:
      outputs=food_web_properties(directory,item,trophic_species_web(directory,item))
    info=infodict[item]
    outfile.write(item+'\t')
    outfile.write('\t'.join(info))
    outfile.write('\t')
    outfile.write('\t'.join(outputs))
    outfile.write('\n')
    # sys.exit()
    # try:
    #   shortitem=item.split('.web')[0].split('WEB')[1]
    # except IndexError:
    #   shortitem=item.split('.web')[0]
    # if item=='WEB294_rotated.web':
    #   shortitem='294'

    # for author in sorted(authorset):
    #   if author in web_to_authors[shortitem]:
    #     outfile.write('\t1')
    #   else:
    #     outfile.write('\t0')

  outfile.close()
           # extfile.write('\t'.join(map(str,[eval(item) for item in extheader])))

  out2=open('../non_TS/summary-properties_corrected_webs.tsv','w')
  # out2.write(u'\t'.join(bigheader).encode('utf-8').strip())
  out2.write(u'\t'.join(header).encode('utf-8').strip())
  out2.write('\n')

  for item in uselist:
    snoutputs=food_web_properties(directory,item,create_web(directory,item))
    info=infodict[item]
    out2.write(item+'\t')
    out2.write('\t'.join(info))
    out2.write('\t')
    out2.write('\t'.join(snoutputs))

    # try:
    #   shortitem=item.split('.web')[0].split('WEB')[1]
    # except IndexError:
    #   shortitem=item.split('.web')[0]
    # if item=='WEB294_rotated.web':
    #   shortitem='294'

    # for author in sorted(authorset):
    #   if author in web_to_authors[shortitem]:
    #     out2.write('\t1')
    #   else:
    #     out2.write('\t0')

    out2.write('\n')

  outfile.close()


def main():
  
  metafile = '../mod_data/sup_data/food_web_notes_updated.csv'
  directory = '../mod_data/lists/pred-prey-lists-2017/'
  Edir='../mod_data/lists/pred-prey-lists-2018-errorwebs/'
  bibfile = '../manuscript/noISN.bib'  
  webkeys = '../manuscript/webs_and_keys.csv'

  websorter(metafile,directory,Edir,bibfile,webkeys)

if __name__ == '__main__':
  main()
