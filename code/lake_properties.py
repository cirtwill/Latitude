import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *


def create_web(web):
  predprey=set()
  spp=set()

  f=open(web)

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


def readweb(infile):
  f=open(infile,'r')
  ints=[]
  nodes=set()

  for line in f:
    [pred,prey]=line.split()
    ints.append((pred,prey))
    nodes.add(pred)
    nodes.add(prey)
  f.close()
  return ints,nodes

def main():

  lakes=['Alford.web','WEBAmundsenFL.web','Beaver.web','Bridge.web','Chub.web','Connery.web','LittleRock.web','WEBPrestonFL.web','Sierra.web','Skipwith.web','Stink.web','Tuesday.web',
        'WEB114.web','WEB116.web','WEB117.web','WEB118.web','WEB120.web','WEB126.web','WEB131.web','WEB132.web','WEB133.web','WEB134.web','WEB135.web','WEB137.web','WEB143.web',
        'WEB204.web','WEB250.web','WEB251.web','WEB263.web','WEB269.web','WEB272.web','WEB277.web','WEB278.web','WEB279.web','WEB289.web','WEB294.web','WEB296.web','WEB310.web',
        'WEB33.web','WEB334.web','WEB335.web','WEB336.web','WEB337.web','WEB343.web','WEB345.web','WEB355.web','WEB356.web','WEB38.web','WEB39.web','WEB46.web','WEB58.web','WEB71.web','WEB72.web','WEB75.web','WEB76.web','WEB78.web','WEB84.web']  
  directory = '../mod_data/lists/pred-prey-lists-to-use/'

  # trophic.links.csv, a list of resource consumer lin
  # nodes.csv is a list of nodes, and/or other info
  # properties.csv is one row of data, with 
  # title   (M.units)   (N.units)
  # [Name]  (e.g., kg)  (e.g., m^-2)
  weblakes=['Alford.web','Bridge.web','Connery.web','LittleRock.web','Sierra.web','Skipwith.web','Stink.web']

  for lake in lakes:
    lac=lake.split('.web')[0]
    # if lake in weblakes:
    #   infile='../Lakefiles/'+lac+'/links.web'
    # else:
    infile='../mod_data/lists/pred-prey-lists-to-use/'+lac+'.web'

    titlef=open('../Lakefiles/'+lac+'/properties.csv','w')
    titlef.write('title\n')
    titlef.write(lac+'\n')
    titlef.close()

    ints, nodes = readweb(infile)    

    nodefile=open('../Lakefiles/'+lac+'/nodes.csv','w')
    nodefile.write('node,\n')
    for node in sorted(nodes):
      nodefile.write('sp'+node+',\n')
    nodefile.close()

    linkfile=open('../Lakefiles/'+lac+'/trophic.links.csv','w')
    linkfile.write('resource,consumer\n')
    for (pred,prey) in ints:
      linkfile.write('sp'+pred+',sp'+prey+'\n')
    linkfile.close()

    print lake
    # web=directory+lake
    # TLs(web)


  # websorter(metafile,directory,bibfile,webkeys)

if __name__ == '__main__':
  main()
