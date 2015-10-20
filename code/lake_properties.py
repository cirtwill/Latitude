import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *

def integer_TLs(species,G):
  B=[]
  H=[]

  for sp in species:
    if G.successors(sp)==[]:
      B.append(sp)

  for sp in species:
    if sp not in B:
      if set(G.successors(sp))&set(B)==set(G.successors(sp)):
        H.append(sp)

  return B, H


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

def nonints(species,G,TLdict):
  B,H=integer_TLs(species,G)

  for sp in species:
    if sp not in TLdict.keys():
      preys=G.successors(sp)
      ints=set(B) | set(H)
      if ints&set(preys)==set(preys): # If all the preys are integers, we can move on
        TLs=[]
        for prey in preys:
          TLs.append(TLdict[prey])
        meanprey=float(sum(TLs))/float(len(TLs))
        TLdict[sp]=1+meanprey

  return TLdict

def no_canns(species,G,TLdict):
  B,H=integer_TLs(species,G)

  for sp in species:
    if sp not in TLdict.keys():
      preys=G.successors(sp)
      if sp in preys:
        preys=list(set(preys)-set(sp))
      ints=set(B) | set(H)
      if ints&set(preys)==set(preys): # If all the preys are integers, we can move on
        TLs=[]
        for prey in preys:
          TLs.append(TLdict[prey])
        meanprey=float(sum(TLs))/float(len(TLs))
        TLdict[sp]=1+meanprey

  return TLdict

def tentative(species,G,TLdict,unattached):
  B,H=integer_TLs(species,G)

  for sp in unattached:
    preys=G.successors(sp)
    if sp in preys:
      preys=list(set(preys)-set(sp))
    ints=set(B) | set(H)
    if ints&set(preys)==set(preys): # If all the preys are integers, we can move on
      TLs=[]
      for prey in preys:
        TLs.append(TLdict[prey])
      meanprey=float(sum(TLs))/float(len(TLs))
      TLdict[sp]=1+meanprey

  return TLdict

def TLs(web):
  G=create_web(web)
  species=G.nodes()

  B,H=integer_TLs(species,G)

  TLdict={}
  for sp in B:
    TLdict[sp]=1
  for sp in H:
    TLdict[sp]=2

  TLdict2=nonints(species,G,TLdict)

  if len(TLdict2.keys())!=len(species):
    TLdict3=nonints(species,G,TLdict2)
  else:
    TLdict3=TLdict2

  if len(TLdict3.keys())!=len(species):
    TLdict4=nonints(species,G,TLdict3)
  else:
    TLdict4=TLdict3

  if len(TLdict4.keys())!=len(species):
    TLdict5=nonints(species,G,TLdict4)
  else:
    TLdict5=TLdict4

  if len(TLdict5.keys())!=len(species):
    TLdict6=no_canns(species,G,TLdict5)
  else:
    TLdict6=TLdict5

  if len(TLdict6.keys())!=len(species):
    TLdict7=no_canns(species,G,TLdict6)
  else:
    TLdict7=TLdict6

  unattached=set(species) - set(TLdict7.keys())
  for sp in unattached:
    preyTLs=[]
    for sp2 in G.successors(sp):
      if sp2 in TLdict7:
        preyTLs.append(TLdict7[sp2])
    meanpreyTL=float(sum(preyTLs))/float(len(preyTLs))
    print sp, meanpreyTL+1
    TLdict7[sp]=meanpreyTL+1

  # Ok so I threw some tentative TL's in based on the available stuff.
  TLdict8=tentative(species,G,TLdict7,unattached)
  # This doesn't change anything, but feels good.
  return TLdict8


def chains(web):

  

def main():

  lakes=['Alford.web','AmundsenFL.web','Bridge.web','Connery.web','LittleRock.web','PrestonFL.web','Sierra.web','Skipwith.web','Stink.web','WEB114.web','WEB116.web','WEB117.web','WEB118.web','WEB120.web','WEB126.web','WEB131.web','WEB132.web','WEB133.web','WEB134.web','WEB135.web','WEB137.web','WEB143.web','WEB204.web','WEB251.web','WEB272.web','WEB277.web','WEB278.web','WEB279.web','WEB310.web','WEB33.web','WEB334.web','WEB335.web','WEB336.web','WEB337.web','WEB343.web','WEB345.web','WEB356.web','WEB38.web','WEB39.web','WEB46.web','WEB58.web','WEB71.web','WEB72.web','WEB75.web','WEB76.web','WEB78.web','WEB84.web']  
  directory = '../mod_data/lists/pred-prey-lists-to-use/'

  for lake in lakes:
    print lake
    web=directory+lake
    TLs(web)

  metafile = '../mod_data/sup_data/food_web_notes_updated.csv'
  motifdir = '../mod_data/Roles/'
  bibfile = '../manuscript/noISN.bib'  
  webkeys = '../manuscript/webs_and_keys.csv'

  websorter(metafile,directory,bibfile,webkeys)

if __name__ == '__main__':
  main()
