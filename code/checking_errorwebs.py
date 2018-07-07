import sys
import os
import re
import random
import networkx as nx
from decimal import *
import numpy as np
from math import *

dubious_webs=['WEB218','WEB220','WEB225','WEB236','WEB238','WEB240','WEB244','WEB246','WEB251','WEB256','WEB307','WEB33','WEB345','WEB350','WEB351','WEB357','WEB359','WEB41','WEB80','WEB214','WEB215','WEB216','WEB217','WEB219','WEB221','WEB222','WEB223','WEB224','WEB291','WEB292','WEB342','WEB114','WEB117','WEB119','WEB120','WEB153','WEB154','WEB18','WEB180','WEB181','WEB182','WEB183','WEB202','WEB213','WEB213','WEB22','WEB23','WEB23','WEB24','WEB249','WEB250','WEB252','WEB253','WEB254','WEB254','WEB255','WEB255','WEB257','WEB257','WEB258','WEB258','WEB259','WEB259','WEB26','WEB260','WEB260','WEB261','WEB261','WEB262','WEB262','WEB265','WEB265','WEB266','WEB267','WEB268','WEB269','WEB269','WEB288','WEB289','WEB290','WEB290','WEB293','WEB293','WEB294','WEB296','WEB296','WEB320','WEB320','WEB321','WEB321','WEB322','WEB322','WEB323','WEB323','WEB324','WEB324','WEB333','WEB333','WEB339','WEB339','WEB37','WEB38','WEB39','WEB39','WEB60']
# # Taking too long to recalculate. Will be better to just
# # read in the datafile and trim these.
corrected_webs=[]

def create_predprey_list(webfile):
  # print webfile
  rowdict={}
  f=open(webfile,'r')
  for line in f:
    if webfile not in ['../mod_data/webs/WEB294_rotated.csv']:
      newline=line.split('\n')[0] #Trims off extraneous newline
      items=newline.split(',')
      if '' not in items[1:]:
        if webfile!='../mod_data/webs/WEB333.csv':
          if '0' in items[1:] or '1' in items[1:] or '0.5' in items[1:] or '0.0005550116' in items[1:]:
            rowdict[items[0]]=items[1:]
          else:
            predators=items[1:]
        else:
          if '0' in items[1:] or '+' in items[1:] or '-' in items[1:]: # Some webs just gotta be special
            rowdict[items[0]]=items[1:]
          else:
            predators=items[1:]
    else:
      mush=line.split('\r')
      for newline in mush:
        items=newline.split(',')
        if '' not in items[1:]:
          if '0' in items[1:] or '1' in items[1:]:
            rowdict[items[0]]=items[1:]
          else:
            predators=items[1:]
  f.close()
  predpreydict={}
  specieslist=set()
  # print webfile
  for pred in predators:
    specieslist.add(pred)
    preylist=[]
    position = predators.index(pred)
    for prey in rowdict:
      specieslist.add(prey)
      if prey in preylist:
        print web, ' has duplicate species'
      if webfile not in ['../mod_data/webs/WEB333.csv','../mod_data/Error_webs/WEB333.csv']:
        try:
          if float(rowdict[prey][position])>0:
            # print float(rowdict[prey][position])
            preylist.append(prey)
            if float(rowdict[prey][position])>1:
              print pred, prey
        except:
          try:
            if float(rowdict[prey][position][1:])>0: # One web has <0.01
              preylist.append(prey)
          except:
            try:
              if float(rowdict[prey][position][0])>0: # Another web has letters to indicate life stages.
                preylist.append(prey)
            except:
              print webfile, pred, 'error'
        try:
          if float(rowdict[prey][position])<0:
            print prey, pred, rowdict[prey][position]
        except:
          pass
      else:
        if '-' in rowdict[prey][position]:
          preylist.append(prey)    
    predpreydict[pred]=preylist
  # print predpreydict
  print sorted(predators)
  print sorted(specieslist)
  # print sorted(predpreydict.keys())
  # print sorted(specieslist)
  # print sorted(predpreydict.values())
  print webfile
  sys.exit()
  # numberdict={}
  # i = 1

  # for species in specieslist:
  #   numberdict[species]=i
  #   i=i+1

  # try:
  #   webname=re.findall(r'(WEB\w+)',webfile)[0]+'.web'
  # except:
  #   webname=webfile.split('/')[-1].split('.')[0] 
  #   # print webname
  # decodefile=open('../mod_data/species-keys/2017/'+webname,'w')
  # for species in specieslist:
  #   decodefile.write(species+'\t'+str(numberdict[species])+'\n')
  # decodefile.close()  
  # eaters=[]
  # links=[]
  # for pred in predpreydict:
  #   if predpreydict[pred]!=[]:
  #     eaters.append(pred)
  #   for prey in predpreydict[pred]:
  #     links.append((pred,prey))
  # if webfile not in backwards_webs:
  #   if set(eaters)&set(['phytoplankton','Phytoplankton','detritus','Detritus'])!=set():
  #     print "Error: basal resources are eating things"
  #     print predpreydict['detritus']
  #     print sorted(eaters)
  #     print webfile
  #     sys.exit()
  # g=open('../mod_data/lists/pred-prey-lists-2017/'+webname,'w')
  # for pred in predators:
  #   for prey in predpreydict[pred]:
  #     if webfile not in backwards_webs:
  #       g.write(str(numberdict[pred])+'\t'+str(numberdict[prey])+'\n')
  #     else:
  #       g.write(str(numberdict[prey])+'\t'+str(numberdict[pred])+'\n')        
  # g.close()

#Short function to make sure there are no duplicate links.
def dupchecker(dirname):
  filelist=os.listdir(dirname)
  print dirname
  for fil in filelist:
    links=set()
    f=open(dirname+'/'+fil,'r')
    for line in f:
      (pred,prey)=line.split()
      links.add((pred,prey))
    f.close()
    f=open(dirname+'/'+fil,'w')
    for link in links:
      f.write(link[0]+'\t'+link[1]+'\n')
    f.close()


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

def main():

  for web in ['../mod_data/Error_webs_corrected/WEB80.csv']:
    create_predprey_list(web)
    sys.exit()
  for web in sorted(os.listdir('../mod_data/Error_webs/'))[81:]:
    print web
    create_predprey_list('../mod_data/Error_webs/'+web)
    print web
  sys.exit()
  directory2 = '../mod_data/lists/pred-prey-lists-2017'

  dupchecker(directory2)
  
  metafile = '../mod_data/sup_data/food_web_notes_updated.csv'
  directory = '../mod_data/lists/pred-prey-lists-2017/'
  motifdir = '../mod_data/Roles/'
  bibfile = '../manuscript/noISN.bib'  
  webkeys = '../manuscript/webs_and_keys.csv'

  for web in sorted(dubious_webs):
    print web
    create_web()

  websorter(metafile,directory,bibfile,webkeys)

if __name__ == '__main__':
  main()
