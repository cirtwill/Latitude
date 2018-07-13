import sys
import os
import re
import random
from decimal import *
from math import *
import numpy as np

# Making lists for webs Bernat flagged as errors. Some should be same, others will not be.
def create_predprey_list(webfile):
  # print webfile
  rowdict={}
  f=open(webfile,'r')
  for line in f:
    newline=line.split('\n')[0] #Trims off extraneous newline
    items=newline.split(',')
    if '' not in items[1:]:
      if '0' in items[1:] or '1' in items[1:]:
        rowdict[items[0]]=items[1:]
      else:
        predators=items[1:]
  f.close()
  predpreydict={}
  specieslist=set()
  for pred in predators:
    specieslist.add(pred)
    preylist=[]
    position = predators.index(pred)
    for prey in rowdict:
      specieslist.add(prey)
      if prey in preylist:
        print web, ' has duplicate species'
      try:
        if float(rowdict[prey][position])>0:
          # print pred, prey
          preylist.append(prey)
      except:
        try:
          if float(rowdict[prey][position][1:])>0: # One web has <0.01
            preylist.append(prey)
        except:
          try:
            if float(rowdict[prey][position][0])>0: # Another web has letters to indicate life stages.
              preylist.append(prey)
          except:
            print rowdict[prey], prey, len(rowdict[prey])            
            print webfile, pred, 'error', position
    predpreydict[pred]=preylist
  numberdict={}
  i = 1
  # Gotta finish checking these, check them the other way.
  if set(rowdict.keys())&set(predators)!=set(rowdict.keys()):
  # if set(predators)&set(rowdict.keys())!=set(predators):
    web=webfile.split('WEB')[-1].split('.')[0]
    # Some webs are known to by asymmetric. That's fine.
    # Webs with extra resources:
    if web not in ['114','117','119','120','153','154','18','180','181','182','183','202','213','214','215','216','217','218','219','22','220','221','222','223','224','225','23','236','238','24','240','244','246','249','250','251','252','253','254','255','256','257','258','259','26','260','261','262','265','266','267','268','269','288','289','290','291','292','293','307','320','321','322','323','324','33','339','342','350','351','37','38','39','41','60','80']:
    # Webs with extra predators:
    # if web not in ['114','117','119','120','153','154','18','180','181','182','183','202','213','22','220','221','23','236','238','24','26','290','291','292','293','307','33','342','350','37','38','39','41','60','80']:
      print sorted(set(predators)-set(rowdict.keys())), sorted(set(rowdict.keys())-set(predators))
      print webfile
      sys.exit()
  for species in specieslist:
    numberdict[species]=i
    i=i+1

  try:
    webname=re.findall(r'(WEB\w+)',webfile)[0]+'.web'
  except:
    webname=webfile.split('/')[-1].split('.')[0] 
    # print webname
  decodefile=open('../mod_data/species-keys/2018/'+webname,'w')
  for species in specieslist:
    decodefile.write(species+'\t'+str(numberdict[species])+'\n')
  decodefile.close()  
  eaters=[]
  links=set()
  for pred in predpreydict:
    if predpreydict[pred]!=[]:
      eaters.append(pred)
    for prey in predpreydict[pred]:
      links.add((pred,prey))
  if set(eaters)&set(['phytoplankton','Phytoplankton','detritus','Detritus'])!=set():
    print "Error: basal resources are eating things"
    if 'detritus' in predpreydict:
      print predpreydict['detritus']
    print sorted(eaters)
    print webfile
    sys.exit()
  g=open('../mod_data/lists/pred-prey-lists-2018-errorwebs/'+webname,'w')
  for (pred,prey) in links:
    g.write(str(numberdict[pred])+'\t'+str(numberdict[prey])+'\n')
  g.close()

def main():

  directory='../mod_data/Error_webs_corrected/'
  filelist=os.listdir(directory)
  # for fil in ['WEB265.csv']:
  for fil in filelist:
    print fil
    # if fil not in os.listdir('../mod_data/lists/pred-prey-lists-to-use'):
    create_predprey_list(directory+fil)


if __name__ == '__main__':
  main()
