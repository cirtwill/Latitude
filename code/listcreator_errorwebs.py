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
    if webfile not in ['../mod_data/Error_webs_corrected/WEB294.csv','../mod_data/Error_webs_corrected/WEB333.csv']:
      newline=line.split('\n')[0] #Trims off extraneous newline
      items=newline.split(',')
      if '' not in items[1:]:
        if '0' in items[1:] or '1' in items[1:]:
          rowdict[items[0]]=items[1:]
        else:
          predators=items[1:]
    else:
      newline=line.split('\r')
      for item in newline:
        items=item.split(',')
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
            print webfile, pred, 'error'
    predpreydict[pred]=preylist
  numberdict={}
  i = 1

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
  for fil in filelist:
    print fil
    # if fil not in os.listdir('../mod_data/lists/pred-prey-lists-to-use'):
    create_predprey_list(directory+fil)


if __name__ == '__main__':
  main()
