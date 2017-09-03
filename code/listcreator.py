import sys
import os
import re
import random
from decimal import *
from math import *

backwards_webs=['../mod_data/webs/WEB294.csv']
# WEB269, WEB296, WEB320 had commas in the headers. Fixed it.
# And WEB321. And WEB322. And WEB323. And WEB324. And WEB222.

def create_predprey_list(webfile):
  # print webfile
  rowdict={}

  f=open(webfile,'r')
  for line in f:
    if webfile!='../mod_data/webs/WEB294_rotated.csv':
      newline=line.split('\n')[0] #Trims off extraneous newline
      items=newline.split(',')
      if '' not in items[1:]:
        if '0' in items[1:] or '1' in items[1:]:
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
      try:
        if rowdict[prey][position]!='0':
          # print pred, prey
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

  decodefile=open('../mod_data/species-keys/2017/'+webname,'w')
  for species in specieslist:
    decodefile.write(species+'\t'+str(numberdict[species])+'\n')
  decodefile.close()  

  eaters=[]
  links=[]
  for pred in predpreydict:
    if predpreydict[pred]!=[]:
      eaters.append(pred)
    for prey in predpreydict[pred]:
      links.append((pred,prey))
  if webfile not in backwards_webs:
    if ['phytoplankton'] in eaters or ['detritus'] in eaters or ['Detritus'] in eaters or ['Phytoplankton'] in eaters:
      print sorted(eaters)
      sys.exit()
  g=open('../mod_data/lists/pred-prey-lists-2017/'+webname,'w')
  for pred in predators:
    for prey in predpreydict[pred]:
      if webfile not in backwards_webs:
        g.write(str(numberdict[pred])+'\t'+str(numberdict[prey])+'\n')
      else:
        g.write(str(numberdict[prey])+'\t'+str(numberdict[pred])+'\n')        
  g.close()

def weblister(directory):
  filelist=os.listdir(directory)
  # 361 webs total.
  if directory == '../mod_data/webs':
    for fil in filelist:
      # if fil not in os.listdir('../mod_data/lists/pred-prey-lists-to-use'):
      create_predprey_list(directory+'/'+fil)
  else:
    for fil in filelist:
      if fil.split('.')[1]!='web':
        filname=fil
        create_predprey_list(directory+'/'+filname)

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


def main():

  for directory in ['../mod_data/webs']:
    weblister(directory)

  directory2 = '../mod_data/lists/pred-prey-lists-2017'

  dupchecker(directory2)


if __name__ == '__main__':
  main()
