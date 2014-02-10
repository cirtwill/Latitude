import sys
import os
import re
import random
from decimal import *
from math import *

def create_aggregated_web(fillist,directory):

  predpreydict={}
  specieslist=set()
  for fil in fillist:
    rowdict={}

    webfile = directory+'/'+fil+'.csv'
    f=open(webfile,'r')
    for line in f:
      newline=line.split('\n')[0] #Trims off extraneous newline
      items=newline.split(',')
      if '' not in items[1:]:
        rowdict[items[0]]=items[1:]
    f.close()

    for row in rowdict:
      for item in rowdict[row]:
        try:
          item = int(item)
        except:
          predators = row

    for item in rowdict[predators]:
      position = rowdict[predators].index(item)
      specieslist.add(item)
      preylist=[]
      for row in rowdict:
        specieslist.add(row)
        if row != predators:
          try:
            if rowdict[row][position]!='0':
              preylist.append(row)
          except:
            print fil, row
            print rowdict[predators][position], position
          
      predpreydict[item]=preylist

  numberdict={}
  i = 1
  for species in specieslist:
    numberdict[species]=i
    i=i+1

  webname=re.findall(r'WEB(\d+)',webfile)[0]

  decodefile=open('Food-web-database/species-keys/agg-webs/'+webname,'w')
  for species in specieslist:
    decodefile.write(species+'\t'+str(numberdict[species])+'\n')
  decodefile.close()  

  g=open('Food-web-database/pred-prey-lists-to-use/WEB'+str(1000+int(webname)),'w')
  for pred in rowdict[predators]:
    for prey in predpreydict[pred]:
      g.write(str(numberdict[pred])+'\t'+str(numberdict[prey])+'\n')
  g.close()

def create_predprey_list(webfile):

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
  
  rowdict={}

  f=open(webfile,'r')
  for line in f:
    newline=line.split('\n')[0] #Trims off extraneous newline
    items=newline.split(',')
    if '' not in items[1:]:
      rowdict[items[0]]=items[1:]
  f.close()

  for row in rowdict:
    for item in rowdict[row]:
      try:
        item = int(item)
      except:
        predators = row
  
  predpreydict={}
  specieslist=set()

  for item in rowdict[predators]:
    specieslist.add(item)
    preylist=[]
    position = rowdict[predators].index(item)
    for row in rowdict:
      specieslist.add(row)
      if row != predators:
        try:
          if rowdict[row][position]!='0':
            preylist.append(row)
        except:
          print webfile, rowdict[predators][position]
    predpreydict[item]=preylist

  numberdict={}
  i = 1
  for species in specieslist:
    numberdict[species]=i
    i=i+1

  webname=re.findall(r'(WEB\d+)',webfile)[0]

  decodefile=open('Food-web-database/species-keys/all-webs/'+webname,'w')
  for species in specieslist:
    decodefile.write(species+'\t'+str(numberdict[species])+'\n')
  decodefile.close()  

  g=open('Food-web-database/pred-prey-lists/'+webname,'w')
  for pred in rowdict[predators]:
    for prey in predpreydict[pred]:
      g.write(str(numberdict[pred])+'\t'+str(numberdict[prey])+'\n')
  g.close()


  if webname not in agglist:
    h=open('Food-web-database/pred-prey-lists-to-use/'+webname,'w')
    for pred in rowdict[predators]:
      for prey in predpreydict[pred]:
        h.write(str(numberdict[pred])+'\t'+str(numberdict[prey])+'\n')
    h.close()
  else:
    print webname
    pass


def weblister(directory):

  filelist=os.listdir(directory)

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


  for fil in filelist:
    filname=fil
    create_predprey_list(directory+'/'+filname)

  for fillist in aggregators:
    create_aggregated_web(fillist,directory)

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
  
  directory = 'Food-web-database/csv_webs'

  directory2 = 'Food-web-database/pred-prey-lists-to-use'
  directory3 = 'Food-web-database/pred-prey-lists'

  weblister(directory)

  for dirname in [directory2,directory3]:
    dupchecker(dirname)

if __name__ == '__main__':
  main()
