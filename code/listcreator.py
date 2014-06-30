import sys
import os
import re
import random
from decimal import *
from math import *

def create_predprey_list(webfile):
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

  print webfile

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
          print webfile, rowdict[predators][position], 'error'
    predpreydict[item]=preylist

  numberdict={}
  i = 1
  for species in specieslist:
    numberdict[species]=i
    i=i+1

  try:
    webname=re.findall(r'(WEB\w+)',webfile)[0]
  except:
    webname='Carpinteria2006' 

  decodefile=open('../mod_data/species-keys/all-webs/'+webname,'w')
  for species in specieslist:
    decodefile.write(species+'\t'+str(numberdict[species])+'\n')
  decodefile.close()  

  g=open('../mod_data/lists/pred-prey-lists-to-use/'+webname,'w')
  for pred in rowdict[predators]:
    for prey in predpreydict[pred]:
      g.write(str(numberdict[pred])+'\t'+str(numberdict[prey])+'\n')
  g.close()

def weblister(directory):
  filelist=os.listdir(directory)
  if directory == '../mod_data/webs':
    for fil in filelist:
      if fil not in os.listdir('../mod_data/lists/pred-prey-lists-to-use'):
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

  directory2 = '../mod_data/lists/pred-prey-lists-to-use'

  dupchecker(directory2)


if __name__ == '__main__':
  main()
