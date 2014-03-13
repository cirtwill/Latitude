import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *

#look up tools to build webs.

def understands_motif_file(directory,filename):
  motifdict={}
  f=open(directory+filename,'r')
  for line in f:
    motif=line.split()[0]
    count=line.split()[1]
    motifdict[motif]=count
  f.close()
  return motifdict

def wrapper(directory):
  files = os.listdir(directory)
  motiffiles=[]
  for filename in files:
    if re.findall('.motifs',filename)!=[]:
      motiffiles.append(filename)

  bigdict={}
  for filename in motiffiles:
    smalldict=understands_motif_file(directory,filename)
    bigdict[filename.split('.')[0]]=smalldict
  return bigdict

  

def main():
  
  directory = '../mod_data/Roles/'

  wrapper(directory)

if __name__ == '__main__':
  main()
