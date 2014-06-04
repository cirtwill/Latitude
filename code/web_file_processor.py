import sys
import os
import re
import random
import networkx as nx
from decimal import *
from math import *

#look up tools to build webs.

def understands_matrices(filename):
  print filename
  namekey={}
  interactions=[]

  f=open(filename,'r')
  for line in f:
    newline=line.split('\n')[0].split(',')
    if len(newline)==1:
      newline=newline[0].split('\t')
    if filename=='../mod_data/webs/WEB5.csv':
      newlines=[]
      for term in newline:
        items=term.split('-')
        for item in items:
          if item[0]=='"':
            item=item[1:]
          if item[-1]=='"':
            item=item[:-1]
          newlines.append(item)
      newline=newlines
    if newline[-2]!='':   #This isn't the useless first line
      try:
        test=float(newline[-2])
      except:
        predline=newline
  f.close()

  for i in range(1,len(predline)):
    namekey[predline[i]]=i

  f=open(filename,'r')
  for line in f:
    newline=line.split('\n')[0].split(',')
    if len(newline)==1:
      newline=newline[0].split('\t')
    if filename=='../mod_data/webs/WEB5.csv':
      newlines=[]
      for term in newline:
        items=term.split('-')
        for item in items:
          if item[0]=='"':
            item=item[1:]
          if item[-1]=='"':
            item=item[:-1]
          newlines.append(item)
      newline=newlines
    if newline!=predline and newline[-2]!='':
      prey=newline[0]
      if prey in namekey:
        preynum=namekey[prey]
      else:
        preynum=sorted(namekey.values())[-1]+1
        namekey[prey]=preynum

      for j in range(1,len(newline)):
        interaction=newline[j]
        if interaction!='0':
          prednum=namekey[predline[j]]
          preynum=namekey[prey]
          predname=predline[j]
          interactions.append((prednum,preynum))
  f.close()
  return interactions

def processor(filename):
  interactions=understands_matrices(filename)
  print interactions



def main():
  
  directory = '../mod_data/webs/'
  files=os.listdir(directory)
  for fil in files:
    processor(directory+fil)

if __name__ == '__main__':
  main()
