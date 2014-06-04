import sys
import os
import re
import random
from decimal import *
import math

from PyGrace.grace import Grace
from PyGrace.colors import RandomColorScheme, MarkovChainColorScheme, ColorBrewerScheme
from PyGrace.dataset import SYMBOLS
from PyGrace.Extensions.panel import Panel,MultiPanelGrace
from PyGrace.drawing_objects import DrawText

#Mean dist from centroids
def linereader(fw_prop):
  linefile="../mod_data/"+fw_prop+"_coeffs"
  linetab=open(linefile,'r')
  lines={}
  for line in linetab:
    if len(line.split())==5:
      effect=line.split()[0][1:-1]
      beta=float(line.split()[1])
      lines[effect]=beta
  return lines

def datareader(fw_prop):
  datafile="../mod_data/summary-properties.tsv"
  f=open(datafile,'r')
  datadict={'estuary':{},'lake':{},'terrestrial':{},'marine':{},'stream':{}}
  for line in f:
    if line.split()[0]=='Web':
      heads=line.split()[1:]
    else:
      web=line.split()[0]
      data=line.split()[1:]
      Ecotype=line.split()[1]
      if Ecotype in datadict:
        datadict[Ecotype][web]={}
        for term in range(0,len(heads)):
          if term!='Ecotype':
            datadict[Ecotype][web][heads[term]]=data[term]
  return datadict

def plotter(fw_prop):
  datadict=datareader(fw_prop)
  lines=linereader(fw_prop)
  print lines
  realdata={}
  for webtype in datadict:
    realdata[webtype]={'Species':[],'Connectance':[],'Latitude':[],fw_prop:[]}
    for web in datadict[webtype]:
      for term in realdata[webtype]:
        realdata[webtype][term].append(datadict[webtype][web][term])

  

      

  sys.exit()

  distfile=open("../data/Dispersion/"+flavour+"/Species/distances.tsv",'r')
  splitdists={'B':{},'I':{},'T':{},'Pp':{},'Pc':{}}
  for line in distfile:
    if len(line.split())==3:
      ID=line.split()[0][1:-1]
      site=ID[0]
      webtype=ID.split(':')[1]
      if len(ID.split(':'))==3:
        TL=ID.split(':')[2]
      elif len(ID.split(':'))==2:
        site=ID.split(':')[1]
        if ID[0]!='P':
          TL=ID[0]
        else:
          TL=ID[:2]

      dist=float(line.split()[2])
      sp=ID.split(':')[0][1:]

    elif len(line.split())==2:
      if line.split()[0]!='"V1"':
        ID=line.split()[0][1:-1]
        site=ID[0]
        webtype=ID.split(':')[1]
        TL=ID.split(':')[2]

        dist=float(line.split()[1])
        sp=ID.split(':')[0][1:]
      else:
        TL=''
    else:
      TL=''

    if TL not in ['P','']:
      if not site in splitdists[TL]:
        splitdists[TL][site]=[dist]
      else:
        splitdists[TL][site].append(dist)          
    if TL=='P':
      if webtype=='parasite':
        if not site in splitdists['Pp']:
          splitdists['Pp'][site]=[dist]
        else:
          splitdists['Pp'][site].append(dist)          
      elif webtype=='con':
        if not site in splitdists['Pc']:
          splitdists['Pc'][site]=[dist]
        else:
          splitdists['Pc'][site].append(dist)          
  distfile.close()

  meandict={'B':{},'I':{},'T':{},'Pp':{},'Pc':{}}
  for TL in splitdists:
    for site in splitdists[TL]:
      mean=float(Decimal(sum(splitdists[TL][site]))/Decimal(len(splitdists[TL][site])))
      SDconts=[]
      for item in splitdists[TL][site]:
        SDconts.append((item-mean)**2)
      SD=math.sqrt(sum(SDconts)/(len(SDconts)-1))
      n=len(splitdists[TL][site])
      meandict[TL][site]=(mean,SD,n)

  if flavour=='CA':
    rawfile=open('../data/Dispersion/'+flavour+'/Species/betadisper_rawdispers_4lm.tsv','w')
  else:
    rawfile=open('../data/Dispersion/'+flavour+'/Species/rawdispers_4lm.tsv','w')
  rawfile.write('group\tTL\tsize\tdisp\tSD\tsite\tB\tI\tT\tPp\tPc\n')

  for TL in meandict:
    if TL=='B':
      tail='\t1\t0\t0\t0\t0'
    elif TL=='I':
      tail='\t0\t1\t0\t0\t0'
    elif TL=='T':
      tail='\t0\t0\t1\t0\t0'
    elif TL=='Pp':      
      tail='\t0\t0\t0\t1\t0'
    elif TL=='Pc':
      tail='\t0\t0\t0\t0\t1'
    for site in meandict[TL]:
      rawfile.write(TL+site+'\t'+TL+'\t'+str(meandict[TL][site][2])+'\t'+str(float(meandict[TL][site][0]))+'\t'+str(meandict[TL][site][2])+'\t'+str(site)+tail+'\n')
  rawfile.close()

  return meandict


def main():
  fw_prop='mean_SWTL'
  plotter(fw_prop)

if __name__ == '__main__':
  main()