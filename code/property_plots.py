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

# Think I want plots of the raw data (S, LS, G, V vs. lat) with slope lines
# Plus predictions for LS, G, V vs S (with corrected obs.)

def datareader(rawdatafile):
  points={'S':{'estuary':[],'lake':[],'marine':[],'stream':[],'terrestrial':[]},
  'LS':{'estuary':[],'lake':[],'marine':[],'stream':[],'terrestrial':[]},
  'G':{'estuary':[],'lake':[],'marine':[],'stream':[],'terrestrial':[]},  
  'V'{'estuary':[],'lake':[],'marine':[],'stream':[],'terrestrial':[]}}

  f=open(rawdatafile,'r')
  for line in f:
    if line.split()[0]!='Web':
      Latitude=float(line.split()[6])
      Species=int(line.split()[7])
      LS=float(line.split()[10])
      G=float(line.split()[12])
      V=float(line.split()[14])
      ecotype=line.split()[1]

      points['S'][ecotype].append((Latitude,Species))
      points['LS'][ecotype].append((Latitude,LS))
      points['G'][ecotype].append((Latitude,G))
      points['V'][ecotype].append((Latitude,V))
  f.close()
  return points

def predictionreader(predfile):

  predpoints={'Lake':{0:[],45:[],75:[]},
              'Marine':{0:[],45:[],75:[]},
              'Stream':{0:[],45:[],75:[]},
              'other':{0:[],45:[],75:[]}}

  f=open(predfile,'r')
  for line in f:
    if line.split()[0]!='Species':
      S=int(line.split()[0])
      Lat=int(line.split()[1])
      pred=float(line.split()[-1])
      if line.split()[2]==1:
        ecotype='Lake'
      elif line.split()[3]==1:
        ecotype='Marine'
      elif line.split()[4]==1:
        ecotype='Stream'
      else:
        ecotype='other'
      predpoints[ecotype][Lat].append((S,pred))
  f.close()

  return predpoints

def latplots(rawdatafile):
  outfile1='../manuscript/Figures/properties_vs_lat.eps'
  outfile2='../manuscript/Figures/properties_vs_lat.jpg'

  data=datareader(rawdatafile)

  grace=MultiPanelGrace()
  for prop in ['S','LS','G','V']:
    graph=grace.add_graph(panel)

    estdata=graph.add_dataset(data[prop]['estuary'])
    terrdata=graph.add_dataset(data[prop]['terrestrial'])
    


def main():
  rawdatafile='../mod_data/summary-properties.tsv'
  predfolder='../mod_data/predictions/'


if __name__ == '__main__':
  main()