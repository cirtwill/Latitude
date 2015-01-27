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
  'V':{'estuary':[],'lake':[],'marine':[],'stream':[],'terrestrial':[]}}

  f=open(rawdatafile,'r')
  for line in f:
    if line.split()[0]!='Web':
      Latitude=float(line.split('\t')[6])
      Species=int(line.split('\t')[7])
      LS=float(line.split('\t')[10])
      G=float(line.split('\t')[12])
      V=float(line.split('\t')[14])
      ecotype=line.split('\t')[1]

      points['S'][ecotype].append((Latitude,Species))
      points['LS'][ecotype].append((Latitude,LS))
      points['G'][ecotype].append((Latitude,G))
      points['V'][ecotype].append((Latitude,V))
  f.close()
  return points

def predictionreader(predfile):
  # Add slopelines (separate slopes for each type?)

  predpoints={'Lake':{0:[],45:[],75:[]},
              'Marine':{0:[],45:[],75:[]},
              'Stream':{0:[],45:[],75:[]},
              'other':{0:[],45:[],75:[]}}

  f=open(predfile,'r')
  for line in f:
    if line.split()[0]!='"Species"':
      S=int(line.split()[0])
      Lat=int(line.split()[1])
      pred=float(line.split()[-1])
      if line.split()[2]=='1':
        ecotype='Lake'
      elif line.split()[3]=='1':
        ecotype='Marine'
      elif line.split()[4]=='1':
        ecotype='Stream'
      else:
        ecotype='other'
      predpoints[ecotype][Lat].append((S,10**pred))

      #log10(prop)~log10(species)+log10(species)*...

  f.close()
  return predpoints

def latplots(rawdatafile):
  outfile1='../manuscript/Figures/properties_vs_lat.eps'
  outfile2='../manuscript/Figures/properties_vs_lat.jpg'

  data=datareader(rawdatafile)

  grace=MultiPanelGrace(colors=ColorBrewerScheme('Paired'))
  for prop in ['S','LS','G','V']:
    for ecotype in ['estuary','terrestrial','marine','lake','stream']:
      graph=grace.add_graph(Panel)

      dataset=graph.add_dataset(data[prop][ecotype])
      dataset.line.linestyle=0
      # terrdata.line.linestyle=0
      # mardata.line.linestyle=0
      # lakedata.line.linestyle=0
      # strdata.line.linestyle=0

      if ecotype=='estuary':
        dataset.symbol.configure(shape=1,color=3,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Estuary',place='opposite',char_size=.75)
      elif ecotype=='terrestrial':
        dataset.symbol.configure(shape=2,color=5,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Terrestrial',place='opposite',char_size=.75)
      elif ecotype=='marine':
        dataset.symbol.configure(shape=3,color=7,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Marine',place='opposite',char_size=.75)
      elif ecotype=='lake':
        dataset.symbol.configure(shape=4,color=9,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Lake',place='opposite',char_size=.75)
      else:
        dataset.symbol.configure(shape=5,color=11,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Stream',place='opposite',char_size=.75)


      graph.world.xmin=0
      graph.world.xmax=80
      graph.xaxis.tick.configure(major=20,minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.xaxis.ticklabel.configure(char_size=.5)
      # graph.xaxis.label.configure(text="Absolute latitude (degrees)",char_size=.75)

      graph.world.ymin=0
      if prop=='S':
        graph.world.ymax=200
        graph.yaxis.tick.major=50
        if ecotype=='estuary':
          graph.yaxis.label.text="Species richness"
      else:
        graph.world.ymax=30
        graph.yaxis.tick.major=10
        if prop=='LS' and ecotype=='estuary':
          graph.yaxis.label.text="Links/species"
        elif prop=='G' and ecotype=='estuary':
          graph.yaxis.label.text="Generality"
        elif prop=='V' and ecotype=='estuary':
          graph.yaxis.label.text="Vulnerability"

      graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.yaxis.ticklabel.configure(char_size=.5)
      graph.yaxis.label.configure(char_size=.75)
      graph.panel_label.configure(char_size=0)

  grace.multi(rows=4,cols=5,vgap=.05,hgap=.02)
  grace.set_row_xaxislabel(row=3,colspan=(0,4),label='Absolute latitude (degrees)',place='normal',just=2,char_size=.75,perpendicular_offset=0.05)
  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  grace.write_file(outfile1)

def scaleplots(rawdatafile,predfolder):
  outfile1='../manuscript/Figures/scaling_with_S.eps'
  outfile2='../manuscript/Figures/scaling_with_S.jpg'

  rawdata=datareader(rawdatafile)

  grace=MultiPanelGrace(colors=ColorBrewerScheme('Paired'))
  for prop in ['LS','Gen','Vul']:
    preddata=predictionreader(predfolder+prop+'.tsv')
    print preddata.keys()
    for ecotype in ['other','Marine','Lake','Stream']:
      graph=grace.add_graph(Panel)

      preddataset1=graph.add_dataset(preddata[ecotype][0])
      preddataset2=graph.add_dataset(preddata[ecotype][45])
      preddataset3=graph.add_dataset(preddata[ecotype][75])

      preddataset1.symbol.shape=0
      preddataset2.symbol.shape=0
      preddataset3.symbol.shape=0

      preddataset1.line.linestyle=1
      preddataset2.line.linestyle=1
      preddataset3.line.linestyle=1
      if ecotype=='other':
        preddataset1.line.color=3
        preddataset2.line.color=3
        preddataset3.line.color=3
        if prop=='LS':
          graph.xaxis.label.configure(text='Estuary & Terrestrial',place='opposite',char_size=.75)
      elif ecotype=='Marine':
        preddataset1.line.color=7
        preddataset2.line.color=7
        preddataset3.line.color=7
        if prop=='LS':
          graph.xaxis.label.configure(text='Marine',place='opposite',char_size=.75)
      elif ecotype=='Lake':
        preddataset1.line.color=9
        preddataset2.line.color=9
        preddataset3.line.color=9
        if prop=='LS':
          graph.xaxis.label.configure(text='Lake',place='opposite',char_size=.75)
      else:
        preddataset1.line.color=11
        preddataset2.line.color=11
        preddataset3.line.color=11
        if prop=='LS':
          graph.xaxis.label.configure(text='Stream',place='opposite',char_size=.75)


      graph.world.xmin=0
      graph.world.xmax=200
      graph.xaxis.tick.configure(major=50,minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.xaxis.ticklabel.configure(char_size=.5)
      # graph.xaxis.label.configure(text="Absolute latitude (degrees)",char_size=.75)

      graph.world.ymin=0
      graph.world.ymax=30
      graph.yaxis.tick.major=10
      if prop=='LS' and ecotype=='estuary':
        graph.yaxis.label.text="Links/species"
      elif prop=='Gen' and ecotype=='estuary':
        graph.yaxis.label.text="Generality"
      elif prop=='Vul' and ecotype=='estuary':
        graph.yaxis.label.text="Vulnerability"

      graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.yaxis.ticklabel.configure(char_size=.5)
      graph.yaxis.label.configure(char_size=.75)
      graph.panel_label.configure(char_size=0)

  grace.multi(rows=3,cols=4,vgap=.05,hgap=.02)
  grace.set_row_xaxislabel(row=2,colspan=(0,3),label='Species richness',place='normal',just=2,char_size=.75,perpendicular_offset=0.05)
  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  grace.write_file(outfile1)
 

def main():
  rawdatafile='../mod_data/summary-properties.tsv'
  predfolder='../mod_data/predictions/'

  # datareader(rawdatafile)
  latplots(rawdatafile)
  scaleplots(rawdatafile,predfolder)

if __name__ == '__main__':
  main()