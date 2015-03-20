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
from PyGrace.axis import LINEAR_SCALE, LOGARITHMIC_SCALE

from PyGrace.Extensions.colorbar import SolidRectangle, ColorBar
from PyGrace.Styles.el import ElGraph, ElLinColorBar, ElLogColorBar

# Think I want plots of the raw data (S, LS, G, V vs. lat) with slope lines
# Plus predictions for LS, G, V vs S (with corrected obs.)

def datareader(rawdatafile):
  points={'S':{'Estuary':[],'Lake':[],'Marine':[],'Stream':[],'Terrestrial':[]},
  'LS':{'Estuary':[],'Lake':[],'Marine':[],'Stream':[],'Terrestrial':[]},
  'Gen':{'Estuary':[],'Lake':[],'Marine':[],'Stream':[],'Terrestrial':[]},  
  'Vul':{'Estuary':[],'Lake':[],'Marine':[],'Stream':[],'Terrestrial':[]},
  'B':{'Estuary':[],'Lake':[],'Marine':[],'Stream':[],'Terrestrial':[]},
  'I':{'Estuary':[],'Lake':[],'Marine':[],'Stream':[],'Terrestrial':[]},
  'T':{'Estuary':[],'Lake':[],'Marine':[],'Stream':[],'Terrestrial':[]}
  }

  f=open(rawdatafile,'r')
  for line in f:
    if line.split()[0]!='Web':
      Latitude=float(line.split('\t')[6])
      Species=int(line.split('\t')[7])
      LS=float(line.split('\t')[10])
      G=float(line.split('\t')[12])
      V=float(line.split('\t')[14])
      ecotype=line.split('\t')[1]
      ecotype=ecotype.capitalize()
      B=float(line.split('\t')[-4])
      I=float(line.split('\t')[-2])
      T=float(line.split('\t')[-1])

      points['S'][ecotype].append((Latitude,Species))
      points['LS'][ecotype].append((Latitude,LS))
      points['Gen'][ecotype].append((Latitude,G))
      points['Vul'][ecotype].append((Latitude,V))
      points['B'][ecotype].append((Latitude,B))
      points['I'][ecotype].append((Latitude,I))
      points['T'][ecotype].append((Latitude,T))

  f.close()
  return points

def coeffreader(rawdatafile,Bformat,TL):
  if rawdatafile=='../non_TS/summary-properties.tsv':
    codir='../non_TS/'+Bformat+'/coefficients/'
  else:
    codir='../mod_data/'+Bformat+'/coefficients/'

  coefficients={}

  cofile=open(codir+TL+'_lat.tsv')
  for line in cofile:
    if len(line.split())==5:
      name=line.split()[0][1:-1]
      value=float(line.split()[1])
      coefficients[name]=value

  return coefficients

def latplots(rawdatafile,outfile,Bformat):
  data=datareader(rawdatafile)

  grace=MultiPanelGrace(colors=ColorBrewerScheme('Paired'))
  for prop in ['S','LS','Gen','Vul']:
    coefficients=coeffreader(rawdatafile,Bformat,prop)
    print coefficients.keys()
    for ecotype in ['Estuary','Terrestrial','Marine','Lake','Stream']:
      graph=grace.add_graph(Panel)


      #########################################################################################
      ###   Best-fit lines
      #########################################################################################
      intercept=coefficients['(Intercept)']
      if ecotype in coefficients:
        intercept=intercept+coefficients[ecotype]

      if 'Latitude:'+ecotype in coefficients:
        slope=coefficients['Latitude:'+ecotype]
      elif ecotype+':Latitude' in coefficients:
        slope=coefficients[ecotype+':Latitude']
      else:
        slope=0

      preds=[]
      for i in range(0,81):
        pred=intercept+i*slope
        preds.append((i,pred))

      predline=graph.add_dataset(preds)
      predline.symbol.configure(shape=0)
      predline.line.configure(linestyle=1,color=1)


      dataset=graph.add_dataset(data[prop][ecotype])
      dataset.line.linestyle=0
      #########################################################################################
      ###   Labels and junk
      #########################################################################################
      dataset.symbol.configure(shape=1,color=1,fill_color=3,linewidth=.5)
      if ecotype=='Estuary':
        if prop=='S':
          graph.xaxis.label.configure(text='Estuary',place='opposite',char_size=.75)
      elif ecotype=='Terrestrial':
        # dataset.symbol.configure(shape=2,color=5,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Terrestrial',place='opposite',char_size=.75)
      elif ecotype=='Marine':
        # dataset.symbol.configure(shape=3,color=7,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Marine',place='opposite',char_size=.75)
      elif ecotype=='Lake':
        # dataset.symbol.configure(shape=4,color=9,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Lake',place='opposite',char_size=.75)
      else:
        # dataset.symbol.configure(shape=5,color=11,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Stream',place='opposite',char_size=.75)

      graph.world.xmin=0
      graph.world.xmax=80
      graph.xaxis.tick.configure(major=20,minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.xaxis.ticklabel.configure(char_size=.75)
      # graph.xaxis.label.configure(text="Absolute latitude (degrees)",char_size=.75)

      graph.world.ymin=0
      if prop=='S':
        graph.world.ymax=200
        graph.yaxis.tick.major=50
        if ecotype=='Estuary':
          graph.yaxis.label.text="Species"
      else:
        graph.world.ymax=30
        graph.yaxis.tick.major=10
        if prop=='LS' and ecotype=='Estuary':
          graph.yaxis.label.text="Links/species"
        elif prop=='Gen' and ecotype=='Estuary':
          graph.yaxis.label.text="Generality"
        elif prop=='Vul' and ecotype=='Estuary':
          graph.yaxis.label.text="Vulnerability"

      graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.yaxis.ticklabel.configure(char_size=.75)
      graph.yaxis.label.configure(char_size=1)
      graph.panel_label.configure(char_size=0)
      graph.frame.linewidth=1
      graph.xaxis.bar.linewidth=1
      graph.yaxis.bar.linewidth=1
  grace.multi(rows=4,cols=5,vgap=.05,hgap=.04)
  grace.set_row_xaxislabel(row=3,colspan=(0,4),label='Absolute latitude (degrees)',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  grace.write_file(outfile)


def TLplots(rawdatafile,outfile,Bformat):
  data=datareader(rawdatafile)

  grace=MultiPanelGrace(colors=ColorBrewerScheme('Paired'))
  for prop in ['S','B','I','T']:
    coefficients=coeffreader(rawdatafile,Bformat,prop)
    print coefficients.keys()
    for ecotype in ['Estuary','Terrestrial','Marine','Lake','Stream']:
      graph=grace.add_graph(Panel)


      #########################################################################################
      ###   Best-fit lines
      #########################################################################################
      intercept=coefficients['(Intercept)']
      if ecotype in coefficients:
        intercept=intercept+coefficients[ecotype]

      if 'Latitude:'+ecotype in coefficients:
        slope=coefficients['Latitude:'+ecotype]
      elif ecotype+':Latitude' in coefficients:
        slope=coefficients[ecotype+':Latitude']
      else:
        slope=0

      preds=[]
      for i in range(0,81):
        pred=intercept+i*slope
        preds.append((i,pred))

      predline=graph.add_dataset(preds)
      predline.symbol.configure(shape=0)
      predline.line.configure(linestyle=1,color=1)


      dataset=graph.add_dataset(data[prop][ecotype])
      dataset.line.linestyle=0
      #########################################################################################
      ###   Labels and junk
      #########################################################################################
      dataset.symbol.configure(shape=1,color=1,fill_color=3,linewidth=.5)
      if ecotype=='Estuary':
        if prop=='S':
          graph.xaxis.label.configure(text='Estuary',place='opposite',char_size=.75)
      elif ecotype=='Terrestrial':
        # dataset.symbol.configure(shape=2,color=5,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Terrestrial',place='opposite',char_size=.75)
      elif ecotype=='Marine':
        # dataset.symbol.configure(shape=3,color=7,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Marine',place='opposite',char_size=.75)
      elif ecotype=='Lake':
        # dataset.symbol.configure(shape=4,color=9,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Lake',place='opposite',char_size=.75)
      else:
        # dataset.symbol.configure(shape=5,color=11,fill_color=0)
        if prop=='S':
          graph.xaxis.label.configure(text='Stream',place='opposite',char_size=.75)

      graph.world.xmin=0
      graph.world.xmax=80
      graph.xaxis.tick.configure(major=20,minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.xaxis.ticklabel.configure(char_size=.75)
      # graph.xaxis.label.configure(text="Absolute latitude (degrees)",char_size=.75)

      graph.world.ymin=0
      if prop=='S':
        graph.world.ymax=200
        graph.yaxis.tick.major=50
        if ecotype=='Estuary':
          graph.yaxis.label.text="Species"
      else:
        graph.world.ymax=1
        graph.yaxis.tick.major=.2
        if prop=='B' and ecotype=='Estuary':
          graph.yaxis.label.text="Basal"
        elif prop=='I' and ecotype=='Estuary':
          graph.yaxis.label.text="Intermediate"
        elif prop=='T' and ecotype=='Estuary':
          graph.yaxis.label.text="Top"

      graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.yaxis.ticklabel.configure(char_size=.75)
      graph.yaxis.label.configure(char_size=1)
      graph.panel_label.configure(char_size=0)
      graph.frame.linewidth=1
      graph.xaxis.bar.linewidth=1
      graph.yaxis.bar.linewidth=1
  grace.multi(rows=4,cols=5,vgap=.05,hgap=.04)
  grace.set_row_xaxislabel(row=3,colspan=(0,4),label='Absolute latitude (degrees)',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  grace.write_file(outfile)


def main():

  # TS datafile
  # rawdatafile='../mod_data/summary-properties.tsv'
  # Raw datafile

  for Bformat in ['proportions','numbers']:

    for rawdatafile in ['../non_TS/summary-properties.tsv']:#,'../mod_data/summary-properties.tsv']:
      if rawdatafile=='../non_TS/summary-properties.tsv':
        outfile='../manuscript/Figures/by_TL/properties_vs_lat/'+Bformat+'/nonts_propsvlat.eps'
      else:
        outfile='../manuscript/Figures/by_TL/properties_vs_lat/'+Bformat+'/ts_propsvlat.eps'

      latplots(rawdatafile,outfile,Bformat)
    
      if rawdatafile=='../non_TS/summary-properties.tsv':
        outfile2='../manuscript/Figures/by_TL/properties_vs_lat/'+Bformat+'/nonts_TLvlat.eps'
      else:
        outfile2='../manuscript/Figures/by_TL/properties_vs_lat/'+Bformat+'/ts_TLvlat.eps'
      TLplots(rawdatafile,outfile2,Bformat)

  # for 

 
if __name__ == '__main__':
  main()