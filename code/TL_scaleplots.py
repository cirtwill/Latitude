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

from TL_latplots import datareader

def fixed_reader(coefffile):
  fixed_effects={}
  f=open(coefffile,'r')
  for line in f:
    if line.split('\t')[0]!='"Estimate"':
      name=line.split('\t')[0][1:-1]
      beta=float(line.split('\t')[1])
      fixed_effects[name]=beta
  f.close()
  return fixed_effects

def heatmappoints(rawdatafile,fixed,prop,ecotype,TL,Bformat):
  # Would probably be more helpful to do a heatmap of observed latitudes.
  obs={'Lake':{},
              'Stream':{},
              'Marine':{},
              'Terrestrial':{},
              'Estuary':{}}
  fixed_effects=fixed
  f=open(rawdatafile,'r')
  for line in f:
    if line.split()[0]!='Web':
      Latitude=float(line.split('\t')[6])
      S=int(line.split('\t')[7])
      LS=float(line.split('\t')[10])
      L=LS
      G=float(line.split('\t')[12])
      Gen=G
      V=float(line.split('\t')[14])
      Vul=V
      ecotype=line.split('\t')[1]
      ecotype=ecotype.capitalize()
      B=float(line.split('\t')[-4])
      I=float(line.split('\t')[-2])
      T=float(line.split('\t')[-1])
      if Bformat=='numbers':
        B=B*S
        I=I*S
        T=T*S
      obs[ecotype][(eval(TL),eval(prop))]=Latitude
  f.close()

  return obs

def predictionreader(predfile,TL,Bformat):
  predpoints={'Lake':{0:[],30:[],60:[]},
            'Marine':{0:[],30:[],60:[]},
            'Stream':{0:[],30:[],60:[]},
            'Terrestrial':{0:[],30:[],60:[]},
            'Estuary':{0:[],30:[],60:[]}}
  f=open(predfile,'r')
  for line in f:
    if line.split()[0]!='"Latitude"':
      Lat=int(line.split()[0])
      pred=float(line.split()[-1])
      if line.split()[1]=='1':
        ecotype='Lake'
      elif line.split()[2]=='1':
        ecotype='Marine'
      elif line.split()[3]=='1':
        ecotype='Stream'
      elif line.split()[4]=='1':
        ecotype='Terrestrial'
      else:
        ecotype='Estuary'
      B=float(line.split()[5])
      S=float(line.split()[6])
      I=float(line.split()[7])
      T=float(line.split()[8])

      if Bformat=='proportions':
        if TL=='B':
          predpoints[ecotype][Lat].append((B,10**pred))
        elif TL=='I':
          predpoints[ecotype][Lat].append((I,10**pred))
        elif TL=='T':
          predpoints[ecotype][Lat].append((T,10**pred))
        elif TL=='S':
          predpoints[ecotype][Lat].append((S,10**pred))
      else:
        if TL=='B':
          predpoints[ecotype][Lat].append((B*S,10**pred))
        elif TL=='I':
          predpoints[ecotype][Lat].append((I*S,10**pred))
        elif TL=='T':
          predpoints[ecotype][Lat].append((T*S,10**pred))
        elif TL=='S':
          predpoints[ecotype][Lat].append((S*S,10**pred))


  f.close()
  return predpoints

def scaleplots(rawdatafile,outfile,Bformat,prop,predfolder):

  ecotypes=['Estuary','Lake','Marine','Stream','Terrestrial']

  xwidth=10
  ywidth=1.5

  rawdata=datareader(rawdatafile)

  dummygrace=MultiPanelGrace(colors=ColorBrewerScheme("RdYlBu",reverse=True,n=253))
  colorbar = dummygrace.add_graph(ElLinColorBar,domain=(0,90),scale=LINEAR_SCALE,autoscale=False)

  grace=MultiPanelGrace(colors=ColorBrewerScheme("RdYlBu",reverse=True,n=253))

  for TL in ['B','I','T','S']:
    if TL!='S':
      preddata=predictionreader(predfolder+'/predictions/'+prop[0]+TL+'.tsv',TL,Bformat)
      fixed=fixed_reader(predfolder+'/coefficients/'+prop[0]+TL+'_co.tsv')
    else:
      if rawdatafile=='../non_TS/summary-properties.tsv':
        preddata=predictionreader('../non_TS/subset/predictions/'+prop+'.tsv',TL,Bformat)
        fixed=fixed_reader('../non_TS/subset/coefficients/'+prop+'_co.tsv')
      else:
        preddata=predictionreader('../mod_data/subset/predictions/'+prop+'.tsv',TL,Bformat)
        fixed=fixed_reader('../mod_data/subset/coefficients/'+prop+'_co.tsv')

    for ecotype in ecotypes:
      graph=grace.add_graph(Panel)
      heatpoints=heatmappoints(rawdatafile,fixed,prop,ecotype,TL,Bformat)

      datadict=heatpoints[ecotype]

      for d in datadict:
        color=colorbar.z2color(datadict[d])
        dat=graph.add_dataset([(d[0],d[1])])
        dat.symbol.configure(fill_color=color,color=1,shape=1,linewidth=.5)
          # [(d[0]-0.5*xwidth,d[1]-0.5*ywidth), (d[0]+0.5*xwidth,d[1]+0.5*ywidth)], SolidRectangle, color)

      preddataset1=graph.add_dataset(preddata[ecotype][0])
      preddataset2=graph.add_dataset(preddata[ecotype][30])
      preddataset3=graph.add_dataset(preddata[ecotype][60])

      preddataset1.symbol.shape=0
      preddataset2.symbol.shape=0
      preddataset3.symbol.shape=0

      preddataset1.line.configure(linestyle=1,color=colorbar.z2color(0))
      preddataset2.line.configure(linestyle=1,color=colorbar.z2color(30))
      preddataset3.line.configure(linestyle=1,color=colorbar.z2color(60))

      if TL=='B':
        if ecotype=='Estuary':
          graph.xaxis.label.configure(text='Estuarine',place='opposite',char_size=.75)
        elif ecotype=='Marine':
          graph.xaxis.label.configure(text='Marine',place='opposite',char_size=.75)
        elif ecotype=='Lake':
          graph.xaxis.label.configure(text='Lake',place='opposite',char_size=.75)
        elif ecotype=='Stream':
            graph.xaxis.label.configure(text='Stream',place='opposite',char_size=.75)
        else:
          graph.xaxis.label.configure(text='Terrestrial',place='opposite',char_size=.75)
          graph.legend.configure(loc=(210,30),loctype='world',char_size=.75)
          graph.legend.box_linestyle=0

      if ecotype=='Terrestrial':
        if TL=='B':
          ytex='Basal'
        elif TL=='I':
          ytex='Intermediate'
        elif TL=='T':
          ytex='Top'
        else:
          ytex='Species'
        graph.yaxis.label.configure(text=ytex,place='opposite',char_size=.75)

      graph.world.xmin=0
      if TL!='S' and Bformat=='proportions':
        graph.world.xmax=.6001
        graph.xaxis.tick.major=.2
      else:
        graph.world.xmax=200
        graph.xaxis.tick.major=50

      graph.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.xaxis.ticklabel.configure(char_size=.75)
      graph.frame.linewidth=1
      graph.xaxis.bar.linewidth=1
      graph.yaxis.bar.linewidth=1

      graph.world.ymin=0
      graph.world.ymax=30
      graph.yaxis.tick.major=10

      graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.yaxis.ticklabel.configure(char_size=.75)
      graph.panel_label.configure(char_size=0)


  grace.multi(rows=4,cols=5,vgap=.08,hgap=.04)
  grace.set_row_xaxislabel(row=3,colspan=(0,4),label='Species richness',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  grace.set_row_xaxislabel(row=2,colspan=(0,4),label='Proportion',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  # grace.set_row_xaxislabel(row=1,colspan=(0,4),label='Proportion of Intermediate consumers',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  # grace.set_row_xaxislabel(row=0,colspan=(0,4),label='Proportion of Basal resources',place='normal',just=2,char_size=1,perpendicular_offset=0.05)

  if prop=='LS':
    yax="Links/species"
  elif prop=='Gen':
    yax="Generality"
  elif prop=='Vul':
    yax="Vulnerability"
  grace.set_col_yaxislabel(col=0,rowspan=(0,3),label=yax,place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()

  showbar = grace.add_graph(ElLinColorBar,domain=(0,90),scale=LINEAR_SCALE,autoscale=False)
  showbar.frame.linewidth=1
  showbar.xaxis.bar.linewidth=1
  showbar.yaxis.bar.linewidth=1
  showbar.yaxis.tick.configure(major_linewidth=1,minor_linewidth=1,major_size=.5,minor_size=.3)
  showbar.yaxis.ticklabel.configure(char_size=.75)

  showbar.autoticky()
  showbar.set_view(1.318,0.3688,1.368,0.889)
  grace.add_drawing_object(DrawText,text='Latitude (degrees from equator)', x=1.43, y=.6289, rot=270,char_size=.75,just=2)

  grace.write_file(outfile)

def main():

  for Bformat in ['numbers','proportions']:
    for rawdatafile in ['../non_TS/summary-properties.tsv']:#,'../mod_data/summary-properties.tsv']:
      for prop in ['LS','Gen','Vul']:
        if rawdatafile=='../non_TS/summary-properties.tsv':
          outfile='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/'+prop+'_nonts.eps'
          predfolder='../non_TS/'+Bformat
        else:
          outfile='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/'+prop+'_ts.eps'
          predfolder='../mod_data/'+Bformat

        scaleplots(rawdatafile,outfile,Bformat,prop,predfolder)

 
if __name__ == '__main__':
  main()