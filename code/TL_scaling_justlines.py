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
  predpoints={'Lake':{0:[],10:[],20:[],30:[],40:[],50:[],60:[],70:[],80:[],90:[]},
            'Marine':{0:[],10:[],20:[],30:[],40:[],50:[],60:[],70:[],80:[],90:[]},
            'Stream':{0:[],10:[],20:[],30:[],40:[],50:[],60:[],70:[],80:[],90:[]},
            'Terrestrial':{0:[],10:[],20:[],30:[],40:[],50:[],60:[],70:[],80:[],90:[]},
            'Estuary':{0:[],10:[],20:[],30:[],40:[],50:[],60:[],70:[],80:[],90:[]}}
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

def scaleplots(rawdatafile,outfile,Bformat,TL,predfolder):

  ecotypes=['Estuary','Lake','Marine','Stream','Terrestrial']

  xwidth=10
  ywidth=1.5

  rawdata=datareader(rawdatafile)

  dummygrace=MultiPanelGrace(colors=ColorBrewerScheme("Greys",n=253))
  colorbar = dummygrace.add_graph(ElLinColorBar,domain=(0,100),scale=LINEAR_SCALE,autoscale=False)

  grace=MultiPanelGrace(colors=ColorBrewerScheme("Greys",n=253))

  for prop in ['LS','Gen','Vul']:
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

      for line in [80,60,40,20,0]:
        preddataset1=graph.add_dataset(preddata[ecotype][line])

        preddataset1.symbol.shape=0

        linesty=1

        preddataset1.line.configure(linestyle=linesty,color=colorbar.z2color(line))

      if TL!='S':
        stars=[(.35,25)]
      else:
        stars=[(175,25)]
      ### Add stars for significant trends

      # if TL=='B' and ecotype in ['Estuary','Lake','Stream']:

      if ecotype=='Estuary':
        if prop=='LS':
          yax="Link density"
        elif prop=='Gen':
          yax="Generality"
        elif prop=='Vul':
          yax="Vulnerability"
        
        graph.yaxis.label.configure(text=yax,place='normal',char_size=.75)

      if prop=='LS':
        if ecotype=='Estuary':
          typ='Estuarine'
        elif ecotype=='Marine':
          typ='Marine'
        elif ecotype=='Lake':
          typ='Lake'
        elif ecotype=='Stream':
          typ='Stream'
        else:
          typ='Terrestrial'
        graph.xaxis.label.configure(text=typ,place='opposite',char_size=.75)

      graph.world.xmin=0
      if TL!='S' and Bformat=='proportions':
        graph.world.xmax=.4001
        graph.xaxis.tick.major=.1
      else:
        graph.world.xmax=200
        graph.xaxis.tick.major=50

      graph.xaxis.tick.configure(minor_ticks=1,major_size=.5,minor_size=.3,major_linewidth=1,minor_linewidth=1)
      graph.xaxis.ticklabel.configure(char_size=.75)
      graph.frame.linewidth=1
      graph.xaxis.bar.linewidth=1
      graph.yaxis.bar.linewidth=1

      graph.world.ymin=0
      graph.world.ymax=30
      ymaj=10

      graph.yaxis.tick.major=ymaj

      graph.yaxis.tick.configure(minor_ticks=1,major_size=.5,minor_size=.3,major_linewidth=1,minor_linewidth=1)
      graph.yaxis.ticklabel.configure(char_size=.75)
      graph.panel_label.configure(char_size=0)


  grace.multi(rows=3,cols=5,vgap=.04,hgap=.04)
  # grace.set_row_xaxislabel(row=2,colspan=(0,2),label='Species richness',place='normal',just=2,char_size=1,perpendicular_offset=0.05)

  if TL=='B':
    xtex='Basal resources'
  elif TL=='I':
    xtex='Intermediate consumers'
  elif TL=='T':
    xtex='Top predators'
  else:
    xtex='Species richness'

  if TL!='S':
    grace.set_row_xaxislabel(row=2,colspan=(0,4),label='Proportion of '+xtex,place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  else:
    grace.set_row_xaxislabel(row=2,colspan=(0,4),label=xtex,place='normal',just=2,char_size=1,perpendicular_offset=0.05)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()

  showbar = grace.add_graph(ElLinColorBar,domain=(0,90),scale=LINEAR_SCALE,autoscale=False)
  showbar.frame.linewidth=1
  showbar.xaxis.bar.linewidth=1
  showbar.yaxis.bar.linewidth=1
  showbar.yaxis.tick.configure(major_linewidth=1,minor_linewidth=1,major_size=.5,minor_size=.3)
  showbar.yaxis.ticklabel.configure(char_size=.75)

  # for graph in grace.graphs:
  #   print graph.get_view()

  showbar.autoticky()
  showbar.set_view(1.3,0.47767,1.35,0.95)
  grace.add_drawing_object(DrawText,text='Latitude (degrees from equator)', x=1.4, y=.713835, rot=270,char_size=.75,just=2)

  grace.write_file(outfile)

def main():

  Bformat='proportions'
  rawdatafile='../non_TS/summary-properties.tsv'
  for TL in ['B','I','T','S']:
    if rawdatafile=='../non_TS/summary-properties.tsv':
      outfile='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/'+TL+'_latlines_nonts.eps'
      predfolder='../non_TS/'+Bformat
    else:
      outfile='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/'+TL+'_latlines_ts.eps'
      predfolder='../mod_data/'+Bformat

    scaleplots(rawdatafile,outfile,Bformat,TL,predfolder)
    print TL
 
if __name__ == '__main__':
  main()