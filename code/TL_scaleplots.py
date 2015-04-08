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

LS_centre=1.092381
LS_scale=0.7058436
G_centre=1.411286
G_scale=0.7577303
V_centre=1.092381
V_scale=0.7058436

B_centre=-1.663803
B_scale=0.8298647
I_centre=-1.728887
I_scale=0.9339571
T_centre=-1.774597
T_scale=0.8749923

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
  obs={'Lake':[],
              'Stream':[],
              'Marine':[],
              'Terrestrial':[],
              'Estuary':[]}
  fixed_effects=fixed  

  if TL=='B':
    key='Basal'
  elif TL=='I':
    key='Intermediate'
  elif TL=='T':
    key='Toppreds'
  else:
    key='Species'

  f=open(rawdatafile,'r')
  for line in f:
    if line.split()[0]!='Web':
      Latitude=float(line.split('\t')[6])
      S=int(line.split('\t')[7])
      LS=float(line.split('\t')[10])
      G=float(line.split('\t')[12])
      V=float(line.split('\t')[14])
      # Correct
      # LS=(math.log(LS)-LS_centre)/LS_scale
      # Gen=(math.log(G)-G_centre)/G_scale
      # Vul=(math.log(V)-V_centre)/V_scale
      ecotype=line.split('\t')[1]
      ecotype=ecotype.capitalize()

      B=float(line.split('\t')[-4])
      I=float(line.split('\t')[-2])
      T=float(line.split('\t')[-1])

      if B>0 and I>0 and T>0:
        num=math.log(eval(prop))-fixed['(Intercept)']
        den=math.log(eval(TL))
        y=(num/den)

        if 'log10('+key+'):'+ecotype in fixed:
          delta=fixed['log10('+key+'):'+ecotype ]
          y=y-delta


        # if Bformat=='numbers':
        #   B=B*S
        #   I=I*S
        #   T=T*S
        # B=(math.log(B)-B_centre)/B_scale
        # I=(math.log(I)-I_centre)/I_scale
        # T=(math.log(T)-T_centre)/T_scale

        obs[ecotype].append((Latitude,y))
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

def predictionlines(fixed,prop,ecotype,TL):
  ecoline=[]

  if TL=='B':
    key='Basal'
  elif TL=='I':
    key='Intermediate'
  elif TL=='T':
    key='Toppreds'
  else:
    key='Species'

  # print fixed['(Intercept)']
  delta=fixed['log10('+key+')']

  if 'log10('+key+'):Latitude' in fixed:
    if 'log10('+key+'):'+ecotype+':Latitude' in fixed:
     gamma=fixed['log10('+key+'):Latitude']+fixed['log10('+key+'):'+ecotype+':Latitude'] 
    elif 'log10('+key+'):Latitude:'+ecotype in fixed:
      gamma=fixed['log10('+key+'):Latitude']+fixed['log10('+key+'):Latitude:'+ecotype]
    else:
      gamma=fixed['log10('+key+'):Latitude']
  else:
    gamma=0
  for lat in range(0,91):
    exponent=delta+float(lat)*gamma
    ecoline.append((lat,exponent))
  return ecoline

def scaleplots(rawdatafile,outfile,Bformat,prop,predfolder):

  ecotypes=['Lake','Marine','Stream','Terrestrial','Estuary']

  rawdata=datareader(rawdatafile)

  grace=MultiPanelGrace(colors=ColorBrewerScheme("Paired"))

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

    graph=grace.add_graph(Panel)
    print fixed
    for ecotype in ecotypes:
      if ecotype=='Estuary':
        linecol=1
      elif ecotype=='Lake':
        linecol=3
      elif ecotype=='Marine':
        linecol=5
      elif ecotype=='Stream':
        linecol=7
      else:
        linecol=11

      heatpoints=heatmappoints(rawdatafile,fixed,prop,ecotype,TL,Bformat)
      datadict=heatpoints[ecotype]
      obspoints=graph.add_dataset(datadict)
      obspoints.line.configure(linestyle=0)
      obspoints.symbol.configure(size=.2,shape=1,fill_color=linecol,fill_pattern=1,color=linecol)

      predictions=predictionlines(fixed,prop,ecotype,TL)
      predline=graph.add_dataset(predictions)
      predline.symbol.shape=0

      predline.line.configure(linestyle=1,color=linecol)
      if TL=='S':
        predline.legend=ecotype

      # preddataset1=graph.add_dataset(preddata[ecotype][0])
      # preddataset2=graph.add_dataset(preddata[ecotype][30])
      # preddataset3=graph.add_dataset(preddata[ecotype][60])

      # preddataset1.symbol.shape=0
      # preddataset2.symbol.shape=0
      # preddataset3.symbol.shape=0

      # if TL=='B':
      #   if ecotype=='Estuary':
      #     graph.xaxis.label.configure(text='Estuarine',place='opposite',char_size=.75)
      #   elif ecotype=='Marine':
      #     graph.xaxis.label.configure(text='Marine',place='opposite',char_size=.75)
      #   elif ecotype=='Lake':
      #     graph.xaxis.label.configure(text='Lake',place='opposite',char_size=.75)
      #   elif ecotype=='Stream':
      #       graph.xaxis.label.configure(text='Stream',place='opposite',char_size=.75)
      #   else:
      #     graph.xaxis.label.configure(text='Terrestrial',place='opposite',char_size=.75)
      if TL=='S':
        graph.legend.configure(loc=(110,1),loctype='world',char_size=.75)
        graph.legend.box_linestyle=0

      if ecotype=='Terrestrial':
        if TL=='B':
          ytex='% Basal'
        elif TL=='I':
          ytex='% Intermediate'
        elif TL=='T':
          ytex='% Top'
        else:
          ytex='Species Richness'
        graph.xaxis.label.configure(text=ytex,place='opposite',char_size=.75)

      graph.world.xmin=0
      graph.world.xmin=0
      graph.world.xmax=90
      graph.xaxis.tick.major=15

      graph.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.xaxis.ticklabel.configure(char_size=.75)
      graph.frame.linewidth=1
      graph.xaxis.bar.linewidth=1
      graph.yaxis.bar.linewidth=1

      if TL=='B':
        graph.world.ymin=-2
        graph.world.ymax=.75
      elif TL=='I':
        graph.world.ymax=1
        graph.world.ymin=-6
      elif TL=='T':
        graph.world.ymin=-6
        graph.world.ymax=.5
      else:
        graph.world.ymin=0
        graph.world.ymax=1

      graph.world.ymin=-8
      graph.world.ymax=2


      graph.yaxis.tick.major=5

      graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.yaxis.ticklabel.configure(char_size=.75)
      graph.panel_label.configure(char_size=0)


  grace.multi(rows=1,cols=4,vgap=.04,hgap=.06)
  # grace.set_row_xaxislabel(row=3,colspan=(0,4),label='Latitude (degrees from equator)',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  # grace.set_row_xaxislabel(row=2,colspan=(0,4),label='Proportion',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  # grace.set_row_xaxislabel(row=1,colspan=(0,4),label='Proportion of Intermediate consumers',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  # grace.set_row_xaxislabel(row=0,colspan=(0,4),label='Proportion of Basal resources',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  grace.add_drawing_object(DrawText,text="Latitude (degrees from equator)",x=.505,y=.725,char_size=1)

  if prop=='LS':
    yax="Ratio of log(LS)"
  elif prop=='Gen':
    yax="Ratio of Generality"
  elif prop=='Vul':
    yax="Ratio of Vulnerability"
  grace.set_col_yaxislabel(col=0,rowspan=(0,0),label=yax,place='normal',just=2,char_size=1,perpendicular_offset=0.07)
  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  print 'changed'

  grace.write_file(outfile)

def main():

  for Bformat in ['proportions']:#['numbers','proportions']:
    for rawdatafile in ['../non_TS/summary-properties.tsv']:#,'../mod_data/summary-properties.tsv']:
      for prop in ['LS']:#,'Gen','Vul']:
        if rawdatafile=='../non_TS/summary-properties.tsv':
          outfile='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/'+prop+'_nonts.jpg'
          predfolder='../non_TS/'+Bformat
        else:
          outfile='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/'+prop+'_ts.eps'
          predfolder='../mod_data/'+Bformat

        scaleplots(rawdatafile,outfile,Bformat,prop,predfolder)

 
if __name__ == '__main__':
  main()