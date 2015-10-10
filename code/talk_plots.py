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

import marginal_latitude_fig_noTL as marg

# Think I want plots of the raw data (S, LS, G, V vs. lat) with slope lines
# Plus predictions for LS, G, V vs S (with corrected obs.)

def datareader(rawdatafile,TL):

  points={'LS':[],'Gen':[],'Vul':[]}

  f=open(rawdatafile,'r')
  for line in f:
    if line.split()[0]!='Web':
      ecotype=line.split('\t')[1]
      ecotype=ecotype.capitalize()
      site=line.split('\t')[2]
      Latitude=float(line.split('\t')[3])
      S=int(line.split('\t')[4])
      L=line.split('\t')[5]
      C=line.split('\t')[6]
      LS=float(line.split('\t')[7])
      LSsd=line.split('\t')[8]
      Gen=float(line.split('\t')[9])
      GenSD=float(line.split('\t')[10])
      Vul=float(line.split('\t')[11])
      VulSD=float(line.split('\t')[12])
      B=float(line.split('\t')[15])
      H=float(line.split('\t')[16])
      I=float(line.split('\t')[17])
      I=I+H # Include the herbivores as intermediates
      T=float(line.split('\t')[18])

      for response in ['LS','Gen','Vul']:
        points[response].append((eval(TL),eval(response)))

  f.close()
  return points

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

def heatmappoints(rawdatafile,fixed,prop,ecotype,TL):
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
    # print line.split()
    # sys.exit()
    if line.split()[0]!='Web':
      ecotype=line.split('\t')[1]
      ecotype=ecotype.capitalize()
      Latitude=float(line.split('\t')[3])
      S=float(line.split('\t')[4])
      LS=float(line.split('\t')[7])
      Gen=float(line.split('\t')[9])
      Vul=float(line.split('\t')[11])
      B=float(line.split('\t')[15])
      H=float(line.split('\t')[16])
      I=float(line.split('\t')[17])
      I=H+I
      T=float(line.split('\t')[18])

      # Correct the observed property based on latitude and ecotype effects

      if 'log('+key+'):'+ecotype in fixed_effects: 
        delta=fixed_effects['log('+key+'):'+ecotype]
      else:   # If there's no intercept effect of ecosystem
        delta=0

      if 'log('+key+'):Latitude' in fixed_effects:
        if 'log('+key+'):'+ecotype+':Latitude' in fixed_effects:
         gamma=fixed_effects['log('+key+'):Latitude']+fixed_effects['log('+key+'):'+ecotype+':Latitude'] 
        elif 'log('+key+'):Latitude:'+ecotype in fixed_effects:
          gamma=fixed_effects['log('+key+'):Latitude']+fixed_effects['log('+key+'):Latitude:'+ecotype]
        else:  #If there's no lat-by-ecotype interaction
          gamma=fixed_effects['log('+key+'):Latitude'] 
      else: # If there's no latitude effect at all
        gamma=0

      exponent=delta+Latitude*gamma

      if B>0 and I>0 and T>0:
        correction=eval(TL)**exponent
        y=float(Decimal(eval(prop))/Decimal(correction))
        obs[ecotype].append((eval(TL),y))
  f.close()

  return obs

def predictionreader(predfile,TL):
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

      if TL=='B':
        predpoints[ecotype][Lat].append((B,math.exp(pred)))
      elif TL=='I':
        predpoints[ecotype][Lat].append((I,math.exp(pred)))
      elif TL=='T':
        predpoints[ecotype][Lat].append((T,math.exp(pred)))
      elif TL=='S':
        predpoints[ecotype][Lat].append((S,math.exp(pred)))

  f.close()
  return predpoints

def predictionlines(fixed,prop,TL):
  ecoline=[]

  if TL=='B':
    key='Basal'
  elif TL=='I':
    key='Intermediate'
  elif TL=='T':
    key='Toppreds'
  else:
    key='Species'

  alpha=math.exp(fixed['(Intercept)'])

  delta=fixed['log('+key+')']

  if TL=='S':
    for S in range(1,350):
      predy=alpha*(S**delta)
      ecoline.append((S,predy))
  else:
    ## Need to do a range of percents
    for frac in range(1,201):
      perc=float(frac)/float(200)
      predy=alpha*(perc**delta)
      ecoline.append((perc,predy))

  return ecoline

def S_rawplots(rawdatafile,outfile1,predfolder):

  # Lets make clear that these are the original, uncorrected points

  grace=Grace(colors=ColorBrewerScheme("Greys"))

  TL='S'
  prop='Gen'
  outfile=outfile1+prop+'_vs_S_fitline_observed.eps'  
  print outfile

  rawdata=datareader(rawdatafile,TL)

  fixed=fixed_reader('../non_TS/coefficients/'+prop+'_obs.tsv')

  graph=grace.add_graph()

  obspoints=graph.add_dataset(rawdata[prop])
  obspoints.line.configure(linestyle=0)
  obspoints.symbol.configure(size=.5,shape=1,fill_color=0,fill_pattern=1,color=8)

  predictions=predictionlines(fixed,prop,TL)
  predline=graph.add_dataset(predictions)
  predline.symbol.shape=0

  predline.line.configure(linestyle=1,color=1,linewidth=2.5)

  ytex='Species richness'
  graph.xaxis.label.configure(text=ytex,place='normal',char_size=1)

  if prop=='LS':
    graph.yaxis.label.configure(text="Link density",place='normal',char_size=1)
  elif prop=='Gen':
    graph.yaxis.label.configure(text="Generality",place='normal',char_size=1)

  graph.world.xmin=1
  graph.world.ymin=.1

  graph.world.ymax=100
  graph.xaxis.set_log()
  graph.yaxis.set_log()

  graph.world.xmax=300

  graph.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
  graph.xaxis.ticklabel.configure(char_size=.75)
  graph.frame.linewidth=1
  graph.xaxis.bar.linewidth=1
  graph.yaxis.bar.linewidth=1

  graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
  graph.yaxis.ticklabel.configure(char_size=.75)

  print 'got a raw plot'

  grace.write_file(outfile)

  # Lets make clear that these are the original, uncorrected points
  outfile2='../talk/Figures/results/'+prop+'_vs_lat_simulated.eps'

  grace=Grace(colors=ColorBrewerScheme("PRGn"))

  graph=grace.add_graph()

  narrowniches=graph.add_dataset([(0,1),(90,100)])
  narrowniches.symbol.shape=0
  narrowniches.line.color=2
  narrowniches.legend="Narrower niches in the tropics"

  sameniches=graph.add_dataset([(0,3),(90,3)])
  sameniches.symbol.shape=0
  sameniches.line.color=11
  sameniches.legend="Similar niches everywhere"

  ytex='Absolute latitude'
  graph.xaxis.label.configure(text=ytex,place='normal',char_size=1)

  if prop=='LS':
    graph.yaxis.label.configure(text="Link density",place='normal',char_size=1)
  elif prop=='Gen':
    graph.yaxis.label.configure(text="Generality",place='normal',char_size=1)

  graph.legend.configure(char_size=.75,frame=0,loc=(5,.3),loctype='world',box_linestyle=0)

  graph.world.ymin=.1
  graph.world.ymax=100
  graph.yaxis.set_log()

  graph.world.xmin=0
  graph.world.xmax=90

  graph.xaxis.tick.configure(major=30,minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
  graph.xaxis.ticklabel.configure(char_size=.75)
  graph.frame.linewidth=1
  graph.xaxis.bar.linewidth=1
  graph.yaxis.bar.linewidth=1

  graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
  graph.yaxis.ticklabel.configure(char_size=.75)


  grace.write_file(outfile2)

def marginal_plotter(prop,TL,ecotype,graph):

  dataset=marg.linereader(prop,TL)

  if dataset[ecotype]['upper']!=[]:
    upper=dataset[ecotype]['upper']
    lower=dataset[ecotype]['lower']
  else:
    upper=dataset['Estuary']['upper']
    lower=dataset['Estuary']['lower']

  upper2=graph.add_dataset(upper)
  upper2.symbol.configure(shape=0)
  upper2.line.configure(linestyle=0,color=10,linewidth=.5)
  upper2.fill.configure(color=5,type=2)

  lower2=graph.add_dataset(lower)
  lower2.symbol.configure(shape=0)
  lower2.line.configure(linestyle=0,color=10,linewidth=.5)
  lower2.fill.configure(color=0,type=2)


  if dataset[ecotype]['main']!=[]:
    main=graph.add_dataset(dataset[ecotype]['main'])
  else:
    main=graph.add_dataset(dataset['Estuary']['main'])
  main.symbol.shape=0
  main.line.configure(linestyle=1,color=1,linewidth=1)

  graph.xaxis.bar.linewidth=1
  graph.yaxis.bar.linewidth=1
  graph.frame.linewidth=1

  graph.world.xmin=0
  graph.world.xmax=90

  graph.world.ymin=0
  graph.world.ymax=1.5
  major=.5
  prec=1

  graph.xaxis.ticklabel.configure(char_size=.75)
  graph.xaxis.tick.configure(major_linewidth=.5,minor_linewidth=.5,major_size=.6,minor_size=.4,major=30)

  graph.yaxis.tick.configure(major=major,major_linewidth=.5,minor_linewidth=.5,major_size=.6,minor_size=.4)
  graph.yaxis.ticklabel.configure(format='decimal',prec=prec,char_size=.75)

  if ecotype=='Stream':
    graph.xaxis.label.configure(text='Absolute latitude',place='normal',char_size=1,perpendicular_offset=0.05)  

  return graph


def dummy_marginal_plotter(plottype,ecotypes,prop):
  grace=MultiPanelGrace(colors=ColorBrewerScheme('Greys'))

  names=['Estuarine','Marine','Terrestrial','Lake','Stream']

  grace.add_label_scheme('dummy',names)
  grace.set_label_scheme('dummy')
  
  dataset=marg.linereader('Gen','S')

  upper=dataset['Estuary']['upper']
  lower=dataset['Estuary']['lower']

  for ecotype in ecotypes:   

    graph=grace.add_graph(Panel)

    if plottype=='one' and ecotype=='Estuary':
      upper2=graph.add_dataset(upper)
      upper2.symbol.configure(shape=0)
      upper2.line.configure(linestyle=0,color=10,linewidth=.5)
      upper2.fill.configure(color=5,type=2)

      lower2=graph.add_dataset(lower)
      lower2.symbol.configure(shape=0)
      lower2.line.configure(linestyle=0,color=10,linewidth=.5)
      lower2.fill.configure(color=0,type=2)

      main=graph.add_dataset(dataset['Estuary']['main'])
      main.symbol.shape=0
      main.line.configure(linestyle=1,color=1,linewidth=1)

    graph.xaxis.bar.linewidth=1
    graph.yaxis.bar.linewidth=1
    graph.frame.linewidth=1

    graph.world.xmin=0
    graph.world.xmax=90

    graph.world.ymin=0
    graph.world.ymax=1.5
    major=.5
    prec=1

    graph.xaxis.ticklabel.configure(char_size=.75)
    graph.xaxis.tick.configure(major_linewidth=.5,minor_linewidth=.5,major_size=.6,minor_size=.4,major=30)

    graph.yaxis.tick.configure(major=major,major_linewidth=.5,minor_linewidth=.5,major_size=.6,minor_size=.4)
    graph.yaxis.ticklabel.configure(format='decimal',prec=prec,char_size=.75)

    if ecotype=='Stream':
      graph.xaxis.label.configure(text='Absolute latitude',place='normal',char_size=1,perpendicular_offset=0.05)  
    graph.panel_label.configure(char_size=.75,placement='iul',dy=.02,dx=.03)

  grace.multi(rows=2,cols=3,vgap=.04,hgap=.04)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  
  grace.set_col_yaxislabel(col=0,rowspan=(0,1),label='Marginal effect of latitude on scaling',place='normal',just=2,char_size=1,perpendicular_offset=0.07)
  # Sgrace.set_row_xaxislabel(row=1,colspan=(0,2),label='Absolute latitude',place='normal',just=2,char_size=1,perpendicular_offset=0.05)

  grace.write_file('../talk/Figures/results/'+prop+'_vs_S_marginal_'+plottype+'.eps')

  print plottype

def main():

  ecotypes=['Estuary','Marine','Terrestrial','Lake','Stream']

  Bformat='proportions'
  rawdatafile='../non_TS/summary-properties.tsv'

  outfile1='../talk/Figures/results/'
  predfolder='../non_TS/'+Bformat

  S_rawplots(rawdatafile,outfile1,predfolder)

  TL='S'

  for prop in ["LS","Gen"]:

    Sgrace=MultiPanelGrace(colors=ColorBrewerScheme('Greys'))

    Snames=['Estuarine','Marine','Terrestrial','Lake','Stream']

    Sgrace.add_label_scheme('dummy',Snames)
    Sgrace.set_label_scheme('dummy')
    
    for ecotype in ecotypes:   

      graph=Sgrace.add_graph(Panel)

      marginal_plotter(prop,TL,ecotype,graph)
      graph.panel_label.configure(char_size=.75,placement='iul',dy=.02,dx=.03)

    Sgrace.multi(rows=2,cols=3,vgap=.04,hgap=.04)

    Sgrace.hide_redundant_xticklabels()
    Sgrace.hide_redundant_yticklabels()

    Sgrace.set_col_yaxislabel(col=0,rowspan=(0,1),label='Marginal effect of latitude on scaling',place='normal',just=2,char_size=1,perpendicular_offset=0.07)

    Sgrace.write_file('../talk/Figures/results/'+prop+'_vs_S_marginal.eps')
 
  for plottype in ['axis','one']:
    dummy_marginal_plotter(plottype,ecotypes,'Gen')
 
if __name__ == '__main__':
  main()