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

def datareader(rawdatafile,TL,Bformat):

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
          predpoints[ecotype][Lat].append((B,math.exp(pred)))
        elif TL=='I':
          predpoints[ecotype][Lat].append((I,math.exp(pred)))
        elif TL=='T':
          predpoints[ecotype][Lat].append((T,math.exp(pred)))
        elif TL=='S':
          predpoints[ecotype][Lat].append((S,math.exp(pred)))
      else:
        if TL=='B':
          predpoints[ecotype][Lat].append((B*S,math.exp(pred)))
        elif TL=='I':
          predpoints[ecotype][Lat].append((I*S,math.exp(pred)))
        elif TL=='T':
          predpoints[ecotype][Lat].append((T*S,math.exp(pred)))
        elif TL=='S':
          predpoints[ecotype][Lat].append((S*S,math.exp(pred)))


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

def S_scaleplots(rawdatafile,outfile1,Bformat,predfolder):

  ecotypes=['Lake','Marine','Stream','Terrestrial','Estuary']

  grace=Grace(colors=ColorBrewerScheme("Greys"))

  TL='S'
  prop='LS'

  rawdata=datareader(rawdatafile,TL,Bformat)
  fixed=fixed_reader('../non_TS/coefficients/'+prop+'_co.tsv')

  graph=grace.add_graph()
  for ecotype in ecotypes:
    heatpoints=heatmappoints(rawdatafile,fixed,prop,ecotype,TL,Bformat)
    datadict=heatpoints[ecotype]
    obspoints=graph.add_dataset(datadict)
    obspoints.line.configure(linestyle=0)
    obspoints.symbol.configure(size=.5,shape=1,fill_color=0,fill_pattern=1,color=8)

    predictions=predictionlines(fixed,prop,TL)
    predline=graph.add_dataset(predictions)
    predline.symbol.shape=0

    predline.line.configure(linestyle=1,color=1,linewidth=2.5)

  graph.legend.configure(loc=(110,1),loctype='world',char_size=.75)
  graph.legend.box_linestyle=0

  ytex='species richness'
  graph.xaxis.label.configure(text=ytex,place='normal',char_size=1)

  graph.yaxis.label.configure(text="re-scaled link density",place='normal',char_size=1)

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

  print 'changed'

  grace.write_file(outfile1)


def S_rawplots(rawdatafile,outfile1,Bformat,predfolder):

  # Lets make clear that these are the original, uncorrected points
  outfile=outfile1.split('.eps')[0]+'_observed.eps'  

  grace=Grace(colors=ColorBrewerScheme("Greys"))

  TL='S'
  prop='LS'

  rawdata=datareader(rawdatafile,TL,Bformat)

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

  graph.yaxis.label.configure(text="Link density",place='normal',char_size=1)

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
  outfile2='../talk/Figures/results/ls_vs_lat_simulated.eps'

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

  graph.yaxis.label.configure(text="Link density",place='normal',char_size=1)

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
def main():

  Bformat='proportions'
  rawdatafile='../non_TS/summary-properties.tsv'

  outfile1='../talk/Figures/results/LS_vs_S_fitline.eps'
  predfolder='../non_TS/'+Bformat

  S_scaleplots(rawdatafile,outfile1,Bformat,predfolder)

  S_rawplots(rawdatafile,outfile1,Bformat,predfolder)


 
if __name__ == '__main__':
  main()