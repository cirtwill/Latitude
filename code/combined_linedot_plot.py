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
# [[Redoing this with the new data, old files called are in comments]]


def datareader(rawdatafile,TL,Bformat):

  points={'LS':[],'Gen':[],'Vul':[]}

  f=open(rawdatafile,'r')
  i=1
  for line in f:
    if line.split('\t')[0]!='Web':
      ecotype=line.split('\t')[1]
      ecotype=ecotype.capitalize()
      site=line.split('\t')[2]
      Latitude=float(line.split('\t')[3])
      S=int(line.split('\t')[4])
      L=line.split('\t')[5]
      C=line.split('\t')[6]
      LS=float(line.split('\t')[7])
      Gen=float(line.split('\t')[8])
      Vul=float(line.split('\t')[9])
      B=float(line.split('\t')[10])
      H=float(line.split('\t')[11])
      I=float(line.split('\t')[12])
      T=float(line.split('\t')[13])

      # B=float(line.split('\t')[15])
      # H=float(line.split('\t')[16])
      # I=float(line.split('\t')[17])
      # I=I+H # Include the herbivores as intermediates
      # T=float(line.split('\t')[18])

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
    if line.split()[0]!='Web':
      ecotype=line.split('\t')[1]
      ecotype=ecotype.capitalize()
      Latitude=float(line.split('\t')[3])
      S=float(line.split('\t')[4])
      LS=float(line.split('\t')[7])
      Gen=float(line.split('\t')[8])
      Vul=float(line.split('\t')[9])
      B=float(line.split('\t')[10])
      H=float(line.split('\t')[11])
      I=float(line.split('\t')[12])
      T=float(line.split('\t')[13])

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

def predictionlines(fixed,prop,TL):
  ecoline=[]
  # print prop, TL
  # print fixed
  if TL=='B':
    key='Basal'
  elif TL=='I':
    key='Intermediate'
  elif TL=='T':
    key='Toppreds'
  else:
    key='Species'

  alpha=math.exp(fixed['(Intercept)'])

  try:
    delta=fixed['log10('+key+')']
  except KeyError:
    try:
      delta=fixed['log('+key+')']
    except:
      sys.exit()
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

def format_graph(graph,prop,TL):
  graph.frame.linewidth=1
  graph.xaxis.bar.linewidth=1
  graph.yaxis.bar.linewidth=1

  graph.xaxis.set_log()
  graph.yaxis.set_log()
  graph.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
  graph.xaxis.ticklabel.configure(char_size=.75,font='Times-Roman')
  graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
  graph.yaxis.ticklabel.configure(char_size=.75,font='Times-Roman')
  graph.panel_label.configure(char_size=0)
  graph.world.ymin=.1
  if prop=='LS':
    graph.world.ymax=100
  elif prop=='Gen':
    graph.world.ymax=50
  else:
    graph.world.ymax=50

  if TL=='S':
    graph.legend.configure(loc=(110,1),loctype='world',char_size=.75)
    graph.legend.box_linestyle=0
    graph.world.xmin=5
    graph.world.xmax=200
  else:
    graph.world.xmin=0.005
    graph.world.xmax=1

  if prop=='Vul':
    if TL=='S':
      xtex='species richness'
    elif TL=='B':
      xtex='% basal'
    elif TL=='I':
      xtex='% intermediate'
    elif TL=='T':
      xtex='% top'
    graph.xaxis.label.configure(text=xtex,place='normal',char_size=.75,font='Times-Roman')

  if TL=='T':
    if prop=='LS':
      graph.yaxis.label.configure(text="link density",place='opposite',char_size=.75,font='Times-Roman')
    elif prop=='Gen':
      graph.yaxis.label.configure(text="generality",place='opposite',char_size=.75,font='Times-Roman')
    elif prop=='Vul':
      graph.yaxis.label.configure(text="vulnerability",place='opposite',char_size=.75,font='Times-Roman')
  elif TL=='S':
    graph.yaxis.label.configure(text='re-scaled value',place='normal',char_size=.75,font='Times-Roman')

  return graph

def scaleplot(rawdatafile,scalefile,Bformat,predfolder):

  ecotypes=['Lake','Marine','Stream','Terrestrial','Estuary']

  grace=MultiPanelGrace(colors=ColorBrewerScheme("Greys"))
  grace.set_fonts('Times-Roman')
  
  for prop in ['LS','Gen','Vul']:
    for TL in ['S','B','I','T']:
      rawdata=datareader(rawdatafile,TL,Bformat)
      if TL=='S':
        fixed=fixed_reader('../non_TS/coefficients/'+prop+'_co_corrected.tsv')
      else:
        fixed=fixed_reader(predfolder+'/coefficients/'+prop[0]+TL+'_co.tsv')

      graph=grace.add_graph(Panel)
      graph=format_graph(graph,prop,TL)
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

  grace.multi(rows=3,cols=4,vgap=.04,hgap=.04)

  # grace.set_col_yaxislabel(col=0,rowspan=(0,2),label='Re-scaled value',place='normal',just=2,char_size=1,perpendicular_offset=0.06)
  # grace.add_drawing_object(DrawText,text='Re-scaled value of:',x=0.06,y=.555,loctype='view',rot=90,char_size=.85,just=2)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  print 'changed'

  grace.write_file(scalefile)

def obsplot(rawdatafile,obsfile,Bformat,predfolder):
  grace=MultiPanelGrace(colors=ColorBrewerScheme("Greys"))
  grace.set_fonts('Times-Roman')

  for prop in ['LS','Gen','Vul']:
    for TL in ['S','B','I','T']:
      rawdata=datareader(rawdatafile,TL,Bformat)
      if TL=='S':
        fixed=fixed_reader('../non_TS/coefficients/'+prop+'_obs_corrected.tsv')
      else:
        fixed=fixed_reader(predfolder+'/coefficients/'+prop[0]+TL+'_obs.tsv')

      graph=grace.add_graph(Panel)
      graph=format_graph(graph,prop,TL)

      obspoints=graph.add_dataset(rawdata[prop])
      obspoints.line.configure(linestyle=0)
      obspoints.symbol.configure(size=.5,shape=1,fill_color=0,fill_pattern=1,color=8)

      predictions=predictionlines(fixed,prop,TL)
      predline=graph.add_dataset(predictions)
      predline.symbol.shape=0

      predline.line.configure(linestyle=1,color=1,linewidth=2.5)

  grace.multi(rows=3,cols=4,vgap=.04,hgap=.04)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  print 'got a raw plot'

  grace.write_file(obsfile)



def main():

  Bformat='proportions'
  rawdatafile='../non_TS/summary-properties_corrected_webs.tsv'
  outfile1='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/S_fitlines_nonts_corrected.eps'
  outfile2='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/TL_fitlines_nonts_corrected.eps'
  predfolder='../non_TS/'+Bformat

  scalefile='../manuscript/Figures/by_TL/scaling_with_S/proportions/STL_fitlines_nonts.eps'
  scaleplot(rawdatafile,scalefile,Bformat,predfolder)

  obsfile='../manuscript/Figures/by_TL/scaling_with_S/proportions/STL_fitlines_nonts_observed.eps'
  obsplot(rawdatafile,obsfile,Bformat,predfolder)

 
if __name__ == '__main__':
  main()