import sys
import os
import re
import random
from decimal import *
import math
# import nose

from PyGrace.grace import Grace
from PyGrace.colors import RandomColorScheme, MarkovChainColorScheme, ColorBrewerScheme
from PyGrace.dataset import SYMBOLS
from PyGrace.Extensions.panel import Panel,MultiPanelGrace
from PyGrace.drawing_objects import DrawText, DrawLine

#from PyGrace.Extensions.distribution import CDFGraph, PDFGraph
from PyGrace.Extensions.latex_string import LatexString, CONVERT

# Ranges of the parameters
weight_min=0.1
weight_max=2800
prey_gen_min=0
prey_gen_max=10

ecotypes=['Estuary','Lake','Marine','Stream','Terrestrial']

def linereader(prop,TL):
  linefile=open('../non_TS/marginals/'+prop+'_'+TL+'_marginal.tsv','r')
  lines={}
  for ecotype in ecotypes:
    lines[ecotype]={'main':[],'upper':[],'lower':[]}

  for line in linefile:
    if line.split()[0] != '"Latitude"':
      s0=float(line.split()[1])
      marginal=float(line.split()[2])
      upper=float(line.split()[3])
      lower=float(line.split()[4])

      if TL=='S':
        terr_m=float(line.split()[5])
        terr_u=float(line.split()[6])
        terr_l=float(line.split()[7])

        lake_m=float(line.split()[8])
        lake_u=float(line.split()[9])
        lake_l=float(line.split()[10])

        lines['Terrestrial']['main'].append((s0,terr_m))
        lines['Terrestrial']['upper'].append((s0,terr_u))
        lines['Terrestrial']['lower'].append((s0,terr_l))
        lines['Lake']['main'].append((s0,lake_m))
        lines['Lake']['upper'].append((s0,lake_u))
        lines['Lake']['lower'].append((s0,lake_l))

        if prop=='Gen':
          stream_m=float(line.split()[11])
          stream_u=float(line.split()[12])
          stream_l=float(line.split()[13])
    
          lines['Stream']['main'].append((s0,stream_m))
          lines['Stream']['upper'].append((s0,stream_u))
          lines['Stream']['lower'].append((s0,stream_l))

      elif TL=='B':
        stream_m=float(line.split()[5])
        stream_u=float(line.split()[6])
        stream_l=float(line.split()[7])

        marine_m=float(line.split()[8])
        marine_u=float(line.split()[9])
        marine_l=float(line.split()[10])

        terr_m=float(line.split()[11])
        terr_u=float(line.split()[12])
        terr_l=float(line.split()[13])  

        lines['Stream']['main'].append((s0,stream_m))
        lines['Stream']['upper'].append((s0,stream_u))
        lines['Stream']['lower'].append((s0,stream_l))
        lines['Terrestrial']['main'].append((s0,terr_m))
        lines['Terrestrial']['upper'].append((s0,terr_u))
        lines['Terrestrial']['lower'].append((s0,terr_l))
        lines['Marine']['main'].append((s0,marine_m))
        lines['Marine']['upper'].append((s0,marine_u))
        lines['Marine']['lower'].append((s0,marine_l))

      elif TL=='I':

        stream_m=float(line.split()[5])
        stream_u=float(line.split()[6])
        stream_l=float(line.split()[7])

        lake_m=float(line.split()[8])
        lake_u=float(line.split()[9])
        lake_l=float(line.split()[10])

        lines['Stream']['main'].append((s0,stream_m))
        lines['Stream']['upper'].append((s0,stream_u))
        lines['Stream']['lower'].append((s0,stream_l))
        lines['Lake']['main'].append((s0,lake_m))
        lines['Lake']['upper'].append((s0,lake_u))
        lines['Lake']['lower'].append((s0,lake_l))   

        if prop=='Gen':
          terr_m=float(line.split()[11])
          terr_u=float(line.split()[12])
          terr_l=float(line.split()[13])  
          lines['Terrestrial']['main'].append((s0,terr_m))
          lines['Terrestrial']['upper'].append((s0,terr_u))
          lines['Terrestrial']['lower'].append((s0,terr_l))

      elif TL=='T':
        marine_m=float(line.split()[5])
        marine_u=float(line.split()[6])
        marine_l=float(line.split()[7])

        if prop!='Gen':
          lake_m=float(line.split()[8])
          lake_u=float(line.split()[9])
          lake_l=float(line.split()[10])

          stream_m=float(line.split()[11])
          stream_u=float(line.split()[12])
          stream_l=float(line.split()[13])

          terr_m=float(line.split()[14])
          terr_u=float(line.split()[15])
          terr_l=float(line.split()[16])  

          lines['Lake']['main'].append((s0,lake_m))
          lines['Lake']['upper'].append((s0,lake_u))
          lines['Lake']['lower'].append((s0,lake_l))   
          lines['Terrestrial']['main'].append((s0,terr_m))
          lines['Terrestrial']['upper'].append((s0,terr_u))
          lines['Terrestrial']['lower'].append((s0,terr_l))
        else:
          stream_m=float(line.split()[8])
          stream_u=float(line.split()[9])
          stream_l=float(line.split()[10])

        lines['Marine']['main'].append((s0,marine_m))
        lines['Marine']['upper'].append((s0,marine_u))
        lines['Marine']['lower'].append((s0,marine_l))        
        lines['Stream']['main'].append((s0,stream_m))
        lines['Stream']['upper'].append((s0,stream_u))
        lines['Stream']['lower'].append((s0,stream_l))

      lines['Estuary']['main'].append((s0,marginal))
      lines['Estuary']['upper'].append((s0,upper))
      lines['Estuary']['lower'].append((s0,lower))
  linefile.close()
  return lines  # This is un-scaled, un-logged weight but was scaled and logged before calculating the y's

def plotter(prop,TL,ecotype,graph):

  dataset=linereader(prop,TL)

  up1=[]  # Greater than 0
  up2=[]  # Less than 0

  down1=[]
  down2=[]

  if dataset[ecotype]['upper']!=[]:
    upper=dataset[ecotype]['upper']
    lower=dataset[ecotype]['lower']
  else:
    upper=dataset['Estuary']['upper']
    lower=dataset['Estuary']['lower']

  for (x,y) in upper:
    if y>=0:
      up1.append((x,y))
    else:
      up2.append((x,y)) 

  for (x,y) in lower:
    if y>=0:
      down1.append((x,y))
    else:
      down2.append((x,y)) 

  upper2=graph.add_dataset(up1)
  upper2.symbol.configure(shape=0)
  upper2.line.configure(linestyle=0,color=10,linewidth=.5)
  upper2.fill.configure(color=5,type=2)

  lower2=graph.add_dataset(down1)
  lower2.symbol.configure(shape=0)
  lower2.line.configure(linestyle=0,color=10,linewidth=.5)
  lower2.fill.configure(color=0,type=2)

  lower2=graph.add_dataset(down2)
  lower2.symbol.configure(shape=0)
  lower2.line.configure(linestyle=0,color=10,linewidth=.5)
  lower2.fill.configure(color=5,type=2)

  upper2=graph.add_dataset(up2)
  upper2.symbol.configure(shape=0)
  upper2.line.configure(linestyle=0,color=10,linewidth=.5)
  upper2.fill.configure(color=0,type=2)

  print dataset['Estuary']['main'][1:10]

  if dataset[ecotype]['main']!=[]:
    main=graph.add_dataset(dataset[ecotype]['main'])
  else:
    main=graph.add_dataset(dataset['Estuary']['main'])
  main.symbol.shape=0
  main.line.configure(linestyle=1,color=1,linewidth=1)

  graph.xaxis.bar.linewidth=1
  graph.yaxis.bar.linewidth=1
  graph.frame.linewidth=1

  # graph.autoscale()
  graph.world.xmin=0
  graph.world.xmax=90

  if TL=='S':
    graph.world.ymin=0
    graph.world.ymax=1.5
    major=.5
    prec=1
  elif TL=='B':
    graph.world.ymin=-1.25
    graph.world.ymax=1
    major=.5
    prec=1
  elif TL=='I':
    graph.world.ymin=-.5
    graph.world.ymax=1.5
    major=.5
    prec=1
  else:
    graph.world.ymin=-2
    graph.world.ymax=1
    major=1
    prec=0

  graph.xaxis.ticklabel.configure(char_size=.75)
  graph.xaxis.tick.configure(major_linewidth=.5,minor_linewidth=.5,major_size=.6,minor_size=.4,major=30)

  graph.yaxis.tick.configure(major=major,major_linewidth=.5,minor_linewidth=.5,major_size=.6,minor_size=.4)
  graph.yaxis.ticklabel.configure(format='decimal',prec=prec,char_size=.75)

  # Fancy-pants labelling
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

  return graph

def main():

  properties=['LS','Gen','Vul']
  TLs=['S','B','I','T']


  names=['','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','']

  for TL in TLs:
    # Make a separate graph for each TL
    grace=MultiPanelGrace(colors=ColorBrewerScheme('Greys'))

    grace.add_label_scheme('dummy',names)
    grace.set_label_scheme('dummy')
    for prop in properties:
      print prop
      for ecotype in ecotypes:
        graph=grace.add_graph(Panel)

        plotter(prop,TL,ecotype,graph)
        # graph.remove_extraworld_drawing_objects()

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

    grace.set_row_xaxislabel(row=2,colspan=(0,4),label='Absolute latitude',place='normal',just=2,char_size=1,perpendicular_offset=0.06)

    grace.hide_redundant_xticklabels()
    grace.hide_redundant_yticklabels()

    if TL=='S':
      troph="Species richness"
    elif TL=="B":
      troph="% Basal resources"
    elif TL=="I":
      troph="% Intermediate consumers"
    else:
      troph=="% Top predators"

    grace.add_drawing_object(DrawText,text='Scaling exponent with '+troph, x=0.045, y=.7137, rot=90,char_size=1,just=2)

    grace.write_file('../manuscript/Figures/by_TL/marginal/'+TL+'_marginal_latitude.eps')


if __name__ == '__main__':
  main()
