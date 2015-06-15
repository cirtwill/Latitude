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
  print prop, TL
  linefile=open('../non_TS/marginals/'+prop+'_'+TL+'_marginal.tsv','r')
  lines={}
  for ecotype in ecotypes:
    lines[ecotype]={'main':[],'upper':[],'lower':[]}

  for line in linefile:
    if line.split()[0] not in ['"Species"','"Basal"','"Intermediate"','"Top"']:
      s0=float(line.split()[1])
      marginal=float(line.split()[2])
      upper=float(line.split()[3])
      lower=float(line.split()[4])

      if TL=='S':
        lake_m=float(line.split()[5])
        lake_u=float(line.split()[6])
        lake_l=float(line.split()[7])

        lines['Lake']['main'].append((s0,lake_m))
        lines['Lake']['upper'].append((s0,lake_u))
        lines['Lake']['lower'].append((s0,lake_l))

        if prop=='Gen':
          stream_m=float(line.split()[8])
          stream_u=float(line.split()[9])
          stream_l=float(line.split()[10])
    
          lines['Stream']['main'].append((s0,stream_m))
          lines['Stream']['upper'].append((s0,stream_u))
          lines['Stream']['lower'].append((s0,stream_l))

      lines['Estuary']['main'].append((s0,math.exp(s0*marginal)))
      lines['Estuary']['upper'].append((s0,math.exp(s0*upper)))
      lines['Estuary']['lower'].append((s0,math.exp(s0*lower)))
  linefile.close()
  return lines  # This is un-scaled, un-logged weight but was scaled and logged before calculating the y's

def splitlinereader(combo,plottype): # For count, only CP in GC had two hosts
  linefile=open('../../data/R_datafiles/predictions/'+combo[0]+'/'+combo[1]+'_Host'+plottype+'.tsv','r')
  lines={'Paracalliope':{'main':[],'upper':[],'lower':[]},'Paracorophium':{'main':[],'upper':[],'lower':[]}}

  for line in linefile:
    if line.split()[0]!='"Weight"':
      w0=float(line.split()[1])
      marginal1=float(line.split()[2])
      upper1=float(line.split()[3])
      lower1=float(line.split()[4])
      marginal2=float(line.split()[5])
      upper2=float(line.split()[6])
      lower2=float(line.split()[7])

      lines['Paracalliope']['main'].append((w0,marginal1))
      lines['Paracalliope']['upper'].append((w0,upper1))
      lines['Paracalliope']['lower'].append((w0,lower1))

      lines['Paracorophium']['main'].append((w0,marginal2))
      lines['Paracorophium']['upper'].append((w0,upper2))
      lines['Paracorophium']['lower'].append((w0,lower2))
  linefile.close()
  return lines

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

  if dataset[ecotype]['main']!=[]:
    main=graph.add_dataset(dataset[ecotype]['main'])
  else:
    main=graph.add_dataset(dataset['Estuary']['main'])
  main.symbol.shape=0
  main.line.configure(linestyle=1,color=1,linewidth=1)

  graph.xaxis.bar.linewidth=1
  graph.yaxis.bar.linewidth=1
  graph.frame.linewidth=1


  graph.xaxis.set_log()
  # graph.autoscale()
  if TL=='S':
    graph.world.xmin=1
    graph.world.xmax=200
  else:
    graph.world.xmin=0.0001
    graph.world.xmax=1

  if prop!='Gen':
    graph.world.ymin=-.015
    graph.world.ymax=4
    major=2
    prec=0
  else:
    graph.world.ymin=-.02
    graph.world.ymax=4
    major=2
    prec=0

  graph.xaxis.ticklabel.configure(char_size=.75)
  graph.xaxis.tick.configure(major_linewidth=.5,minor_linewidth=.5,major_size=.8,minor_size=.5)

  graph.yaxis.tick.configure(major=major,major_linewidth=.5,minor_linewidth=.5,major_size=.8,minor_size=.5)
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
  TLs=['S']#,'B','I','T']

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

    if TL!='S':
      grace.set_row_xaxislabel(row=2,colspan=(0,4),label='Proportion of '+xtex,place='normal',just=2,char_size=1,perpendicular_offset=0.06)
    else:
      grace.set_row_xaxislabel(row=2,colspan=(0,4),label=xtex,place='normal',just=2,char_size=1,perpendicular_offset=0.06)

    grace.hide_redundant_xticklabels()
    grace.hide_redundant_yticklabels()

    grace.add_drawing_object(DrawText,text='Marginal effect of latitude on:', x=0.045, y=.7137, rot=90,char_size=.75,just=2)

    grace.write_file('../manuscript/Figures/by_TL/marginal/'+TL+'_marginal_latitude.eps')


if __name__ == '__main__':
  main()
