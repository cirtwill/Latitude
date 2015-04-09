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

from TL_scaleplots import fixed_reader

def scaleplots(rawdatafile,outfile,Bformat,prop,predfolder):

  ecotypes=['Estuary','Lake','Marine','Stream','Terrestrial']

  xwidth=10
  ywidth=1.5

  dummygrace=MultiPanelGrace(colors=ColorBrewerScheme("RdYlBu",reverse=True,n=253))
  colorbar = dummygrace.add_graph(ElLinColorBar,domain=(0,90),scale=LINEAR_SCALE,autoscale=False)

  grace=MultiPanelGrace(colors=ColorBrewerScheme("RdYlBu",reverse=True,n=253))

  for TL in ['B','I','T','S']:
    graph=grace.add_graph(Panel)

    if TL!='S':
      fixed=fixed_reader(predfolder+'/coefficients/'+prop[0]+TL+'_co.tsv')
    else:
      if rawdatafile=='../non_TS/summary-properties.tsv':
        fixed=fixed_reader('../non_TS/subset/coefficients/'+prop+'_co.tsv')
      else:
        fixed=fixed_reader('../mod_data/subset/coefficients/'+prop+'_co.tsv')


    for ecotype in ecotypes:
      ecoline=[]

      if TL=='B':
        key='Basal'
        xtex='% Basal'
      elif TL=='I':
        key='Intermediate'
        xtex='% Intermediate'
      elif TL=='T':
        key='Toppreds'
        xtex='% Top'
      else:
        key='Species'
        xtex='Species Richness'

      if 'log10('+key+'):'+ecotype in fixed:
        delta=fixed['log10('+key+')']+fixed['log10('+key+'):'+ecotype ]
      else:
        delta=fixed['log10('+key+')']

      if 'log10('+key+'):Latitude' in fixed:
        if 'log10('+key+'):'+ecotype+':Latitude' in fixed:
          gamma=fixed['log10('+key+'):Latitude']+fixed['log10('+key+'):'+ecotype+':Latitude']
        else:
          gamma=fixed['log10('+key+'):Latitude']
      else:
        gamma=0

      for lat in range(0,91):
        exponent=delta+lat*gamma
        ecoline.append((lat,exponent))

      ecoplot=graph.add_dataset(ecoline)
      ecoplot.symbol.shape=0

    graph.xaxis.label.configure(text=xtex,place='opposite',char_size=.75)
    graph.legend.configure(loc=(210,30),loctype='world',char_size=.75)
    graph.legend.box_linestyle=0

    graph.world.xmin=0
    graph.world.xmax=90
    graph.xaxis.tick.major=15

    graph.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
    graph.xaxis.ticklabel.configure(char_size=.75)
    graph.frame.linewidth=1
    graph.xaxis.bar.linewidth=1
    graph.yaxis.bar.linewidth=1

    graph.world.ymin=-1.4
    graph.world.ymax=1.2
    graph.yaxis.tick.major=.2

    graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
    graph.yaxis.ticklabel.configure(char_size=.75)
    graph.panel_label.configure(char_size=0)

  grace.multi(rows=1,cols=4,vgap=.08,hgap=.04)
  # grace.set_row_xaxislabel(row=3,colspan=(0,4),label='Species richness',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  # grace.set_row_xaxislabel(row=2,colspan=(0,4),label='Proportion',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  # grace.set_row_xaxislabel(row=1,colspan=(0,4),label='Proportion of Intermediate consumers',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  # grace.set_row_xaxislabel(row=0,colspan=(0,4),label='Proportion of Basal resources',place='normal',just=2,char_size=1,perpendicular_offset=0.05)
  grace.add_drawing_object(DrawText,text="Latitude (degrees from equator)", x=.5,y=.725,char_size=.75)

  if prop=='LS':
    yax="Exponent"
  elif prop=='Gen':
    yax="Generality"
  elif prop=='Vul':
    yax="Vulnerability"
  grace.set_col_yaxislabel(col=0,rowspan=(0,0),label=yax,place='normal',just=2,char_size=.75,perpendicular_offset=0.05)
  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()


  grace.write_file(outfile)

def main():

  for Bformat in ['proportions']:#['numbers','proportions']:
    for rawdatafile in ['../non_TS/summary-properties.tsv']:#,'../mod_data/summary-properties.tsv']:
      for prop in ['LS']:#,'Gen','Vul']:
        outfile='../manuscript/Figures/test.eps'
        predfolder='../non_TS/'+Bformat
        scaleplots(rawdatafile,outfile,Bformat,prop,predfolder)

 
if __name__ == '__main__':
  main()