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
from PyGrace.axis import LINEAR_SCALE, LOGARITHMIC_SCALE
from PyGrace.Styles.el import ElGraph, ElLinColorBar, ElLogColorBar
from PyGrace.Extensions.latex_string import LatexString, CONVERT

# Ranges of the parameters
weight_min=0.1
weight_max=2800
prey_gen_min=0
prey_gen_max=10

ecotypes=['Estuary','Lake','Marine','Stream','Terrestrial']

def linereader(prop,TL):
  linefile=open('../non_TS/proportions/marginals/'+prop+'_'+TL+'_marginal.tsv','r')
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
        if prop!='Gen':
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
          lake_m=float(line.split()[5])
          lake_u=float(line.split()[6])
          lake_l=float(line.split()[7])

          stream_m=float(line.split()[8])
          stream_u=float(line.split()[9])
          stream_l=float(line.split()[10])

          lines['Lake']['main'].append((s0,lake_m))
          lines['Lake']['upper'].append((s0,lake_u))
          lines['Lake']['lower'].append((s0,lake_l))
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

        if prop=='Gen':
          lake_m=float(line.split()[8])
          lake_u=float(line.split()[9])
          lake_l=float(line.split()[10])

          lines['Lake']['main'].append((s0,lake_m))
          lines['Lake']['upper'].append((s0,lake_u))
          lines['Lake']['lower'].append((s0,lake_l))   

        lines['Stream']['main'].append((s0,stream_m))
        lines['Stream']['upper'].append((s0,stream_u))
        lines['Stream']['lower'].append((s0,stream_l))

      elif TL=='T':
        marine_m=float(line.split()[5])
        marine_u=float(line.split()[6])
        marine_l=float(line.split()[7])

        lake_m=float(line.split()[8])
        lake_u=float(line.split()[9])
        lake_l=float(line.split()[10])

        stream_m=float(line.split()[11])
        stream_u=float(line.split()[12])
        stream_l=float(line.split()[13])

        if prop!='Gen':
          terr_m=float(line.split()[14])
          terr_u=float(line.split()[15])
          terr_l=float(line.split()[16])  

          lines['Terrestrial']['main'].append((s0,terr_m))
          lines['Terrestrial']['upper'].append((s0,terr_u))
          lines['Terrestrial']['lower'].append((s0,terr_l))

        lines['Marine']['main'].append((s0,marine_m))
        lines['Marine']['upper'].append((s0,marine_u))
        lines['Marine']['lower'].append((s0,marine_l))        
        lines['Lake']['main'].append((s0,lake_m))
        lines['Lake']['upper'].append((s0,lake_u))
        lines['Lake']['lower'].append((s0,lake_l))   
        lines['Stream']['main'].append((s0,stream_m))
        lines['Stream']['upper'].append((s0,stream_u))
        lines['Stream']['lower'].append((s0,stream_l))

      lines['Estuary']['main'].append((s0,marginal))
      lines['Estuary']['upper'].append((s0,upper))
      lines['Estuary']['lower'].append((s0,lower))
  linefile.close()
  return lines  # This is un-scaled, un-logged weight but was scaled and logged before calculating the y's


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
      B=float(line.split('\t')[-4])
      H=float(line.split('\t')[-3])
      I=float(line.split('\t')[-2])
      I=I+H # Include the herbivores as intermediates
      T=float(line.split('\t')[-1])

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
      B=float(line.split('\t')[-4])
      H=float(line.split('\t')[-3])
      I=float(line.split('\t')[-2])
      I=H+I
      T=float(line.split('\t')[-1])

      # Correct the observed property based on latitude and ecotype effects

      if 'log('+key+'):'+ecotype in fixed_effects: 
        delta=fixed_effects['log('+key+'):'+ecotype]
      else:   # If there's no intercept effect of ecosystem
        delta=0

      if 'log('+key+'):Latitude' in fixed_effects:
        gamma=fixed_effects['log('+key+'):Latitude'] 
      else: # If there's no latitude effect at all
        gamma=0
      if ecotype=='Lake':
        gamma=gamma+fixed_effects['log(Species):Lake:Latitude']

      exponent=Latitude*gamma

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

def predictionlines(fixed,prop,TL,ecotype):
  if ecotype=='Terrestrial':
    ecotype="Terr"
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

  # Intercept adjustment for ecosystem type
  if 'log(Species):'+ecotype in fixed.keys():
    rho=fixed['log(Species):'+ecotype]
  else:
    rho=0

  for S in range(1,350):
    predy=alpha*(S**delta)*(S**rho)
    # predy=alpha*(S**gamma)    
    ecoline.append((S,predy))

  return ecoline

def scaleplots(rawdatafile,Bformat,TL,prop,graph2,ecolist):

  rawdata=datareader(rawdatafile,TL,Bformat)
  fixed=fixed_reader('../non_TS/coefficients/'+prop+'_co.tsv')

  if ecolist==["Lake"]:
    ecotype="Lake"
    predictions=predictionlines(fixed,prop,TL,ecotype)
    predline=graph2.add_dataset(predictions)
    predline.symbol.shape=0
    predline.line.configure(linestyle=1,color="purble",linewidth=2.5)

    heatpoints=heatmappoints(rawdatafile,fixed,prop,ecotype,TL,Bformat)
    datadict=heatpoints[ecotype]
    obspoints=graph2.add_dataset(datadict)
    obspoints.line.configure(linestyle=0)
    obspoints.symbol.configure(size=.5,shape=1,fill_color="purble",fill_pattern=1,color=1,linewidth=.5)

    obspoints.legend='Lake'

  else:
    for ecotype in ecolist:
      if ecotype != 'Terrestrial':
        if ecotype=='Estuary': # Lets only make the line once
          predictions=predictionlines(fixed,prop,TL,ecotype)
          predline=graph2.add_dataset(predictions)
          predline.symbol.shape=0
          predline.line.configure(linestyle=1,color=4,linewidth=2)

        heatpoints=heatmappoints(rawdatafile,fixed,prop,ecotype,TL,Bformat)
        datadict=heatpoints[ecotype]
        obspoints=graph2.add_dataset(datadict)
        obspoints.line.configure(linestyle=0)
        if ecotype=='Estuary':
          schap=2
        elif ecotype=='Stream':
          schap=4
        else:
          schap=3
        obspoints.symbol.configure(size=.5,shape=schap,fill_color=4,fill_pattern=1,color=1,linewidth=.5)
        obspoints.legend=ecotype

      else:
        predictions=predictionlines(fixed,prop,TL,ecotype)
        predline=graph2.add_dataset(predictions)
        predline.symbol.shape=0
        predline.line.configure(linestyle=1,color=12,linewidth=2)

        heatpoints=heatmappoints(rawdatafile,fixed,prop,ecotype,TL,Bformat)
        datadict=heatpoints[ecotype]
        obspoints=graph2.add_dataset(datadict)
        obspoints.line.configure(linestyle=0)
        obspoints.symbol.configure(size=.5,shape=1,fill_color=12,fill_pattern=1,color=1,linewidth=.5)

        obspoints.legend='Terrestrial'

  if ecotype=='lake':
    graph2.legend.configure(loc=(40,.56),loctype='world',char_size=.75,box_linestyle=0)
  else:
    graph2.legend.configure(loc=(40,1.6),loctype='world',char_size=.75,box_linestyle=0)

  graph2.world.xmin=1
  graph2.world.xmax=300
  graph2.world.ymin=.1
  graph2.world.ymax=100

  graph2.xaxis.set_log()
  graph2.yaxis.set_log()

  graph2.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
  graph2.xaxis.ticklabel.configure(char_size=.75)
  graph2.frame.linewidth=1
  graph2.xaxis.bar.linewidth=1
  graph2.yaxis.bar.linewidth=1

  graph2.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
  graph2.yaxis.ticklabel.configure(char_size=.75)
  graph2.panel_label.configure(char_size=.75,placement='iul',dy=.03,dx=.04)

  graph2.xaxis.label.configure(text='Species richness',place='normal',char_size=1)
  return graph2

def plotter(prop,TL,ecolist,graph):

  dataset=linereader(prop,TL)

  if ecolist==["Lake"]:
    ecotype='Lake'

    up=[]  # Greater than 0
    down=[]

    upper=dataset[ecotype]['upper']
    lower=dataset[ecotype]['lower']

    for (x,y) in upper:
      up.append((x,y))

    for (x,y) in lower:
      down.append((x,y))

    upper=graph.add_dataset(up)
    upper.symbol.configure(shape=0)
    upper.line.configure(linestyle=1,color=1,linewidth=.5)
    upper.fill.configure(color="grey",type=2)

    lower=graph.add_dataset(down)
    lower.symbol.configure(shape=0)
    lower.line.configure(linestyle=1,color=1,linewidth=.5)
    lower.fill.configure(color=0,type=2)

    main=graph.add_dataset(dataset[ecotype]['main'])
    main.symbol.shape=0
    main.line.configure(linestyle=1,color="purble",linewidth=2)
    main.legend="Lake"

  else:

    topper=[]
    tops=dataset['Estuary']['upper']
    for (x,y) in tops:
      topper.append((x,y))
    topd=graph.add_dataset(topper)
    topd.symbol.shape=0
    topd.line.configure(linestyle=1,color=1,linewidth=.5)
    topd.fill.configure(color=5,type=2,pattern=1)

    midtop=[]
    mtps=dataset['Terrestrial']['upper']
    for (x,y) in mtps:
      midtop.append((x,y))
    midt=graph.add_dataset(midtop)
    midt.symbol.shape=0
    midt.line.configure(linestyle=1,color=1,linewidth=.5)
    midt.fill.configure(color='grey',type=2,pattern=1)

    greens=[]
    grs=dataset['Estuary']['lower']
    for (x,y) in grs:
      greens.append((x,y))
    greeny=graph.add_dataset(greens)
    greeny.symbol.shape=0
    greeny.line.configure(linestyle=1,color=1,linewidth=.5)
    greeny.fill.configure(color=9,type=2,pattern=1)

    whites=[]
    wts=dataset['Terrestrial']['lower']
    for (x,y) in wts:
      whites.append((x,y))
    whitey=graph.add_dataset(whites)
    whitey.symbol.shape=0
    whitey.line.configure(linestyle=1,color=1,linewidth=.5)
    whitey.fill.configure(color=0,type=2,pattern=1)


    for ecotype in ["Estuary","Terrestrial"]:  # Stream and marine are identical to estuary
      if ecotype =='Estuary':
        colo=3
      elif ecotype=='Terrestrial':
        colo=12

      if dataset[ecotype]['main']!=[]:
        main=graph.add_dataset(dataset[ecotype]['main'])
      else:
        main=graph.add_dataset(dataset['Estuary']['main'])

      main.symbol.shape=0
      main.line.configure(linestyle=1,color=colo,linewidth=2)
      if ecotype=='Terrestrial':
        main.legend=ecotype
      elif ecotype=='Estuary':
        main.legend="Estuarine, Stream, Marine"

  graph.legend.configure(loc=(5,.3),loctype='world',char_size=.75,box_linestyle=0,box_fill=0)
    # graph.legend.configure(loc=(.95,-2),loctype='world',char_size=1.6,box_linestyle=0,box_fill=0)
  graph.panel_label.configure(char_size=.75,placement='iul',dy=.03,dx=.04)

  graph.xaxis.bar.linewidth=1
  graph.yaxis.bar.linewidth=1
  graph.frame.linewidth=1

  # graph.autoscale()
  graph.world.xmin=0
  graph.world.xmax=90

  graph.world.ymin=0
  graph.world.ymax=1.200000001

  graph.xaxis.ticklabel.configure(char_size=.75)
  graph.xaxis.tick.configure(major_linewidth=.5,minor_linewidth=.5,major_size=.6,minor_size=.4,major=30)

  graph.yaxis.tick.configure(major=.4,major_linewidth=.5,minor_linewidth=.5,major_size=.6,minor_size=.4)
  graph.yaxis.ticklabel.configure(format='decimal',prec=1,char_size=.75)

  graph.xaxis.label.configure(text='Absolute latitude',place='normal',char_size=1)

  return graph

def main():

  prop='LS'
  TL='S'
  ecolists=[['Terrestrial','Marine','Stream','Estuary'],["Lake"]]
  rawdatafile='../non_TS/summary-properties.tsv'
  predfolder='../non_TS/proportions'
  Bformat='proportions'

  colors=ColorBrewerScheme('PRGn')
  colors.add_color(200, 200, 200,"grey")  
  colors.add_color(8,  81,  156, "purble")
  colors.add_color(168, 211, 191, "ltgreen")

  grace=MultiPanelGrace(colors=colors)
  grace.add_label_scheme('dummy',['A','C','B','D'])
  grace.set_label_scheme('dummy')
  for ecolist in ecolists:
    graph=grace.add_graph(Panel)
    plotter(prop,TL,ecolist,graph)

    graph2=grace.add_graph(Panel)
    scaleplots(rawdatafile,Bformat,TL,prop,graph2,ecolist)


  grace.multi(rows=2,cols=2,vgap=.04,hgap=.15)

  # grace.set_row_xaxislabel(row=1,colspan=(0,1),label='Absolute latitude',place='normal',just=2,char_size=1,perpendicular_offset=0.06)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_xlabels()
  # grace.hide_redundant_yticklabels()

  # grace.add_drawing_object(DrawText,text='Scaling with '+troph, x=0.045, y=.7137, rot=90,char_size=1,just=2)
  grace.set_col_yaxislabel(col=0,rowspan=(0,1),label='Scaling exponent of link density with species richness',just=2,char_size=1,perpendicular_offset=.07)
  grace.set_col_yaxislabel(col=1,rowspan=(0,1),label='Link density',just=2,char_size=1,perpendicular_offset=.07)

  grace.write_file('../manuscript/Figures/by_TL/marginal/minifig.eps')
  grace.write_file('../manuscript/Figures/by_TL/marginal/minifig.jpg')


if __name__ == '__main__':
  main()
