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

#Global mins, means, maxes. Doing by ecotype below.
mean_C=0.1
min_C=0.025
max_C=0.25

mean_S=36
min_S=5
max_S=109

#Mean dist from centroids
def linereader(fw_prop):
  linefile="../mod_data/"+fw_prop+"_coeffs"
  linetab=open(linefile,'r')
  lines={}
  for line in linetab:
    if len(line.split())==5:
      effect=line.split()[0][1:-1]
      beta=float(line.split()[1])
      lines[effect]=beta
  return lines

def datareader(fw_prop):
  datafile="../mod_data/summary-properties.tsv"
  f=open(datafile,'r')
  datadict={'estuary':{},'lake':{},'terrestrial':{},'marine':{},'stream':{}}
  for line in f:
    if line.split()[0]=='Web':
      heads=line.split()[1:]
    else:
      web=line.split()[0]
      data=line.split()[1:]
      Ecotype=line.split()[1]
      if Ecotype in datadict:
        datadict[Ecotype][web]={}
        for term in range(0,len(heads)):
          if term!='Ecotype':
            datadict[Ecotype][web][heads[term]]=data[term]
  return datadict

def plotter(fw_prop,ecotype):
  datadict=datareader(fw_prop)
  lines=linereader(fw_prop)

  realdata={}
  realdata={'Species':[],'Connectance':[],'Latitude':[],fw_prop:[]}

  for web in datadict[ecotype]:
    for term in realdata:
      realdata[term].append(float(datadict[ecotype][web][term]))

  mean_C=float(sum(realdata['Connectance']))/len(realdata['Connectance'])
  min_C=float(sorted(realdata['Connectance'])[1])
  max_C=float(sorted(realdata['Connectance'])[-1])

  mean_S=float(sum(realdata['Species']))/len(realdata['Species'])
  min_S=float(sorted(realdata['Species'])[1])
  max_S=float(sorted(realdata['Species'])[-1])

  if fw_prop != 'pInt':
    if ecotype!='estuary':
      inter=lines['(Intercept)']+lines['Ecotype'+ecotype]
    else:
      inter=lines['(Intercept)'] 
  else:
    inter=lines['(Intercept)'] 

  #Since I'm talking about things over latitude, Lat is the x-axis
  linedata={}
  for C in ['min_C','mean_C','max_C']:
    linedata[C]={}
    conn=eval(C)
    if fw_prop!='mean_SWTL':
      con=math.log(conn,10)*lines['log10(Connectance)']
    else:
      con=0

    for S in ['min_S','mean_S','max_S']:
      spec=eval(S)
      linedata[C][S]=[]

      sp=math.log(spec,10)*lines['log10(Species)']

      for i in range(0,90):
        Latitude=i

        lat=Latitude*lines['Latitude']+Latitude*math.log(spec,10)*lines['Latitude:log10(Species)']

        if fw_prop == 'pInt':
          lat=lat+Latitude*math.log(conn,10)*lines['Latitude:log10(Connectance)']

        if fw_prop =='pInt':
          type_adjust=0
        elif fw_prop=='max_SWTL':
          if ecotype!='estuary': #Was the intercept level
            type_adjust=math.log(conn,10)*lines['Ecotype'+ecotype+':log10(Connectance)']
          else:
            type_adjust=0
        elif fw_prop=='mean_SWTL':
          if ecotype!='estuary': #Was the intercept level
            type_adjust=math.log(spec,10)*lines['Ecotype'+ecotype+':log10(Species)']
          else:
            type_adjust=0
        else:
          if ecotype!='estuary': #Was the intercept level
            type_adjust=math.log(spec,10)*lines['log10(Species):Ecotype'+ecotype]
          else:
            type_adjust=0

        point=(Latitude,10**(inter+type_adjust+con+sp+lat))

        if fw_prop =='pInt':
          point=(Latitude,(inter+type_adjust+con+sp+lat))

        linedata[C][S].append(point)


  realpoints=[]
  for i in range(0,len(realdata['Latitude'])):
    realpoints.append((realdata['Latitude'][i],realdata[fw_prop][i]))


  grace=MultiPanelGrace(colors=ColorBrewerScheme('Paired'))
  for C_level in ['min_C','mean_C','max_C']:
    if C_level=='min_C':
      colo=2
    elif C_level=='mean_C':
      colo=10
    else:
      colo=6

    graph=grace.add_graph(Panel) 

    lowdata=graph.add_dataset(linedata[C_level]['min_S'],type='xy')
    lowdata.symbol.shape=0
    lowdata.line.configure(color=colo,linewidth=2)

    middata=graph.add_dataset(linedata[C_level]['mean_S'],type='xy')
    middata.symbol.shape=0
    middata.line.configure(color=colo+1,linewidth=2)

    highdata=graph.add_dataset(linedata[C_level]['max_S'],type='xy')
    highdata.symbol.shape=0
    highdata.line.configure(color=colo+2,linewidth=2)

    really=graph.add_dataset(realpoints,type='xy')
    really.line.type=0
    really.symbol.configure(color=1,fill_color=1)

    graph.world.xmax=90
    graph.world.xmin=0

    if fw_prop=='Gen':
      graph.world.ymax=30
    else:
      graph.world.ymax=6
    graph.legend.configure(loc=(1.1,.5),loctype='view')

    graph.xaxis.label.configure(text='Degrees from equator',char_size=1)
    graph.xaxis.tick.configure(major=30,major_size=.8,minor_size=.5)
    graph.xaxis.ticklabel.configure(char_size=.8)
    graph.yaxis.label.configure(text=fw_prop,char_size=1)
    if fw_prop!='Gen':
      graph.yaxis.tick.configure(major=2,major_size=.8,minor_size=.5)
    else:
      graph.yaxis.tick.configure(major=10,major_size=.8,minor_size=.5)

    graph.yaxis.ticklabel.configure(char_size=.8)

  grace.multi(rows=1,cols=3,hgap=0.05)
  grace.hide_redundant_labels()
  grace.write_file('../manuscript/Figures/'+fw_prop+'/'+ecotype+'.eps')



def main():

  for fw_prop in ["Gen",'max_SWTL','mean_SWTL']:
    for ecotype in ['stream','terrestrial','marine','estuary','lake']:
      plotter(fw_prop,ecotype)

if __name__ == '__main__':
  main()