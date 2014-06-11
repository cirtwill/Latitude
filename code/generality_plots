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

def plotter(fw_prop):
  datadict=datareader(fw_prop)
  lines=linereader(fw_prop)

  realdata={}
  for webtype in datadict:
    realdata[webtype]={'Species':[],'Connectance':[],'Latitude':[],fw_prop:[]}
    for web in datadict[webtype]:
      for term in realdata[webtype]:
        realdata[webtype][term].append(float(datadict[webtype][web][term]))

  #Since I'm talking about things over latitude, Lat is the x-axis
  linedata={}
  for webtype in realdata:
    linedata[webtype]=[]
    for i in range(0,90):
      Latitude=i
      Species=mean_S
      Connectance=mean_C

      type_dependent=[]
      interactions=[]

      if 'log10(Connectance)' in lines:
        con=math.log(Connectance,10)*lines['log10(Connectance)']
      else:
        con=0
      if 'log10(Species)' in lines:
        sp=math.log(Species,10)*lines['log10(Species)']
      else:
        sp=0
      if 'Latitude' in lines:
        lat=Latitude*lines['Latitude']
      else:
        lat=0
      inter=lines['(Intercept)']

      for term in lines:
        if 'Ecotype' in term:
          if webtype in term:
            type_dependent.append(term)
        else:
          if len(term.split(':'))>1:
            interactions.append(term)

      for term in type_dependent:
        if len(term.split(':'))==1:
          inter=inter+lines[term]
        else:
          if 'Species' in term and 'Connectance' not in term:
            sp=sp+math.log(Species,10)*lines[term]
          elif 'Connectance' in term and 'Species' not in term:
            con=con+math.log(Connectance,10)*lines[term]
          elif 'species' in term and 'Connectance' in term:
            print 'thats a big one'
            sys.exit()
          elif 'Latitude' in term:
            if 'Species' not in term and 'Connectance' not in term:
              lat=lat+Latitude*lines[term]
            else:
              print term
              print 'more work yet to do'
              sys.exit()

      y=inter+lat+sp+con

      for term in interactions:
        if len(term.split(':'))==2:
          if 'Species' in term and 'Latitude' in term:
            add=Latitude*math.log(Species,10)*lines[term]
          elif 'Connectance' in term and 'Latitude' in term:
            add=Latitude*math.log(Connectance,10)*lines[term]
          else:
            add=math.log(Connectance,10)*math.log(Species,10)*lines[term]
          y=y+add
        else:
          print term, 'too big'
          sys.exit()

      try:
        float(fw_prop)
        point=(Latitude,y)
      except:
        point=(Latitude,10**y)

      linedata[webtype].append(point)

  
  grace=Grace(colors=ColorBrewerScheme('Paired'))
  graph=grace.add_graph()

  for webtype in linedata:
    realpoints=[]
    for i in range(0,len(realdata[webtype]['Latitude'])):
      realpoints.append((realdata[webtype]['Latitude'][i],realdata[webtype][fw_prop][i]))

    data=graph.add_dataset(linedata[webtype],type='xy')
    data.symbol.shape=0
    if webtype=='marine':
      colo=3
    elif webtype=='stream':
      colo=11
    elif webtype=='estuary':
      colo=9
    elif webtype=='lake':
      colo=7
    else:
      colo=5
    data.legend=webtype
    data.line.configure(color=colo,linewidth=2)

    really=graph.add_dataset(realpoints,type='xy')
    really.line.type=0
    really.symbol.configure(color=colo,fill_color=colo)


  graph.world.xmax=90
  graph.world.xmin=0
  graph.world.ymax=int(sorted(realdata['marine'][fw_prop])[-1])+2
  graph.legend.configure(loc=(1.1,.5),loctype='view')

  graph.xaxis.label.configure(text='Degrees from equator',char_size=1)
  graph.xaxis.tick.configure(major=10,major_size=1,minor_size=.7)
  graph.xaxis.ticklabel.configure(char_size=.8)
  graph.yaxis.label.configure(text=fw_prop,char_size=1)
  if graph.world.ymax<10:
    graph.yaxis.tick.configure(major=1,major_size=1,minor_size=.7)
  else:
    graph.yaxis.tick.configure(major=5,major_size=1,minor_size=.7)

  graph.yaxis.ticklabel.configure(char_size=.8)

  grace.write_file('../manuscript/Figures/'+fw_prop+'_medium.eps')



def main():
  for fw_prop in ["LinkSD","Gen","GenSD","Vul","VulSD","mean_SWTL","max_SWTL",
                  "Path","pBas","pInt","pTop","pHerb","pOmni","102","108",
                  "110","12","14","238","36","38","46","6","74","78","98"]:
    plotter(fw_prop)

if __name__ == '__main__':
  main()