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

propnames={'gen':'Gen','vul':'Vul','TL':'mean_SWTL','path':'Path'}

#Mean dist from centroids
def linereader(fw_prop):
  linefile="../mod_data/fresh_"+fw_prop+".coeffs"
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
  datadict={'estuary':{},'freshwater':{},'terrestrial':{},'marine':{}}
  for line in f:
    if line.split()[0]=='Web':
      heads=line.split()[1:]
    else:
      web=line.split()[0]
      data=line.split()[1:]
      Ecotype=line.split()[1]
      Ecotype2=line.split()[2]
      if Ecotype2 in datadict:
        datadict[Ecotype2][web]={}
        for term in range(0,len(heads)):
          if term not in ['Ecotype2','Ecotype']:
            datadict[Ecotype2][web][heads[term]]=data[term]
  return datadict

def plotter(fw_prop):
  lines=linereader(fw_prop)
  datadict=datareader(fw_prop)

  colors=ColorBrewerScheme('GnBu')
  colors.add_color(255, 85, 66,"land")
  colors.add_color(0,204,196,"fresh")
  colors.add_color(20,167,204,"est")
  colors.add_color(42,95,153,"marine")

  grace=MultiPanelGrace(colors=colors)
  grace.add_label_scheme("ecotypes",['terrestrial','freshwater','estuary','marine'])
  grace.set_label_scheme("ecotypes")

  for ecotype2 in ['terrestrial','freshwater','estuary','marine']:
    graph=grace.add_graph(Panel) 

    if ecotype2=='terrestrial':
      colo="land"
    elif ecotype2=='freshwater':
      colo="fresh"
    elif ecotype2=='estuary':
      colo="est"
    else:
      colo="marine"

    realdata={}
    realdata={'Species':[],'Connectance':[],'Latitude':[],propnames[fw_prop]:[]}

    for web in datadict[ecotype2]:
      for term in realdata:
        realdata[term].append(float(datadict[ecotype2][web][term]))

    mean_C=float(sum(realdata['Connectance']))/len(realdata['Connectance'])
    min_C=float(sorted(realdata['Connectance'])[1])
    max_C=float(sorted(realdata['Connectance'])[-1])

    mean_S=float(sum(realdata['Species']))/len(realdata['Species'])
    min_S=float(sorted(realdata['Species'])[1])
    max_S=float(sorted(realdata['Species'])[-1])

    if fw_prop != 'vul':
      if ecotype2!='estuary':
        inter=lines['(Intercept)']+lines['Ecotype2'+ecotype2]
      else:
        inter=lines['(Intercept)'] 
    else:
      inter=lines['(Intercept)'] 

    #Since I'm talking about things over latitude, Lat is the x-axis
    linedata={}
    if fw_prop!='path':
      for C in ['min_C','mean_C','max_C']:
        linedata[C]={}
        conn=eval(C)
        if fw_prop!='TL':
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

            if fw_prop=='TL':
              if ecotype2!='estuary': #Was the intercept level
                type_adjust=math.log(spec,10)*lines['Ecotype2'+ecotype2+':log10(Species)']
              else:
                type_adjust=0
            elif fw_prop=='vul':
              type_adjust=0
            else:
              if ecotype2!='estuary': #Was the intercept level
                type_adjust=math.log(spec,10)*lines['log10(Species):Ecotype2'+ecotype2]
              else:
                type_adjust=0

            point=(Latitude,10**(inter+type_adjust+con+sp+lat))
            linedata[C][S].append(point)
    else:
      for C in ['min_C','mean_C','max_C']:
        linedata[C]=[]
        for i in range(0,90):
          Latitude=i
          conn=eval(C)
          con=math.log(conn,10)*lines['log10(Connectance)']
          lat=Latitude*lines['Latitude']
          if ecotype2!='estuary':
            type_adjust=math.log(conn,10)*lines['Ecotype2'+ecotype2+':log10(Connectance)']
          else:
            type_adjust=0
          point=(Latitude,10**(inter+type_adjust+con+lat))
          linedata[C].append(point)

    realpoints=[]
    for i in range(0,len(realdata['Latitude'])):
      realpoints.append((realdata['Latitude'][i],realdata[propnames[fw_prop]][i]))


    if fw_prop!='path':
      C_level='mean_C'
      lowdata=graph.add_dataset(linedata[C_level]['min_S'],type='xy')
      lowdata.symbol.shape=0
      lowdata.line.configure(color=colo,linewidth=2,linestyle=2)

      middata=graph.add_dataset(linedata[C_level]['mean_S'],type='xy')
      middata.symbol.shape=0
      middata.line.configure(color=colo,linewidth=2,linestyle=3)

      highdata=graph.add_dataset(linedata[C_level]['max_S'],type='xy')
      highdata.symbol.shape=0
      highdata.line.configure(color=colo,linewidth=2)

    else:
      for C_level in ['min_C','mean_C','max_C']:
        lowdata=graph.add_dataset(linedata[C_level],type='xy')
        lowdata.symbol.shape=0
        if C_level=='min_C':
          lowdata.line.configure(color=colo,linewidth=2,linestyle=2)
        elif C_level=='mean_C':
          lowdata.line.configure(color=colo,linewidth=2,linestyle=3)
        else:
          lowdata.line.configure(color=colo,linewidth=2)

    # really=graph.add_dataset(realpoints,type='xy')
    # really.line.type=0
    # really.symbol.configure(color=1,fill_color=1)

    graph.world.xmax=90
    graph.world.xmin=0

    if fw_prop=='gen':
      graph.world.ymax=30
      graph.yaxis.tick.configure(major=5,major_size=.6,minor_size=.4,major_linewidth=.8,minor_linewidth=.8)
    elif fw_prop=='vul':
      graph.world.ymax=4
      graph.yaxis.tick.configure(major=1,major_size=.6,minor_size=.4,major_linewidth=.8,minor_linewidth=.8)
    elif fw_prop=='path':
      graph.world.ymax=1.6
      graph.yaxis.tick.configure(major=0.4,major_size=.6,minor_size=.4,major_linewidth=.8,minor_linewidth=.8)
    else:
      graph.world.ymax=4
      graph.yaxis.tick.configure(major=2,major_size=.6,minor_size=.4,major_linewidth=.8,minor_linewidth=.8)    
    graph.legend.configure(loc=(1.1,.5),loctype='view')
    graph.xaxis.label.configure(text='Degrees from equator',char_size=.8)
    graph.xaxis.tick.configure(major=30,major_size=.6,minor_size=.4,major_linewidth=.8,minor_linewidth=.8)
    graph.xaxis.ticklabel.configure(char_size=.6)
    graph.yaxis.label.configure(text=fw_prop,char_size=.8)
    graph.yaxis.ticklabel.configure(char_size=.6)
    graph.frame.linewidth=1
    graph.xaxis.bar.linewidth=.5
    graph.yaxis.bar.linewidth=.5

    graph.panel_label.configure(char_size=.6,placement='iul',dy=0.02,dx=0.02)

  grace.multi(rows=1,cols=4,hgap=0.05)
  grace.set_row_xaxislabel(0,"Degrees from equator",char_size=.8,perpendicular_offset=0.05)
  grace.hide_redundant_labels()
  grace.write_file('../manuscript/Figures/'+fw_prop+'_all.eps')



def main():

  for fw_prop in ['gen','path','TL']:
    plotter(fw_prop)

if __name__ == '__main__':
  main()