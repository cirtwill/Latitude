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

def websbytype(datafile):
  webtypedict={'terrestrial':[],'stream':[],'estuary':[],'lake':[],'marine':[]}

  f=open(datafile)
  for line in f:
    if line.split()[1] in webtypedict:
      web=line.split()[0]
      # Because I didn't want webs missing a TL
      if float(line.split()[15])>0 and float(line.split()[16])+float(line.split()[17])>0 and float(line.split()[18])>0:
        webtypedict[line.split()[1]].append(line.split()[0])

  return webtypedict


def baselines(directory,prop):  # Get non-jackknifed parameters to make red dashed lines.
  f=open('../non_TS/coefficients/'+prop+'_co.tsv','r')

  basis={}

  for line in f:
    if line.split()[0]!='"Estimate"':
      basis[line.split()[0]]=float(line.split()[1])

  return basis

# Jackknifing by webs
def web_coefficient_zoo(directory,webfiles,prop):
  weblist=[]

  if prop in ['LS','Vul']:
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},
              '"log(Species):Terr"':{},'"log(Species):Latitude"':{},'"log(Species):Lakeweb:Latitude"':{}}
  elif prop=='Gen':
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},'"log(Species):Stream"':{},
              '"log(Species):Latitude"':{},'"log(Species):Lakeweb:Latitude"':{},'"log(Species):Stream:Latitude"':{}}

  for fil in webfiles:
    web=fil.split(prop)[0]
    weblist.append(web)
    f=open(directory+fil,'r')
    for line in f:
      if line.split()[0] in betadict:
        betadict[line.split()[0]][web]=(float(line.split()[1]),float(line.split()[2]))
  return betadict, weblist

# Jackknifing by authors
def author_coefficient_zoo(directory,authorfiles,prop):
  weblist=[]

  if prop in ['LS','Vul']:
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},
              '"log(Species):Terr"':{},'"log(Species):Latitude"':{},'"log(Species):Lakeweb:Latitude"':{}}
  elif prop=='Gen':
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},'"log(Species):Stream"':{},
              '"log(Species):Latitude"':{},'"log(Species):Lakeweb:Latitude"':{},'"log(Species):Stream:Latitude"':{}}

  for fil in authorfiles:
    web=fil.split(prop)[0]
    weblist.append(web)
    f=open(directory+fil,'r')
    for line in f:
      if line.split()[0] in betadict:
        betadict[line.split()[0]][web]=(float(line.split()[1]),float(line.split()[2]))
  return betadict, weblist

def utilities(prop):
  if prop in ['LS','Vul']:
    labels=[\
    LatexString(r"Intercept\n  "), \
    LatexString(r"Species\n  "), \
    LatexString(r"Species:\nLatitude"), \
    LatexString(r"Species:\nLake"), \
    LatexString(r"Species:\nTerrestrial"), \
    LatexString(r"Species:\nLake:Latitude")]
    keylist=['"(Intercept)"','"log(Species)"','"log(Species):Latitude"','"log(Species):Lakeweb"','"log(Species):Terr"','"log(Species):Lakeweb:Latitude"']
  elif prop=='Gen':
    labels=[\
      LatexString(r"Intercept\n  "), \
      LatexString(r"Species\n  "), \
      LatexString(r"Species:\nLatitude"), \
      LatexString(r"Species:\nLake"), \
      LatexString(r"Species:\nStream"), \
      LatexString(r"Species:\nLake:Latitude"), \
      LatexString(r"Species:\nStream:Latitude")]
    keylist=['"(Intercept)"','"log(Species)"','"log(Species):Latitude"','"log(Species):Lakeweb"','"log(Species):Stream"','"log(Species):Lakeweb:Latitude"','"log(Species):Stream:Latitude"']

  return labels, keylist

def web_jackknife_plotter(directory,webfiles,prop,datafile):

  betadict, weblist=web_coefficient_zoo(directory,webfiles,prop)
  webtypedict=websbytype(datafile)
  
  labels, keylist=utilities(prop)

  basis=baselines(directory,prop)

  grace=MultiPanelGrace(colors=ColorBrewerScheme('Paired'))
  grace.add_label_scheme("dummy",labels)
  grace.set_label_scheme("dummy")
  for key in keylist:
    graph=grace.add_graph(Panel)
    zerod=graph.add_dataset([(0,-1),(0,500)],type='xy')
    zerod.symbol.shape=0
    zerod.line.configure(linestyle=3,linewidth=.75)

    i=196
    for webtype in ['terrestrial','estuary','marine','lake','stream']:
      for web in webtypedict[webtype]:
        dats=[(betadict[key][web][0],i,betadict[key][web][1]*1.96)]
        data=graph.add_dataset(dats,type='xydx')
        i=i-1

        data.line.linestyle=0
        data.symbol.configure(size=.2,linewidth=.25)
        if webtype=='stream':
          col=3
        elif webtype=='terrestrial':
          col=9
        elif webtype=='marine':
          col=11
        elif webtype=='lake':
          col=13
        else:
          col=5

        data.errorbar.configure(linestyle=0,riser_linewidth=1,color=col)

    redline=graph.add_dataset([(basis[key],-1),(basis[key],500)],type='xy')
    redline.symbol.shape=0
    redline.line.configure(linestyle=3,linewidth=1,color=7)

    graph.legend.configure(char_size=.6,box_linestyle=0,box_fill=0,length=6)

    graph.panel_label.configure(char_size=.5,placement='ouc',dy=0.01,dx=0,just=2)

    specials=graph.yaxis.tick.set_spec_ticks([36.5,95.5,133,156.5,181.5],[],tick_labels=['Stream','Lake','Marine','Estuary','Terrestrial'])
    
    graph.yaxis.tick.configure(place='both',major_size=0,minor_ticks=0,minor_size=.4,major=200,major_linewidth=0,minor_linewidth=0)
    graph.yaxis.ticklabel.configure(char_size=.5,angle=90)

    graph.world.ymax=198
    graph.world.ymin=0

    if prop in ['LS','Vul']:
      if key=='"(Intercept)"' and prop=='LS':
        graph.world.xmin=-.600001
        graph.world.xmax=0
        major=.3
        labtext=labels[0]
      elif key=='"(Intercept)"' and prop=='Vul':
        graph.world.xmin=-1.25
        graph.world.xmax=-.75
        major=.25
        labtext=labels[0]
      elif key=='"log(Species)"':
        graph.world.xmin=.49999999
        graph.world.xmax=.75
        major=.1
        labtext=labels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-.0025
        graph.world.xmax=.0005
        major=.001
        labtext=labels[2]
      elif key=='"log(Species):Lakeweb"':
        graph.world.xmin=-.2
        graph.world.xmax=0.05
        major=.1
        labtext=labels[3]
      elif key=='"log(Species):Terr"':
        graph.world.xmin=-.1
        graph.world.xmax=0
        major=.05
        labtext=labels[4]
      else:
        graph.world.xmin=0
        graph.world.xmax=.01
        major=.005
        labtext=labels[5]
    elif prop=='Gen':
      if key in ['"log(Species):Lakeweb:Latitude"','"log(Species):Stream:Latitude"']:
        graph.world.xmin=0
        graph.world.xmax=.01
        major=.005
        if key=='"log(Species):Lakeweb:Latitude"':
          labtext=labels[5]
        else:
          labtext=labels[6]
      elif key=='"(Intercept)"':
        graph.world.xmax=-.3999999
        graph.world.xmin=-.9
        major=.2
        labtext=labels[0]
      elif key=='"log(Species)"':
        graph.world.xmax=.65
        graph.world.xmin=.45
        major=.1
        labtext=labels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-.003
        graph.world.xmax=.002
        major=.002
        labtext=labels[2]
      elif key=='"log(Species):Stream"':
        graph.world.xmax=0
        graph.world.xmin=-.4
        major=.2
        labtext=labels[4]
      else: # Lake
        graph.world.xmax=.1
        graph.world.xmin=-.2
        major=.1
        labtext=labels[3]

    graph.xaxis.tick.configure(place='both',major_size=.4,minor_ticks=1,minor_size=.3,major=major,major_linewidth=.5,minor_linewidth=.5)
    graph.xaxis.ticklabel.configure(char_size=.5,format='decimal',prec=3,angle=90)
    graph.xaxis.label.configure(text=labtext,char_size=.5,just=2)


    graph.frame.linewidth=.5
    graph.xaxis.bar.linewidth=.5
    graph.yaxis.bar.linewidth=.5

  if prop in ['LS','Vul']:
    grace.multi(rows=1,cols=6,hgap=.04,width_to_height_ratio=0.08/1)
    cmax=5
  else:
    grace.multi(rows=1,cols=7,hgap=.04,width_to_height_ratio=0.08/1)
    cmax=6

  grace.add_drawing_object(DrawText,text='Estimate of coefficient after removing one web',char_size=.75,x=.442,y=0.03,loctype='view',just=2)

  grace.hide_redundant_labels()

  grace.write_file('../manuscript/Figures/Jackknife/'+prop+'_web.eps')

def author_jackknife_plotter(directory,authorfiles,prop,datafile):

  betadict, authorlist=author_coefficient_zoo(directory,authorfiles,prop)

  labels, keylist=utilities(prop)

  basis=baselines(directory,prop)
  
  colors=ColorBrewerScheme('Greys')
  # print ColorBrewerScheme('Paired')
  colors.add_color(227, 26, 28,'red') # Same red as Paired-7

  grace=MultiPanelGrace(colors=colors)
  grace.add_label_scheme("dummy",labels)
  grace.set_label_scheme("dummy")
  ticklabs=[]
  ticklist=[]

  for key in keylist:
    graph=grace.add_graph(Panel)
    zerod=graph.add_dataset([(0,-1),(0,500)],type='xy')
    zerod.symbol.shape=0
    zerod.line.configure(linestyle=3,linewidth=.75)

    i=53
    for author in sorted(betadict[key]):
      ticklabs.append('Au'+str(i))
      ticklist.append(54-i)
      dats=[(betadict[key][author][0],i,betadict[key][author][1]*1.96)]
      data=graph.add_dataset(dats,type='xydx')
      i=i-1

      data.line.linestyle=0
      data.symbol.configure(size=.2,linewidth=.25)
      data.errorbar.configure(linestyle=0,riser_linewidth=1.75,color=8)

    redline=graph.add_dataset([(basis[key],-1),(basis[key],500)],type='xy')
    redline.symbol.shape=0
    redline.line.configure(linestyle=3,linewidth=1,color='red')

    graph.panel_label.configure(char_size=.5,placement='ouc',dy=0.01,dx=0,just=2)

    if key=='"(Intercept)"':
      specials=graph.yaxis.tick.set_spec_ticks(ticklist,[],tick_labels=ticklabs)
     
      graph.yaxis.tick.configure(place='both',major_size=0,minor_ticks=0,minor_size=.4,major=200,major_linewidth=0,minor_linewidth=0)
      graph.yaxis.ticklabel.configure(char_size=.5,angle=0)
    else:
      graph.yaxis.tick.configure(place='both',major_size=0,minor_ticks=0,minor_size=.4,major=200,major_linewidth=0,minor_linewidth=0)
      graph.yaxis.ticklabel.configure(char_size=0,angle=0)

    graph.world.ymax=54
    graph.world.ymin=0

    if prop in ['LS','Vul']:
      if key=='"(Intercept)"' and prop=='LS':
        graph.world.xmin=-.800001
        graph.world.xmax=.2
        major=.4
        labtext=labels[0]
      elif key=='"(Intercept)"' and prop=='Vul':
        graph.world.xmin=-1.5
        graph.world.xmax=-.5
        major=.5
        labtext=labels[0]
      elif key=='"log(Species)"':
        graph.world.xmin=.4
        graph.world.xmax=.9
        major=.2
        labtext=labels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-.004
        graph.world.xmax=.002
        major=.002
        labtext=labels[2]
      elif key=='"log(Species):Lakeweb"':
        graph.world.xmin=-.3000001
        graph.world.xmax=0.15000001
        major=.15
        labtext=labels[3]
      elif key=='"log(Species):Terr"':
        graph.world.xmin=-.2
        graph.world.xmax=.05
        major=.1
        labtext=labels[4]
      else:
        graph.world.xmin=-.005
        graph.world.xmax=.01
        major=.005
        labtext=labels[5]
    elif prop=='Gen':
      if key in ['"log(Species):Lakeweb:Latitude"','"log(Species):Stream:Latitude"']:
        graph.world.xmin=0
        graph.world.xmax=.01
        major=.005
        if key=='"log(Species):Lakeweb:Latitude"':
          labtext=labels[5]
        else:
          labtext=labels[6]
      elif key=='"(Intercept)"':
        graph.world.xmax=-.3999999
        graph.world.xmin=-.9
        major=.2
        labtext=labels[0]
      elif key=='"log(Species)"':
        graph.world.xmax=.75
        graph.world.xmin=.45
        major=.1
        labtext=labels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-.003
        graph.world.xmax=.002
        major=.002
        labtext=labels[2]
      elif key=='"log(Species):Stream"':
        graph.world.xmax=0
        graph.world.xmin=-.4
        major=.2
        labtext=labels[4]
      else: # Lake
        graph.world.xmax=.1
        graph.world.xmin=-.2
        major=.1
        labtext=labels[3]

    graph.xaxis.tick.configure(place='both',major_size=.4,minor_ticks=0,minor_size=.4,major=major,major_linewidth=.5,minor_linewidth=.5)
    graph.xaxis.ticklabel.configure(char_size=.5,format='decimal',prec=3,angle=90)

    graph.xaxis.label.configure(text=labtext,char_size=.5,just=2)

    graph.frame.linewidth=.5
    graph.xaxis.bar.linewidth=.5
    graph.yaxis.bar.linewidth=.5

  if prop in ['LS','Vul']:
    grace.multi(rows=1,cols=6,hgap=.04,width_to_height_ratio=0.08/1)
    cmax=5
  else:
    grace.multi(rows=1,cols=7,hgap=.04,width_to_height_ratio=0.08/1)
    cmax=6

  grace.add_drawing_object(DrawText,text='Estimate of coefficient after removing one author',char_size=.75,x=.442,y=0.03,loctype='view',just=2)
  
  grace.hide_redundant_labels()

  grace.write_file('../manuscript/Figures/Jackknife/'+prop+'_author.eps')


def main():

  directory='../Jackknifed/main/coefficients/'
  files=os.listdir(directory)
  datafile='../non_TS/summary-properties.tsv'

  webfiles=[]
  authorfiles=[]

  LS_webfiles=[]
  Gen_webfiles=[]
  Vul_webfiles=[]

  LS_authorfiles=[]
  Gen_authorfiles=[]
  Vul_authorfiles=[]

  for fil in files:
    if 'web' in fil:
      webfiles.append(fil)
      if 'LS' in fil:
        LS_webfiles.append(fil)
      elif 'Gen' in fil:
        Gen_webfiles.append(fil)
      else:
        Vul_webfiles.append(fil)
    else:
      authorfiles.append(fil)
      if 'LS' in fil:
        LS_authorfiles.append(fil)
      elif 'Gen' in fil:
        Gen_authorfiles.append(fil)
      else:
        Vul_authorfiles.append(fil)

  for prop in ['Vul','Gen','LS']:
    if prop=='LS':
      authorfiles=LS_authorfiles
    elif prop=='Gen':
      authorfiles=Gen_authorfiles
    else:
      authorfiles=Vul_authorfiles

    author_jackknife_plotter(directory,authorfiles,prop,datafile)

  for prop in ['LS','Gen','Vul']:
    if prop=='LS':
      webfiles=LS_webfiles
    elif prop=='Gen':
      webfiles=Gen_webfiles
    else:
      webfiles=Vul_webfiles

    web_jackknife_plotter(directory,webfiles,prop,datafile)



if __name__ == '__main__':
  main()
