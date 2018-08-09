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

redundants={
  'Egloff':['X38_coauthors','Richardson','Coauthors'],
  'Abele':['Simberloff'],
  'Ali_Dawah':['Banasek.Richter','Berlow','Bersier','Blanchard','Brey',
              'Brose','Cushing','Dell','Harper.Smith','Jacob','Knapp',
              'Ledger','Memmot','Mintenbeck','Pinnegar','Rall','Ruess','Ulrich','Yodzis'],
  'Aloisio':['Carvalho'],
  'Althaus':['Bax','Bulman','He','Williams_A'],
  'Amundsen':['Klemetsen','Knudsen','Kristoffersen','Primicerio'],
  'Antezana':['Cornejo.Donoso'],
  'Arreguin_Sanchez':['Zetina_Rejon'],
  'Baeta':['Niquil'],
  'Bailey.Watts':['Bindloss','Holden','Smith_I'],
  'Beaney':['Huxham','Raffaelli'],
  'Bradstreet':['Cross'],
  'Bretschko':['Gollmann','Pechlaner','Pfeifer','Tilzer','Weissenbach'],
  'Briand':['Newman'],
  'Brittain':['Larsson','Lien','Lillehammer','Tangen'],
  'Britton':['Kowalczewski','Lack','Matthews','McDonald'],
  'Brown':['Hechinger'],
  'Burgis':['Ganf','McGowan','Viner'],
  'Canuel':['Douglass','Emmett_Duffy'],
  'Castro':['Filgueira'],
  'Catella':['Resende','de_Morais'],
  'Cattaneo.Vietti':['Smith_WO'],
  'Chen':['Chen_Y','Li_J','Liu'],
  'Christian':['Luczkovich'],
  'Clarke_T':['Flechsig','Grigg'],
  'Clarke_W':['Dayton','Rosenthal'],
  'Closs':['Lake'],
  'Coffman':['Cummins','Roff'],
  'Collins':['Mitchell','Wiegert'],
  'Conover':['Edwards_D','Sutter'],
  'Cummins':['Coffman','Roff'],
  'Dai':['Fang','Hsieh','Hung','Lin','Lo','Shao','Su'],
  'Darlington':['Moriarty_C','Moriarty_D','Tevlin'],
  'Detloff':['Josten','Zander'],
  'Ebenman':['Emmerson','Montoya','Olesen','Valido'],
  'Edwards_E':['Kilroy','McIntosh','Scarsbrook'],
  'Feroz_Khan':['Panikkar'],
  'Fetahi':['Mengistou','Schagerl'],
  'Fradkin':['Hampton','Leavitt','Rosenberger'],
  'Garcia.Cantizano':['Massana','Pedros.Alio'],
  'Godfrey':['Pearson','Pusey'],
  'Goldwasser':['Roughgarden'],
  'Gontikaki':['Soetaert','Witte','van_Oevelen'],
  'Harris':['Paur'],
  'Hasham':['Hildrew'],
  'Heald':['Odum_W'],
  'Hildrew':['Hasham'],
  'Homer':['Kemp','Lehman','McKellar','Odum_H','Smith_WH','Young'],
  'Hopkins':['Lancraft','Torres'],
  'Huang':['Li_X','Yan'],
  'Hurlbert':['Mulla','Wilson_H'],
  'Johnson':['Orlofske','Preston'],
  'Lauridsen':['Papantoniou'],
  'Marques':['Patricio'],
  'Motta':['Uieda'],
  'Pattie':['Verbeek'],
  'Price':['Mayse'],
  'Ratsirarson':['Silander'],
  'Seifert_F':['Seifert_R'],
  'Shushkina':['Vinogradov'],
  'Sprules':['Stewart'],
  'Stagliano':['Whiles'],
  'Tavares.Cromar':['Williams_D']}



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
  f=open('../non_TS/coefficients/'+prop+'_co_corrected.tsv','r')

  basis={}

  for line in f:
    if line.split()[0]!='"Estimate"':
      basis[line.split()[0]]=float(line.split()[1])
  return basis

# Jackknifing by webs
def web_coefficient_zoo(directory,webfiles,prop):
  weblist=[]

  if prop=='LS':
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},
              '"log(Species):Terr"':{},'"log(Species):Latitude"':{},#'"log(Species):Lakeweb:Latitude"':{},
              '"log(Species):Latitude:Lakeweb"':{}}
  elif prop=='Vul':
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

  if prop=='LS':
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},
              '"log(Species):Terr"':{},'"log(Species):Latitude"':{},#'"log(Species):Lakeweb:Latitude"':{},
              '"log(Species):Latitude:Lakeweb"':{}}
  elif prop=='Vul':
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


def removalcounter(removalfile):
  removals={}
  f=open(removalfile,'r')
  for line in f:
    if len(line.split())==3:
      author=line.split()[1][1:-1]
      removed=int(line.split()[2][1:-1])
      removals[author]=removed
  f.close()
  return removals


def utilities(prop):
  if prop in ['LS','Vul']:
    labels=[\
    LatexString(r"\v{-.6}\f{Symbol}a\f{}"), \
    LatexString(r"\n\f{Symbol}b\f{}\s0\s"), \
    LatexString(r"\n\v{-.12}\f{Symbol}b\f{}\s1\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Lake\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Terrestrial\s"), \
    LatexString(r"\n\v{-.12}\f{Symbol}b\f{}\s3, E = Lake\s")]

    altlabels=[\
    LatexString(r"\n\v{.4}\f{Symbol}a\f{}"), \
    LatexString(r"\n\f{Symbol}b\f{}\s0\s"), \
    LatexString(r"\n\n\v{.5}\f{Symbol}b\f{}\s1\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Lake\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Terrestrial\s"), \
    LatexString(r"\n\n\v{.5}\f{Symbol}b\f{}\s3, E = Lake\s")]
    if prop=='LS':
      keylist=['"(Intercept)"','"log(Species)"','"log(Species):Latitude"','"log(Species):Lakeweb"','"log(Species):Terr"','"log(Species):Latitude:Lakeweb"']
    else:
      keylist=['"(Intercept)"','"log(Species)"','"log(Species):Latitude"','"log(Species):Lakeweb"','"log(Species):Terr"','"log(Species):Lakeweb:Latitude"']
  elif prop=='Gen':
    labels=[\
    LatexString(r"\v{-.6}\f{Symbol}a\f{}"), \
    LatexString(r"\n\f{Symbol}b\f{}\s0\s"), \
    LatexString(r"\n\v{-.15}\f{Symbol}b\f{}\s1\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Lake\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Stream\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s3, E = Lake\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s3, E = Stream\s")]
    keylist=['"(Intercept)"','"log(Species)"','"log(Species):Latitude"','"log(Species):Lakeweb"','"log(Species):Stream"','"log(Species):Lakeweb:Latitude"','"log(Species):Stream:Latitude"']

    altlabels=[\
    LatexString(r"\n\v{.3}\f{Symbol}a\f{}"), \
    LatexString(r"\n\v{-.11}\f{Symbol}b\f{}\s0\s"), \
    LatexString(r"\n\v{-.65}\f{Symbol}b\f{}\s1\s"), \
    LatexString(r"\v{-1.15}\f{Symbol}b\f{}\s2, E = Lake\s"), \
    LatexString(r"\f{Symbol}b\f{}\s2, E = Stream\s"), \
    LatexString(r"\n\v{-.68}\f{Symbol}b\f{}\s3, E = Lake\s"), \
    LatexString(r"\n\v{-.58}\f{Symbol}b\f{}\s3, E = Stream\s")]


  return labels, keylist, altlabels

def web_jackknife_plotter(directory,webfiles,prop,datafile):

  betadict, weblist=web_coefficient_zoo(directory,webfiles,prop)
  webtypedict=websbytype(datafile)
  
  labels, keylist, altlabels=utilities(prop)

  basis=baselines(directory,prop)

  Jacks={}
  JackSDs={}

  N=len(weblist)

  for key in betadict:
    Jacks[key]=[]
    JackSDs[key]=[]
    for web in betadict[key]:
      Jacks[key].append(N*basis[key]-(N-1)*betadict[key][web][0]) # Pseudovalues

  Jmeans={}
  for key in Jacks:
    Jmeans[key]=float(sum(Jacks[key])/len(Jacks[key]))  # Mean of Pseudovalues


  for key in Jacks:
    for item in Jacks[key]:
      JackSDs[key].append((item-Jmeans[key])**2)

  JSDs={}
  for key in Jacks:
    JSDs[key]=float(sum(JackSDs[key])/(len(Jacks[key])-1)) # Variances of Pseudovalues

  JSEs={}
  for key in Jacks:
    JSEs[key]=math.sqrt(JSDs[key]/len(Jacks[key]))  # SEs of Pseudovalues
  # print JSEs

  grace=MultiPanelGrace(colors=ColorBrewerScheme('Paired'))
  grace.add_label_scheme("dummy",labels)
  grace.set_label_scheme("dummy")
  for key in keylist:
    graph=grace.add_graph(Panel)
    zerod=graph.add_dataset([(0,-1),(0,500)],type='xy')
    zerod.symbol.shape=0
    zerod.line.configure(linestyle=3,linewidth=.75)

    # line1=graph.add_dataset([(Jmeans[key]-1.96*JSEs[key],-1),(Jmeans[key]-1.96*JSEs[key],500)])
    # line1.line.configure(linestyle=1,linewidth=2,color=2)

    # line2=graph.add_dataset([(Jmeans[key]+1.96*JSEs[key],-1),(Jmeans[key]+1.96*JSEs[key],500)])
    # line2.line.configure(linestyle=1,linewidth=2,color=2)

    streamline=graph.add_dataset([(-50,73.5),(50,73.5)])
    streamline.line.configure(linestyle=1,linewidth=.5)

    lakeline=graph.add_dataset([(-50,119.5),(50,119.5)])
    lakeline.line.configure(linestyle=1,linewidth=.5)

    marineline=graph.add_dataset([(-50,147.5),(50,147.5)])
    marineline.line.configure(linestyle=1,linewidth=.5)

    terrestrialline=graph.add_dataset([(-50,166.5),(50,166.5)])
    terrestrialline.line.configure(linestyle=1,linewidth=.5)

    redline=graph.add_dataset([(basis[key],-1),(basis[key],500)],type='xy')
    redline.symbol.shape=0
    redline.line.configure(linestyle=3,linewidth=1,color=7)

    i=196
    for webtype in ['terrestrial','estuary','marine','lake','stream']:
      for web in webtypedict[webtype]:
        theta=basis[key]
        theta_n=betadict[key][web][0]
        N=len(weblist)
        pseudov=N*theta-(N-1)*theta_n
        dats=[(pseudov,i)]

        # dats=[(betadict[key][web][0],i)]
        data=graph.add_dataset(dats,type='xy')
        i=i-1

        data.line.linestyle=0
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

        data.symbol.configure(size=.15,linewidth=.25)
        # data.errorbar.configure(linestyle=0,riser_linewidth=1,color=col)


    graph.legend.configure(char_size=.6,box_linestyle=0,box_fill=0,length=6)

    graph.panel_label.configure(char_size=.7,placement='ouc',dy=0.01,dx=0,just=2)

    specials=graph.yaxis.tick.set_spec_ticks([36.5,95.5,133,156.5,181.5],[],tick_labels=['stream','lake','marine','estuary','terrestrial'])
    
    graph.yaxis.tick.configure(place='both',major_size=0,minor_ticks=0,minor_size=.4,major=200,major_linewidth=0,minor_linewidth=0)
    graph.yaxis.ticklabel.configure(char_size=.75,angle=90)

    graph.world.ymax=198
    graph.world.ymin=0

    if prop in ['LS','Vul']:
      if key=='"(Intercept)"' and prop=='LS':
        graph.world.xmin=-10
        graph.world.xmax=10
        major=10
        labtext=labels[0]
      elif key=='"(Intercept)"' and prop=='Vul':
        graph.world.xmin=-15
        graph.world.xmax=10
        major=10
        labtext=labels[0]
      elif key=='"log(Species)"':
        graph.world.xmin=-4.5
        graph.world.xmax=6
        major=4
        labtext=labels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-.08
        graph.world.xmax=.08
        major=.08
        labtext=labels[2]
      elif key=='"log(Species):Lakeweb"':
        graph.world.xmin=-9
        graph.world.xmax=9
        major=5
        labtext=labels[3]
      elif key=='"log(Species):Terr"':
        graph.world.xmin=-4
        graph.world.xmax=2.5
        major=2
        labtext=labels[4]
      else:
        graph.world.xmin=-.15
        graph.world.xmax=.15
        major=.1
        labtext=labels[5]
    elif prop=='Gen':
      if key in ['"log(Species):Lakeweb:Latitude"','"log(Species):Stream:Latitude"']:
        graph.world.xmin=-.2
        graph.world.xmax=.3
        major=.2
        if key=='"log(Species):Lakeweb:Latitude"':
          labtext=labels[5]
        else:
          labtext=labels[6]
      elif key=='"(Intercept)"':
        graph.world.xmax=10
        graph.world.xmin=-10
        major=10
        labtext=labels[0]
      elif key=='"log(Species)"':
        graph.world.xmax=5
        graph.world.xmin=-6
        major=5
        labtext=labels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-.15
        graph.world.xmax=.1
        major=.1
        labtext=labels[2]
      elif key=='"log(Species):Stream"':
        graph.world.xmax=7
        graph.world.xmin=-8
        major=5
        labtext=labels[4]
      else: # Lake
        graph.world.xmax=6
        graph.world.xmin=-7
        major=5
        labtext=labels[3]

    graph.xaxis.tick.configure(place='both',major_size=.4,minor_ticks=1,minor_size=.3,major=major,major_linewidth=.5,minor_linewidth=.5)
    graph.xaxis.ticklabel.configure(char_size=.5,format='decimal',prec=1,angle=90)
    graph.xaxis.label.configure(text=labtext,char_size=.7,just=2,place='normal')


    graph.frame.linewidth=.5
    graph.xaxis.bar.linewidth=.5
    graph.yaxis.bar.linewidth=.5

  if prop in ['LS','Vul']:
    grace.multi(rows=1,cols=6,hgap=.06,width_to_height_ratio=0.08/1)
    cmax=5
    grace.add_drawing_object(DrawText,text='estimate of coefficient after removing one web',char_size=1,x=.4928,y=0.04,loctype='view',just=2)
  else:
    grace.multi(rows=1,cols=7,hgap=.06,width_to_height_ratio=0.08/1)
    cmax=6
    # grace.set_row_xaxislabel(row=0,colspan=(0,cmax),label='estimate of coefficient after removing one web',char_size=1,just=2,color=7)
    grace.add_drawing_object(DrawText,text='estimate of coefficient after removing one web',char_size=1,x=.5546,y=0.04,loctype='view',just=2)


  # grace.set_row_xaxislabel(row=0,colspan=(0,cmax),just=2, label='Estimate of coefficient after removing one web',char_size=.75,color=6)

  grace.hide_redundant_labels()

  grace.write_file('../manuscript/Figures/Jackknife/'+prop+'_web.eps')

def author_jackknife_plotter(directory,authorfiles,prop,datafile,removalfile,webfiles):
  removals=removalcounter(removalfile)

  dumbetas, weblist=web_coefficient_zoo(directory,webfiles,prop)

  betadict, authorlist=author_coefficient_zoo(directory,authorfiles,prop)
  labels, keylist, altlabels=utilities(prop)

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

    i=58
    for author in sorted(betadict[key]):
      Nn=removals[author]
      theta=basis[key]
      theta_n=betadict[key][author][0]
      N=len(weblist)
      pseudov=N*theta-(N-1)*theta_n
      dats=[(pseudov,i)]

      if author in redundants.keys():
        ticklab='authors '+str(i)+' ('+str(Nn)+')'
      else:
        ticklab='author '+str(i)+' ('+str(Nn)+')'
      ticklabs.append(ticklab)
      ticklist.append(i)
      # dats=[(betadict[key][author][0],i,betadict[key][author][1]*1.96)]
      data=graph.add_dataset(dats,type='xy')
      i=i-1

      data.line.linestyle=0
      data.symbol.configure(size=.5,linewidth=.25)
      data.errorbar.configure(linestyle=0,riser_linewidth=1.75,color=8)

    redline=graph.add_dataset([(basis[key],-1),(basis[key],500)],type='xy')
    redline.symbol.shape=0
    redline.line.configure(linestyle=3,linewidth=1,color='red')

    graph.panel_label.configure(char_size=.7,placement='ouc',dy=0.01,dx=0,just=2)

    if key=='"(Intercept)"':
      specials=graph.yaxis.tick.set_spec_ticks(ticklist,[],tick_labels=ticklabs)
     
      graph.yaxis.tick.configure(place='both',major_size=0,minor_ticks=0,minor_size=.4,major=200,major_linewidth=0,minor_linewidth=0)
      graph.yaxis.ticklabel.configure(char_size=.5,angle=0,align='left')
    else:
      graph.yaxis.tick.configure(place='both',major_size=0,minor_ticks=0,minor_size=.4,major=200,major_linewidth=0,minor_linewidth=0)
      graph.yaxis.ticklabel.configure(char_size=0,angle=0)

    graph.world.ymax=59
    graph.world.ymin=0

    if prop in ['LS','Vul']:
      if key=='"(Intercept)"' and prop=='LS':
        graph.world.xmin=-200
        graph.world.xmax=200
        major=100
        labtext=altlabels[0]
      elif key=='"(Intercept)"' and prop=='Vul':
        graph.world.xmin=-200
        graph.world.xmax=200
        major=100
        labtext=altlabels[0]
      elif key=='"log(Species)"':
        graph.world.xmin=-60
        graph.world.xmax=80
        major=50
        labtext=altlabels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-1
        graph.world.xmax=1
        major=1
        labtext=altlabels[2]
      elif key=='"log(Species):Lakeweb"':
        graph.world.xmin=-75
        graph.world.xmax=50
        major=50
        labtext=altlabels[3]
      elif key=='"log(Species):Terr"':
        graph.world.xmin=-50
        graph.world.xmax=50
        major=50
        labtext=altlabels[4]
      else:
        graph.world.xmin=-1
        graph.world.xmax=2
        major=1
        labtext=altlabels[5]
    elif prop=='Gen':
      if key=='"log(Species):Lakeweb:Latitude"':
        graph.world.xmin=-1
        graph.world.xmax=2
        major=1
        labtext=altlabels[5]
      elif key=='"log(Species):Stream:Latitude"':
        graph.world.xmin=-2.5
        graph.world.xmax=3
        major=1
        labtext=altlabels[6]
      elif key=='"(Intercept)"':
        graph.world.xmax=100
        graph.world.xmin=-150
        major=100
        labtext=altlabels[0]
      elif key=='"log(Species)"':
        graph.world.xmax=60
        graph.world.xmin=-50
        major=50
        labtext=altlabels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-1
        graph.world.xmax=1
        major=1
        labtext=altlabels[2]
      elif key=='"log(Species):Stream"':
        graph.world.xmax=100
        graph.world.xmin=-125
        major=100
        labtext=altlabels[4]
      else: # Lake
        graph.world.xmax=50
        graph.world.xmin=-100
        major=50
        labtext=altlabels[3]

    graph.xaxis.tick.configure(place='both',major_size=.4,minor_ticks=0,minor_size=.4,major=major,major_linewidth=.5,minor_linewidth=.5)
    graph.xaxis.ticklabel.configure(char_size=.5,format='decimal',prec=0,angle=90)

    graph.xaxis.label.configure(text=labtext,char_size=.7,just=2)

    graph.frame.linewidth=.5
    graph.xaxis.bar.linewidth=.5
    graph.yaxis.bar.linewidth=.5

  if prop in ['LS','Vul']:
    grace.multi(rows=1,cols=6,hgap=.06,width_to_height_ratio=0.08/1)
    cmax=5
    grace.add_drawing_object(DrawText,text='estimate of coefficient after removing webs with a common author',char_size=1,x=.4928,y=0.04,loctype='view',just=2)
  else:
    grace.multi(rows=1,cols=7,hgap=.06,width_to_height_ratio=0.08/1)
    cmax=6
    grace.add_drawing_object(DrawText,text='estimate of coefficient after removing webs with a common author',char_size=1,x=.5546,y=0.04,loctype='view',just=2)
  
  grace.hide_redundant_labels()

  grace.write_file('../manuscript/Figures/Jackknife/'+prop+'_author.eps')


def main():

  directory='../Jackknifed/main/coefficients/'
  files=os.listdir(directory)
  datafile='../non_TS/summary-properties.tsv'
  removalfile='../Jackknifed/webs_per_author.tsv'

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

    # author_jackknife_plotter(directory,authorfiles,prop,datafile,removalfile,webfiles)

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
