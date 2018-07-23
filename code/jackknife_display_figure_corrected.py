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
colors=ColorBrewerScheme('Paired')
colors.add_color(120,120,120,'grey')

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
    if line.split('\t')[1] in webtypedict:
      web=line.split('\t')[0]
      # Because I didn't want webs missing a TL
      if float(line.split('\t')[14])>0 and float(line.split('\t')[15])+float(line.split('\t')[16])>0 and float(line.split('\t')[17])>0:
        webtypedict[line.split('\t')[1]].append(line.split('\t')[0])

  return webtypedict

def baselines(directory,prop):  # Get non-jackknifed parameters to make red dashed lines.
  f=open('../non_TS/coefficients/'+prop+'_co_corrected.tsv','r')

  basis={}

  for line in f:
    if line.split()[0]!='"Estimate"':
      basis[line.split()[0]]=(float(line.split()[1]),float(line.split()[2]))

  return basis

# Jackknifing by webs
def web_coefficient_zoo(directory,webfiles,prop):
  weblist=[]

  if prop == 'LS':
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},'"log(Species):Marine"':{},
    '"log(Species):Latitude"':{},'"log(Species):Latitude:Lakeweb"':{}}
  elif prop=='Gen':
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},'"log(Species):Stream"':{},'"log(Species):Marine"':{},
              '"log(Species):Latitude"':{},'"log(Species):Lakeweb:Latitude"':{},'"log(Species):Stream:Latitude"':{}}
  else:
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},'"log(Species):Marine"':{},
    '"log(Species):Latitude"':{},'"log(Species):Lakeweb:Latitude"':{}}

  for fil in webfiles:
    if prop in fil:
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
  if prop == 'LS':
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},'"log(Species):Marine"':{},
    '"log(Species):Latitude"':{},'"log(Species):Latitude:Lakeweb"':{}}
  elif prop=='Gen':
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},'"log(Species):Stream"':{},'"log(Species):Marine"':{},
              '"log(Species):Latitude"':{},'"log(Species):Lakeweb:Latitude"':{},'"log(Species):Stream:Latitude"':{}}
  else:
    betadict={'"(Intercept)"':{},'"log(Species)"':{},'"log(Species):Lakeweb"':{},'"log(Species):Marine"':{},
    '"log(Species):Latitude"':{},'"log(Species):Lakeweb:Latitude"':{}}

  for fil in authorfiles:
    if prop in fil:
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
    if len(line.split())==3 and line.split()[1]!='NA':
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
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Marine\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s3, E = Lake\s"), \
    ]
    altlabels=[\
    LatexString(r"\n\v{.4}\f{Symbol}a\f{}"), \
    LatexString(r"\n\f{Symbol}b\f{}\s0\s"), \
    LatexString(r"\n\n\v{.5}\f{Symbol}b\f{}\s1\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Lake\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Marine\s"), \
    LatexString(r"\n\v{-.68}\f{Symbol}b\f{}\s3, E = Lake\s"), \
    ]
    if prop=='LS':
      keylist=['"(Intercept)"','"log(Species)"','"log(Species):Lakeweb"','"log(Species):Marine"','"log(Species):Latitude"','"log(Species):Latitude:Lakeweb"']
    else:
      keylist=['"(Intercept)"','"log(Species)"','"log(Species):Lakeweb"','"log(Species):Marine"','"log(Species):Latitude"','"log(Species):Lakeweb:Latitude"']
  elif prop=='Gen':
    labels=[\
    LatexString(r"\v{-.6}\f{Symbol}a\f{}"), \
    LatexString(r"\n\f{Symbol}b\f{}\s0\s"), \
    LatexString(r"\n\v{-.15}\f{Symbol}b\f{}\s1\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Lake\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Stream\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s2, E = Marine\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s3, E = Lake\s"), \
    LatexString(r"\n\f{Symbol}b\f{}\s3, E = Stream\s")]
    keylist=['"(Intercept)"','"log(Species)"','"log(Species):Latitude"','"log(Species):Lakeweb"','"log(Species):Stream"','"log(Species):Marine"','"log(Species):Lakeweb:Latitude"','"log(Species):Stream:Latitude"']

    altlabels=[\
    LatexString(r"\n\v{.3}\f{Symbol}a\f{}"), \
    LatexString(r"\n\v{-.11}\f{Symbol}b\f{}\s0\s"), \
    LatexString(r"\n\v{-.65}\f{Symbol}b\f{}\s1\s"), \
    LatexString(r"\v{-1.15}\f{Symbol}b\f{}\s2, E = Lake\s"), \
    LatexString(r"\v{-1.15}\f{Symbol}b\f{}\s2, E = Marine\s"), \
    LatexString(r"\f{Symbol}b\f{}\s2, E = Stream\s"), \
    LatexString(r"\n\v{-.68}\f{Symbol}b\f{}\s3, E = Lake\s"), \
    LatexString(r"\n\v{-.58}\f{Symbol}b\f{}\s3, E = Stream\s")]


  return labels, keylist, altlabels

def web_jackknife_plotter(directory,webfiles,prop,datafile):

  betadict, weblist=web_coefficient_zoo(directory,webfiles,prop)
  webtypedict=websbytype(datafile)

  labels, keylist, altlabels=utilities(prop)

  basis=baselines(directory,prop)
  # print basis

  N=len(weblist)

  grace=MultiPanelGrace(colors=colors)
  grace.add_label_scheme("dummy",labels)
  grace.set_label_scheme("dummy")
  for key in ['"(Intercept)"']:
    print basis[key]
    print Jmeans[key], betadict[key].values()[:5]
  # for key in keylist:
    graph=grace.add_graph(Panel)
    # Lines to divide environment types
    zerod=graph.add_dataset([(0,-1),(0,500)],type='xy')
    zerod.symbol.shape=0
    zerod.line.configure(linestyle=3,linewidth=.75)

    line1=graph.add_dataset([(Jmeans[key]-1.96*JSEs[key],-1),(Jmeans[key]-1.96*JSEs[key],500)])
    line1.line.configure(linestyle=1,linewidth=2,color=2)

    line2=graph.add_dataset([(Jmeans[key]+1.96*JSEs[key],-1),(Jmeans[key]+1.96*JSEs[key],500)])
    line2.line.configure(linestyle=1,linewidth=2,color=2)

    streamline=graph.add_dataset([(-500,59.5),(500,59.5)])
    streamline.line.configure(linestyle=1,linewidth=.5)

    lakeline=graph.add_dataset([(-500,98.5),(500,98.5)])
    lakeline.line.configure(linestyle=1,linewidth=.5)

    marineline=graph.add_dataset([(-500,124.5),(500,124.5)])
    marineline.line.configure(linestyle=1,linewidth=.5)

    terrestrialline=graph.add_dataset([(-500,143.5),(500,143.5)])
    terrestrialline.line.configure(linestyle=1,linewidth=.5)

    redline=graph.add_dataset([(basis[key][0],-1),(basis[key][0],500)],type='xy')
    # greyline1=graph.add_dataset([(basis[key][0]-1.96*basis[key][1],-1),(basis[key][0]-1.96*basis[key][1],500)],type='xy')
    # greyline1.symbol.shape=0
    # greyline1.line.configure(linestyle=1,linewidth=1,color='grey')

    redline.symbol.shape=0
    redline.line.configure(linestyle=3,linewidth=1,color=7)

    i=163
    for webtype in ['terrestrial','estuary','marine','lake','stream']:
      for web in webtypedict[webtype]:
        if web in betadict[key].keys():
          theta=basis[key][0]
          theta_n=betadict[key][web][0]
          N=len(betadict[key].keys())
          pseudov=N*theta-(N-1)*theta_n # Looks like this is how much the beta changes relative to the baseline?
          dats=[(pseudov,i)]

          data=graph.add_dataset(dats,type='xy')
          i=i-1

          data.line.linestyle=0

          data.symbol.configure(size=.15,linewidth=.25)

    graph.legend.configure(char_size=.6,box_linestyle=0,box_fill=0,length=6)

    graph.panel_label.configure(char_size=.7,placement='ouc',dy=0.01,dx=0,just=2)

    specials=graph.yaxis.tick.set_spec_ticks([32,81.5,114,136.5,156],[],tick_labels=['stream','lake','marine','estuary','terrestrial'])
    
    graph.yaxis.tick.configure(place='both',major_size=0,minor_ticks=0,minor_size=.4,major=200,major_linewidth=0,minor_linewidth=0)
    graph.yaxis.ticklabel.configure(char_size=.75,angle=90)

    graph.world.ymax=165
    graph.world.ymin=0

    if prop in ['LS','Vul']:
      if key=='"(Intercept)"':
        graph.world.xmin=-50
        graph.world.xmax=50
        major=25
        labtext=labels[0]
      elif key=='"log(Species)"':
        graph.world.xmin=-10
        graph.world.xmax=10
        major=5
        labtext=labels[1]
      elif key=='"log(Species):Lakeweb"':
        graph.world.xmin=-5
        graph.world.xmax=5
        major=5
        labtext=labels[3]
      elif key=='"log(Species):Marine"':
        graph.world.xmin=-5
        graph.world.xmax=5
        major=5
        labtext=labels[4]
      else:
        graph.world.xmin=-5
        graph.world.xmax=5
        major=5
        labtext=labels[5]

    elif prop=='Gen':
      if key in ['"log(Species):Lakeweb:Latitude"','"log(Species):Stream:Latitude"']:
        graph.world.xmin=-.2
        graph.world.xmax=.3
        major=.2
        if key=='"log(Species):Lakeweb:Latitude"':
          labtext=labels[6]
        else:
          labtext=labels[7]
      elif key=='"(Intercept)"':
        graph.world.xmax=100
        graph.world.xmin=-100
        major=50
        labtext=labels[0]
      elif key=='"log(Species)"':
        graph.world.xmax=10
        graph.world.xmin=-10
        major=5
        labtext=labels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-.1
        graph.world.xmax=.1
        major=.1
        labtext=labels[2]
      elif key=='"log(Species):Stream"':
        graph.world.xmax=7
        graph.world.xmin=-6
        major=5
        labtext=labels[4]
      elif key=='"log(Species):Marine"':
        graph.world.xmax=7
        graph.world.xmin=-6
        major=5
        labtext=labels[5]
      else: # Lake
        graph.world.xmax=10
        graph.world.xmin=-10
        major=5
        labtext=labels[3]

    graph.xaxis.tick.configure(place='both',major_size=.4,minor_ticks=1,minor_size=.3,major=major,major_linewidth=.5,minor_linewidth=.5)
    graph.xaxis.ticklabel.configure(char_size=.5,format='decimal',prec=0,angle=90)
    graph.xaxis.label.configure(text=labtext,char_size=.7,just=2,place='normal')


    graph.frame.linewidth=.5
    graph.xaxis.bar.linewidth=.5
    graph.yaxis.bar.linewidth=.5

  if prop in ['LS','Vul']:
    grace.multi(rows=1,cols=6,hgap=.06,width_to_height_ratio=0.08/1)
    grace.add_drawing_object(DrawText,text=\
      LatexString(r'estimate of coefficient after removing one web'),char_size=1,x=.3048,y=0.04,loctype='view',just=2)
  else:
    grace.multi(rows=1,cols=8,hgap=.06,width_to_height_ratio=0.08/1)
    # grace.set_row_xaxislabel(row=0,colspan=(0,cmax),label='estimate of coefficient after removing one web',char_size=1,just=2,color=7)
    grace.add_drawing_object(DrawText,text='estimate of coefficient after removing one web',char_size=1,x=.5546,y=0.04,loctype='view',just=2)


  # grace.set_row_xaxislabel(row=0,colspan=(0,cmax),just=2, label='Estimate of coefficient after removing one web',char_size=.75,color=6)

  # grace.hide_redundant_labels()

  grace.write_file('../manuscript/Figures/Jackknife/'+prop+'_web_corrected.eps')

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

    i=54
    k=0
    for author in sorted(betadict[key]):
      if author in removals:
        k=k+1
        Nn=removals[author]
        theta=basis[key][0]
        theta_n=betadict[key][author][0]
        N=len(betadict[key].keys())
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

    redline=graph.add_dataset([(basis[key][0],-1),(basis[key][0],500)],type='xy')
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

    graph.world.ymax=55
    graph.world.ymin=0

    if prop in ['LS','Vul']:
      if key=='"(Intercept)"':
        graph.world.xmin=-20
        graph.world.xmax=20
        major=10
        labtext=altlabels[0]
      else:
        graph.world.xmin=-10
        graph.world.xmax=10
        major=5
        labtext=altlabels[3]
    elif prop=='Gen':
      if key=='"log(Species):Lakeweb:Latitude"':
        graph.world.xmin=-1
        graph.world.xmax=1.5
        major=1
        labtext=altlabels[5]
      elif key=='"log(Species):Stream:Latitude"':
        graph.world.xmin=-2.5
        graph.world.xmax=2.5
        major=1
        labtext=altlabels[6]
      elif key=='"(Intercept)"':
        graph.world.xmax=20
        graph.world.xmin=-20
        major=10
        labtext=altlabels[0]
      elif key=='"log(Species)"':
        graph.world.xmax=20
        graph.world.xmin=-20
        major=10
        labtext=altlabels[1]
      elif key=='"log(Species):Latitude"':
        graph.world.xmin=-1
        graph.world.xmax=1
        major=1
        labtext=altlabels[2]
      elif key=='"log(Species):Stream"':
        graph.world.xmax=20
        graph.world.xmin=-20
        major=10
        labtext=altlabels[4]
      else: # Lake
        graph.world.xmax=20
        graph.world.xmin=-20
        major=10
        labtext=altlabels[3]

    graph.xaxis.tick.configure(place='both',major_size=.4,minor_ticks=0,minor_size=.4,major=major,major_linewidth=.5,minor_linewidth=.5)
    graph.xaxis.ticklabel.configure(char_size=.5,format='decimal',prec=0,angle=90)

    graph.xaxis.label.configure(text=labtext,char_size=.7,just=2)

    graph.frame.linewidth=.5
    graph.xaxis.bar.linewidth=.5
    graph.yaxis.bar.linewidth=.5

  if prop in ['LS','Vul']:
    grace.multi(rows=1,cols=6,hgap=.06,width_to_height_ratio=0.08/1)
    grace.add_drawing_object(DrawText,text=\
      LatexString(r'estimate of coefficient after removing\n       webs with a common author'),char_size=1,x=.3048,y=0.04,loctype='view',just=2)
  else:
    grace.multi(rows=1,cols=8,hgap=.06,width_to_height_ratio=0.08/1)
    grace.add_drawing_object(DrawText,text='estimate of coefficient after removing webs with a common author',char_size=1,x=.5546,y=0.04,loctype='view',just=2)
  
  grace.hide_redundant_labels()

  grace.write_file('../manuscript/Figures/Jackknife/'+prop+'_author_corrected.eps')


def main():

  directory='../Jackknifed/main/coefficients/'
  files=os.listdir(directory)
  datafile='../non_TS/summary-properties_corrected_webs.tsv'
  removalfile='../Jackknifed/webs_per_author.tsv'

  webfiles=[]
  authorfiles=[]

  LS_webfiles=[]
  Gen_webfiles=[]
  Vul_webfiles=[]

  LS_authorfiles=[]
  Gen_authorfiles=[]
  Vul_authorfiles=[]

  weblist=set()
  for fil in files:
    if '_corrected' in fil: # Only want the corrected coefficients
      if 'web' in fil and 'Lakeweb' not in fil:
        weblist.add(fil.split('.web')[0])
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

  # print sorted(weblist)
  for prop in ['Vul','Gen','LS']:
    if prop=='LS':
      authorfiles=LS_authorfiles
    elif prop=='Gen':
      authorfiles=Gen_authorfiles
    else:
      authorfiles=Vul_authorfiles

    author_jackknife_plotter(directory,authorfiles,prop,datafile,removalfile,webfiles)

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
