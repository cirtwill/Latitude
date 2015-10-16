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

import marginal_latitude_fig_noTL as marg

# Think I want plots of the raw data (S, LS, G, V vs. lat) with slope lines
# Plus predictions for LS, G, V vs S (with corrected obs.)

def datareader(rawdatafile,TL):

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
      B=float(line.split('\t')[15])
      H=float(line.split('\t')[16])
      I=float(line.split('\t')[17])
      I=I+H # Include the herbivores as intermediates
      T=float(line.split('\t')[18])

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

def heatmappoints(rawdatafile,fixed,prop,ecotype,TL):
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
      B=float(line.split('\t')[15])
      H=float(line.split('\t')[16])
      I=float(line.split('\t')[17])
      I=H+I
      T=float(line.split('\t')[18])

      # Correct the observed property based on latitude and ecotype effects

      if 'log('+key+'):'+ecotype in fixed_effects: 
        delta=fixed_effects['log('+key+'):'+ecotype]
      else:   # If there's no intercept effect of ecosystem
        delta=0

      if 'log('+key+'):Latitude' in fixed_effects:
        if 'log('+key+'):'+ecotype+':Latitude' in fixed_effects:
         gamma=fixed_effects['log('+key+'):Latitude']+fixed_effects['log('+key+'):'+ecotype+':Latitude'] 
        elif 'log('+key+'):Latitude:'+ecotype in fixed_effects:
          gamma=fixed_effects['log('+key+'):Latitude']+fixed_effects['log('+key+'):Latitude:'+ecotype]
        else:  #If there's no lat-by-ecotype interaction
          gamma=fixed_effects['log('+key+'):Latitude'] 
      else: # If there's no latitude effect at all
        gamma=0

      exponent=delta+Latitude*gamma

      if B>0 and I>0 and T>0:
        correction=eval(TL)**exponent
        y=float(Decimal(eval(prop))/Decimal(correction))
        obs[ecotype].append((eval(TL),y))
  f.close()

  return obs

def predictionreader(predfile,TL):
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

      if TL=='B':
        predpoints[ecotype][Lat].append((B,math.exp(pred)))
      elif TL=='I':
        predpoints[ecotype][Lat].append((I,math.exp(pred)))
      elif TL=='T':
        predpoints[ecotype][Lat].append((T,math.exp(pred)))
      elif TL=='S':
        predpoints[ecotype][Lat].append((S,math.exp(pred)))

  f.close()
  return predpoints

def predictionlines(fixed,prop,TL):
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

  if TL=='S':
    for S in range(1,350):
      predy=alpha*(S**delta)
      ecoline.append((S,predy))
  else:
    ## Need to do a range of percents
    for frac in range(1,201):
      perc=float(frac)/float(200)
      predy=alpha*(perc**delta)
      ecoline.append((perc,predy))

  return ecoline

def Gen_vs_lat_sim(prop):
  # Lets make clear that these are the original, uncorrected points
  outfile='../talk/Figures/results/'+prop+'_vs_lat_negative.eps'

  grace=Grace(colors=ColorBrewerScheme("PRGn"))

  graph=grace.add_graph()

  # narrowniches=graph.add_dataset([(0,50),(90,2)])
  # narrowniches.symbol.shape=0
  # narrowniches.line.configure(color=2,linewidth=5)

  ytex='Absolute latitude'
  graph.xaxis.label.configure(text=ytex,place='normal',char_size=1.5)

  if prop=='LS':
    graph.yaxis.label.configure(text="Link density",place='normal',char_size=1.5)
  elif prop=='Gen':
    graph.yaxis.label.configure(text="Generality",place='normal',char_size=1.5)

  graph.add_drawing_object(DrawText,text='?',x=25,y=8,just=2,loctype='world',char_size=6,color=11)
  graph.add_drawing_object(DrawText,text='?',x=65,y=.5,just=2,loctype='world',char_size=6,color=2)

  narrowniches=graph.add_dataset([(100,1),(100,100)])
  narrowniches.symbol.shape=0
  narrowniches.line.configure(color=2,linewidth=5)
  narrowniches.legend="Narrower niches in the tropics"

  sameniches=graph.add_dataset([(100,2),(190,3)])
  sameniches.symbol.shape=0
  sameniches.line.configure(color=11,linewidth=5)
  sameniches.legend="Similar niches everywhere"

  graph.legend.configure(char_size=1.25,frame=0,loc=(5,.4),loctype='world',box_linestyle=0)

  graph.world.ymin=.1
  graph.world.ymax=100
  graph.yaxis.set_log()

  graph.world.xmin=0
  graph.world.xmax=90

  graph.xaxis.tick.configure(major=30,minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph.xaxis.ticklabel.configure(char_size=1.5)
  graph.frame.linewidth=2
  graph.xaxis.bar.linewidth=2
  graph.yaxis.bar.linewidth=2

  graph.yaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph.yaxis.ticklabel.configure(char_size=1.5)


  grace.write_file(outfile)

def S_rawplots(rawdatafile,outfile1,predfolder,plottype):

  # Lets make clear that these are the original, uncorrected points

  grace=Grace(colors=ColorBrewerScheme("PRGn"))

  TL='S'
  prop='Gen'
  outfile=outfile1+prop+'_'+plottype+'_vs_S_fitline_observed.eps'  

  rawdata=datareader(rawdatafile,TL)

  fixed=fixed_reader('../non_TS/coefficients/'+prop+'_obs.tsv')

  graph=grace.add_graph()
  print plottype
  if plottype=='dots':
    obspoints=graph.add_dataset(rawdata[prop])
  else:
    obspoints=graph.add_dataset([])
    graph.add_drawing_object(DrawText,text='Latitude effect on slope?',char_size=1,loc=(0,0),loctype='view')
  obspoints.line.configure(linestyle=0)
  obspoints.symbol.configure(size=.75,shape=1,fill_color=0,fill_pattern=1,color=2,linewidth=3)

  predictions=predictionlines(fixed,prop,TL)
  predline=graph.add_dataset(predictions)
  predline.symbol.shape=0

  predline.line.configure(linestyle=1,color=3,linewidth=5)

  ytex='Species richness'
  graph.xaxis.label.configure(text=ytex,place='normal',char_size=1.5)

  if prop=='LS':
    graph.yaxis.label.configure(text="Link density",place='normal',char_size=1.5)
  elif prop=='Gen':
    graph.yaxis.label.configure(text="Generality",place='normal',char_size=1.5)

  graph.world.xmin=1
  graph.world.ymin=.1

  graph.world.ymax=100
  graph.xaxis.set_log()
  graph.yaxis.set_log()

  graph.world.xmax=300

  graph.xaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph.xaxis.ticklabel.configure(char_size=1.5)
  graph.frame.linewidth=2
  graph.xaxis.bar.linewidth=2
  graph.yaxis.bar.linewidth=2

  graph.yaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph.yaxis.ticklabel.configure(char_size=1.5)

  print 'got a raw plot'

  grace.write_file(outfile)

  # Lets make clear that these are the original, uncorrected points
  outfile2='../talk/Figures/results/'+prop+'_vs_lat_simulated.eps'

  grace=Grace(colors=ColorBrewerScheme("PRGn"))

  graph=grace.add_graph()

  narrowniches=graph.add_dataset([(0,1),(90,100)])
  narrowniches.symbol.shape=0
  narrowniches.line.configure(color=2,linewidth=5)
  narrowniches.legend="Narrower niches in the tropics"

  sameniches=graph.add_dataset([(0,3),(90,3)])
  sameniches.symbol.shape=0
  sameniches.line.configure(color=11,linewidth=5)
  sameniches.legend="Similar niches everywhere"

  ytex='Absolute latitude'
  graph.xaxis.label.configure(text=ytex,place='normal',char_size=1.5)

  if prop=='LS':
    graph.yaxis.label.configure(text="Link density",place='normal',char_size=1.5)
  elif prop=='Gen':
    graph.yaxis.label.configure(text="Generality",place='normal',char_size=1.5)

  graph.legend.configure(char_size=1.25,frame=0,loc=(5,.4),loctype='world',box_linestyle=0)

  graph.world.ymin=.1
  graph.world.ymax=100
  graph.yaxis.set_log()

  graph.world.xmin=0
  graph.world.xmax=90

  graph.xaxis.tick.configure(major=30,minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph.xaxis.ticklabel.configure(char_size=1.5)
  graph.frame.linewidth=2
  graph.xaxis.bar.linewidth=2
  graph.yaxis.bar.linewidth=2

  graph.yaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph.yaxis.ticklabel.configure(char_size=1.5)


  grace.write_file(outfile2)

def marginal_plotter(prop,TL,ecotype,graph):

  dataset=marg.linereader(prop,TL)

  if dataset[ecotype]['upper']!=[]:
    upper=dataset[ecotype]['upper']
    lower=dataset[ecotype]['lower']
  else:
    upper=dataset['Estuary']['upper']
    lower=dataset['Estuary']['lower']

  upper2=graph.add_dataset(upper)
  upper2.symbol.configure(shape=0)
  upper2.line.configure(linestyle=0,color=10,linewidth=.5)
  upper2.fill.configure(color=5,type=2)

  lower2=graph.add_dataset(lower)
  lower2.symbol.configure(shape=0)
  lower2.line.configure(linestyle=0,color=10,linewidth=.5)
  lower2.fill.configure(color=0,type=2)


  if dataset[ecotype]['main']!=[]:
    main=graph.add_dataset(dataset[ecotype]['main'])
  else:
    main=graph.add_dataset(dataset['Estuary']['main'])
  main.symbol.shape=0
  main.line.configure(linestyle=1,color=1,linewidth=3)

  graph.xaxis.bar.linewidth=2
  graph.yaxis.bar.linewidth=2
  graph.frame.linewidth=2

  graph.world.xmin=0
  graph.world.xmax=90

  graph.world.ymin=0
  graph.world.ymax=1.5
  major=.5
  prec=1

  graph.xaxis.ticklabel.configure(char_size=1.5)
  graph.xaxis.tick.configure(major_linewidth=1.5,minor_linewidth=1.5,major_size=1,minor_size=.7,major=30)

  graph.yaxis.tick.configure(major=major,major_linewidth=1.5,minor_linewidth=1.5,major_size=1,minor_size=.7)
  graph.yaxis.ticklabel.configure(format='decimal',prec=prec,char_size=1.5)

  if ecotype=='Stream':
    graph.xaxis.label.configure(text='Absolute latitude',place='normal',char_size=1.5,perpendicular_offset=0.05)  

  return graph

def dummy_marginal_plotter(plottype,ecotypes,prop):
  grace=MultiPanelGrace(colors=ColorBrewerScheme('PRGn'))

  names=['Estuarine','Marine','Terrestrial','Lake','Stream']

  grace.add_label_scheme('dummy',names)
  grace.set_label_scheme('dummy')
  
  dataset=marg.linereader('Gen','S')

  upper=dataset['Estuary']['upper']
  lower=dataset['Estuary']['lower']

  for ecotype in ecotypes:   

    graph=grace.add_graph(Panel)

    if plottype=='one' and ecotype=='Estuary':
      upper2=graph.add_dataset(upper)
      upper2.symbol.configure(shape=0)
      upper2.line.configure(linestyle=0,color=10,linewidth=.5)
      upper2.fill.configure(color=5,type=2)

      lower2=graph.add_dataset(lower)
      lower2.symbol.configure(shape=0)
      lower2.line.configure(linestyle=0,color=10,linewidth=.5)
      lower2.fill.configure(color=0,type=2)

      main=graph.add_dataset(dataset['Estuary']['main'])
      main.symbol.shape=0
      main.line.configure(linestyle=1,color=1,linewidth=3)

    graph.xaxis.bar.linewidth=2
    graph.yaxis.bar.linewidth=2
    graph.frame.linewidth=2

    graph.world.xmin=0
    graph.world.xmax=90

    graph.world.ymin=0
    graph.world.ymax=1.5
    major=.5
    prec=1

    graph.xaxis.ticklabel.configure(char_size=1.5)
    graph.xaxis.tick.configure(major_linewidth=1.5,minor_linewidth=1.5,major_size=1,minor_size=.7,major=30)

    graph.yaxis.tick.configure(major=major,major_linewidth=1.5,minor_linewidth=1.5,major_size=1,minor_size=.7)
    graph.yaxis.ticklabel.configure(format='decimal',prec=prec,char_size=1.5)

    if ecotype=='Stream':
      graph.xaxis.label.configure(text='Absolute latitude',place='normal',char_size=1.5)  
    graph.panel_label.configure(char_size=1.5,placement='ouc',dy=.01)

  grace.multi(rows=2,cols=3,vgap=.08,hgap=.08)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  
  grace.set_col_yaxislabel(col=0,rowspan=(0,1),label='Scaling exponent',place='normal',just=2,char_size=1.5,perpendicular_offset=0.1)
  # Sgrace.set_row_xaxislabel(row=1,colspan=(0,2),label='Absolute latitude',place='normal',just=2,char_size=1,perpendicular_offset=0.05)

  grace.write_file('../talk/Figures/results/'+prop+'_vs_S_marginal_'+plottype+'.eps')

  print plottype

def lat_histogram_plot(datafile):

	histlist=[]

	a=1
	b=1
	c=1
	d=1
	e=1
	f=1
	g=1
	h=1
	i=1
	j=1
	k=1
	l=1
	m=1
	n=1
	o=1
	p=1

	q=open(datafile,'r')
	for line in q:
		web=line.split('\t')[0]
		if web!='Web':
			ecotype=line.split('\t')[1]
			site=line.split('\t')[2]
			latitude=float(line.split('\t')[3])
			species=float(line.split('\t')[4])
			gen=float(line.split('\t')[9])

			if latitude < 5:
				histlist.append((1,a))
				a=a+1
			elif 5 <= latitude <10:
				histlist.append((2,b))
				b=b+1
			elif 10 <= latitude <15:
				histlist.append((3,c))
				c=c+1
			elif 15 <= latitude <20:
				histlist.append((4,d))
				d=d+1
			elif 20 <= latitude <25:
				histlist.append((5,e))
				e=e+1
			elif 25 <= latitude <30:
				histlist.append((6,f))
				f=f+1
			elif 30 <= latitude <35:
				histlist.append((7,g))
				g=g+1
			elif 35 <= latitude < 40:
				histlist.append((8,h))
				h=h+1
			elif 40 <= latitude <45:
				histlist.append((9,i))
				i=i+1
			elif 45 <= latitude <50:
				histlist.append((10,j))
				j=j+1
			elif 50 <= latitude <55:
				histlist.append((11,k))
				k=k+1
			elif 55 <= latitude <60:
				histlist.append((12,l))
				l=l+1
			elif 60 <= latitude <65:
				histlist.append((13,m))
				m=m+1
			elif 65 <= latitude <70:
				histlist.append((14,n))
				n=n+1
			elif 70 <= latitude <75:
				histlist.append((15,o))
				o=o+1
			elif 75 <= latitude:
				histlist.append((16,p))
				p=p+1

	q.close()

	grace=Grace(colors=ColorBrewerScheme('PRGn'))
	graph=grace.add_graph()

	data=graph.add_dataset(histlist)
	data.line.linestyle=0
	data.symbol.configure(shape=2,color=2,fill_color=2,size=1.5)

	graph.world.xmin=0
	graph.world.xmax=17
	graph.world.ymin=0
	graph.world.ymax=70

	labels=['0','10','20','30','40','50','60','70','80']
	graph.xaxis.tick.set_spec_ticks([.5,2.5,4.5,6.5,8.5,10.5,12.5,14.5,16.5],[],labels)
	graph.xaxis.tick.configure(onoff='off')
	graph.xaxis.ticklabel.configure(char_size=1.5,just=2)
	graph.yaxis.ticklabel.configure(char_size=1.5)
	graph.yaxis.tick.configure(major=20,major_linewidth=1.5,minor_linewidth=1.5,major_size=1,minor_size=.7)
	graph.yaxis.ticklabel.configure(format='decimal',prec=0,char_size=1.5)

	graph.xaxis.label.configure(text='Absolute latitude',char_size=1.5,just=2,perpendicular_offset=0.1)
	graph.yaxis.label.configure(text='Number of webs',char_size=1.5,just=2,perpendicular_offset=0.1)

	graph.xaxis.bar.linewidth=2
	graph.yaxis.bar.linewidth=2
	graph.frame.linewidth=2

	grace.write_file('../talk/Figures/results/latitude_hist.eps')

def gen_histogram_plot(datafile):

	histlist=[]

	a=1
	b=1
	c=1
	d=1
	e=1
	f=1
	g=1
	h=1
	i=1
	j=1
	k=1
	l=1
	m=1
	n=1
	o=1
	p=1

	q=open(datafile,'r')
	for line in q:
		web=line.split('\t')[0]
		if web!='Web':
			ecotype=line.split('\t')[1]
			site=line.split('\t')[2]
			latitude=float(line.split('\t')[3])
			species=float(line.split('\t')[4])
			gen=float(line.split('\t')[9])

			if gen < 3:
				histlist.append((1,a))
				a=a+1
			elif 3 <= gen <6:
				histlist.append((2,b))
				b=b+1
			elif 6 <= gen <9:
				histlist.append((3,c))
				c=c+1
			elif 9 <= gen <12:
				histlist.append((4,d))
				d=d+1
			elif 12 <= gen <15:
				histlist.append((5,e))
				e=e+1
			elif 15 <= gen <18:
				histlist.append((6,f))
				f=f+1
			elif 18 <= gen <21:
				histlist.append((7,g))
				g=g+1
			elif 21 <= gen < 24:
				histlist.append((8,h))
				h=h+1
			elif 24 <= gen <27:
				histlist.append((9,i))
				i=i+1
			elif 27 <= gen <30:
				histlist.append((10,j))
				j=j+1
	q.close()

	grace=Grace(colors=ColorBrewerScheme('PRGn'))
	graph=grace.add_graph()

	data=graph.add_dataset(histlist)
	data.line.linestyle=0
	data.symbol.configure(shape=2,color=2,fill_color=2,size=1.5)

	graph.world.xmin=0
	graph.world.xmax=11
	graph.world.ymin=0
	graph.world.ymax=100

	labels=['0','3','6','9','12','15','18','21','24','27','30','55','60','65','70','75']
	graph.xaxis.tick.set_spec_ticks([.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5],[],labels)
	graph.xaxis.tick.configure(onoff='off')
	graph.xaxis.ticklabel.configure(char_size=1.5,just=2)
	graph.yaxis.ticklabel.configure(char_size=1.5)
	graph.yaxis.tick.configure(major=20,major_linewidth=1.5,minor_linewidth=1.5,major_size=1,minor_size=.7)
	graph.yaxis.ticklabel.configure(format='decimal',prec=0,char_size=1.5)

	graph.xaxis.label.configure(text='Mean generality (number of prey)',char_size=1.5,just=2,perpendicular_offset=0.1)
	graph.yaxis.label.configure(text='Number of webs',char_size=1.5,just=2,perpendicular_offset=0.1)

	graph.xaxis.bar.linewidth=2
	graph.yaxis.bar.linewidth=2
	graph.frame.linewidth=2

	grace.write_file('../talk/Figures/results/generality_hist.eps')

def Gen_predictedlines(rawdatafile,outfile1,predfolder):

  # Lets make clear that these are the original, uncorrected points

  grace=Grace(colors=ColorBrewerScheme("PRGn"))

  TL='S'
  prop='Gen'
  plottype='dots'
  outfile=outfile1+'Gen_vs_S_expectations.eps'  

  rawdata=datareader(rawdatafile,TL)

  fixed=fixed_reader('../non_TS/coefficients/'+prop+'_obs.tsv')

  graph=grace.add_graph()
  obspoints=graph.add_dataset(rawdata[prop])
  obspoints.line.configure(linestyle=0)
  obspoints.symbol.configure(size=.75,shape=1,fill_color=0,fill_pattern=1,color=2,linewidth=3)

  # predictions=predictionlines(fixed,prop,TL)
  # predline=graph.add_dataset(predictions)
  # predline.symbol.shape=0
  # predline.line.configure(linestyle=1,color=3,linewidth=5)

  predline1=graph.add_dataset([(1,.8),(250,10)])
  predline1.symbol.shape=0
  predline1.line.configure(linestyle=3,color=12,linewidth=5)
  predline1.legend='Low-latitude food webs'

  predline2=graph.add_dataset([(1,.3),(250,20)])
  predline2.symbol.shape=0
  predline2.line.configure(linestyle=3,color=10,linewidth=5)
  predline2.legend='High-latitude food webs'

  ytex='Species richness'
  graph.xaxis.label.configure(text=ytex,place='normal',char_size=1.5)

  if prop=='LS':
    graph.yaxis.label.configure(text="Link density",place='normal',char_size=1.5)
  elif prop=='Gen':
    graph.yaxis.label.configure(text="Generality",place='normal',char_size=1.5)

  graph.world.xmin=1
  graph.world.ymin=.1

  graph.world.ymax=100
  graph.xaxis.set_log()
  graph.yaxis.set_log()

  graph.world.xmax=300

  graph.xaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph.xaxis.ticklabel.configure(char_size=1.5)
  graph.frame.linewidth=2
  graph.xaxis.bar.linewidth=2
  graph.yaxis.bar.linewidth=2

  graph.yaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph.yaxis.ticklabel.configure(char_size=1.5)

  graph.legend.configure(char_size=1.25,frame=0,loc=(5,.4),loctype='world',box_linestyle=0)

  print 'got a plot with extra lines'

  grace.write_file(outfile)


def interpretations(rawdatafile,predfolder,outfile1):
  grace=MultiPanelGrace(colors=ColorBrewerScheme("PRGn"))
  graph1=grace.add_graph(Panel)

  TL='S'
  prop='Gen'
  plottype='dots'
  outfile=outfile1+'no_effect_interpretations.eps'  

  rawdata=datareader(rawdatafile,TL)

  fixed=fixed_reader('../non_TS/coefficients/'+prop+'_obs.tsv')

  # obspoints=graph1.add_dataset(rawdata[prop])
  # obspoints.line.configure(linestyle=0)
  # obspoints.symbol.configure(size=.75,shape=1,fill_color=0,fill_pattern=1,color=2,linewidth=3)

  # predictions=predictionlines(fixed,prop,TL)
  # predline=graph1.add_dataset(predictions)
  # predline.symbol.shape=0
  # predline.line.configure(linestyle=1,color=3,linewidth=5)

  predline1=graph1.add_dataset([(1,.55),(250,14)])
  predline1.symbol.shape=0
  predline1.line.configure(linestyle=1,color=12,linewidth=5)
  predline1.legend='Low-latitude food webs'

  predline2=graph1.add_dataset([(1,.45),(250,16)])
  predline2.symbol.shape=0
  predline2.line.configure(linestyle=1,color=10,linewidth=5)
  predline2.legend='High-latitude food webs'

  ytex='Species richness'
  graph1.xaxis.label.configure(text=ytex,place='normal',char_size=1.5)

  if prop=='LS':
    graph1.yaxis.label.configure(text="Link density",place='normal',char_size=1.5)
  elif prop=='Gen':
    graph1.yaxis.label.configure(text="Generality",place='normal',char_size=1.5)

  graph1.world.xmin=1
  graph1.world.ymin=.1

  graph1.world.ymax=100
  graph1.xaxis.set_log()
  graph1.yaxis.set_log()

  graph1.world.xmax=300

  graph1.xaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph1.xaxis.ticklabel.configure(char_size=1.5)
  graph1.frame.linewidth=2
  graph1.xaxis.bar.linewidth=2
  graph1.yaxis.bar.linewidth=2

  graph1.yaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph1.yaxis.ticklabel.configure(char_size=1.5)

  graph1.legend.configure(char_size=1,frame=0,loc=(3.5,.7),loctype='world',box_linestyle=0,fill_pattern=0)

  print 'got a plot with extra lines'

  graph2=grace.add_graph(Panel)

  sameniches=graph2.add_dataset([(0,3),(90,3)])
  sameniches.symbol.shape=0
  sameniches.line.configure(color=2,linewidth=5)
  # sameniches.legend="Similar niches everywhere"

  ytex='Absolute latitude'
  graph2.xaxis.label.configure(text=ytex,place='normal',char_size=1.5)

  if prop=='LS':
    graph2.yaxis.label.configure(text="Link density",place='normal',char_size=1.5)
  elif prop=='Gen':
    graph2.yaxis.label.configure(text="Generality",place='normal',char_size=1.5)

  graph2.legend.configure(char_size=1.25,frame=0,loc=(5,.4),loctype='world',box_linestyle=0)

  graph2.world.ymin=.1
  graph2.world.ymax=100
  graph2.yaxis.set_log()

  graph2.world.xmin=0
  graph2.world.xmax=90

  graph2.xaxis.tick.configure(major=30,minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph2.xaxis.ticklabel.configure(char_size=1.5)
  graph2.frame.linewidth=2
  graph2.xaxis.bar.linewidth=2
  graph2.yaxis.bar.linewidth=2

  graph2.yaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph2.yaxis.ticklabel.configure(char_size=1.5)

  graph1.panel_label.configure(char_size=0)
  graph2.panel_label.configure(char_size=0)

  grace.multi(rows=1,cols=2,vgap=.08,hgap=.08)
  grace.hide_redundant_labels()
  grace.write_file(outfile)

def interpretations2(rawdatafile,predfolder,outfile1):
  grace=MultiPanelGrace(colors=ColorBrewerScheme("PRGn"))
  graph1=grace.add_graph(Panel)

  TL='S'
  prop='Gen'
  plottype='dots'
  outfile=outfile1+'effect_interpretations.eps'  

  rawdata=datareader(rawdatafile,TL)

  fixed=fixed_reader('../non_TS/coefficients/'+prop+'_obs.tsv')

  # obspoints=graph1.add_dataset(rawdata[prop])
  # obspoints.line.configure(linestyle=0)
  # obspoints.symbol.configure(size=.75,shape=1,fill_color=0,fill_pattern=1,color=2,linewidth=3)

  # predictions=predictionlines(fixed,prop,TL)
  # predline=graph1.add_dataset(predictions)
  # predline.symbol.shape=0
  # predline.line.configure(linestyle=1,color=3,linewidth=5)
  predline1=graph1.add_dataset([(1,.8),(250,10)])
  predline1.symbol.shape=0
  predline1.line.configure(linestyle=1,color=12,linewidth=5)
  predline1.legend='Low-latitude food webs'

  predline2=graph1.add_dataset([(1,.3),(250,20)])
  predline2.symbol.shape=0
  predline2.line.configure(linestyle=1,color=10,linewidth=5)
  predline2.legend='High-latitude food webs'

  ytex='Species richness'
  graph1.xaxis.label.configure(text=ytex,place='normal',char_size=1.5)

  if prop=='LS':
    graph1.yaxis.label.configure(text="Link density",place='normal',char_size=1.5)
  elif prop=='Gen':
    graph1.yaxis.label.configure(text="Generality",place='normal',char_size=1.5)

  graph1.world.xmin=1
  graph1.world.ymin=.1

  graph1.world.ymax=100
  graph1.xaxis.set_log()
  graph1.yaxis.set_log()

  graph1.world.xmax=300

  graph1.xaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph1.xaxis.ticklabel.configure(char_size=1.5)
  graph1.frame.linewidth=2
  graph1.xaxis.bar.linewidth=2
  graph1.yaxis.bar.linewidth=2

  graph1.yaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph1.yaxis.ticklabel.configure(char_size=1.5)

  graph1.legend.configure(char_size=1,frame=0,loc=(3.5,.7),loctype='world',box_linestyle=0,fill_pattern=0)

  print 'got a plot with extra lines'

  graph2=grace.add_graph(Panel)

  # sameniches=graph2.add_dataset([(0,3),(90,3)])
  # sameniches.symbol.shape=0
  # sameniches.line.configure(color=2,linewidth=5)

  narrowniches=graph2.add_dataset([(0,1),(90,100)])
  narrowniches.symbol.shape=0
  narrowniches.line.configure(color=2,linewidth=5)

  ytex='Absolute latitude'
  graph2.xaxis.label.configure(text=ytex,place='normal',char_size=1.5)

  if prop=='LS':
    graph2.yaxis.label.configure(text="Link density",place='normal',char_size=1.5)
  elif prop=='Gen':
    graph2.yaxis.label.configure(text="Generality",place='normal',char_size=1.5)

  graph2.legend.configure(char_size=1.25,frame=0,loc=(5,.4),loctype='world',box_linestyle=0)

  graph2.world.ymin=.1
  graph2.world.ymax=100
  graph2.yaxis.set_log()

  graph2.world.xmin=0
  graph2.world.xmax=90

  graph2.xaxis.tick.configure(major=30,minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph2.xaxis.ticklabel.configure(char_size=1.5)
  graph2.frame.linewidth=2
  graph2.xaxis.bar.linewidth=2
  graph2.yaxis.bar.linewidth=2

  graph2.yaxis.tick.configure(minor_ticks=1,major_size=1,minor_size=.7,major_linewidth=1.5,minor_linewidth=1.5)
  graph2.yaxis.ticklabel.configure(char_size=1.5)

  graph1.panel_label.configure(char_size=0)
  graph2.panel_label.configure(char_size=0)

  grace.multi(rows=1,cols=2,vgap=.08,hgap=.08)
  grace.hide_redundant_labels()
  grace.write_file(outfile)


def main():

  ecotypes=['Estuary','Marine','Terrestrial','Lake','Stream']

  Bformat='proportions'
  rawdatafile='../non_TS/summary-properties.tsv'

  outfile1='../talk/Figures/results/'
  predfolder='../non_TS/'+Bformat

  interpretations(rawdatafile,predfolder,outfile1)
  interpretations2(rawdatafile,predfolder,outfile1)

  Gen_predictedlines(rawdatafile,outfile1,predfolder)

  for plottype in ['dots','nodots']:
    S_rawplots(rawdatafile,outfile1,predfolder,plottype)

  TL='S'

  lat_histogram_plot(rawdatafile)
  gen_histogram_plot(rawdatafile)

  for prop in ["LS","Gen"]:

    Sgrace=MultiPanelGrace(colors=ColorBrewerScheme('PRGn'))

    Snames=['Estuarine','Marine','Terrestrial','Lake','Stream']

    Sgrace.add_label_scheme('dummy',Snames)
    Sgrace.set_label_scheme('dummy')
    
    for ecotype in ecotypes:   

      graph=Sgrace.add_graph(Panel)

      marginal_plotter(prop,TL,ecotype,graph)
      graph.panel_label.configure(char_size=1.5,placement='ouc',dy=.01)

    Sgrace.multi(rows=2,cols=3,vgap=.08,hgap=.08)

    Sgrace.hide_redundant_xticklabels()
    Sgrace.hide_redundant_yticklabels()

    Sgrace.set_col_yaxislabel(col=0,rowspan=(0,1),label='Scaling exponent',place='normal',just=2,char_size=1.5,perpendicular_offset=0.1)

    Sgrace.write_file('../talk/Figures/results/'+prop+'_vs_S_marginal.eps')
 
  for plottype in ['axis','one']:
    dummy_marginal_plotter(plottype,ecotypes,'Gen')

  prop="Gen" # Split the plot

  Ngrace=MultiPanelGrace(colors=ColorBrewerScheme('PRGn'))

  Nnames=['Estuarine','Marine','Terrestrial']

  Ngrace.add_label_scheme('dummy',Nnames)
  Ngrace.set_label_scheme('dummy')
  
  for ecotype in ecotypes[:3]:   

    graph=Ngrace.add_graph(Panel)

    marginal_plotter(prop,TL,ecotype,graph)
    graph.panel_label.configure(char_size=1.5,placement='ouc',dy=.01)

  Ngrace.multi(rows=1,cols=3,vgap=.08,hgap=.08)

  Ngrace.hide_redundant_xticklabels()
  Ngrace.hide_redundant_yticklabels()

  Ngrace.set_col_yaxislabel(col=0,rowspan=(0,0),label='Scaling exponent',place='normal',just=2,char_size=1.5,perpendicular_offset=0.1)
  Ngrace.set_row_xaxislabel(row=0,colspan=(0,2),label='Absolute latitude',place='normal',just=2,char_size=1.5,perpendicular_offset=0.1)

  Ngrace.write_file('../talk/Figures/results/no_effect.eps')


  Egrace=MultiPanelGrace(colors=ColorBrewerScheme('PRGn'))

  Nnames=['Lake','Stream','']

  Egrace.add_label_scheme('dummy',Nnames)
  Egrace.set_label_scheme('dummy')
  
  for ecotype in ecotypes[3:]:   

    graph=Egrace.add_graph(Panel)

    marginal_plotter(prop,TL,ecotype,graph)
    graph.panel_label.configure(char_size=1.5,placement='ouc',dy=.01)

  # blank=Egrace.add_graph(Panel)
  # blank.xaxis.bar.linestyle=0
  # blank.yaxis.bar.linestyle=0
  # blank.frame.linestyle=0
  # blank.world.xmin=0
  # blank.world.xmax=90
  # blank.world.ymin=0
  # blank.world.ymax=1.5
  # major=.5
  # prec=1

  # blank.xaxis.ticklabel.configure(char_size=1.5,color=0)
  # blank.xaxis.tick.configure(major_linewidth=0,minor_linewidth=0,major_size=0,minor_size=.0,major=30)
  # blank.yaxis.tick.configure(major=major,major_linewidth=1.5,minor_linewidth=1.5,major_size=1,minor_size=.7)
  # blank.yaxis.ticklabel.configure(format='decimal',prec=prec,char_size=1.5)

  Egrace.multi(rows=1,cols=2,vgap=.08,hgap=.08)

  Egrace.hide_redundant_xticklabels()
  Egrace.hide_redundant_yticklabels()

  Egrace.set_col_yaxislabel(col=0,rowspan=(0,0),label='Scaling exponent',place='normal',just=2,char_size=1.5,perpendicular_offset=0.1)
  Egrace.set_row_xaxislabel(row=0,colspan=(0,1),label='Absolute latitude',place='normal',just=2,char_size=1.5,perpendicular_offset=0.1)

  Egrace.write_file('../talk/Figures/results/effect.eps')

  Gen_vs_lat_sim('Gen')
 
if __name__ == '__main__':
  main()