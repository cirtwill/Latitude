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

# Think I want plots of the raw data (S, LS, G, V vs. lat) with slope lines
# Plus predictions for LS, G, V vs S (with corrected obs.)
# [[Redoing this with the new data, old files called are in comments]]


def datareader(rawdatafile,TL,Bformat):

  points={'LS':[],'Gen':[],'Vul':[]}

  f=open(rawdatafile,'r')
  i=1
  for line in f:
    if line.split('\t')[0]!='Web':
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
      Path=line.split('\t')[13]
      Chain=line.split('\t')[14]
      SWTL=line.split('\t')[15]
      Clus=line.split('\t')[16]
      B=float(line.split('\t')[17])
      H=float(line.split('\t')[18])
      I=float(line.split('\t')[19])
      I=I+H # Include the herbivores as intermediates
      T=float(line.split('\t')[20])

      # B=float(line.split('\t')[15])
      # H=float(line.split('\t')[16])
      # I=float(line.split('\t')[17])
      # I=I+H # Include the herbivores as intermediates
      # T=float(line.split('\t')[18])

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

# def predictionreader(predfile,TL,Bformat):
#   predpoints={'Lake':{0:[],30:[],60:[]},
#             'Marine':{0:[],30:[],60:[]},
#             'Stream':{0:[],30:[],60:[]},
#             'Terrestrial':{0:[],30:[],60:[]},
#             'Estuary':{0:[],30:[],60:[]}}
#   f=open(predfile,'r')
#   for line in f:
#     if line.split()[0]!='"Latitude"':
#       Lat=int(line.split()[0])
#       pred=float(line.split()[-1])
#       if line.split()[1]=='1':
#         ecotype='Lake'
#       elif line.split()[2]=='1':
#         ecotype='Marine'
#       elif line.split()[3]=='1':
#         ecotype='Stream'
#       elif line.split()[4]=='1':
#         ecotype='Terrestrial'
#       else:
#         ecotype='Estuary'
#       B=float(line.split()[5])
#       S=float(line.split()[6])
#       I=float(line.split()[7])
#       T=float(line.split()[8])

#       if Bformat=='proportions':
#         if TL=='B':
#           predpoints[ecotype][Lat].append((B,math.exp(pred)))
#         elif TL=='I':
#           predpoints[ecotype][Lat].append((I,math.exp(pred)))
#         elif TL=='T':
#           predpoints[ecotype][Lat].append((T,math.exp(pred)))
#         elif TL=='S':
#           predpoints[ecotype][Lat].append((S,math.exp(pred)))
#       else:
#         if TL=='B':
#           predpoints[ecotype][Lat].append((B*S,math.exp(pred)))
#         elif TL=='I':
#           predpoints[ecotype][Lat].append((I*S,math.exp(pred)))
#         elif TL=='T':
#           predpoints[ecotype][Lat].append((T*S,math.exp(pred)))
#         elif TL=='S':
#           predpoints[ecotype][Lat].append((S*S,math.exp(pred)))


#   f.close()
#   return predpoints

def predictionlines(fixed,prop,TL):
  ecoline=[]
  # print prop, TL
  # print fixed
  if TL=='B':
    key='Basal'
  elif TL=='I':
    key='Intermediate'
  elif TL=='T':
    key='Toppreds'
  else:
    key='Species'

  alpha=math.exp(fixed['(Intercept)'])

  try:
    delta=fixed['log10('+key+')']
  except KeyError:
    try:
      delta=fixed['log('+key+')']
    except:
      sys.exit()
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

def S_scaleplots(rawdatafile,outfile1,Bformat,predfolder):

  ecotypes=['Lake','Marine','Stream','Terrestrial','Estuary']

  grace=MultiPanelGrace(colors=ColorBrewerScheme("Greys"))
  grace.add_label_scheme("dummy",["(a)","(b)","(c)"])
  grace.set_label_scheme("dummy")

  TL='S'
  for prop in ['LS','Gen','Vul']:
    rawdata=datareader(rawdatafile,TL,Bformat)
    # if rawdatafile=='../non_TS/summary-properties.tsv':
    if rawdatafile=='../non_TS/summary-properties_trimmed.tsv':
      # fixed=fixed_reader('../non_TS/coefficients/'+prop+'_co.tsv')
      fixed=fixed_reader('../updated/non_TS/coefficients/'+prop+'_co.tsv')
    else:
      # fixed=fixed_reader('../mod_data/coefficients/'+prop+'_co.tsv')
      fixed=fixed_reader('../updated/mod_data/coefficients/'+prop+'_co.tsv')

    graph=grace.add_graph(Panel)
    for ecotype in ecotypes:
      heatpoints=heatmappoints(rawdatafile,fixed,prop,ecotype,TL,Bformat)
      datadict=heatpoints[ecotype]
      obspoints=graph.add_dataset(datadict)
      obspoints.line.configure(linestyle=0)
      obspoints.symbol.configure(size=.5,shape=1,fill_color=0,fill_pattern=1,color=8)

      predictions=predictionlines(fixed,prop,TL)
      predline=graph.add_dataset(predictions)
      predline.symbol.shape=0

      predline.line.configure(linestyle=1,color=1,linewidth=2.5)

    if TL=='S':
      graph.legend.configure(loc=(110,1),loctype='world',char_size=.75)
      graph.legend.box_linestyle=0

    if prop=='Vul':
      ytex='species richness'
      graph.xaxis.label.configure(text=ytex,place='normal',char_size=1)

    if prop=='LS':
      graph.yaxis.label.configure(text="re-scaled link density",place='normal',char_size=1)
    elif prop=='Gen':
      graph.yaxis.label.configure(text="re-scaled generality",place='normal',char_size=1)
    elif prop=='Vul':
      graph.yaxis.label.configure(text="re-scaled vulnerability",place='normal',char_size=1)

    graph.world.xmin=1
    graph.world.ymin=.1

    if prop=='LS':
      graph.world.ymax=100
    elif prop=='Gen':
      graph.world.ymax=50
    else:
      graph.world.ymax=50
    graph.xaxis.set_log()
    graph.yaxis.set_log()

    graph.world.xmax=300

    graph.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
    graph.xaxis.ticklabel.configure(char_size=.75)
    graph.frame.linewidth=1
    graph.xaxis.bar.linewidth=1
    graph.yaxis.bar.linewidth=1

    graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
    graph.yaxis.ticklabel.configure(char_size=.75)
    graph.panel_label.configure(char_size=.75,placement='iul',dy=.03,dx=.03)

  grace.multi(rows=3,cols=1,vgap=.05,hgap=.04)

  # grace.add_drawing_object(DrawText,text='Re-scaled value of:',x=0.06,y=.555,loctype='view',rot=90,char_size=1,just=2)
  # grace.set_col_yaxislabel(col=0,rowspan=(0,2),label='Re-scaled value',place='normal',just=2,char_size=1,perpendicular_offset=0.06)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  print 'changed'

  grace.write_file(outfile1)


def S_rawplots(rawdatafile,outfile1,Bformat,predfolder):

  outfile=outfile1.split('.eps')[0]+'_observed.eps'  
  # Lets make clear that these are the original, uncorrected points

  grace=MultiPanelGrace(colors=ColorBrewerScheme("Greys"))
  grace.add_label_scheme("dummy",["(a)","(b)","(c)"])
  grace.set_label_scheme("dummy")

  TL='S'

  for prop in ['LS','Gen','Vul']:
    rawdata=datareader(rawdatafile,TL,Bformat)

    if rawdatafile=='../non_TS/summary-properties_trimmed.tsv':
    # if rawdatafile=='../non_TS/summary-properties.tsv':
      # fixed=fixed_reader('../non_TS/coefficients/'+prop+'_obs.tsv')
      fixed=fixed_reader('../updated/non_TS/coefficients/'+prop+'_obs.tsv')
    else:
      # fixed=fixed_reader('../mod_data/coefficients/'+prop+'_obs.tsv')
      fixed=fixed_reader('../updated/mod_data/coefficients/'+prop+'_obs.tsv')

    graph=grace.add_graph(Panel)

    obspoints=graph.add_dataset(rawdata[prop])
    obspoints.line.configure(linestyle=0)
    obspoints.symbol.configure(size=.5,shape=1,fill_color=0,fill_pattern=1,color=8)

    predictions=predictionlines(fixed,prop,TL)
    predline=graph.add_dataset(predictions)
    predline.symbol.shape=0

    predline.line.configure(linestyle=1,color=1,linewidth=2.5)

    if prop=='Vul':
      ytex='species richness'
      graph.xaxis.label.configure(text=ytex,place='normal',char_size=1)

    if prop=='LS':
      graph.yaxis.label.configure(text="observed link density",place='normal',char_size=1)
    elif prop=='Gen':
      graph.yaxis.label.configure(text="observed generality",place='normal',char_size=1)
    elif prop=='Vul':
      graph.yaxis.label.configure(text="observed vulnerability",place='normal',char_size=1)

    graph.world.xmin=1
    graph.world.ymin=.1

    if prop=='LS':
      graph.world.ymax=100
    elif prop=='Gen':
      graph.world.ymax=50
    else:
      graph.world.ymax=50
    graph.xaxis.set_log()
    graph.yaxis.set_log()

    if TL=='S':
      graph.world.xmax=300
    else:
      graph.world.xmax=1

    graph.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
    graph.xaxis.ticklabel.configure(char_size=.75)
    graph.frame.linewidth=1
    graph.xaxis.bar.linewidth=1
    graph.yaxis.bar.linewidth=1

    graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
    graph.yaxis.ticklabel.configure(char_size=.75)
    graph.panel_label.configure(char_size=.75,placement='iul',dy=.03,dx=.03)

  grace.multi(rows=3,cols=1,vgap=.05,hgap=.04)

  # grace.set_col_yaxislabel(col=0,rowspan=(0,2),label='Observed value',place='normal',just=2,char_size=1,perpendicular_offset=0.06)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  print 'got a raw plot'

  grace.write_file(outfile)


def TL_scaleplots(rawdatafile,outfile2,Bformat,predfolder):

  ecotypes=['Lake','Marine','Stream','Terrestrial','Estuary']

  grace=MultiPanelGrace(colors=ColorBrewerScheme("Greys"))

  for prop in ['LS','Gen','Vul']:
    for TL in ['B','I','T']:
      rawdata=datareader(rawdatafile,TL,Bformat)
      fixed=fixed_reader(predfolder+'/coefficients/'+prop[0]+TL+'_co.tsv')

      graph=grace.add_graph(Panel)
      for ecotype in ecotypes:
        heatpoints=heatmappoints(rawdatafile,fixed,prop,ecotype,TL,Bformat)
        datadict=heatpoints[ecotype]
        obspoints=graph.add_dataset(datadict)
        obspoints.line.configure(linestyle=0)
        obspoints.symbol.configure(size=.5,shape=1,fill_color=0,fill_pattern=1,color=8)

        predictions=predictionlines(fixed,prop,TL)
        predline=graph.add_dataset(predictions)
        predline.symbol.shape=0

        predline.line.configure(linestyle=1,color=1,linewidth=2.5)

      if TL=='B':
        graph.legend.configure(loc=(110,1),loctype='world',char_size=.75)
        graph.legend.box_linestyle=0

      if prop=='Vul':
        if TL=='B':
          ytex='% basal resources'
        elif TL=='I':
          ytex='% intermediate consumers'
        elif TL=='T':
          ytex='% top predators'
        graph.xaxis.label.configure(text=ytex,place='normal',char_size=.75)

      if TL=='B':
        if prop=='LS':
          graph.yaxis.label.configure(text="re-scaled link density",place='normal',char_size=.75)
        elif prop=='Gen':
          graph.yaxis.label.configure(text="re-scaled generality",place='normal',char_size=.75)
        elif prop=='Vul':
          graph.yaxis.label.configure(text="re-scaled vulnerability",place='normal',char_size=.75)

      graph.world.xmin=0.005
      graph.world.ymin=.1

      if prop=='LS':
        graph.world.ymax=100
      elif prop=='Gen':
        graph.world.ymax=50
      else:
        graph.world.ymax=50
      graph.xaxis.set_log()
      graph.yaxis.set_log()

      graph.world.xmax=1

      graph.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.xaxis.ticklabel.configure(char_size=.75)
      graph.frame.linewidth=1
      graph.xaxis.bar.linewidth=1
      graph.yaxis.bar.linewidth=1


      graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.yaxis.ticklabel.configure(char_size=.75)
      graph.panel_label.configure(char_size=0)

  grace.multi(rows=3,cols=3,vgap=.04,hgap=.04)

  # grace.set_col_yaxislabel(col=0,rowspan=(0,2),label='Re-scaled value of:',place='normal',just=2,char_size=1,perpendicular_offset=0.06)
  # grace.add_drawing_object(DrawText,text='Re-scaled value of:',x=0.06,y=.555,loctype='view',rot=90,char_size=.85,just=2)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  print 'changed'

  grace.write_file(outfile2)


def TL_rawplots(rawdatafile,outfile2,Bformat,predfolder):

  outfile=outfile2.split('.eps')[0]+'_observed.eps'  
  # Lets make clear that these are the original, uncorrected points

  grace=MultiPanelGrace(colors=ColorBrewerScheme("Greys"))

  for prop in ['LS','Gen','Vul']:
    for TL in ['B','I','T']:
      rawdata=datareader(rawdatafile,TL,Bformat)

      fixed=fixed_reader(predfolder+'/coefficients/'+prop[0]+TL+'_obs.tsv')

      graph=grace.add_graph(Panel)

      obspoints=graph.add_dataset(rawdata[prop])
      obspoints.line.configure(linestyle=0)
      obspoints.symbol.configure(size=.5,shape=1,fill_color=0,fill_pattern=1,color=8)

      predictions=predictionlines(fixed,prop,TL)
      predline=graph.add_dataset(predictions)
      predline.symbol.shape=0

      predline.line.configure(linestyle=1,color=1,linewidth=2.5)

      if prop=='Vul':
        if TL=='B':
          ytex='% Basal resources'
        elif TL=='I':
          ytex='% Intermediate consumers'
        elif TL=='T':
          ytex='% Top predators'
        graph.xaxis.label.configure(text=ytex,place='normal',char_size=.75)

      if TL=='B':
        if prop=='LS':
          graph.yaxis.label.configure(text="observed link density",place='normal',char_size=.75)
        elif prop=='Gen':
          graph.yaxis.label.configure(text="observed generality",place='normal',char_size=.75)
        elif prop=='Vul':
          graph.yaxis.label.configure(text="observed vulnerability",place='normal',char_size=.75)

      graph.world.xmin=0.005
      graph.world.ymin=.1

      if prop=='LS':
        graph.world.ymax=100
      elif prop=='Gen':
        graph.world.ymax=50
      else:
        graph.world.ymax=50
      graph.xaxis.set_log()
      graph.yaxis.set_log()

      graph.world.xmax=1

      graph.xaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.xaxis.ticklabel.configure(char_size=.75)
      graph.frame.linewidth=1
      graph.xaxis.bar.linewidth=1
      graph.yaxis.bar.linewidth=1

      graph.yaxis.tick.configure(minor_ticks=1,major_size=.7,minor_size=.4,major_linewidth=1,minor_linewidth=1)
      graph.yaxis.ticklabel.configure(char_size=.75)
      graph.panel_label.configure(char_size=0)

  grace.multi(rows=3,cols=3,vgap=.04,hgap=.04)

  # grace.set_col_yaxislabel(col=0,rowspan=(0,2),label='Observed value',place='normal',just=2,char_size=1,perpendicular_offset=0.06)

  grace.hide_redundant_xticklabels()
  grace.hide_redundant_yticklabels()
  print 'got a raw plot'

  grace.write_file(outfile)


def main():

  for Bformat in ['proportions']:#['numbers','proportions']:
    # for rawdatafile in ['../non_TS/summary-properties.tsv']:#,'../mod_data/summary-properties.tsv']:
    for rawdatafile in ['../non_TS/summary-properties_trimmed.tsv','../mod_data/summary-properties_trimmed.tsv']:
      if rawdatafile=='../non_TS/summary-properties_trimmed.tsv':
        outfile1='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/S_fitlines_nonts_new.eps'
        outfile2='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/TL_fitlines_nonts_new.eps'
        # predfolder='../non_TS/'+Bformat
        predfolder='../updated/non_TS/'+Bformat
      else:
        outfile1='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/S_fitlines_ts_new.eps'
        outfile2='../manuscript/Figures/by_TL/scaling_with_S/'+Bformat+'/TL_fitlines_ts_new.eps'
        # predfolder='../mod_data/'+Bformat
        predfolder='../updated/mod_data/'+Bformat

      S_scaleplots(rawdatafile,outfile1,Bformat,predfolder)

      print 'S okay'
      S_rawplots(rawdatafile,outfile1,Bformat,predfolder)

      print 'Raw okay'
      # # Didn't use these in the final MS, didn't update the TL-specific models.
      # TL_rawplots(rawdatafile,outfile2,Bformat,predfolder)

      # TL_scaleplots(rawdatafile,outfile2,Bformat,predfolder)



 
if __name__ == '__main__':
  main()