import sys
import os
import re
import random
import pybtex
from pybtex.database.input import bibtex

used_webs=set(["Alcorlo2001","Allan1982","Amundsen2013","Angelini2005","Angelini2006","Angelini2010","Angelini2011","Angelini2013","Badcock1949","Baeta2011","Baril1983","Beaver1985","Bindloss1972","Bird1930","Boit2012","Bradstreet1982","Brodeur1992","Brose2005","Brown1975","Bulman2001","Burgis1972","Carlson1968","CattinBlandenier2004","Christian1999","Clarke1967","Closs1994","Cohen1990","Cohen2003","Collins1976","Corker1984","Cornejo-Donoso2008","Cromar1996","Cruz-Escalona2007","Cummins1966","Dexter1947","Douglass2011","Edwards1982","Fetahi2011","Filgueira2011","Fryer1959","Gaedke1994","Goldwasser1993","Gontikaki2011","Hampton2011","Harris1972","Harrison1995","Hartley1948","Havens1992","Hechinger2011","Hewatt1937","Hildrew1985","Hogetsu1979","Hopkins1993","Howes1954","Huang2008","Hurlbert1972","Huxham1996","Jones1949","Jones1950","Jonsson2005","Kelleway2010","Kemp1977","Khan2009","Kitching1987","Koslucher1973","Kremer1978","Lafferty2006","Lancraft1991","Larsson1978","Lin2006","Link2002","Liu2007","MacGinitie1935","Mann1972","Mann1972a","Marshall1982","Martinez1991","Massana1996","Mayse1978","Menge1976","Minckley1963","Minshall1967","Moriarty1973","Motta2005","Mouritsen2011","Niering1963a","Nixon1973","Odum1975","Opitz1996","Paine1980a","Parker2006","Patricio2006","Patten1975","Patten1979","Pattie1966","Paviour-Smith1956","Pechlaner1972","Peterson1979","Polis1991a","Preston2012","Rasmussen1941","Ratsirarson1996","Rayner2010","Richards1926","Rosenthal1974","Ruzicka2012","Savely1939","Schiemer1979","Schneider1997","Seifert1979","Shushkina1979","Simberloff1976","Smith2007","Snow1958","Sorokin1972","Stagliano2002","Stewart2011","Summerhayes1923","Summerhayes1928","Teal1962","Thieltges2011","Thompson2003","Thompson2004","Thompson2004c","Tilly1968","Townsend1998","Tsuda1972","Twomey1945","VanEs1977","Warren1989","Wilbur1972","Woodward2005","Woodward2008","Woodwell1967","Yanez1978","Zander2011","Zaret1973","Zetina-Rejon2003"])

def duplicate_surnames(sur,auth):
	if auth=='Brown, Jerry':
		surname='Brown_a'
	elif auth=='Chen, Yong':
		surname='Chen_Y'
	elif sur=='Clarke':
		if auth=='Clarke, T. A.':
			surname='Clarke_T'
		else:
			surname='Clarke_W'
	elif sur=='Edwards':
		if auth=='Edwards, D. Craig':
			surname='Edwards_D'
		elif auth=='Edwards, Francois':
			surname='Edwards_F'
		else:
			surname='Edwards_E'
	elif sur=='Li':
		if auth=='Li, Jia Le':
			surname='Li_J'
		else:
			surname='Li_X'
	elif sur=='Moriarty':
		if auth=='Moriarty, C. M.':
			surname='Moriarty_C'
		else:
			surname='Moriarty_D'
	elif sur=='Odum':
		if auth=='Odum, H. T.':
			surname='Odum_H'
		else:
			surname='Odum_W'
	elif sur=='Seifert':
		if auth=='Seifert, Florence Hammett':
			surname='Seifert_F'
		else:
			surname='Seifert_R'
	elif sur=='Smith':
		if auth=='Smith, I. R.':
			surname='Smith_I'
		elif auth=='Smith, W. H. B.':
			surname='Smith_WH'
		else:
			surname='Smith_WO'
	elif sur=='Williams':
		if auth=='Williams, A.':
			surname='Williams_A'
		elif auth=='Williams, D. D.':
			surname='Williams_D'
		else:
			surname='Williams_R'
	elif sur=='Wilson':
		if auth=='Wilson, G. Glenn':
			surname='Wilson_G'
		else:
			surname='Wilson_H'
			# print auth

	# Surprisingly, Joseph J. Torres and Jose J. Torres seem to be the same person.
	else:
		surname=sur

	return surname

def bibreader(bibfile,used_webs):

	parser=bibtex.Parser()
	bibdata=parser.parse_file(bibfile)

	authors_by_key={}
	authorset=set([])
	authors_with_initials=set([])

	for bib_id in bibdata.entries:
		if bib_id in used_webs:
			b = bibdata.entries[bib_id].fields

			try:
				auths=bib_id,b["author"]
			except KeyError:
				auths=bib_id,b["editor"] # Just one web which comes from an undefined chapter in a book. Using the editors as authors.

			if 'Dunne' in auths:
				print auths

			authors=auths[1].split(' and ')
			# print sorted(authors)
			surnames=[]
			for auth in authors:
				sur=auth.split(',')[0]

				# Now to deal with a few duplicate surnames
				surname=duplicate_surnames(sur,auth)

				surnames.append(surname)

				authorset.add(surname)
				authors_with_initials.add(auth)

			authors_by_key[bib_id]=set(surnames)

	return authorset,authors_by_key,authors_with_initials

def common_author_matcher(bibfile,used_webs):
	
	authorset,authors_by_key,authors_with_initials = bibreader(bibfile,used_webs)

	commonauths={}

	for key1 in authors_by_key:
		commonauths[key1]=set([])
		authors1=authors_by_key[key1]

		for key2 in authors_by_key:
			if key2!=key1:
				authors2=authors_by_key[key2]

				if authors1&authors2!=set([]):
					commonauths[key1].add(key2)

	return commonauths

def web_key_dict(webkeys):
	web_to_key={}
	f=open(webkeys)
	for line in f:
		newline=line.split('\n')[0].split('*')
		web_to_key[newline[0]]=newline[1]

	return web_to_key


def main():

	bibfile = '../manuscript/noISN.bib'  
	webkeys = '../manuscript/webs_and_keys.csv'

	bibreader(bibfile,used_webs)
	# common_author_matcher(bibfile,used_webs)
	web_key_dict(webkeys)



if __name__ == '__main__':
  main()
