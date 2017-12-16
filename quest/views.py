# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.shortcuts import render
from django.template import RequestContext
from django.conf import settings
from django.core.files.storage import FileSystemStorage
import warnings, os
import sys
sys.path.append('quest/src')
#from pptToTopic import pptToTopic
from textGenerator import file2Text
from q_n_a_function import get_answer
from keywordsExtr import helper, pmcid_to_crawl, pmcid_to_display
import json
# import pandas as pd
# import numpy as np 

template_input = {}


def run_query(request):
	# Data Upload
	upload = request.GET.get('Upload Your Data')
	explore = request.GET.get('Explore Libraries')
	question_a = request.GET.get('question_a')
	# print (upload, explore)
	template_input['give_answer_a'] = False 

	# A2
	if question_a != None:
		print(str(question_a))
		template_input['give_answer_a'] = True
		template_input['question_a'] = str(question_a)
		template_input['answer_a'] = get_answer(template_input["passage_a"], template_input["question_a"])
		return render(request, "quest/upload_analysis.html", template_input)

	template_input['question_a'] = "What do you want to know!!"
	template_input['pmcid_to_table'] = None
	# A1
	if request.method == 'POST' and request.FILES['myfile']:
		myfile = request.FILES['myfile']
		fs = FileSystemStorage()
		print (myfile.name)
		filename = fs.save('quest/data/'+ str(myfile.name), myfile)
		uploaded_file_url = fs.url(filename)
		template_input["uploaded_file_url"] = uploaded_file_url
		template_input['files'] = os.listdir("quest/data")
		template_input["active_tab"] = "data_upload"
		template_input["passage_a"] = (file2Text(template_input["uploaded_file_url"])).decode('utf-8')
		print (uploaded_file_url)
		return render(request, "quest/upload_analysis.html", template_input)

	# B1
	if explore != None:
		return render(request, "quest/explore_b1.html", template_input)

	template_input["active_tab"] = "data_upload"
	return render(request, "quest/index.html", template_input)	

def explore(request):
	
	research_area = request.GET.get("research")
	print(research_area)
	question_b = request.GET.get('question_b')
	template_input['research_area'] = research_area
	template_input['give_answer_b'] = False 
	
	if research_area != None:
		template_input['give_answer_b'] = False
		keywords, key_to_pmcid = helper(str(template_input['research_area']))
		print(keywords)
		if len(keywords)>12:
			template_input['keywords'] = keywords[:12]
		else:
			template_input['keywords'] = keywords
		
		print(key_to_pmcid)
		template_input['key_to_pmcid'] = key_to_pmcid
		
		#template_input['abstract'], template_input['passage_b'], template_input['article_title'] = pmcid_to_crawl(pmcid[0])
		return render(request, 'quest/explore_b3.html',template_input)

	if question_b!= None:
		template_input['give_answer_b'] = True
		template_input['question_b'] = str(question_b)
		print (template_input['question_b'], template_input['passage_b'])
		template_input['answer_b'] = get_answer(template_input["passage_b"], template_input["question_b"])
		return render(request, "quest/explore_b2.html", template_input)

	#return render(request, 'quest/connect.html',{'visual_list':visual_list})
	return render(request, "quest/explore_b1.html", template_input) 

def explore_article(request):

	key = request.GET.get("key")
	key_pmcid = {}
	
	for k,v in template_input['key_to_pmcid'].items():
		key_pmcid[k.decode('utf-8')] = v
		
	pmcid_to_table = []
	pmcid_title_to_table = []

	#print(key_pmcid)

	for ids in key_pmcid[key]:
		pmcid_to_table.append(ids)
		pmcid_title_to_table.append(pmcid_to_display(ids))
		#template_input['abstract'], template_input['passage_b'], template_input['article_title'] = pmcid_to_crawl(pmcid[0])
	
	template_input['pmcid_to_table'] = pmcid_to_table
	template_input['pmcid_title_to_table'] = pmcid_title_to_table
  
	return render(request, "quest/explore_b3.html", template_input)

def explore_question(request):

	to_crawl = request.GET.get("PMCID")
	template_input['abstract'], template_input['passage_b'], template_input['article_title'] = pmcid_to_crawl(to_crawl)
	return render(request, "quest/explore_b2.html", template_input)



# Not in use
def index(request):
	return render(request, "quest/index.html") 
# Create your views here.
