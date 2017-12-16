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
from keywordsExtr import helper, pmcid_to_crawl
import json
# import pandas as pd
# import numpy as np 

template_input = {}


def run_query(request):
	# Data Upload
	upload = request.GET.get('Upload Your Data')
	explore = request.GET.get('Explore Your Data')
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

	template_input['question_a'] = "Ask Question"
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
	question_b = request.GET.get('question_b')
	template_input['research_area'] = research_area
	template_input['give_answer_b'] = False 



	if research_area != None:
		keywords, size ,pmcid = helper(str(template_input['research_area']))
		print(pmcid)

		if question_b!= None:
			template_input['abstract'], template_input['passage_b'], template_input['article_title'] = pmcid_to_crawl(pmcid[0])
			print(str(question_b))
			template_input['give_answer_b'] = True
			template_input['question_b'] = str(question_b)
			template_input['answer_b'] = get_answer(template_input["passage_b"].decode('utf-8'), template_input["question_b"])
			return render(request, "quest/explore_b2.html", template_input)

		return render(request, 'quest/explore_b2.html',template_input)

	#return render(request, 'quest/connect.html',{'visual_list':visual_list})
	return render(request, "quest/explore_b1.html", template_input) 

# Not in use
def index(request):
	return render(request, "quest/index.html") 
# Create your views here.
