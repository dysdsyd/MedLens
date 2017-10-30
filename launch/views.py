from django.shortcuts import render
from django.template import RequestContext

# Create your views here.
import warnings, os
import numpy as np
import pandas as pd
import sys

sys.path.append('launch/03_PyScripts/')
data_path = 'launch/01_Data/bank_mkt.csv'
full_data_path = 'launch/01_Data/bank_mkt.csv'

import model_utils

def run_mob(request):
	depth = request.GET.get('depth')
	min_size = request.GET.get('min_size')
	trn_split = request.GET.get('trn_split')
	ver_string = request.GET.get('ver_string')
	context = RequestContext(request)
	print (type(int(depth)), type(int(min_size)), type(float(trn_split)), type(ver_string))
	print ((depth), (min_size), (trn_split), (ver_string))
	# model = model_utils.MOB(data_path, full_data_path,id_key='ID', depth=int(depth), min_size=int(min_size), trn_split=float(trn_split), 
	# 	ver_string=ver_string)
	# model.show_formula()
	# print (model.depth, model.min_size, model.trn_split, model.ver_string)
	#model.mob_fit()
	return render(request, "launch/home.html")

def index(request):
	return render(request, "launch/index.html")