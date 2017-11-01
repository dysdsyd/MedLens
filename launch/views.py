from django.shortcuts import render
from django.template import RequestContext

# Create your views here.
import warnings, os
import sys
import pandas as pd
import numpy as np 

sys.path.append('launch/03_PyScripts/')
data_path = 'launch/01_Data/bank_mkt.csv'
full_data_path = 'launch/01_Data/bank_mkt.csv'
# all the variables for bank mkt
# To Do : File upload
data = pd.read_csv(data_path)
variables = data.columns.values

import model_utils, plotly_viz

def run_mob(request):
	seg = request.GET.getlist('seg')
	reg = request.GET.getlist('reg')
	target = request.GET.getlist('target')
	depth = request.GET.get('depth')
	min_size = request.GET.get('min_size')
	trn_split = request.GET.get('trn_split')
	context = RequestContext(request)

	# ver_string = request.GET.get('ver_string')
	template_input = {"variables":variables}
	# Setting Default Variables (Specific to bank market)
	if seg == []:
		seg = ['job', 'marital', 'education', 'default', 'housing', 'loan', 'contact', 'month', 'poutcome', 'day','age']
	if reg == []:
		reg =['balance', 'age', 'campaign', 'pdays', 'previous','calls', 'emails', 'coupons']
	if target == []:
		target = ['target']

	if depth and min_size and trn_split !=None:
		model = model_utils.MOB(data_path, full_data_path,id_key='ID', depth=int(depth), min_size=int(min_size), trn_split=float(trn_split), 
			ver_string='default')
		model.show_formula()
		print (model.depth, model.min_size, model.trn_split, model.ver_string)
		print ('***************** Fittting Model ****************')
		#model.mob_fit()
		print ('***************** Creating Summary *****************')
		model.create_summary()
		template_input = {"variables":variables, "seg":model.part, "reg":model.reg, "target":model.target, 
					"depth":model.depth, "min_size":model.min_size, "trn_split":model.trn_split}
		#code
		model.seg_lvl_data['Size'] = pd.cut(model.seg_lvl_data['target'], 4, labels=[28,31,34,37])
		exp_plots = []
		for X_var, Y_var in zip(model.beta,model.var_mean):
			plot_html, plotdivid, width, height = (plotly_viz.exp_vs_eff(model.seg_lvl_data, X_var, Y_var, "nodes", "age", 'Size'))
			exp_plots.append((Y_var,plot_html))
		# adding plots to the dictionary
		template_input["exp_plots"] = exp_plots
		#adding overall impact table
		template_input["overall_impact"] = model.overall_impact.to_html( justify='center', classes=
			"table table-hover", header=False, index=False)
		
		print ('***************** temp input *****************')
		# print (template_input)
		print (target, depth, min_size, trn_split)
	return render(request, "launch/index.html", template_input)

# Not in use
def index(request):
	return render(request, "launch/index.html")