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

model = model_utils.MOB(data_path, full_data_path,id_key='ID', ver_string='default')


def run_mob(request):
	seg = request.GET.getlist('seg')
	reg = request.GET.getlist('reg')
	target = request.GET.getlist('target')
	depth = request.GET.get('depth')
	min_size = request.GET.get('min_size')
	trn_split = request.GET.get('trn_split')
	exp_graph_var = request.GET.getlist('graph_variable')
	exp_color_var = request.GET.getlist('color_variable')
	context = RequestContext(request)
	template_input = {"variables":variables}

	# EDA Plots
	# plotly_viz.correlation_plot(data)


	# Exp Vs Eff plots
	print ('----------------------- Debugging -----------------------------------')
	print (exp_graph_var, exp_color_var)
	if exp_graph_var != [] and exp_color_var != []:
		model.seg_lvl_data['Size'] = pd.cut(model.seg_lvl_data['target'], 4, labels=[28,31,34,37])
		plot_html, plotdivid, width, height = (plotly_viz.exp_vs_eff
			(model.seg_lvl_data, str('beta_'+str(exp_graph_var[0])), str(exp_graph_var[0]),
			 "nodes", str(exp_color_var[0]), 'Size'))
		template_input = {"variables":variables, "seg":model.part, "reg":model.reg, "target":model.target, 
					"depth":model.depth, "min_size":model.min_size, "trn_split":model.trn_split,
					"exp_plot":plot_html, 'active_tab':'exp_vs_eff', 
					"exp_graph_var":exp_graph_var, "exp_color_var": exp_color_var}
		#code
		#adding overall impact table
		template_input["overall_impact"] = model.overall_impact.to_html( justify='center', classes=
			"table table-hover", header=False, index=False)
		print (template_input)
		return render(request, "launch/index.html", template_input)



	# ver_string = request.GET.get('ver_string')
	# Setting Default Variables (Specific to bank market)
	if seg == []:
		seg = ['job', 'marital', 'education', 'default', 'housing', 'loan', 'contact', 'month', 'poutcome', 'day','age']
	if reg == []:
		reg =['balance', 'campaign', 'pdays', 'previous','calls', 'emails', 'coupons']
	if target == []:
		target = ['target']

	if depth and min_size and trn_split !=None:
		model.depth = int(depth)
		model.min_size = int(min_size)
		model.trn_split = float(trn_split)
		model.show_formula()
		print ('***************** Fittting Model ****************')
		#model.mob_fit()
		print ('***************** Creating Summary *****************')
		model.create_summary()
		template_input = {"variables":variables, "seg":model.part, "reg":model.reg, "target":model.target, 
					"depth":model.depth, "min_size":model.min_size, "trn_split":model.trn_split, 
					'active_tab':'summary'}
		#code
		#adding overall impact table
		template_input["overall_impact"] = model.overall_impact.to_html( justify='center', classes=
			"table table-hover", header=False, index=False)
		
		print ('***************** temp input *****************')
		#print (template_input)
		#print (target, depth, min_size, trn_split)
	return render(request, "launch/index.html", template_input)

# Not in use
def index(request):
	return render(request, "launch/index.html")