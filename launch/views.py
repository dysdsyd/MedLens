from django.shortcuts import render
from django.template import RequestContext
from django.conf import settings
from django.core.files.storage import FileSystemStorage
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
template_input = {"variables":variables}

import model_utils, plotly_viz
model = model_utils.MOB(data_path, full_data_path,id_key='ID', ver_string='default')
model.create_summary()
template_input["seg"] = model.part
template_input["reg"] = model.reg
template_input["target"] = model.target
template_input["depth"] = "4"
template_input["min_size"] = "100"
template_input["trn_split"] = "0.8"

def DQ_CHECK(data):
	# DQ Check
	dscrt = []
	cont = []
	for col in data.columns:
		if len(data[col].describe().reset_index()) ==4:
			dscrt.append((col,data[col].describe().reset_index().to_html(justify='center', classes="table table-hover", header=False, index=False)))
		else:
			cont.append((col,data[col].describe().reset_index().to_html(justify='center', classes="table table-hover", header=False, index=False)))
	return [dscrt, cont]

def run_mob(request):

	template_input['files'] = os.listdir("launch/01_Data")
	global data

	# Input from templates
	file = request.GET.getlist('file')
	seg = request.GET.getlist('seg')
	reg = request.GET.getlist('reg')
	target = request.GET.getlist('target')
	depth = request.GET.get('depth')
	min_size = request.GET.get('min_size')
	trn_split = request.GET.get('trn_split')
	exp_graph_var = request.GET.getlist('graph_variable')
	exp_color_var = request.GET.getlist('color_variable')
	line_X = request.GET.getlist('line_X')
	line_Y = request.GET.getlist('line_Y')
	line_pivot = request.GET.getlist('line_pivot')

	bar_X = request.GET.getlist('bar_X')
	bar_Y = request.GET.getlist('bar_Y')
	context = RequestContext(request)

	# Create DQ Check as it gets file name
	template_input["dq_check"] = DQ_CHECK(data)

	print (file, seg, reg, target, depth, min_size, trn_split, exp_graph_var, exp_color_var,
		line_X, line_Y, line_pivot, bar_X, bar_Y)


	# Choose Data
	if file != []:
		data_path = "launch/01_Data/"+str(file[0])
		data = pd.read_csv(data_path)
		template_input["variables"] = data.columns.values
		template_input["dq_check"] = DQ_CHECK(data)
		template_input["choosen_file"] = file
		model.data_path = data_path
		model.full_data_path = data_path
		# EDA Plots
		plotly_viz.correlation_plot(data)
		# Setting Default Variables (Specific to bank market)
		if file[0] == "bank_mkt.csv":
			if seg == []:
				model.part = ['job', 'marital', 'loan', 'month', 'age']
			if reg == []:
				model.reg =['calls', 'emails', 'coupons']
			if target == []:
				model.target = ['target']
		template_input["seg"] = model.part
		template_input["reg"] = model.reg
		template_input["target"] = model.target
		template_input["active_tab"] = "dq_check"
		return render(request, "launch/index.html", template_input)

	# Universe Setting
	if seg != [] and reg != [] and target != []:
		model.part = seg
		model.reg = reg
		model.target = target
		template_input["variables"]=data.columns.values
		template_input["seg"] = model.part
		template_input["reg"] = model.reg
		template_input["target"] = model.target
		template_input["active_tab"] = "line_plot"
		return render(request, "launch/index.html", template_input)



	# Data Upload
	if request.method == 'POST' and request.FILES['myfile']:
		myfile = request.FILES['myfile']
		fs = FileSystemStorage()
		print (myfile.name)
		filename = fs.save('launch/01_Data/'+ str(myfile.name), myfile)
		uploaded_file_url = fs.url(filename)
		template_input["uploaded_file_url"] = uploaded_file_url
		template_input['files'] = os.listdir("launch/01_Data")
		template_input["active_tab"] = "data_upload"
		return render(request, "launch/index.html", template_input)
	


	#Line Plots
	if line_X !=[] and line_Y != [] and line_pivot!=[]:
		df = data
		if df[line_X[0]].nunique() <=4 or df[line_X[0]].dtype != np.number:
			df[line_X[0]+'_bins'] = df[line_X[0]]
		else:
			df[line_X[0]+'_bins'] = pd.cut(df[line_X[0]], 5)
		temp = df.groupby([line_pivot[0], line_X[0]+'_bins'])[model.target[0]].mean()
		#temp[line_X[0]+'_bins'] = temp[line_X[0]+'_bins'].apply(lambda x : str(x))
		# ToDo : pivotted on nodes only
		plot_html, plotdivid, width, height = plotly_viz.gen_multi_variate_lines(temp, 
			line_X[0]+'_bins', line_Y[0], pivot_on=line_pivot[0], title='')  
		template_input["line_plot"] = plot_html
		template_input["line_X"] = line_X
		template_input["line_Y"] = line_Y
		template_input["line_pivot"] = line_pivot
		template_input["active_tab"] = "line_plot"
		#print (template_input)
		return render(request, "launch/index.html", template_input)

	#Bar Plots
	if bar_X !=[] and bar_Y != []:
		df = data
		if df[bar_X[0]].nunique() <=4 or df[bar_X[0]].dtype != np.number:
			df[bar_X[0]+'_bins'] = df[bar_X[0]]
		else:
			df[bar_X[0]+'_bins'] = pd.cut(df[bar_X[0]], 5)
		temp = df.groupby(bar_X[0]+'_bins')[model.target[0]].mean().reset_index()
		temp[bar_X[0]+'_bins'] = temp[bar_X[0]+'_bins'].apply(lambda x : str(x))
		# ToDo : pivotted on nodes only
		plot_html, plotdivid, width, height = plotly_viz.gen_multi_variate_bar(temp, 
			bar_X[0]+'_bins', bar_Y[0], title='')  
		template_input["bar_plot"] = plot_html
		template_input["bar_X"] = bar_X
		template_input["bar_Y"] = bar_Y
		template_input["active_tab"] = "bar_plot"
		#print (template_input)
		return render(request, "launch/index.html", template_input)

	# Exp Vs Eff plots
	print ('----------------------- Debugging -----------------------------------')
	print (exp_graph_var, exp_color_var)
	if exp_graph_var != [] and exp_color_var != []:
		model.create_summary()
		model.seg_lvl_data['Size'] = pd.cut(model.seg_lvl_data[model.target[0]], 4, labels=[28,31,34,37])
		plot_html, plotdivid, width, height = (plotly_viz.exp_vs_eff
			(model.seg_lvl_data, str('beta_'+str(exp_graph_var[0])), str(exp_graph_var[0]),
			 "nodes", str(exp_color_var[0]), 'Size'))
		template_input["exp_plot"] = plot_html
		template_input["active_tab"] = "exp_vs_eff"
		template_input["exp_graph_var"] = exp_graph_var
		template_input["exp_color_var"] = exp_color_var
		#print (template_input)
		return render(request, "launch/index.html", template_input)

	# ver_string = request.GET.get('ver_string')


	# Run MOB	
	if depth and min_size and trn_split !=None:
		model.depth = int(depth)
		model.min_size = int(min_size)
		model.trn_split = float(trn_split)
		print ("Depth = " ,model.depth)
		model.show_formula()
		print ('***************** Fittting Model ****************')
		model.id_key = "ID"
		model.mob_fit()
		print ('***************** Creating Summary *****************')
		model.create_summary()

		template_input["depth"] = str(model.depth)
		template_input["min_size"] = str(model.min_size)
		template_input["trn_split"] = str(model.trn_split)
		template_input["active_tab"] = "summary"
		#code
		#adding overall impact table
		template_input["overall_impact"] = model.overall_impact.to_html( justify='center', classes=
			"table table-hover", header=False, index=False)
		
		#print (template_input)
		#print (target, depth, min_size, trn_split)
	return render(request, "launch/index.html", template_input)

# Not in use
def index(request):
	return render(request, "launch/index.html")