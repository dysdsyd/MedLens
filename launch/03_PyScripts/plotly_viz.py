import plotly
from plotly import __version__
from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot
import plotly.graph_objs as go
from plotly import tools
from plotly.offline.offline import _plot_html
# import seaborn as sns
# import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

	


'''
Multi-variate Graph with target variable
quater_wise_trace : helper function for creating trace
'''
def quater_wise_trace(data, var, target = 'XTAMPZA_ER_Q1_TRx'):
    dist = data.groupby(var)[target].sum().reset_index()
    trace = go.Bar(x=dist[var],y=dist[target],width = .8, marker=dict(color='rgb(158,202,225)',line=dict(color='rgb(8,48,107)',width=1.5,)),
    opacity=0.9)
    return trace

def multi_variate_bar(data, var, quater='all'):
	x_template = dict(title=var,showticklabels=True,showgrid=True,showline=True,zeroline=True,zerolinewidth=1, #,tickangle=0
	                  titlefont=dict(
	                      #family='Arial, sans-serif',
	                      size=14,					
	                      #color='black',
	                  ))
	y_template = dict(title='XTAMPZA TRx',showticklabels=True,showgrid=True,showline=True,zeroline=True,zerolinewidth=1,
	                  titlefont=dict(
	                      #family='Arial, sans-serif',
	                      size=14,
	                      #color='black',
	                  ))
	layout = go.Layout(width=600, height=600, 
	                   xaxis=x_template,
	                   yaxis=y_template)

	if quater == 'all':
		fig=tools.make_subplots(rows=2,cols=2, subplot_titles=('Quater 4','Quater 3','Quater 2','Quater 1'))
		fig.append_trace(quater_wise_trace(data, var, target='XTAMPZA_ER_Q4_TRx'),1,1)
		fig.append_trace(quater_wise_trace(data, var, target='XTAMPZA_ER_Q3_TRx'),1,2)
		fig.append_trace(quater_wise_trace(data, var, target='XTAMPZA_ER_Q2_TRx'),2,1)
		fig.append_trace(quater_wise_trace(data, var, target='XTAMPZA_ER_Q1_TRx'),2,2)

		fig['layout']['xaxis1'].update(x_template)
		fig['layout']['xaxis2'].update(x_template)
		fig['layout']['xaxis3'].update(x_template)
		fig['layout']['xaxis4'].update(x_template)

		fig['layout']['yaxis1'].update(y_template)
		fig['layout']['yaxis2'].update(y_template)
		fig['layout']['yaxis3'].update(y_template)
		fig['layout']['yaxis4'].update(y_template)

		fig['layout'].update(title=var+'-wise TRx distrbution over all Quarter',titlefont=dict(color='black'),width=800, height=800, showlegend=False)
		for i in range(4):
		    fig['layout']['annotations'][i]['font'].update(dict( size=16))
		#fig = go.Figure(data=data, layout=layout)
		iplot(fig, filename='multi_variate_bar')
	
	else:
		fig=tools.make_subplots(rows=1,cols=1)
		fig.append_trace(quater_wise_trace(data, var, target='XTAMPZA_ER_Q4_TRx'),1,1)
	
		fig['layout']['xaxis1'].update(x_template)
		
		fig['layout']['yaxis1'].update(y_template)
		
		fig['layout'].update(title=var+'-wise TRx distrbution over all Quarter',titlefont=dict(color='black'),width=600, height=600, showlegend=False)
		#fig = go.Figure(data=data, layout=layout)
		iplot(fig, filename='multi_variate_bar')
		

def gen_multi_variate_bar(data, X_var, Y_var, title):
	x_template = dict(title=X_var,showticklabels=True,showgrid=True,showline=True,zeroline=True,zerolinewidth=1, #,tickangle=0
                  titlefont=dict(
                      #family='Arial, sans-serif',
                      size=14,
                      #color='black',
                  ))
	y_template = dict(title=Y_var,showticklabels=True,showgrid=True,showline=True,zeroline=True,zerolinewidth=1,
	                  titlefont=dict(
	                      #family='Arial, sans-serif',
	                      size=14,
	                      #color='black',
	                  ))
	layout = go.Layout(width=600, height=600, 
	                   xaxis=x_template,
	                   yaxis=y_template,
	                   paper_bgcolor='rgba(0,0,0,0)',
	                   plot_bgcolor='rgba(0,0,0,0)')

	fig=tools.make_subplots(rows=1,cols=1)
	trace = go.Bar(x=data[X_var],y=data[Y_var],width = .8, marker=dict(color='rgb(158,202,225)',line=dict(color='rgb(8,48,107)',width=1.5,)),
	    opacity=0.9)
	fig.append_trace(trace,1,1)
	fig['layout']['xaxis1'].update(x_template)
	fig['layout']['yaxis1'].update(y_template)

	fig['layout'].update(title=title,
	                     titlefont=dict(color='black'),width=600, height=600, showlegend=False,
	                     paper_bgcolor='rgba(0,0,0,0)',
	                     plot_bgcolor='rgba(0,0,0,0)')
	#fig = go.Figure(data=data, layout=layout)
	#iplot(fig, filename='multi_variate_bar')
	return (_plot_html(fig, False, "", True, '100%', 525))





'''
General plot for multi-variate sctter plot
pivot : dataset
X_var : X variable
Y_var : Y variable
title : title for the plot
'''
# Todo : Too slow currently
def gen_multi_variate_scatter (pivot, X_var, Y_var, title):
	x_template = dict(title=X_var,showticklabels=True,showgrid=True,showline=True,zeroline=False,zerolinewidth=1, #,tickangle=0
	                  titlefont=dict(
	                      #family='Arial, sans-serif',
	                      size=14,
	                      #color='black',
	                  ))
	y_template = dict(title=Y_var,showticklabels=True,showgrid=True,showline=True,zeroline=False,zerolinewidth=1,
	                  titlefont=dict(
	                      #family='Arial, sans-serif',
	                      size=14,
	                      #color='black',
	                  ))
	layout = go.Layout(width=600, height=600, 
	                   xaxis=x_template,
	                   yaxis=y_template,
	                   title=title,
	                   titlefont=dict(color='black')
	                   )

	trace1 = go.Scatter(
	    y = pivot[Y_var],
	    x = pivot[X_var],
	    mode='markers',
	    #text = (pivot[pivot_on]).astype(str),#'Node = '+(pivot['Node']).astype(str),
	    textfont = dict(color ='white'),
	    marker=dict(
	        size=10,#pivot['Mean.y'],
	        #color = pivot[color_on], #set color equal to a variable
	        color='rgb(158,202,225)',
	        line=dict(color='rgb(8,48,107)', width=2),
	        showscale=False,
	        opacity = .7
	    )
	)

	data = [trace1]
	fig = dict(data=data, layout=layout)
	iplot(fig, filename='scatter-plot-with-colorscale')


'''
Exposure Vs Effectivness graph
pivot : dataset
X_var : X variable
Y_var : Y variable
pivot_on : Variable to pivot on, it may be node or othe categorical variable
color_on : color of scatter bubbles(on a variable) 
'''
def exp_vs_eff (pivot, X_var, Y_var, pivot_on, color_on, size):
	x_template = dict(title=X_var,showticklabels=True,showgrid=True,showline=True,zeroline=False,zerolinewidth=1, #,tickangle=0
	                  titlefont=dict(
	                      #family='Arial, sans-serif',
	                      size=14,
	                      #color='black',
	                  ))
	y_template = dict(title=Y_var,showticklabels=True,showgrid=True,showline=True,zeroline=False,zerolinewidth=1,
	                  titlefont=dict(
	                      #family='Arial, sans-serif',
	                      size=14,
	                      #color='black',
	                  ))
	layout = go.Layout(width=600, height=600, 
	                   xaxis=x_template,
	                   yaxis=y_template,
	                   #title='Effectiveness VS Exposure',
	                   titlefont=dict(color='black'),
	                   paper_bgcolor='rgba(0,0,0,0)',
	                   plot_bgcolor='rgba(0,0,0,0)'
	                   )

	trace1 = go.Scatter(
	    y = pivot[Y_var],
	    x = pivot[X_var],
	    mode='markers+text',
	    text = (pivot[pivot_on]).astype(str),#'Node = '+(pivot['Node']).astype(str),
	    textfont = dict(color ='white'),
	    marker=dict(
	        size=pivot[size],
	        color = pivot[color_on], #set color equal to a variable
	        colorscale='Viridis',
	        showscale=True,
	        opacity = .7
	    )
	)

	data = [trace1]
	fig = dict(data=data, layout=layout)
	#iplot(fig)
	return (_plot_html(fig, False, "", True, '100%', 525))


def gen_multi_variate_lines(data, X_var, Y_var, pivot_on,title):
    x_template = dict(title=X_var,showticklabels=True,showgrid=True,showline=True,zeroline=False, #,tickangle=0
                  titlefont=dict(
                      #family='Arial, sans-serif',
                      size=14,
                      #color='black',
                  ))
    y_template = dict(title=Y_var,showticklabels=True,showgrid=True,showline=True,zeroline=True,zerolinewidth=1,
                      titlefont=dict(
                          #family='Arial, sans-serif',
                          size=14,
                          #color='black',
                      ))
    layout = go.Layout(width=800, height=600, 
                       xaxis=x_template,
                       yaxis=y_template,
                       title=title,
                       paper_bgcolor='rgba(0,0,0,0)',
	                   plot_bgcolor='rgba(0,0,0,0)')
    trace = []
    segments = data.reset_index()[pivot_on].unique()
    for segment in segments:
        temp = data[segment].reset_index()
        temp[X_var] = temp[X_var].astype(str)
        trace.append(go.Scatter(x=temp[X_var],y=temp[Y_var], mode='lines+markers', name=str(segment)))
    fig = dict(data=trace, layout=layout)
    #iplot(fig)
    return (_plot_html(fig, False, "", True, '100%', 525))


# def joint_plot(X, Y, u_perc_x, u_perc_y, train_df):
# 	ulimit = np.percentile(train_df[Y].values, u_perc_y)
# 	print ('------------------------------------------------------------------------------------------')
# 	Y_IDS = (np.unique(train_df['IMS_ID'].loc[train_df[Y]>ulimit]))
# 	print ('Unique IDs Dropped for '+Y+' = ',len(Y_IDS))
# 	print ('------------------------------------------------------------------------------------------')
# 	#llimit = np.percentile(train_df[Y].values, 5)
# 	train_df[Y].loc[train_df[Y]>ulimit] = ulimit
# 	#train_df[Y].ix[train_df[Y]<llimit] = llimit

# 	ulimit = np.percentile(train_df[X].values, u_perc_x)
# 	print ('------------------------------------------------------------------------------------------')
# 	X_IDS = np.unique(train_df['IMS_ID'].loc[train_df[X]>ulimit])
# 	print ('Unique IDs Dropped for '+X+' = ',len(X_IDS))
# 	print ('------------------------------------------------------------------------------------------')
# 	#llimit = np.percentile(train_df[X].values, 5)
# 	train_df[X].loc[train_df[X]>ulimit] = ulimit
# 	#train_df[X].ix[train_df[X]<llimit] = llimit

# 	plt.figure(figsize=(8,8))
# 	sns.jointplot(x=train_df[X].values, y=train_df[Y].values, size=10, color='g')
# 	plt.ylabel(Y, fontsize=12)
# 	plt.xlabel(X, fontsize=12)
# 	plt.title(Y+" VS "+X, fontsize=15)
# 	plt.show()
# 	return X_IDS, 


# def correlation_plot(data):
# 	numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
# 	temp = data.select_dtypes(include=numerics)
# 	corrmat = temp.corr()
# 	f, ax = plt.subplots(figsize=(80, 80))
# 	# Draw the heatmap using seaborn
# 	mask = np.zeros_like(corrmat, dtype=np.bool)
# 	mask[np.triu_indices_from(mask)] = True 
# 	sns.heatmap(corrmat, mask=mask, vmax=1., square=True, cmap="YlGnBu", annot=True)
# 	plt.title("Correlation Map", fontsize=40)
# 	plt.savefig("launch/static/media/correlation.jpg", transparent=True)