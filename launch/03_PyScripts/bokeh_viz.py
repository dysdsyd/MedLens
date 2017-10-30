from __future__ import division,print_function
from bokeh.plotting import figure, show, output_file
from bokeh.io import output_notebook, show, gridplot, push_notebook
from bokeh.models import ColumnDataSource, Range1d, LabelSet, Label, FixedTicker, ColorBar, LinearColorMapper, Plot, LinearAxis, TickFormatter
from bokeh.core.properties import Dict, Int, String
from bokeh.charts import Bar, output_file, show
import bokeh
from ipywidgets import interact
from bokeh.models import HoverTool
import numpy as np
import math

class FixedTickFormatter(TickFormatter):
    """
    Class used to allow custom axis tick labels on a bokeh chart
    Extends bokeh.model.formatters.TickFormatte
    """

    JS_CODE =  """
        import {Model} from "model"
        import * as p from "core/properties"

        export class FixedTickFormatter extends Model
          type: 'FixedTickFormatter'
          doFormat: (ticks) ->
            labels = @get("labels")
            return (labels[tick] ? "" for tick in ticks)
          @define {
            labels: [ p.Any ]
          }
    """

    labels = Dict(Int, String, help="""
    A mapping of integer ticks values to their labels.
    """)

    __implementation__ = JS_CODE


'''
*** Horizontal Bar Chart ***(deprecated)

ind : labels(str or int) for the x-axis
val : y-axis value for the bar plot
x_label : label for x-axis
y_label : label for y-axis
'''
def plot_vbar(ind, val, x_label='Quater', y_label='TRx', title='Quater-wise TRx'):
    output_notebook()
    source = ColumnDataSource(data=dict(labels = ind,
                                       y = np.arange(len(val)),
                                       x = val))
    p = figure(plot_width=400, plot_height=400, title = title)
    p.vbar(x = 'y', width=0.5, bottom=0, top = 'x', source = source )
    p.title.align = "center"
    p.title.text_font_size = "25px"
    p.yaxis.axis_label = x_label
    p.xaxis.axis_label = y_label
    label_dict = dict()
    for i, s in enumerate(ind):
        label_dict[i] = s
    p.xaxis[0].formatter = FixedTickFormatter(labels=label_dict)
    p.xaxis.major_label_orientation = math.pi/4
    show(p)


'''
Multivariate PLot with target variable over the 4 quarter
'''
def plot_multi_variate(data, variable = 'ER.Branded.Decile', agg_fun = 'sum'):

	p1=Bar(data, label=variable, values='XTAMPZA_ER_Q1_TRx', title='Quater 1', agg=agg_fun, legend=None)
	p1.title.align = "center"
	p1.title.text_font_size = "15px"

	p2=Bar(data, label=variable, values='XTAMPZA_ER_Q2_TRx', title='Quater 2', agg=agg_fun, legend=None)
	p2.title.align = "center"
	p2.title.text_font_size = "15px"

	p3=Bar(data, label=variable, values='XTAMPZA_ER_Q3_TRx', title='Quater 3', agg=agg_fun, legend=None)
	p3.title.align = "center"
	p3.title.text_font_size = "15px"

	p4=Bar(data, label=variable, values='XTAMPZA_ER_Q4_TRx', title='Quater 4', agg=agg_fun, legend=None)
	p4.title.align = "center"
	p4.title.text_font_size = "15px"

	p=gridplot([[p1,p2],[p3,p4]], plot_width = 300, plot_height = 300)
	show(p)