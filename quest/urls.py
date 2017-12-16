from django.conf.urls import url
from . import views

urlpatterns = [ 
	# changed view.index to view.run_mob
	url(r'^$', views.run_query, name = 'index'),
	#url(r'^form/', views.get_name, name = 'get_name'),
	# currently not in use
	url(r'^explore/$', views.explore, name='explore'),
	]