from django.conf.urls import url
from . import views

urlpatterns = [ 
	# changed view.index to view.run_mob
	url(r'^$', views.run_query, name = 'index'),
	#url(r'^form/', views.get_name, name = 'get_name'),
	# currently not in use
	url(r'^explore/$', views.explore, name='explore'),
	url(r'^explore_article/$', views.explore_article, name='explore_article'),
	url(r'^explore_question/$', views.explore_question, name='explore_question'),
	]