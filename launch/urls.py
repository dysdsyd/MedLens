from django.conf.urls import url
from . import views

urlpatterns = [
	url(r'^$', views.index, name = 'index'),
	#url(r'^form/', views.get_name, name = 'get_name'),
	url(r'^run/$', views.run_mob, name='run'),
]