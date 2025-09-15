from django.urls import path
from . import views

app_name = 'documentacao'

urlpatterns = [
    path('', views.texto, name='home_documentacao'),
    path('<path:nome_arquivo>/', views.texto, name='texto_documentacao'),
]
