from spsc import views
import webapp2 as webapp

app = webapp.WSGIApplication(
                             [('/', views.Root),
                              ('/all', views.All),
                              ('/supercompiler', views.Supercompiler),
                              ('/edit', views.Edit),
                              ('/delete', views.Delete),
                              ('/view', views.Get),
                              ('/svg', views.Svg),
                              ('/svgpreview', views.SvgPreview),
                              ('/authors', views.Authors),
                              ('/author', views.Author),
                              ('/mine', views.Mine)]
                             )
