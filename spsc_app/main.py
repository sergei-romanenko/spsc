from spsc import views
from google.appengine.ext import webapp
from google.appengine.ext.webapp import util

application = webapp.WSGIApplication(
                                     [('/', views.Recent),
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

def main():
  util.run_wsgi_app(application)

if __name__ == "__main__":
  main()