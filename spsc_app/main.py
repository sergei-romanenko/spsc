from spsc import views
from google.appengine.ext import webapp
from google.appengine.ext.webapp import util

application = webapp.WSGIApplication(
                                     [('/', views.Recent),
                                      ('/new', views.New),
                                      ('/edit', views.Edit),
                                      ('/delete', views.Delete),
                                      ('/svg', views.Svg),
                                      ('/users', views.Authors)]
                                     )

def main():
  util.run_wsgi_app(application)

if __name__ == "__main__":
  main()