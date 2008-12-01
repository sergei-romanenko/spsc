
from spsc import spsc
from google.appengine.ext import webapp
from google.appengine.ext.webapp import util

application = webapp.WSGIApplication(
                                     [('/', spsc.SLProgramList),
                                      ('/new', spsc.SLProgramAdd),
                                      ('/delete', spsc.SLProgramDelete),
                                      ('/edit', spsc.SLProgramEdit)]
                                     )

def main():
  util.run_wsgi_app(application)

if __name__ == "__main__":
  main()