from google.appengine.api import urlfetch
from google.appengine.ext import webapp
from google.appengine.ext.webapp import util

RUN_URL = 'http://spsc.ilyushkin.staxapps.net/'

class Wakeup(webapp.RequestHandler):
    def get(self):
        self.response.headers['Content-Type'] = 'text/plain; charset=utf-8'
        result = urlfetch.fetch(url=RUN_URL)
        self.response.out.write(result.content)
        
application = webapp.WSGIApplication([('/wakeup', Wakeup)])

def main():
  util.run_wsgi_app(application)

if __name__ == "__main__":
  main()