import cgi
import os
import sys
import urllib

from xml.dom.minidom import parse, parseString

from google.appengine.api import users
from google.appengine.api import urlfetch

from google.appengine.ext import db
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from google.appengine.ext.webapp import util
from spsc import models

class Recent(webapp.RequestHandler):
    def get(self):
        programs = spsc.Program.all().order('-date')#.fetch(50)
        template_values = {
                        'programs': programs,
                        'user': users.get_current_user(),
                        'sign_in': users.create_login_url(self.request.uri),
                        'sign_out': users.create_logout_url(self.request.uri)
                        }
        path = os.path.join(os.path.dirname(__file__), 'templates/recent.html')
        self.response.out.write(template.render(path, template_values))