import cgi
import os
import sys
import pyparsing
import slparsing

from google.appengine.ext.webapp import template

from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app

class MenuItem(object):
    def __init__(self, key, text, link=None):
        self.key = key
        self.text = text
        self.link = link
        
navItems = [MenuItem('samples', 'Samples', '/'), MenuItem('sample_add', 'Add Sample', '/add')]
    
def createNavItems(currentPage):
    result = []
    for item in navItems:
        if item.key == currentPage:
            result.append(MenuItem(item.key, item.text))
        else:
            result.append(item)
    return result


def createUserItems(login_redirect_uri, logout_redirect_uri):
    if users.get_current_user():
      return [MenuItem('logout', 'Logout', users.create_logout_url(logout_redirect_uri))]
    else:
      return [MenuItem('login', 'Login', users.create_login_url(login_redirect_uri))]

class SLProgram(db.Model):  
    name = db.StringProperty()
    author = db.UserProperty()
    code = db.StringProperty(multiline=True)
    goal = db.StringProperty()
    description = db.StringProperty(multiline=True)
    date = db.DateTimeProperty(auto_now_add=True)

class SLProgramList(webapp.RequestHandler):
    def get(self):
        programs = SLProgram.all().order('-date')#.fetch(50)
        navitems = createNavItems("samples")
        useritems = createUserItems(self.request.uri, self.request.uri)
        template_values = {
                        'programs': programs,
                        'navitems':navitems,
                        'useritems':useritems,
                        'user':users.get_current_user()
                        }

        path = os.path.join(os.path.dirname(__file__), 'samples.html')
        self.response.out.write(template.render(path, template_values))

class SLProgramAdd(webapp.RequestHandler):
    def post(self):
        if not users.get_current_user():
            self.redirect(users.create_login_url(self.request.uri))
            return
        code = self.request.get('code')
        goal = self.request.get('goal')
        try:
            funs = slparsing.parseAndValidate(code)
        except pyparsing.ParseException, pe:
            line = pyparsing.line(pe.loc, pe.pstr)
            msg = str(pe)
            self.display_errors(code_error=str(pe), code_line=line)
            return
        
        found = False
        for fun in funs:
            if fun.fFun:
                if fun.fFun.name == goal:
                    found = True
                    break
        
        if not found:
            self.display_errors(no_f_function=True)
            return
        
        program = SLProgram()        
        program.code = self.request.get('code')
        program.author = users.get_current_user()
        program.name = self.request.get('name')        
        program.goal = self.request.get('goal')
        program.description = self.request.get('description')
        program.put()
        self.redirect('/')
    def get(self):
        if not users.get_current_user():
            self.redirect(users.create_login_url(self.request.uri))         
        navitems = createNavItems("sample_add")
        useritems = createUserItems(self.request.uri, '/')
        template_values = {
                        'navitems':navitems,
                        'useritems':useritems
                        }

        path = os.path.join(os.path.dirname(__file__), 'sample_add.html')
        self.response.out.write(template.render(path, template_values))
    def display_errors(self, no_f_function=False, code_error=None, code_line=None):
        navitems = createNavItems("sample_add")
        useritems = createUserItems(self.request.uri, '/')
        template_values = {
                        'no_f_function': no_f_function,
                        'code_error': code_error,
                        'code_line': code_line,
                        'code' : self.request.get('code'),
                        'name' : self.request.get('name'),
                        'goal' : self.request.get('goal'),
                        'description' : self.request.get('description'),
                        'navitems':navitems,
                        'useritems':useritems
                        }

        path = os.path.join(os.path.dirname(__file__), 'sample_add.html')
        self.response.out.write(template.render(path, template_values))
        
class SLProgramEdit(webapp.RequestHandler):
    def post(self):
        if not users.get_current_user():
            self.redirect(users.create_login_url(self.request.uri))
        
        code = self.request.get('code')
        goal = self.request.get('goal')
        try:
            funs = slparsing.parseAndValidate(code)
        except pyparsing.ParseException, pe:
            line = pyparsing.line(pe.loc, pe.pstr)
            msg = str(pe)
            self.display_errors(code_error=str(pe), code_line=line)
            return
        
        found = False
        for fun in funs:
            if fun.fFun:
                if fun.fFun.name == goal:
                    found = True
                    break
        
        if not found:
            self.display_errors(no_f_function=True)
            return
        
        try:
            key_name = self.request.get('key')
            slprogram = db.get(db.Key(key_name))
            if slprogram:         
                slprogram.code = self.request.get('code')
                slprogram.name = self.request.get('name')        
                slprogram.goal = self.request.get('goal')
                slprogram.description = self.request.get('description')
                slprogram.put()
            self.redirect('/')
        except db.BadKeyError:
            self.redirect('/') 
            return
    def get(self):
        if not users.get_current_user():
            self.redirect(users.create_login_url(self.request.uri))
            return
        key_name = self.request.get('key')
        try:
            slprogram = db.get(db.Key(key_name))
            if slprogram:
                navitems = createNavItems("sample_edit")
                useritems = createUserItems(self.request.uri, '/')
                template_values = {
                                   'navitems': navitems,
                                   'useritems':useritems,
                                   'key'  : slprogram.key,
                                   'code' : slprogram.code,
                                   'name' : slprogram.name,
                                   'goal' : slprogram.goal,
                                   'description' : slprogram.description,
                                   }
                path = os.path.join(os.path.dirname(__file__), 'sample_edit.html')
                self.response.out.write(template.render(path, template_values))
            else:
                self.redirect('/')
        except db.BadKeyError:
            self.redirect('/') 
    def display_errors(self, no_f_function=False, code_error=None, code_line=None):
        navitems = createNavItems("sample_edit")
        useritems = createUserItems(self.request.uri, '/')
        template_values = {
                        'no_f_function': no_f_function,
                        'code_error': code_error,
                        'code_line': code_line,
                        'key': self.request.get('key'),
                        'code' : self.request.get('code'),
                        'name' : self.request.get('name'),
                        'goal' : self.request.get('goal'),
                        'description' : self.request.get('description'),
                        'navitems':navitems,
                        'useritems':useritems
                        }

        path = os.path.join(os.path.dirname(__file__), 'sample_edit.html')
        self.response.out.write(template.render(path, template_values))
        
class SLProgramDelete(webapp.RequestHandler):
    def get(self):
        key_name = self.request.get('key')
        try:
            slProgram = db.get(db.Key(key_name))
            if slProgram:
                slProgram.delete()
            self.redirect('/')
        except db.BadKeyError:
            self.redirect('/')

application = webapp.WSGIApplication(
                                     [('/', SLProgramList),
                                      ('/add', SLProgramAdd),
                                      ('/delete', SLProgramDelete),
                                      ('/edit', SLProgramEdit)]
                                     )

def main():
  run_wsgi_app(application)

if __name__ == "__main__":
  main()