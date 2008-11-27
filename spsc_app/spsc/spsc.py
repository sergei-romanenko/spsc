import cgi
import os
import sys
import pyparsing
import slparsing
import urllib

from xml.dom.minidom import parse, parseString

from google.appengine.api import users
from google.appengine.api import urlfetch

from google.appengine.ext import db
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from google.appengine.ext.webapp import util

OK = 'ok'
UNKNOWN_FUNCTION = 'unknownFunction'
PARSE_ERROR = 'parseError'
NETWORK_ERROR = 'networkError'

VALIDATION_URL = 'http://pat.keldysh.ru/spsc_web/validate'
RUN_URL = 'http://pat.keldysh.ru/spsc_web/run'

class ValidationResult(object):
    def __init__(self, status, message=None, line=None, column=None, code_line=None):
        self.status = status
        self.message = message
        self.line = line
        self.column = column
        self.code_line = code_line
        
def validateProgram(code, goal):
    form_fields = {'program': code, 'fname': goal}
    form_data = urllib.urlencode(form_fields)
    result = urlfetch.fetch(url=VALIDATION_URL,
                            payload=form_data,
                            method=urlfetch.POST,
                            headers={'Content-Type': 'application/x-www-form-urlencoded'})   
    if result.status_code == 200:
        xmlresponse = result.content
        dom = parseString(xmlresponse)
        status = dom.documentElement.getAttribute('status')
        if status == PARSE_ERROR:
            details = dom.documentElement.getElementsByTagName('details')[0]
            msg = details.getAttribute('message')
            line = int(details.getAttribute('line'))
            column = int(details.getAttribute('column'))
            code_line = ''
            lines = code.splitlines()
            if lines:
                code_line = lines[line-1] 
            return ValidationResult(status, message=msg, line=line, column=column, code_line=code_line)
        else:
            return ValidationResult(status)
    else:
        return ValidationResult(NETWORK_ERROR)

class SupercompilationResult(object):
    def __init__(self, status, residualCode=None, svgTree=None):
        self.status = status
        self.residualCode = residualCode
        self.svgTree = svgTree
        
def supercompileProgram(code, goal):
    form_fields = {'program': code, 'fname': goal}
    form_data = urllib.urlencode(form_fields)
    result = urlfetch.fetch(url=RUN_URL,
                            payload=form_data,
                            method=urlfetch.POST,
                            headers={'Content-Type': 'application/x-www-form-urlencoded'})   
    if result.status_code == 200:
        xmlresponse = result.content
        dom = parseString(xmlresponse)
        status = dom.documentElement.getAttribute('status')
        if status == PARSE_ERROR:
            details = dom.documentElement.getElementsByTagName('details')[0]
            msg = details.getAttribute('message')
            line = int(details.getAttribute('line'))
            column = int(details.getAttribute('column'))
            code_line = ''
            lines = code.splitlines()
            if lines:
                code_line = lines[line-1] 
            return ValidationResult(status, message=msg, line=line, column=column, code_line=code_line)
        else:
            codeElement = dom.documentElement.getElementsByTagName('code')[0]
            residualCode = codeElement.firstChild.data
            svg = dom.documentElement.getElementsByTagName('tree')[0].firstChild.toxml()
            return SupercompilationResult(OK, residualCode, svg)
    else:
        return ValidationResult(NETWORK_ERROR)

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
        code = self.request.get('code')
        goal = self.request.get('goal')
        
        validationResult = validateProgram(code, goal)
        if validationResult.status == UNKNOWN_FUNCTION:
            self.display_errors(no_f_function=True)
            return
        elif validationResult.status == PARSE_ERROR:
            self.display_errors(code_error=validationResult.message, code_line=validationResult.code_line)
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
        
        validationResult = validateProgram(code, goal)
        if validationResult.status == UNKNOWN_FUNCTION:
            self.display_errors(no_f_function=True)
            return
        elif validationResult.status == PARSE_ERROR:
            self.display_errors(code_error=validationResult.message, code_line=validationResult.code_line)
            return
        
        try:
            key_name = self.request.get('key')
            slprogram = db.get(db.Key(key_name))
            if slprogram and users.get_current_user() == slprogram.author:       
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
                                   'key'  : slprogram.key(),
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
            slprogram = db.get(db.Key(key_name))
            if slprogram and users.get_current_user() == slprogram.author:
                slprogram.delete()
            self.redirect('/')
        except db.BadKeyError:
            self.redirect('/')

class SLProgramRun(webapp.RequestHandler):
    def get(self):
        key_name = self.request.get('key')
        try:
            slprogram = db.get(db.Key(key_name))
            if slprogram:
                scpResult = supercompileProgram(slprogram.code, slprogram.goal)
                # print scpResult.residualCode
                # print scpResult.svgTree
                navitems = createNavItems("sample_edit")
                useritems = createUserItems(self.request.uri, '/')
                template_values = {
                                   'program': slprogram,
                                   'scpResult': scpResult,
                                   'navitems':navitems,
                                   'useritems':useritems
                                   }

                path = os.path.join(os.path.dirname(__file__), 'sample_run.xhtml')
                self.response.out.write(template.render(path, template_values))
                self.response.headers.add_header('Content-Type', 'text/xml; charset=utf-8')
                return
            else:
                self.redirect('/')
        except db.BadKeyError:
            self.redirect('/')

application = webapp.WSGIApplication(
                                     [('/', SLProgramList),
                                      ('/add', SLProgramAdd),
                                      ('/delete', SLProgramDelete),
                                      ('/edit', SLProgramEdit),
                                      ('/run', SLProgramRun)]
                                     )

def main():
  util.run_wsgi_app(application)

if __name__ == "__main__":
  main()