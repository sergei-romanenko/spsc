import urllib
import uuid

from xml.dom import minidom

from google.appengine.api import users
from google.appengine.api import urlfetch
from google.appengine.api import memcache
from google.appengine.ext import db
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from google.appengine.ext.webapp import util

import models

OK = 'ok'
UNKNOWN_FUNCTION = 'unknownFunction'
PARSE_ERROR = 'parseError'
NETWORK_ERROR = 'networkError'

RUN_URL = 'http://spsc.ilyushkin.staxapps.net/run'
SVG_INTRO = """<?xml version="1.0" encoding="utf-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN" "http://www.w3.org/TR/SVG/DTD/svg10.dtd">
"""
    
class SupercompilationResult(object):
    def __init__(self, status, residualCode=None, svgTree=None, message=None, line=None, column=None, code_line=None):
        self.status = status
        self.residualCode = residualCode
        self.svgTree = svgTree
        self.message = message
        self.line = line
        self.column = column
        self.code_line = code_line
        
def supercompileProgram(code, goal):
    form_fields = {'program': code, 'fname': goal}
    form_data = urllib.urlencode(form_fields)
    result = urlfetch.fetch(url=RUN_URL,
                            payload=form_data,
                            method=urlfetch.POST,
                            headers={'Content-Type': 'application/x-www-form-urlencoded'})   
    if result.status_code == 200:
        xmlresponse = result.content
        doc = minidom.parseString(xmlresponse)
        status = doc.documentElement.getAttribute('status')
        if status == PARSE_ERROR:
            details = doc.documentElement.getElementsByTagName('details')[0]
            msg = details.getAttribute('message')
            line = int(details.getAttribute('line'))
            column = int(details.getAttribute('column'))
            code_line = ''
            lines = code.splitlines()
            if lines:
                code_line = lines[line-1] 
            return SupercompilationResult(status, message=msg, line=line, column=column, code_line=code_line)
        elif status == OK:
            codeElement = doc.documentElement.getElementsByTagName('code')[0]
            residualCode = codeElement.firstChild.data
            svg = doc.documentElement.getElementsByTagName('tree')[0].firstChild.toxml()
            return SupercompilationResult(OK, residualCode=residualCode, svgTree=svg)
        else:
            return SupercompilationResult(status)
    else:
        return ValidationResult(NETWORK_ERROR)

class Svg(webapp.RequestHandler):
    def get(self):
        key_name = self.request.get('key')
        program = db.get(db.Key(key_name))
        if program:
            self.response.out.write(SVG_INTRO)
            self.response.out.write(program.svg_tree)
            self.response.headers['Content-Type'] = 'image/svg+xml; charset=utf-8'
            
class SvgPreview(webapp.RequestHandler):
    def get(self):
        key = self.request.get('key')
        svg = memcache.get(key)
        if svg:
            self.response.out.write(SVG_INTRO)
            self.response.out.write(svg)
            self.response.headers['Content-Type'] = 'image/svg+xml; charset=utf-8'
        
class All(webapp.RequestHandler):
    def get(self):
        order = self.request.get('order')
        if order not in ['name', '-name', 'date', '-date', 'summary', '-summary', 'author', '-author', 'modified', '-modified']:
            order = '-modified'
        programs = models.Program.all().order(order)
        template_values = {
                        'order': order,
                        'programs': programs,
                        'user': users.get_current_user(),
                        'sign_in': users.create_login_url(self.request.uri),
                        'sign_out': users.create_logout_url(self.request.uri)
                        }
        self.response.out.write(template.render('templates/recent.html', template_values))
        
class Supercompiler(webapp.RequestHandler):
    def post(self):
        code = self.request.get('code')
        goal = self.request.get('goal')
        
        validationResult = supercompileProgram(code, goal)
        if validationResult.status == UNKNOWN_FUNCTION:
            self.display_errors(no_f_function=True)
            return
        elif validationResult.status == PARSE_ERROR:
            self.display_errors(code_error=validationResult.message, code_line=validationResult.code_line)
            return
        action = self.request.get('action')
        
        user = users.get_current_user()
        scp_code = validationResult.residualCode
        svg_tree = validationResult.svgTree
        svg_xml = minidom.parseString(svg_tree)
        svg_width = svg_xml.documentElement.getAttribute('width')
        svg_height = svg_xml.documentElement.getAttribute('height')
        name = self.request.get('name').strip()
        empty_name = name == ''
        if action == 'Supercompile' or empty_name:
            key = uuid.uuid1().hex
            memcache.set(key, svg_tree, time=60)
            template_values = {
                               'user': users.get_current_user(),
                               'sign_in': users.create_login_url(self.request.uri),
                               'sign_out': users.create_logout_url(self.request.uri),
                               'code':code,
                               'goal':goal,
                               'scp_code':scp_code,
                               'key':key,
                               'name':name,
                               'summary':self.request.get('summary'),
                               'notes':self.request.get('notes'),
                               'empty_name':empty_name,
                               'svg_width': svg_width, 'svg_height': svg_height
                               }
            self.response.out.write(template.render('templates/supercompiler.html', template_values))
            return
        author = models.get_author_for_user(user)
        models.add_program_for_user(author.key(), name=self.request.get('name'), summary=self.request.get('summary'), 
                                    code=self.request.get('code'), goal=self.request.get('goal'), 
                                    notes=self.request.get('notes'), scp_code=validationResult.residualCode, 
                                    svg_tree=validationResult.svgTree)
        self.redirect('/')
    def get(self):
        template_values = {
            'user': users.get_current_user(),
            'sign_in': users.create_login_url(self.request.uri),
            'sign_out': users.create_logout_url(self.request.uri)
            }
        self.response.out.write(template.render('templates/supercompiler.html', template_values))
    def display_errors(self, no_f_function=False, code_error=None, code_line=None, empty_name=False):
        template_values = {
                        'user': users.get_current_user(),
                        'sign_in': users.create_login_url(self.request.uri),
                        'sign_out': users.create_logout_url(self.request.uri),
                        'no_f_function': no_f_function,
                        'code_error': code_error,
                        'code_line': code_line,
                        'code' : self.request.get('code'),
                        'name' : self.request.get('name').strip(),
                        'goal' : self.request.get('goal'),
                        'summary' : self.request.get('summary'),
                        'notes' : self.request.get('notes'),
                        'empty_name': empty_name
                        }
        self.response.out.write(template.render('templates/supercompiler.html', template_values))
        
class Edit(webapp.RequestHandler):
    def post(self):
        if not users.get_current_user():
            self.redirect(users.create_login_url(self.request.uri))
        code = self.request.get('code')
        goal = self.request.get('goal')
        
        scp_result = supercompileProgram(code, goal)
        if scp_result.status == UNKNOWN_FUNCTION:
            self.display_errors(no_f_function=True)
            return
        elif scp_result.status == PARSE_ERROR:
            self.display_errors(code_error=scp_result.message, code_line=scp_result.code_line)
            return
        action = self.request.get('action')
        if action == 'Preview':
            scp_code = scp_result.residualCode
            svg_tree = scp_result.svgTree
            svg_xml = minidom.parseString(svg_tree)
            svg_width = svg_xml.documentElement.getAttribute('width')
            svg_height = svg_xml.documentElement.getAttribute('height')
            key = uuid.uuid1().hex
            memcache.set(key, svg_tree, time=60)
            template_values = {
                               'user': users.get_current_user(),
                               'sign_in': users.create_login_url(self.request.uri),
                               'sign_out': users.create_logout_url(self.request.uri),
                               'code':code,
                               'goal':goal,
                               'scp_code':scp_code,
                               'tmp_key':key,
                               'name':self.request.get('name'),
                               'summary':self.request.get('summary'),
                               'notes':self.request.get('notes'),
                               'key':self.request.get('key'),
                               'svg_width': svg_width, 'svg_height': svg_height
                               }
            self.response.out.write(template.render('templates/edit.html', template_values))
            return
        name = self.request.get('name').strip()
        if name == '':
            self.display_errors(empty_name=True)
            return
        try:
            key_name = self.request.get('key')
            program = db.get(db.Key(key_name))
            if program and users.get_current_user() == program.author.user:       
                program.code = self.request.get('code')
                program.name = self.request.get('name')        
                program.goal = self.request.get('goal')
                program.notes = self.request.get('notes')
                program.summary = self.request.get('summary')
                program.scp_code = scp_result.residualCode
                program.svg_tree = scp_result.svgTree
                program.put()
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
            program = db.get(db.Key(key_name))
            if program:
                svg_key = uuid.uuid1().hex
                memcache.set(svg_key, program.svg_tree, time=60)
                template_values = {
                                   'program': program,
                                   'user': users.get_current_user(),
                                   'sign_in': users.create_login_url(self.request.uri),
                                   'sign_out': users.create_logout_url(self.request.uri),
                                   'key'  : program.key(),
                                   'code' : program.code,
                                   'name' : program.name,
                                   'goal' : program.goal,
                                   'notes' : program.notes,
                                   'summary' : program.summary,
                                   'scp_code' : program.scp_code,
                                   'tmp_key' : svg_key
                                   }
                self.response.out.write(template.render('templates/edit.html', template_values))
            else:
                self.redirect('/')
        except db.BadKeyError:
            self.redirect('/') 
    def display_errors(self, no_f_function=False, code_error=None, code_line=None, empty_name=False):
        template_values = {
                        'user': users.get_current_user(),
                        'sign_in': users.create_login_url(self.request.uri),
                        'sign_out': users.create_logout_url(self.request.uri),
                        'no_f_function': no_f_function,
                        'code_error': code_error,
                        'code_line': code_line,
                        'code' : self.request.get('code'),
                        'name' : self.request.get('name'),
                        'goal' : self.request.get('goal'),
                        'summary' : self.request.get('summary'),
                        'notes' : self.request.get('notes'),
                        'key':self.request.get('key'),
                        'empty_name': empty_name
                        }
        self.response.out.write(template.render('templates/edit.html', template_values))

class Get(webapp.RequestHandler):
    def get(self):
        key_name = self.request.get('key')
        try:
            program = models.Program.get(db.Key(key_name))
            if program:
                svg = program.svg_tree
                svg_xml = minidom.parseString(svg)
                svg_width = svg_xml.documentElement.getAttribute('width')
                svg_height = svg_xml.documentElement.getAttribute('height')
                template_values = {
                                   'program': program,
                                   'user': users.get_current_user(),
                                   'sign_in': users.create_login_url(self.request.uri),
                                   'sign_out': users.create_logout_url(self.request.uri),
                                   'svg_width': svg_width, 'svg_height': svg_height
                                   }
                self.response.out.write(template.render('templates/program.html', template_values))
        except db.BadKeyError:
            self.redirect('/')
    def post(self):
        key_name = self.request.get('key')
        action = self.request.get('action')
        if action == 'Delete':
            comment_key = self.request.get('comment_key')
            comment = models.Comment.get(db.Key(comment_key))
            comment.delete()
            self.redirect('/view?key=' + key_name)
            return
        try:
            program = models.Program.get(db.Key(key_name))
            if program:
                comment_text = self.request.get('comment')
                user = users.get_current_user()
                author = models.get_author_for_user(user)
                comment = models.Comment(program = program, author = author, text = comment_text)
                comment.put()
                self.redirect('/view?key=' + key_name)
        except db.BadKeyError:
            self.redirect('/')

class Delete(webapp.RequestHandler):
    def get(self):
        key_name = self.request.get('key')
        try:
            program = db.get(db.Key(key_name))
            if program and users.get_current_user() == program.author.user:
                models.delete_program(program)
            self.redirect('/')
        except db.BadKeyError:
            self.redirect('/')
            
class Authors(webapp.RequestHandler):
    def get(self):
        authors = models.Author.all().order('user')
        template_values = {
                        'authors': authors,
                        'user': users.get_current_user(),
                        'sign_in': users.create_login_url(self.request.uri),
                        'sign_out': users.create_logout_url(self.request.uri)
                        }
        self.response.out.write(template.render('templates/authors.html', template_values))
        
class Author(webapp.RequestHandler):
    def get(self):
        order = self.request.get('order')
        if order not in ['name', '-name', 'date', '-date', 'summary', '-summary', 'modified', '-modified']:
            order = '-modified'
        author_key = self.request.get('key')
        author = db.get(db.Key(author_key))
        programs = models.Program.all().ancestor(author).order(order)
        template_values = {
                        'order': order,
                        'programs': programs,
                        'user': users.get_current_user(),
                        'sign_in': users.create_login_url(self.request.uri),
                        'sign_out': users.create_logout_url(self.request.uri),
                        'author':author
                        }
        self.response.out.write(template.render('templates/author.html', template_values))
        
class Mine(webapp.RequestHandler):
    def get(self):
        user = users.get_current_user()
        if not user:
            self.redirect(users.create_login_url(self.request.uri))
            return
        order = self.request.get('order')
        if order not in ['name', '-name', 'date', '-date', 'summary', '-summary', 'modified', '-modified']:
            order = '-modified'
        author = models.get_author_for_user(user)
        programs = models.Program.all().ancestor(author).order(order)
        template_values = {
                        'order': order,   
                        'programs': programs,
                        'user': users.get_current_user(),
                        'sign_in': users.create_login_url(self.request.uri),
                        'sign_out': users.create_logout_url(self.request.host_url),
                        'author':author
                        }
        self.response.out.write(template.render('templates/mine.html', template_values))
        
class Root(webapp.RequestHandler):
    def get(self):
        self.redirect('/all')