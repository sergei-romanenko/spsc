from google.appengine.ext import db

class Program(db.Model):
    name = db.StringProperty()
    author = db.ReferenceProperty(Author) # ==parent
    code = db.TextProperty()
    goal = db.StringProperty()
    description = db.TextProperty()
    date = db.DateTimeProperty(auto_now_add=True)
    scp_code = db.TextProperty()
    svg_tree = db.TextProperty()
    
class Author(db.Model):
    user = db.UserProperty()
    n_programs = db.IntegerProperty()
    
def get_author_for_user(cls, user):
    author = db.Query(Author).filter('user=', user).get()
    if author is not None:
        return author
    else:
        author = Author()
        author.user = user
        author.n_programs = 0
        author.put()
        return author
        
    
def _add_program_for_user(user, name=None, code=None, goal=None, description=None, scp_code=None, svg_tree=None):
    author = db.Query(Author).filter('user=', user).get()
    author.n_programs = author.n_programs + 1
    program = Program(parent=author)
    program.name = name
    program.author = author
    program.code = code
    program.goal = goal
    program.description = description
    program.scp_code = scp_code
    program.svg_tree = svg_tree
    program.put()
    author.put()
    
def add_program_for_user(user, name=None, code=None, goal=None, description=None, scp_code=None, svg_tree=None):
    db.run_in_transaction(_add_program_for_user, user, name, code, goal, description, scp_code, svg_tree)

def _delete_program(program):
    author = program.author
    author.n_programs = author.n_programs - 1
    program.delete()
    author.put()
    
def delete_program(program):
    db.run_in_transaction(_delete_program, program)