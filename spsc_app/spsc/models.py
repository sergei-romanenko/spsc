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
    
#    @classmethod
#    def method(cls, get):