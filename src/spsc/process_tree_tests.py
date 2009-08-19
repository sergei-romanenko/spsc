'''
Created on Aug 18, 2009

@author: Sergei Romanenko
'''

from process_tree import *

n1 = Node(None, None, None, [])
n2 = Node(None, None, n1, [])
n3 = Node(None, None, n2, [])
n1.children = [n2]
n2.children = [n3]

print "%s" %[n for n in n3.ancestors()]