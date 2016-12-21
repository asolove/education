# Fall 2012 6.034 Lab 2: Search
#
# Your answers for the true and false questions will be in the following form.  
# Your answers will look like one of the two below:
#ANSWER1 = True
#ANSWER1 = False

# 1: True or false - Hill Climbing search is guaranteed to find a solution
#    if there is a solution
ANSWER1 = False

# 2: True or false - Best-first search will give an optimal search result
#    (shortest path length).
#    (If you don't know what we mean by best-first search, refer to
#     http://courses.csail.mit.edu/6.034f/ai3/ch4.pdf (page 13 of the pdf).)
ANSWER2 = False

# 3: True or false - Best-first search and hill climbing make use of
#    heuristic values of nodes.
ANSWER3 = True

# 4: True or false - A* uses an extended-nodes set.
ANSWER4 = True

# 5: True or false - Breadth first search is guaranteed to return a path
#    with the shortest number of nodes.
ANSWER5 = True

# 6: True or false - The regular branch and bound uses heuristic values
#    to speed up the search for an optimal path.
ANSWER6 = False

# Import the Graph data structure from 'search.py'
# Refer to search.py for documentation
from search import Graph

## Optional Warm-up: BFS and DFS
# If you implement these, the offline tester will test them.
# If you don't, it won't.
# The online tester will not test them.

# Note: many of these answers have substantially worse-than-expected performance
# because the tester demands we use lists instead of proprely-implemented queues or heaps
# So I've been lazy and used lots of O(n) and O(logn) things inside the loops, which I shouldn't.

def bfs(graph, start, goal):
    agenda = [[start]]
    while agenda:
        path = agenda.pop(0)

        # If extending path ending at goal:
        last = path[-1]

        if last == goal:
           return path

        # Extend path with nodes not yet in path
        possible_extensions = graph.get_connected_nodes(last)
        extensions = list(set(possible_extensions) - set(path))
        new_paths = [path + [extension] for extension in extensions]
        [agenda.append(path) for path in new_paths]
        
    return []



## Once you have completed the breadth-first search,
## this part should be very simple to complete.
def dfs(graph, start, goal):
    agenda = [[start]]
    while agenda:
        path = agenda.pop(0)

        # If extending path ending at goal:
        last = path[-1]
        if last == goal:
            return path

        # Extend path with nodes not yet in path
        possible_extensions = graph.get_connected_nodes(last)
        extensions = list(set(possible_extensions) - set(path))
        new_paths = [path + [extension] for extension in extensions]
        new_paths.reverse()
        [agenda.insert(0, path) for path in new_paths]

    return []


## Now we're going to add some heuristics into the search.  
## Remember that hill-climbing is a modified version of depth-first search.
## Search direction should be towards lower heuristic values to the goal.
def hill_climbing(graph, start, goal):
    agenda = [[start]]
    while agenda:
        path = agenda.pop(0)

        # If extending path ending at goal:
        last = path[-1]
        if last == goal:
            return path

        # Extend path with nodes not yet in path
        possible_extensions = graph.get_connected_nodes(last)
        extensions = list(set(possible_extensions) - set(path))
        new_paths = [path + [extension] for extension in extensions]
        sorted_paths = sorted(new_paths, key=lambda path: graph.get_heuristic(path[-1], goal))
        sorted_paths.reverse()
        [agenda.insert(0, path) for path in sorted_paths]

    return []

## Now we're going to implement beam search, a variation on BFS
## that caps the amount of memory used to store paths.  Remember,
## we maintain only k candidate paths of length n in our agenda at any time.
## The k top candidates are to be determined using the 
## graph get_heuristic function, with lower values being better values.

class BeamSearchAgenda:
    def __init__(self, graph, start, goal, width):
        self.minLength = 1
        self.paths = { 1: [[start]] }
        self.graph = graph
        self.goal = goal
        self.width = width

    def __nonzero__(self):
        return self.minLength in self.paths and len(self.paths[self.minLength]) > 0

    def pop(self):
        item = self.paths[self.minLength].pop()
        if len(self.paths[self.minLength]) == 0:
            self.minLength += 1
        return item

    def add_paths(self, paths):
        if not paths:
            return False

        length = len(paths[0])
        if length not in self.paths:
            self.paths[length] = []
        self.paths[length].extend(paths)
        self.paths[length].sort(key=lambda path: self.graph.get_heuristic(path[-1], self.goal))
        self.paths[length] = self.paths[length][:self.width]


def beam_search(graph, start, goal, beam_width):
    agenda = BeamSearchAgenda(graph, start, goal, beam_width)
    while agenda:
        path = agenda.pop()

        # If extending path ending at goal:
        last = path[-1]

        if last == goal:
           return path

        # Extend path with nodes not yet in path
        possible_extensions = graph.get_connected_nodes(last)
        extensions = list(set(possible_extensions) - set(path))
        new_paths = [path + [extension] for extension in extensions]
        agenda.add_paths(new_paths)
        
    return []

## Now we're going to try optimal search.  The previous searches haven't
## used edge distances in the calculation.

## This function takes in a graph and a list of node names, and returns
## the sum of edge lengths along the path -- the total distance in the path.
def path_length(graph, node_names):
    if len(node_names) == 1:
        return 0
    node = node_names[0]
    return graph.get_edge(node, node_names[1]).length + path_length(graph, node_names[1:])

def path_cost(graph, path, goal):
    last = path[-1]
    so_far = path_length(graph, path)
    to_go = graph.get_heuristic(last, goal)
    return so_far + to_go


class BranchAndBoundAgenda:
    def __init__(self, graph, start):
        self.paths = [[start]]
        self.graph = graph

    def __nonzero__(self):
        return len(self.paths) > 0

    def pop(self):
        return self.paths.pop(0)

    def add_paths(self, paths):
        if not paths:
            return False

        self.paths.extend(paths)
        sorted_paths = sorted(self.paths, key=lambda path: path_length(self.graph, path))
        self.paths = sorted_paths


def branch_and_bound(graph, start, goal):
    agenda = BranchAndBoundAgenda(graph, start)
    while agenda:
        path = agenda.pop()

        # If extending path ending at goal:
        last = path[-1]

        if last == goal:
           return path

        # Extend path with nodes not yet in path
        possible_extensions = graph.get_connected_nodes(last)
        extensions = list(set(possible_extensions) - set(path))
        new_paths = [path + [extension] for extension in extensions]
        agenda.add_paths(new_paths)
        
    return []


class AStarAgenda:
    def __init__(self, graph, start, goal):
        self.paths = [[start]]
        self.graph = graph
        self.goal = goal
        self.extended = []

    def __nonzero__(self):
        return len(self.paths) > 0

    def pop(self):
        while self.paths:
            path = self.paths.pop(0)
            if path[-1] in self.extended:
                continue
            else:
                self.extended.append(path[-1])
                return path

    def add_paths(self, paths):
        paths = [path for path in paths if path[-1] not in self.extended]
        if not paths:
            return False

        self.paths.extend(paths)
        sorted_paths = sorted(self.paths, key=lambda path: path_cost(self.graph, path, self.goal))
        self.paths = sorted_paths


def a_star(graph, start, goal):
    agenda = AStarAgenda(graph, start, goal)
    while agenda:
        path = agenda.pop()

        # If extending path ending at goal:
        last = path[-1]

        if last == goal:
           return path

        # Extend path with nodes not yet in path
        possible_extensions = graph.get_connected_nodes(last)
        extensions = list(set(possible_extensions) - set(path))
        new_paths = [path + [extension] for extension in extensions]
        agenda.add_paths(new_paths)
        
    return []


## It's useful to determine if a graph has a consistent and admissible
## heuristic.  You've seen graphs with heuristics that are
## admissible, but not consistent.  Have you seen any graphs that are
## consistent, but not admissible?

def is_admissible(graph, goal):
    for node in graph.nodes:
        real_cost = path_length(graph, a_star(graph, node, goal))
        estimate = graph.get_heuristic(node, goal)
        if estimate > real_cost:
            return False
    return True

"""
Formally, for every node N and each successor P of N, the estimated cost of reaching the goal from N is no greater than the step cost of getting to P plus the estimated cost of reaching the goal from P. That is:

"""

def is_consistent(graph, goal):
    for node1 in graph.nodes:
        for node2 in graph.get_connected_nodes(node1):
            estimate1 = graph.get_heuristic(node1, goal)
            step = graph.get_edge(node1, node2).length
            estimate2 = graph.get_heuristic(node2, goal)
            if estimate1 > step + estimate2:
                return False
    return True

HOW_MANY_HOURS_THIS_PSET_TOOK = '2'
WHAT_I_FOUND_INTERESTING = 'Search algorithms are parameterized on the queue\'s behavior but otherwise the same'
WHAT_I_FOUND_BORING = 'Remembering names of different algorithms. Seems like they represent histocial developments, but we now can see a more structured relationship between them and could name them systematically.'
