from production import AND, OR, NOT, PASS, FAIL, IF, THEN, \
     match, populate, simplify, variables
from zookeeper import ZOOKEEPER_RULES

# This function, which you need to write, takes in a hypothesis
# that can be determined using a set of rules, and outputs a goal
# tree of which statements it would need to test to prove that
# hypothesis. Refer to the problem set (section 2) for more
# detailed specifications and examples.

# Note that this function is supposed to be a general
# backchainer.  You should not hard-code anything that is
# specific to a particular rule set.  The backchainer will be
# tested on things other than ZOOKEEPER_RULES.


def backchain_to_goal_tree(rules, hypothesis):
    return simplify(
        OR(hypothesis,
            OR(filter(None, [satisfy_hypothesis(rule, hypothesis, rules) for rule in rules]))))

          #  OR([goal_tree_for_goal(rules, rule.antecedent) for rule in rules if match_role(rule, match(rule.consequent(), hypothesis) is not None])))

# Given the rule (X has hair OR X has milk -> X is a mammal)
# and the goal of proving (Adam is a mammal):
# Unify the goal's consequent with the hypothesis
# to get the substitution: X: Adam
# Unify that with the antecedent to get the next goal:
# Adam has hair OR Adam has milk
def satisfy_hypothesis(rule, hypothesis, rules):
    substitutions = match(rule.consequent()[0], hypothesis)

    if substitutions == None:
        return None

    def transform(goal_tree):
        if isinstance(goal_tree, AND):
            return AND(map(lambda g: transform(g), goal_tree))
        elif isinstance(goal_tree, OR):
            return OR(map(lambda g: transform(g), goal_tree))
        else:
            return backchain_to_goal_tree(rules, populate(goal_tree, substitutions))


    return transform(rule.antecedent())

# Here's an example of running the backward chainer - uncomment
# it to see it work:
#print backchain_to_goal_tree(ZOOKEEPER_RULES, 'opus is a penguin')
