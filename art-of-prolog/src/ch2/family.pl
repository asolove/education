% Program 1.1: A Biblical family database

father(terach, abraham).
father(terach, nachor).
father(terach, haran).
father(abraham, isaac).
father(haran, lot).
father(haran, milcah).
father(haran, yiscah).
mother(sarah, isaac).

male(terach).
male(abraham).
male(nachor).
male(haran).
male(isaac).
male(lot).
female(sarah).
female(milcah).
female(yiscah).

% Rules from text of 1.7

parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

son(X, Y) :- parent(Y, X), male(X).
daughter(X, Y) :- parent(Y, X), female(X).


% Program 2.1: Defining family relationships

uncle(Uncle, Person) :-
  brother(Uncle, Parent), parent(Parent, Person).
sibling(X, Y) :-
  parent(Z, X), parent(Z, Y), X \= Y.

% Rules in text of 2.1

brother(Brother, Sib) :- sibling(Brother, Sib), male(Brother).

mother(Woman) :- mother(Woman, X).

% Exercises for 2.1

% 1: sister, niece, full sibling
sister(X, Y) :- sibling(X, Y), female(X).

niece(Niece, Person) :- daughter(Niece, Parent), sibling(Parent, Person).

full_sibling(X, Y) :- father(F, X), father(F, Y), mother(M, X), mother(M, Y).

% 2: mother_in_law, brother_in_law, son_in_law assuming married(Wife, Husband)
married(milcah, nachor).
married(sarah, abraham).
married(rebecca, isaac).

mother_in_law(Mother, SonInLaw) :- mother(Mother, Daughter), married(Daughter, SonInLaw).
mother_in_law(Mother, DaughterInLaw) :- mother(Mother, Son), married(DaughterInLaw, Son).

brother_in_law(Brother, Woman) :- brother(Brother, Husband), married(Woman, Husband).
brother_in_law(Brother, Man) :- brother(Brother, Wife), married(Wife, Man).

parents(X, Y, Child) :- parent(X, Child), parent(Y, Child), X \= Y.

son_in_law(Son, InLaw) :- married(Woman, Son), daughter(Woman, InLaw).
