% 2.2 Structured data and data bstractions

course(complexity, time(monday, 9, 11), lecturer(david, harel), location(feinberg, a)).
course(calculus2,  time(monday, 10, 11), lecturer(david, harel), location(feinberg, a)).
course(introtobio, time(monday, 11, 12), lecturer(david, harel), location(feinberg, a)).

lecturer(Lecturer, Course) :-
  course(Course, Time, Lecturer, Location).

duration(Length, Course) :-
  course(Course, time(Day, Start, End), Lecturer, Location),
  plus(Start, Length, End).

teaches(Lecturer, Day) :-
  course(Course, time(Day, Start, End), Lecturer, Location).

occupied(Room, Day, Time) :-
  course(Course, time(Day, Start, End), Lecturer, Room),
  Start =< Time,
  Time =< End.

% Exercises for 2.2

% 1. Add rules: location(Crouse, Building), busy(Lecturer, Time), cannot_meet(Lecturer1, Lecturer2)

location(Course, Building) :-
  course(Course, Time, Lecturer, location(Building, Room)).

busy(Lecturer, Time) :-
  course(Course, Time, Lecturer, Location).

% Not sure what the desired semantics of `cannot_meet` are. Many online answers have a predicate that
% tests whether there exists some time such that both lecturers have classes at that time.
% But that isn't the logical meening of "cannot meet", which I think should be defined as:
% "There does not exist a time such that both lecturers do not have a class at that time."
% Pretty sure we don't know how to do that yet.

% 2. Add rule for schedule_conflict(Time, Place, Course1, Course).

% I think the desired rule is simply:

schedule_conflict(Time, Place, Course1, Course2) :-
  course(Course1, Time, Lecturer1, Place),
  course(Course2, Time, Lecturer2, Place),
  Course1 \= Course2.

% But not that only handles conflicts between classes with identical times.
% It doesn't handle classes with overlapping but different times.
% Trying to fix that, though, yields a rule that doesn't work very well:

schedule_conflict2(Time, Place, Course1, Course2) :-
  course(Course1, time(Day, Start1, End1), Lecturer1, Place),
  course(Course2, time(Day, Start2, End2), Lecturer2, Place),
  Course1 \= Course2,
  Start1 =< Time, Start2 =< Time,
  Time =< End1, Time =< End2.

% It seems prolog cannot execute this rule with a variable for Time, only for specific Times.
% That seems like a big limitation, but whether it's in my understanding or in the language remains to be seen...
