% sample knowledge base
q:- a,b.
q:- c.
a:- f.
c:- b.
c:- d,e,f.
d:- e.
e.
f:- e,d.

% arc finds possible actions that can be taken
% cost supply always >= 1
arc([Head|Tail],Node,Cost,KB) :-
	member([Head|NewList],KB),	% checks if NewList with prepended Head is in KB
	append(NewList,Tail,Node),	% NewList is T with Node appended
	length(NewList,NLLength),	% the length of NewList is NLLength
	Cost is NLLength+1. 		% NLLength = Cost - 1

arc(Node, X) :- X. %TODO

heuristic(Node,Heuristic) :- length(Node,Heuristic).
% the heuristic is the length of the list

goal([]).

search([Node|_]) :- goal(Node).

% modify so that:

search([Node|More]) :-
	findall(X,arc(Node,X),Children),
	% Children = list of all X that satisfy arc(Node,X)
	addtofrontier(Children,More,New),
	search(New).

% astar(+Node, ?Path, ?Cost, +KB)
% return a list Path to the goal node [] with minimal Cost, given Node and KB

astar() :- .

lessthan([[Node1|_],Cost1],[[Node2|_],Cost2]) :-
	heuristic(Node1,Hvalue1),
	heuristic(Node2,Hvalue2),
	F1 is Cost1+Hvalue1,
	F2 is Cost2+Hvalue2,
	F1 =< F2.

% TODO initialises a dynamic predicate kb/1 to a list representing a file
initKB(FileName) :- kb(KB). %???????????


% list of path-cost pairs (not just nodes)
% update the cost and bring heuristic function to form New
% the head of ReturnList has f no larger than any in the tail where
% f(node) = cost(node) + h(node)
% ie. ReturnList is an ordered list of pairs where f is the overall heuristic:
% the known cost + the known heuristic
addtofrontier(Children, More, ReturnList) :- .
% Cost is L+CostPrev+1
