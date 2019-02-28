% path: ['C:/Users/sarah/Documents/college/3rdYear/ai/astar.pl']

:- dynamic(kb/1).

makeKB(File):-
		open(File,read,Str),
		readK(Str,K),
		reformat(K,KB),
		asserta(kb(KB)),
		close(Str).

readK(Stream,[]):- at_end_of_stream(Stream),!.
readK(Stream,[X|L]):-
		read(Stream,X),
		readK(Stream,L).

reformat([],[]).
reformat([end_of_file],[]) :- !.
reformat([:-(H,B)|L],[[H|BL]|R]) :-
		!,
		mkList(B,BL),
		reformat(L,R).

% takes the head of list1, encapsulates it in a list and prepends it to list2
reformat([A|L],[[A]|R]) :- reformat(L,R).

mkList((X,T),[X|R]) :-
		!,
		mkList(T,R).
mkList(X,[X]).

initKB(File) :-
		retractall(kb(_)),
		makeKB(File).

% arc finds possible actions that can be taken
% cost supply always >= 1
arc([Head|Tail],Node,Cost,KB) :-
		member([Head|NewList],KB),	% checks if NewList with prepended Head is in KB
		append(NewList,Tail,Node),	% NewList is Tail with Node appended
		length(NewList,NLLength),	% the length of NewList is NLLength
		Cost is NLLength+1. 		% NLLength = Cost - 1

arc(Node, X) :- X. %TODO

heuristic(Node,Heuristic) :- length(Node,Heuristic).
% the heuristic is the length of the list

goal([]).

search([Node|_]) :- goal(Node).

% modify so that:

search([Node|More]) :-
		findall(X,arc(Node,X),SatisfiesList),
		% SatisfiesList = list of all X that satisfy arc(Node,X)
		addtofrontier(SatisfiesList,More,New),
		search(New).


lessthan([[Node1|_],Cost1],[[Node2|_],Cost2]) :-
		heuristic(Node1,Hvalue1),
		heuristic(Node2,Hvalue2),
		F1 is Cost1+Hvalue1,
		F2 is Cost2+Hvalue2,
		F1 =< F2.


% list of path-cost pairs (not just nodes)
% update the cost and bring heuristic function to form New
% the head of ReturnList has f no larger than any in the tail where
% f(node) = cost(node) + h(node)
% ie. ReturnList is an ordered list of pairs where f is the overall heuristic:
% the known cost + the known heuristic
addtofrontier(Children, More, ReturnList) :- .
% Cost is L+CostPrev+1

% astar(+Node, ?Path, ?Cost, +KB)
% return a list Path to the goal node [] with minimal Cost, given Node and KB
astar(Node,Path,Cost) :- kb(KB), astar(Node,Path,Cost,KB).

astar(Node,Path,Cost,KB) :- Node.
