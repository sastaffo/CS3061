% sample knowledge base
q:- a,b.
q:- c.
a:- f.
c:- b.
c:- d,e,f.
d:- e.
e.
f:- e,d.

arc([H|T],Node,Cost,KB) :- member([H|B],KB), append(B,T,Node),
length(B,L), Cost is L+1.

heuristic(Node,H) :- length(Node,H).
goal([]).


search([Node|_]) :- goal(Node).
search([Node|More]) :- findall(X,arc(Node,X),Children),
		addtofrontier(Children,More,New),
		search(New).

lessthan([[Node1|_],Cost1],[[Node2|_],Cost2]) :-
		heuristic(Node1,Hvalue1), heuristic(Node2,Hvalue2),
		F1 is Cost1+Hvalue1, F2 is Cost2+Hvalue2,
		F1 =< F2.

% Cost is L+CostPrev+1
