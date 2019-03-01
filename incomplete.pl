% path: ['C:/Users/sarah/Documents/college/3rdYear/ai/incomplete.pl']

/* test:
initKB('C:/Users/sarah/Documents/college/3rdYear/ai/example'),
aStar([q], Path, Cost).
*/


% dynamic predicate kb/1
:- dynamic(kb/1).


% makeKB/1
makeKB(File):-
		open(File, read, Str),
		readK(Str, K),
		reformat(K, KB),
		asserta( kb(KB) ),
		close(Str).


% readK/2
readK(Stream,[]):-
		at_end_of_stream(Stream),
		!.
readK(Stream,[X|L]):-
		read(Stream, X),
		readK(Stream, L).


% reformat/2
% takes the head of list1, encapsulates it in a list and prepends it to list2
reformat([],[]).
reformat([end_of_file],[]) :- !.
reformat([:-(H, B) | L],[[H | BL] | R]) :-
		!,
		mkList(B, BL),
		reformat(L, R).
reformat([A | L], [[A] | R]) :- reformat(L,R).

% mkList/2
mkList((X, T), [X | R]) :-
		!,
		mkList(T,R).
mkList(X,[X]).

% initKB/1
initKB(File) :-
		retractall(kb(_)),
		makeKB(File).


% arc/4
% arc finds possible actions that can be taken and their cost
% returns possible destination node and the cost of the arc to that node
arc([Head|Tail],Node,Cost,KB) :-
		member([Head | NewList], KB),	% checks if Head+NewList is in KB
		append(NewList, Tail, Node),	% NewList is Tail with Node appended
		length(NewList, NLLength),		% the length of NewList is NLLength
		Cost is NLLength + 1. 			% NLLength = Cost - 1


% heuristic/2
% the heuristic (estimated cost of getting to destination node) is the length
% of the list
heuristic(Node, Heuristic) :- length(Node, Heuristic).


% goal/1
% the goal is to reduce the given node down the empty list
goal([]).


% lessthan/2
% uses the total expected cost (cost + heuristic) for both Nodes to
% determine the lower-cost Node.
lessthan([Node1,Cost1],[Node2,Cost2]) :-
		% gets heuristics for Nodes
		heuristic(Node1, HVal1),
		heuristic(Node2, HVal2),
		% calculates total f(Node) cost of each node
		F1 is Cost1 + HVal1,
		F2 is Cost2 + HVal2,
		% checks if f(Node1) <= f(Node2)
		F1 =< F2.


% addtofrontier/3
% list of path-cost pairs (not just nodes)
% f(node) = cost(node) + h(node)
% 'Frontier' is the queue with the next node to expand at the head
addtofrontier(Children, InList, Queue) :-
		append(Children, InList, PossibleQueue),
		findSmallestNode(PossibleQueue, Queue).


% findSmallestHead/2
% returns the list sorted so that the node with the lowest cost is at the head
findSmallestNode([List], [List]).
findSmallestNode([CurrentHead | UnsortedTail], SortedTail) :-
		CurrentHead = [CurrNode, CurrCost, _],  % first two elements of CurrentHead
		PreviousHead = [PrevNode, PrevCost, _], % first two elements of PreviousHead
		findSmallestNode(UnsortedTail, [PreviousHead | PreviousSort]),
		(lessthan([PrevNode, PrevCost],[CurrNode, CurrCost]) -> % if
				SortedTail = [PreviousHead, CurrentHead | PreviousSort]; % then
				SortedTail = [CurrentHead, PreviousHead | PreviousSort]). % else



% aStar
% return a list Path to the goal node [] with minimal Cost, given Node and KB
% aStar/3
aStar(Node,Path,Cost) :- kb(KB), aStar(Node,Path,Cost,KB).
% aStar/4
aStar(Node, [Node], 0, _) :- goal(Node).
aStar(Node, Path, Cost, KB) :- aStar([Node, 0, [Node]], Path, Cost, KB, []).
% aStar/6
aStar([Node, TotalCost, TotalPath], TotalPath, TotalCost, _, _) :- goal(Node).
aStar([Node, OldCost, OldPath], ReturnPath, ReturnCost, KB, Frontier) :-
		% finds all Children of current Node
		findall(
				% concats list
				[Head, NodeCost, [Head | OldPath]],
				% all children of current Node
				(arc(Node, Head, NewCost, KB), NodeCost is NewCost + OldCost),
				% from list Children
				Children ),
		% add children to the frontier to find which path to take
		addtofrontier(Children, Frontier, ReturnList),
		% concat listr
		ReturnList = [AddedNodes | NewFrontier],
		% call aStar recursively
		aStar(AddedNodes, ReturnPath, ReturnCost, KB, NewFrontier).

%
