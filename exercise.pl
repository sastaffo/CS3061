% S - {fit, unfit, dead}
% A - {exercise, relax}

% exercise | fit             | unfit            | dead
% fit      | 0.9(0.99)=0.891 | 0.9(0.01)=0.009  | 0.1
% unfit    | 0.9(0.2)=0.18   | 0.9(0.8)=0.72    | 0.1
% dead     | 0               | 0                | 1

% relax    | fit             | unfit           | dead
% fit      | 0.99(0.7)=0.693 | 0.99(0.3)=0.297 | 0.01
% unfit    | 0.99(0)=0       | 0.99(1)=0.99    | 0.01
% dead     | 0               | 0               | 1


% p(StartingState, Action, FinalState, Probability, Reward)
% look-up table for the KB
% StartingState - fit
p(fit, exercise, fit, 0.891, 8).
p(fit, exercise, unfit, 0.009, 8).
p(fit, relax, fit, 0.693, 10).
p(fit, relax, unfit, 0.297, 10).

% StartingState - unfit
p(unfit, exercise, fit, 0.18, 0).
p(unfit, exercise, unfit, 0.72, 0).
p(unfit, relax, fit, 0, 5).
p(unfit, relax, unfit, 0.99, 5).

% StartingState - dead
p(dead, _, dead, 1, 0).
p(dead, _, _, 0, 0).

% FinalState - dead
p(_, exercise, dead, 0.1, 0).
p(_, relax, dead, 0.01, 0).





q0(SS, A, PR) :-
	p(SS,A,fit,PF,RF),
	p(SS,A,unfit,PU,RU),
	%p(SS,A,dead,PD,RD),
	PRF is PF*RF,
	PRU is PU*RU,
	%PRD is PD*RD,
	PR is PRF+PRU. %+PRD.

% q(N, Gamma, StartingState, Action, ProbabilityReward)
% returns Qn
% base case
q(0, _, S, A, PR) :- q0(S, A, PR).
% incremented case
q(N, G, S, A, PR) :-
	q0(S,A,PR0),
	N > 0,
	NewN is N-1,
	product(NewN,G,S,A,fit,ProdFit),
	product(NewN,G,S,A,unfit,ProdUnfit),
	%product(NewN,G,S,A,dead,ProdDead),
	Sum is ProdFit+ProdUnfit,%+ProdDead,
	ProdGam is G*Sum,
	PR is PR0+ProdGam.


% product(N, Gamma, StartingState, Action, FinalState, Product)
% returns Prod, the product of Vn(State) and Prob(StartingState, Action, FinalState)
product(N, G, SS, A, FS, Prod) :-
	p(SS,A,FS,Prob,_),
	v(N,G,FS,PRMax),
	Prod is Prob*PRMax.


% v(N, Gamma, StartingState, MaxProbabilityReward)
% returns Vn(State), the max value of Qn(State, exercise) and Qn(State, relax)
v(N, G, S, PRMax) :-
	q(N,G,S,exercise,PRex),
	q(N,G,S,relax,PRrel),
	max2(PRex,PRrel,PRMax).

% max(InputValue1, InputValue2, MaxValue)
% returns Max, max of two input values
max2(In1, In2, Max) :- (
		In1 > In2 -> Max is In1 ;
		Max is In2
).

% gamma(N, Gamma, StartingState, Qn(StartingState,exercise), Qn(StartingState,relax))
% returns QNexercise and QNrelax
calculate(N, G, SS, QNexercise, QNrelax) :-
	increaseStack(N),
	integer(N),
	N >= 0,
	G > 0,
	G < 1,
	q(N,G,SS,exercise,QNexercise),
	q(N,G,SS,relax,QNrelax).

show(N, StartingState, Gamma, QNexercise, QNrelax) :-
	calculate(N,Gamma,StartingState,QNexercise,QNrelax).

increaseStack(N) :-
	N>=9,
	set_prolog_stack(local, limit(40000000000)).
