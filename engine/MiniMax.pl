/*
    Uitility values: 
      max = 5 winning moves, min = 4 winning moves -> (1)
      -> count the number of n-1 connected pieces 
      -> the square above,beside,on the diagonal can be occupied in one move ex:

    # # # # # # #
    # # # # # # #
    d # a # # # # 
    b r r r s # #
    r b r b b # # 
    b b r r b # #
      
*/

/*Successor(Board, C, StateUtil, Successors):-
for 
in a for loop from i = 0 till the number of columns
-> assuming that computer is blue as it starts second to human.
-> add a piece to column i 
-> append the state to a list of successor states
-> return list of states 

-> Use place_piece(Piece, C, Board, NewBoard) to add a piece */
/*Unsafe*/


/* get successors uses The given piece and current board state to create the successor states*/
get_successors(Board, Player, StateUtil, List):-
    [H | _] = Board,
    length(H, Columns),
    Columns1 is Columns - 1,
    get_successors(Board, Player, Columns1, StateUtil, List).
    
get_successors(_, _,-1, _, []).

get_successors(Board, Player, C, StateUtil,[Successor_State|Rest_Of_Successors] ):-
    Action_Function = StateUtil.perform_move,
    /*fn(Player, Move, State, NewState)*/
    Successor_State = call(Action_Function, Player, C, Board, NewBoard),
    C is C - 1,
    get_successors(Board, Player, C, StateUtil, Rest_Of_Successors).


max_list([X],X).
max_list([X|Xs],S) :- max_list(Xs,Y),(X>=Y,S=X,!;S=Y).

min_list([X],X).
min_list([X|Xs],S) :- min_list(Xs,Y),(X<=Y,S=X,!;S=Y).

/*
-> A Successors list from get_successors
-> A for loop from 0 till number of successors:
-> inside the for loop: 
-> Add to an array: the Successor_evalutaion that is returned from min_value(state,StateUtil,Successor_evaluation)
-> at end of loop The array containing the Successor_evaluation list is returned

Obj[] Successors = get_successors();
int array = new int[Successors.length];
int i = 0;
for (Obj successor in Successors){
    array[i] = min_value(state,StateUtil);
    i++;
}
return array;
*/

/*starts search -> Next_Move -> board state*/
mini_max_decision(Board, StateUtil, Next_Move):-
    get_successors(Board, r, StateUtil, Successors),
    call_min_recursive(Successors, StateUtil, Action_values),
    max_list(Action_values, Max_value),
    nth0(Index, Action_values, Max_value),
    nth0(Index, Successors, Next_Move). 


/*
    int array = new int[Successors.length];
    int i = 0;
    for (Obj successor in Successors){
        array[i] = min_value(state,StateUtil);
        i++;
    }
    return array;
*/

call_min_recursive([], _, []).

call_min_recursive([H|T], StateUtil, [Action_value| Rest_of_Action_values]):-
    min_value(H, StateUtil, Action_value),
    call_min_recursive(T, StateUtil, Rest_of_Action_values).

call_max_recursive([], _, []).

call_max_recursive([H|T], StateUtil, [Action_value| Rest_of_Action_values]):-
    max_value(H, StateUtil, Action_value),
    call_max_recursive(T, StateUtil, Rest_of_Action_values).

/*    -> if in terminal state -> return Uitility value
    -> for each possible action:
    -> evaluation variable assigned to - infinity intially
    -> call the max value function (max turn) and choose the min between current evalutation and returned value from max

    if (winner == max) -> return Action_value = 1
    if (winner == min) -> return Action_value = -1
    if (winner == tie) -> return Action_value = 0
*/

terminal(State, StateUtil, Action_value):-
    CheckWinner = StateUtil.check_winner,
    call(CheckWinner,State, Winner),
    Winner = r,
    Action_value is 1.

terminal(State, StateUtil, Action_value):-
    CheckWinner = StateUtil.check_winner,
    call(CheckWinner,State, Winner),
    Winner = b,
    Action_value is -1.

terminal(State, StateUtil, Action_value):-
    CheckWinner = StateUtil.check_winner,
    call(CheckWinner,State, Winner),
    Winner = #,
    Action_value is 0.

/*
-> inside the min_value function.
-> for loop on all the successors under the state sent to min_value
-> inside for loop: call max_value function to evaluate the branch of the current successor state 
-> return the evaluation of the branch to be an element in the Action_values array

 Obj[] Successors = get_successors();
int Current_Best_Evaluation_For_Min = inf;
for(Obj successor in Successors){
    branch_evaluation = max_value(successor,StateUtil);
    if (branch_evaluation < Current_Best_Evaluation_For_Min)
        Current_Best_Evaluation_For_Min = branch_evaluation;
}
return Current_Best_Evaluation_For_Min; -> this is returned as an element in the Action_values array.

*/

min_value(State, StateUtil, Action_value):-
    terminal(State, StateUtil, Action_value),!. /* do we need the cut operator here ? */

min_value(State, StateUtil, Action_value):-
    /*Current player computer -> r*/
    get_successors(State, r, StateUtil, Successors),                
    call_max_recursive(Successors, StateUtil, Action_values),
    min_list(Action_values, Action_value).

max_value(State, StateUtil, Action_value):-
    terminal(State, StateUtil, Action_value),!.

max_value(State, StateUtil, Action_value):-
    /*Current player human -> b*/
    get_successors(State, b, StateUtil, Successors),
    call_min_recursive(Successors, StateUtil, Action_values).
    max_list(Action_values, Action_value).


/*heuristic function*/
get_heriustic(State, Turn, Value).