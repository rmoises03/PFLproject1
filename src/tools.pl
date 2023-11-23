% replaces Element of old list at index
% replace_list(+Old, +Index, +Ele, -New)
replace_list([_|T], 0, X, [X|T]).
replace_list([H|T], I, X, [H|R]):- 
    I > 0, 
    I1 is I-1, 
    replace_list(T, I1, X, R).

% places element on matrix at specific coordenate
% replace_matrix(+Old, +IndX, +IndY, +Ele, -New)	
replace_matrix([List|T], IndX, 0, Ele, [NewList|T]):-
	replace_list(List, IndX, Ele, NewList).
replace_matrix([List|T], IndX, IndY, Ele, [List|R]):-
	IndY > 0,
	IndY1 is IndY-1,
	replace_matrix(T, IndX, IndY1, Ele, R).

% retireves element from specific position of the matrix 
% at(+Matrix, +IndX, +IndY, -Val)
at(Matrix, IndX, IndY, Val) :-
	nth0(IndY, Matrix, Y1), 
	nth0(IndX, Y1, Val).


% Auxiliary function to verify if char is between 0 and 9
% verify_number(+Ch, -N)
verify_number(Ch, N) :-
	char_code(Ch, Code),
    Code >= 48, Code =< 57, % ASCII codes for digits 0 to 9
        N is Code - 48. % Convert ASCII code to the corresponding digit


% digits_to_number(+List, -Number)
digits_to_number(Digits, Number) :-
    reverse(Digits, ReversedDigits),
    digits_to_number_helper(ReversedDigits, 1, Number).

digits_to_number_helper([], _, 0).
digits_to_number_helper([Digit|Rest], Position, Number) :-
    NewPosition is Position * 10,
    digits_to_number_helper(Rest, NewPosition, RestNumber),
    Number is Digit * Position + RestNumber.
