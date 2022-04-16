-module(main).

-export([start/0]).

start() -> 
    Tests = ["9 10", "0", "-9", "-900123", "9.5", "1.03", "-0.007", "-0.007e+900", "-0.7e-012"],
    Result = lists:map(fun(X) -> 
                      nextToken(X, start, "", [])
              end, Tests),
    io:format("test1(): ~p~n",[Result]).


states() -> 
    [
     start,
     numSign,
     numZero,
     numOneToNine,
     fracDot,
     fracZeroToNine,
     expE,
     expSign,
     expZeroToNine,
     error
    ].
%% Literals | Keywords | Separators | reserverse words | operators | Error | EOF | Identifiers 
%% TODO: Add Line number and character number
nextToken([H|T], State, Spelling, Lexemes) -> 
    case State of
        start ->
            case H of
                $- -> nextToken(T, numSign, [H|Spelling], Lexemes);
                $0 -> nextToken(T, numZero, [H|Spelling], Lexemes);
                D when D >= $1, D =< $9 -> nextToken(T, numOneToNine, [H|Spelling], Lexemes);
                _ -> fail
            end;
        numSign ->
            case H of
                $0 -> nextToken(T, numZero, [H|Spelling], Lexemes);
                D when D >= $1, D =< $9 -> nextToken(T, numOneToNine, [H|Spelling], Lexemes);
                _ -> fail
            end;
        numZero -> 
            case H of
                L when L >= $\t, L =< $\r -> {int, lists:reverse(Spelling), [H|T]};
                L when L == $\s -> {int, lists:reverse(Spelling), [H|T]};
                $. -> nextToken(T, fracDot, [H|Spelling], Lexemes);
                $e -> nextToken(T, expE, [H|Spelling], Lexemes);
                $E -> nextToken(T, expE, [H|Spelling], Lexemes);
                _ -> fail
            end;
        numOneToNine ->
            case H of
                L when L >= $\t, L =< $\r -> {int, lists:reverse(Spelling), [H|T]};
                L when L == $\s -> {int, lists:reverse(Spelling), [H|T]};
                D when D >= $0, D =< $9 -> nextToken(T, numOneToNine, [H|Spelling], Lexemes);
                $. -> nextToken(T, fracDot, [H|Spelling], Lexemes);
                $e -> nextToken(T, expE, [H|Spelling], Lexemes);
                $E -> nextToken(T, expE, [H|Spelling], Lexemes);
                _ -> fail
            end;
        fracDot -> 
            case H of
                D when D >= $0, D =< $9 -> nextToken(T, fracZeroToNine, [H|Spelling], Lexemes);
                _ -> fail
            end;
        fracZeroToNine -> 
            case H of
                D when D >= $0, D =< $9 -> nextToken(T, fracZeroToNine, [H|Spelling], Lexemes);
                L when L >= $\t, L =< $\r -> {int, lists:reverse(Spelling), [H|T]};
                L when L == $\s -> {int, lists:reverse(Spelling), [H|T]};
                $e -> nextToken(T, expE, [H|Spelling], Lexemes);
                $E -> nextToken(T, expE, [H|Spelling], Lexemes);
                _ -> fail
            end;
        expE ->
            case H of
                $+ -> nextToken(T, expSign, [H|Spelling], Lexemes);
                $- -> nextToken(T, expSign, [H|Spelling], Lexemes);
                D when D >= $0, D =< $9 -> nextToken(T, expZeroToNine, [H|Spelling], Lexemes);
                _ -> fail
            end;
        expSign ->
            case H of
                D when D >= $0, D =< $9 -> nextToken(T, expZeroToNine, [H|Spelling], Lexemes);
                _ -> fail
            end;
        expZeroToNine ->
            case H of
                L when L >= $\t, L =< $\r -> {int, lists:reverse(Spelling), [H|T]};
                L when L == $\s -> {int, lists:reverse(Spelling), [H|T]};
                D when D >= $0, D =< $9 -> nextToken(T, expZeroToNine, [H|Spelling], Lexemes);
                _ -> fail
            end;
        _ -> {done, done, done}
    end;

nextToken([], State, Spelling, Lexemes) ->
    case State of
        error -> {fail, fail, fail};
        numOneToNine -> {int, lists:reverse(Spelling), State};
        numZero -> {int, lists:reverse(Spelling), State};
        fracZeroToNine -> {float, lists:reverse(Spelling), State};
        expZeroToNine -> {float, lists:reverse(Spelling), State};
        _ -> {fail, fail, fail}
    end.



getFileContentsInString(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary_to_list(Data).

