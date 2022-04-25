-module(main).

-export([start/0]).

start() -> 
    Tests = ["9 10",
             "0",
             "-9",
             "-900123",
             "9.5",
             "1.03",
             "-0.007", 
             "-0.007e+900",
             "-0.7e-012",
             "1E10",
             "\"\"",
             "\"\"\"",
             "\"\u1234\"",
             "\"test\\b\"",
             "{9e2",
             "}9e2",
             "[9e2",
             "]9e2",
             ":9e2"
            ],
    Result = lists:map(fun(X) -> 
                      nextToken(X, start, "")
              end, Tests),
    TestString = getFileContentsInString("randomJson.txt"),
    Result2 = getAllLexemes(TestString),
    Result3 = lists:append(lexemesToString(Result2)),
    io:format("test1(): ~p~n",[Result3]),
    file:write_file("jsonProcessed.txt", Result3)
    .


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
     stringStart,
     stringEscape,
     stringUnicodeU,
     stringUnicode1,
     stringUnicode2,
     stringUnicode3,
     stringEnd,
     true1,
     true2,
     true3,
     true4,
     false1,
     false2,
     false3,
     false4,
     false5,
     null1,
     null2,
     null3,
     null4,
     error
    ].
%% Literals | Keywords | Separators | reserverse words | operators | Error | EOF | Identifiers 
%% TODO: Add Line number and character number
nextToken([H|T], State, Spelling) -> 
    case State of
        start ->
            case H of
                $- -> nextToken(T, numSign, [H|Spelling]);
                $0 -> nextToken(T, numZero, [H|Spelling]);
                D when D >= $1, D =< $9 -> nextToken(T, numOneToNine, [H|Spelling]);
                $" -> nextToken(T, stringStart, [H|Spelling]);
                $t -> nextToken(T, true1, [H|Spelling]);
                $f -> nextToken(T, false1, [H|Spelling]);
                $n -> nextToken(T, null1, [H|Spelling]);
                WS when WS >= $\t, WS =< $\r -> nextToken(T, start, []);
                WS when WS == $\s -> nextToken(T, start, []);
                ${ -> {lbrace,  [H], T};
                $} -> {rbrace,  [H], T};
                $[ -> {lsquare, [H], T};
                $] -> {rsquare, [H], T};
                $: -> {colon,   [H], T};
                $, -> {comma,   [H], T};
                _ -> fail
            end;
        numSign ->
            case H of
                $0 -> nextToken(T, numZero, [H|Spelling]);
                D when D >= $1, D =< $9 -> nextToken(T, numOneToNine, [H|Spelling]);
                _ -> fail
            end;
        numZero -> 
            case H of
                $. -> nextToken(T, fracDot, [H|Spelling]);
                $e -> nextToken(T, expE, [H|Spelling]);
                $E -> nextToken(T, expE, [H|Spelling]);
                $: -> {int, lists:reverse(Spelling), [H|T]};
                $, -> {int, lists:reverse(Spelling), [H|T]};
                ${ -> {int, lists:reverse(Spelling), [H|T]};
                $} -> {int, lists:reverse(Spelling), [H|T]};
                $[ -> {int, lists:reverse(Spelling), [H|T]};
                $] -> {int, lists:reverse(Spelling), [H|T]};
                L when L >= $\t, L =< $\r -> {int, lists:reverse(Spelling), T};
                L when L == $\s -> {int, lists:reverse(Spelling), T};
                _ -> fail
            end;
        numOneToNine ->
            case H of
                D when D >= $0, D =< $9 -> nextToken(T, numOneToNine, [H|Spelling]);
                $. -> nextToken(T, fracDot, [H|Spelling]);
                $e -> nextToken(T, expE, [H|Spelling]);
                $E -> nextToken(T, expE, [H|Spelling]);
                L when L >= $\t, L =< $\r -> {int, lists:reverse(Spelling), T};
                L when L == $\s -> {int, lists:reverse(Spelling), T};
                $: -> {int, lists:reverse(Spelling), [H|T]};
                $, -> {int, lists:reverse(Spelling), [H|T]};
                ${ -> {int, lists:reverse(Spelling), [H|T]};
                $} -> {int, lists:reverse(Spelling), [H|T]};
                $[ -> {int, lists:reverse(Spelling), [H|T]};
                $] -> {int, lists:reverse(Spelling), [H|T]};
                _ -> fail
            end;
        fracDot -> 
            case H of
                D when D >= $0, D =< $9 -> nextToken(T, fracZeroToNine, [H|Spelling]);
                _ -> fail
            end;
        fracZeroToNine -> 
            case H of
                D when D >= $0, D =< $9 -> nextToken(T, fracZeroToNine, [H|Spelling]);
                $e -> nextToken(T, expE, [H|Spelling]);
                $E -> nextToken(T, expE, [H|Spelling]);
                L when L >= $\t, L =< $\r -> {int, lists:reverse(Spelling), T};
                L when L == $\s -> {int, lists:reverse(Spelling), T};
                $: -> {int, lists:reverse(Spelling), [H|T]};
                $, -> {int, lists:reverse(Spelling), [H|T]};
                ${ -> {int, lists:reverse(Spelling), [H|T]};
                $} -> {int, lists:reverse(Spelling), [H|T]};
                $[ -> {int, lists:reverse(Spelling), [H|T]};
                $] -> {int, lists:reverse(Spelling), [H|T]};
                _ -> fail
            end;
        expE ->
            case H of
                $+ -> nextToken(T, expSign, [H|Spelling]);
                $- -> nextToken(T, expSign, [H|Spelling]);
                D when D >= $0, D =< $9 -> nextToken(T, expZeroToNine, [H|Spelling]);
                _ -> fail
            end;
        expSign ->
            case H of
                D when D >= $0, D =< $9 -> nextToken(T, expZeroToNine, [H|Spelling]);
                _ -> fail
            end;
        expZeroToNine ->
            case H of
                D when D >= $0, D =< $9 -> nextToken(T, expZeroToNine, [H|Spelling]);
                L when L >= $\t, L =< $\r -> {int, lists:reverse(Spelling), T};
                L when L == $\s -> {int, lists:reverse(Spelling), T};
                $: -> {int, lists:reverse(Spelling), [H|T]};
                $, -> {int, lists:reverse(Spelling), [H|T]};
                ${ -> {int, lists:reverse(Spelling), [H|T]};
                $} -> {int, lists:reverse(Spelling), [H|T]};
                $[ -> {int, lists:reverse(Spelling), [H|T]};
                $] -> {int, lists:reverse(Spelling), [H|T]};
                _ -> fail
            end;
        stringStart ->
            case H of
                $" -> nextToken(T, stringEnd, [H|Spelling]);
                $\\ -> nextToken(T, stringEscape, [H|Spelling]);
                L when L >= 32, L =< 1114111 -> nextToken(T, stringStart, [H|Spelling]);
                _ -> fail
            end;
        stringEscape ->
            case H of
                $u -> nextToken(T, stringUnicodeU, [H|Spelling]);
                $" -> nextToken(T, stringStart, [H|Spelling]);
                $\ -> nextToken(T, stringStart, [H|Spelling]);
                $/ -> nextToken(T, stringStart, [H|Spelling]);
                $b -> nextToken(T, stringStart, [H|Spelling]);
                $f -> nextToken(T, stringStart, [H|Spelling]);
                $n -> nextToken(T, stringStart, [H|Spelling]);
                $r -> nextToken(T, stringStart, [H|Spelling]);
                $t -> nextToken(T, stringStart, [H|Spelling]);
                _ -> fail
            end;
        stringUnicodeU ->
            case H of
                SL when SL >= $a, SL =< $z -> nextToken(T, stringUnicode1, [H|Spelling]);
                BL when BL >= $A, BL =< $Z -> nextToken(T, stringUnicode1, [H|Spelling]);
                N when N >= $0, N =< $9 -> nextToken(T, stringUnicode1, [H|Spelling]);
                _ -> fail
            end;
        stringUnicode1 ->
            case H of
                SL when SL >= $a, SL =< $z -> nextToken(T, stringUnicode2, [H|Spelling]);
                BL when BL >= $A, BL =< $Z -> nextToken(T, stringUnicode2, [H|Spelling]);
                N when N >= $0, N =< $9 -> nextToken(T, stringUnicode2, [H|Spelling]);
                _ -> fail
            end;
        stringUnicode2 ->
            case H of
                SL when SL >= $a, SL =< $z -> nextToken(T, stringUnicode3, [H|Spelling]);
                BL when BL >= $A, BL =< $Z -> nextToken(T, stringUnicode3, [H|Spelling]);
                N when N >= $0, N =< $9 -> nextToken(T, stringUnicode3, [H|Spelling]);
                _ -> fail
            end;
        stringUnicode3 ->
            case H of
                SL when SL >= $a, SL =< $z -> nextToken(T, stringStart, [H|Spelling]);
                BL when BL >= $A, BL =< $Z -> nextToken(T, stringStart, [H|Spelling]);
                N when N >= $0, N =< $9 -> nextToken(T, stringUnicode1, [H|Spelling]);
                _ -> fail
            end;
        stringEnd ->
            case H of
                L when L >= $\t, L =< $\r -> {string, lists:reverse(Spelling), T};
                L when L == $\s -> {string, lists:reverse(Spelling), T};
                $: -> {string, lists:reverse(Spelling), [H|T]};
                $, -> {string, lists:reverse(Spelling), [H|T]};
                ${ -> {string, lists:reverse(Spelling), [H|T]};
                $} -> {string, lists:reverse(Spelling), [H|T]};
                $[ -> {string, lists:reverse(Spelling), [H|T]};
                $] -> {string, lists:reverse(Spelling), [H|T]};
                _ -> fail
            end;
        true1 -> 
            case H of
                $r -> nextToken(T,true2, [H|Spelling]);
                _ -> fail
            end;
        true2 ->
            case H of
                $u -> nextToken(T,true3, [H|Spelling]);
                _ -> fail
            end;
        true3 ->
            case H of
                $e -> nextToken(T,true4, [H|Spelling]);
                _ -> fail
            end;
        true4 ->
            case H of
                _ -> {true,lists:reverse(Spelling), [H|T]}
            end;
        false1 -> 
            case H of
                $a -> nextToken(T,false2, [H|Spelling]);
                _ -> fail
            end;
        false2 ->
            case H of
                $l -> nextToken(T,false3, [H|Spelling]);
                _ -> fail
            end;
        false3 ->
            case H of
                $s -> nextToken(T,false4, [H|Spelling]);
                _ -> fail
            end;
        false4 ->
            case H of
                $e -> nextToken(T,false5, [H|Spelling]);
                _ -> fail
            end;
        false5 ->
            case H of
                _ -> {false,lists:reverse(Spelling), [H|T]}
            end;
        null1 ->
            case H of
                $u -> nextToken(T, null2, [H|Spelling]);
                _ -> fail
            end;
        null2 ->
            case H of
                $l -> nextToken(T, null3, [H|Spelling]);
                _ -> fail
            end;
        null3 ->
            case H of
                $l -> nextToken(T, null4, [H|Spelling]);
                _ -> fail
            end;
        null4 ->
            case H of
                _ -> {null, lists:reverse(Spelling), [H|T]}
            end;
        _ -> {done, done, done}
    end;

nextToken([], State, Spelling) ->
    case State of
        numOneToNine -> {int, lists:reverse(Spelling), ""};
        numZero -> {int, lists:reverse(Spelling), ""};
        fracZeroToNine -> {float, lists:reverse(Spelling), ""};
        expZeroToNine -> {float, lists:reverse(Spelling), ""};
        stringEnd -> {string, lists:reverse(Spelling), ""};
        _ -> {fail, fail, fail}
    end.


getAllLexemes(String) ->
    Token = nextToken(String, start, []),
    case Token of
        {_,_,fail} -> [];
        {Lexeme, ReversedSpelling, FinalString} -> [{Lexeme, ReversedSpelling}|getAllLexemes(FinalString)];
        fail -> [fail]
    end
    .

getFileContentsInString(FileName) ->
    {ok, Data} = file:read_file(FileName),
    unicode:characters_to_list(Data).

lexemesToString([]) -> 
    [];
lexemesToString([H|T]=Lexemes) ->
    case  H of
        {int, Spelling} -> [Spelling|lexemesToString(T)];
        {float, Spelling} -> [Spelling|lexemesToString(T)];
        {string, Spelling} -> [Spelling|lexemesToString(T)];
        {rbrace, Spelling} -> [Spelling|lexemesToString(T)];
        {lbrace, Spelling} -> [Spelling|lexemesToString(T)];
        {colon, Spelling} -> [Spelling|lexemesToString(T)];
        {comma, Spelling} -> [Spelling|lexemesToString(T)];
        {lsquare, Spelling} -> [Spelling|lexemesToString(T)];
        {rsquare, Spelling} -> [Spelling|lexemesToString(T)];
        {false, Spelling} -> [Spelling|lexemesToString(T)];
        {true, Spelling} -> [Spelling|lexemesToString(T)];
        {null, Spelling} -> [Spelling|lexemesToString(T)];
        _ -> fail
    end.
