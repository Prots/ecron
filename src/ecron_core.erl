-module(ecron_core).
-author("prots.igor@gmail.com").

%% API
-export([parse_spec/1]).
-export([is_time_to_go/2]).

%% # * * * * * * command to execute
%% # │ │ │ │ │ |
%% # │ │ │ │ │ └ year (> 0)
%% # │ │ │ │ └───── day of week (0 - 6) (0 to 6 are Sunday to Saturday, or use names; 7 is Sunday, the same as 0)
%% # │ │ │ └────────── month (1 - 12)
%% # │ │ └─────────────── day of month (1 - 31)
%% # │ └──────────────────── hour (0 - 23)
%% # └───────────────────────── min (0 - 59)

is_time_to_go(DateTime, Spec) ->
    case parse_spec(Spec) of
        {ok, PreparedSpec} ->
            is_time_to_go(fun go/2, DateTime,  PreparedSpec);
        {error, Reason} ->
            {error, Reason}
    end.

parse_spec(Spec) ->
    StrTokenList = string:tokens(Spec, " "),
    TokenList = [string_to_int(Token) || Token <- StrTokenList],
    ForCheck = lists:zip(units(length(TokenList)), TokenList),
    Res = case try_all(fun validate/1, ForCheck) of
        ok ->
            {ok, prepare(ForCheck)};
        {error, Rsn} ->
            {error, Rsn}
    end,
%%     io:format("Parsing result: ~p~n", [Res]),
    Res.

%% Internal

is_time_to_go(Fun, DateTime, [H|T]) ->
    case Fun(H, DateTime) of
        true    -> is_time_to_go(Fun, DateTime, T);
        false   -> false
    end;
is_time_to_go(_, _, []) -> true.

prepare(TokenList) ->
    prepare(TokenList, []).

prepare([], Acc) ->
    Acc;
prepare([{month, Month}|Rest], Acc) when is_list(Month) ->
    prepare(Rest, [{month, month_to_int(Month)}|Acc]);
prepare([{day_of_week, Day}|Rest], Acc) when is_list(Day) ->
    prepare(Rest, [{day_of_week, day_to_int(Day)}|Acc]);
prepare([Head|Rest], Acc) ->
    prepare(Rest, [Head|Acc]).

units(5) -> [minute, hour, day_of_month, month, day_of_week];
units(6) -> [minute, hour, day_of_month, month, day_of_week, year];
units(_) -> [].

string_to_int(String) ->
    try
        list_to_integer(String)
    catch
        _:_ -> string:to_lower(String)
    end.

validate({_, "*"})                                              -> ok;
validate({year, Y}) when is_integer(Y), Y >= 0                  -> ok;
validate({year, _})                                             -> {error, year};
validate({month, "jan"})                                        -> ok;
validate({month, "feb"})                                        -> ok;
validate({month, "mar"})                                        -> ok;
validate({month, "apr"})                                        -> ok;
validate({month, "may"})                                        -> ok;
validate({month, "jun"})                                        -> ok;
validate({month, "jul"})                                        -> ok;
validate({month, "aug"})                                        -> ok;
validate({month, "sep"})                                        -> ok;
validate({month, "oct"})                                        -> ok;
validate({month, "nov"})                                        -> ok;
validate({month, "dec"})                                        -> ok;
validate({month, M}) when is_integer(M), M >= 1, M =<12         -> ok;
validate({month, _})                                            -> {error, month};
validate({day_of_week, "mon"})                                  -> ok;
validate({day_of_week, "tue"})                                  -> ok;
validate({day_of_week, "wed"})                                  -> ok;
validate({day_of_week, "thu"})                                  -> ok;
validate({day_of_week, "fri"})                                  -> ok;
validate({day_of_week, "sat"})                                  -> ok;
validate({day_of_week, "sun"})                                  -> ok;
validate({day_of_week, D}) when is_integer(D), D >= 1, D =< 7   -> ok;
validate({day_of_week, _})                                      -> {error, day_of_week};
validate({day_of_month, D}) when is_integer(D), D >= 1, D =< 31 -> ok;
validate({day_of_month, _})                                     -> {error, day_of_month};
validate({hour, H}) when is_integer(H), H >= 0, H =< 23         -> ok;
validate({hour, _})                                             -> {error, hour};
validate({minute, M}) when is_integer(M), M >= 0, M =< 59       -> ok;
validate({minute, _})                                           -> {error, minute}.

try_all(Fun, [H|T]) ->
    case Fun(H) of
        ok           -> try_all(Fun, T);
        {error, Rsn} -> {error, Rsn}
    end;
try_all(_Fun, []) -> ok.

day_to_int("*") -> "*";
day_to_int("mon") -> 1;
day_to_int("tue") -> 2;
day_to_int("wed") -> 3;
day_to_int("thu") -> 4;
day_to_int("fri") -> 5;
day_to_int("sat") -> 6;
day_to_int("sun") -> 7.

month_to_int("*") -> "*";
month_to_int("jan") -> 1;
month_to_int("feb") -> 2;
month_to_int("mar") -> 3;
month_to_int("apr") -> 4;
month_to_int("may") -> 5;
month_to_int("jun") -> 6;
month_to_int("jul") -> 7;
month_to_int("aug") -> 8;
month_to_int("sep") -> 9;
month_to_int("oct") -> 10;
month_to_int("nov") -> 11;
month_to_int("dec") -> 12.


go({year, "*"}, _DateTime) -> true;
go({year, Y}, {{Y, _, _}, {_, _, _}}) -> true;
go({year, _}, _) -> false;
go({month, "*"}, _DateTime) -> true;
go({month, M}, {{_, M, _}, {_, _, _}}) -> true;
go({month, _}, _DateTime) -> false;
go({day_of_month, "*"}, _DateTime) -> true;
go({day_of_month, D}, {{_, _, D}, {_, _, _}}) -> true;
go({day_of_month, _}, _DateTime) -> false;
go({hour, "*"}, _DateTime) -> true;
go({hour, H}, {{_, _, _}, {H, _, _}}) -> true;
go({hour, _}, _DateTime) -> false;
go({minute, "*"}, _DateTime) -> true;
go({minute, M}, {{_, _, _}, {_, M, _}}) -> true;
go({minute, _}, _DateTime) -> false;
go({day_of_week, "*"}, _DateTime) -> true;
go({day_of_week, D}, {Date, _}) -> calendar:day_of_the_week(Date) =:= D;
go({day_of_week, _}, _DateTime) -> false;
go(_, _) -> false.


