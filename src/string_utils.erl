-module(string_utils).
-compile(export_all).
 
flatten_string_list(List) when is_list(List) ->
    flatten_string_list(List, []).
 
flatten_string_list([H|T], Tail) when is_list(H) ->
    case io_lib:char_list(H) of
      false ->
        flatten_string_list(H, flatten_string_list(T, Tail));
      true ->
        [H | flatten_string_list(T, Tail)]
    end;
flatten_string_list([H|T], Tail) ->
    [H|flatten_string_list(T, Tail)];
flatten_string_list([], Tail) ->
    Tail.
