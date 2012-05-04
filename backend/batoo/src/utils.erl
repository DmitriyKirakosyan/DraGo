-module(utils).
-compile(export_all).

milliseconds_diff(T2, T1) ->
    timer:now_diff(T2, T1) div 1000.

milliseconds_now() ->
    {Mg, S, Mc} = now(),
    (Mg * 1000000000) + (S * 1000) + (Mc div 1000).

hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X])).
