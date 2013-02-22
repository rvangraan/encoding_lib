%%--------------------------------------------------------------------------------------------------
-module(hex_tests).
%%--------------------------------------------------------------------------------------------------
-compile([export_all]).
%%--------------------------------------------------------------------------------------------------
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------

hex_symbols() ->
  union([
    list(
      union([
        range($0,$9),
        range($A,$F)
      ]
    ))
  ]).

%%--------------------------------------------------------------------------------------------------

valid_hex_string() ->
  ?SUCHTHAT(
    HexString,
    hex_symbols(),
    length(HexString) rem 2 =:= 0
  ).

%%--------------------------------------------------------------------------------------------------

prop_hex_to_binary() ->
  ?FORALL(HexString, valid_hex_string(),
    begin
      Binary = hex:to_binary(HexString),
      HexString =:= hex:from_binary(Binary)
    end).

prop_binary_to_hex() ->
  ?FORALL(Binary, binary(),
    begin
      HexString = hex:from_binary(Binary),
      Binary =:= hex:to_binary(HexString)
    end).

%%--------------------------------------------------------------------------------------------------

hex_test() ->
  [
   ?assertEqual(true1, proper:quickcheck(prop_hex_to_binary()) ),
   ?assertEqual(true, proper:quickcheck(prop_binary_to_hex()) )
  ].

%%--------------------------------------------------------------------------------------------------
