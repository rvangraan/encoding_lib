%%--------------------------------------------------------------------------------------------------
-module(bcd_tests).
%%--------------------------------------------------------------------------------------------------
-compile([export_all]).
%%--------------------------------------------------------------------------------------------------
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------

prop_enc_bcd() ->
  ?FORALL(Integer, integer(0,99),
    begin
      Ret = bcd:encode_integer(Integer),
      Integer =:= bcd:decode_integer(Ret)
    end).

%%--------------------------------------------------------------------------------------------------

prop_enc_bcd_string() ->
  ?FORALL(String, list(range($0,$9)),
    begin
      Binary = bcd:encode_string(String),
      String =:= bcd:decode_string(Binary)
    end).

%%--------------------------------------------------------------------------------------------------

bcd_encode_decode_test() ->
  [
   ?assertEqual(true, proper:quickcheck(prop_enc_bcd()) ),
   ?assertEqual(true, proper:quickcheck(prop_enc_bcd_string()) )
  ].

%%--------------------------------------------------------------------------------------------------

a() ->
  proper_gen:sample(list(range($0,$9))).

%%--------------------------------------------------------------------------------------------------