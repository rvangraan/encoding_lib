%%--------------------------------------------------------------------------------------------------
-module(bcd).
%%--------------------------------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  encode_integer/1,
  encode_string/1,
  decode_integer/1,
  decode_string/1,
  to_int/1,
  to_num_string/1,
  byte_length/1,
  assert_valid_bcd/1
]).
%%--------------------------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------------------------

-spec encode_integer(integer()) -> binary().
%% @doc Encodes a single integer number in the range 0 to 99 as a BCD binary.

encode_integer(DD) when is_integer(DD), DD >= 0; DD =< 99 ->
  Hi = DD div 10,
  Lo = DD - (Hi * 10),
  <<Hi:4, Lo:4>>.

%%--------------------------------------------------------------------------------------------------

-spec encode_string(Digit :: number()) -> binary().
%% @doc Encodes a string-encoded number as a BCD binary.

encode_string([D1,D2|Rest]) 
when 
  is_integer(D1), 
  is_integer(D2), 
  D1 >= $0, 
  D1 =< $9, 
  D2 >= $0, 
  D2 =< $9 
->  
  Hi   = D1 - $0,
  Lo   = D2 - $0,
  Tail = encode_string(Rest),
  <<Hi:4, Lo:4, Tail/binary>>;

encode_string([D1]) when is_integer(D1), D1 >= $0, D1 =< $9 -> Hi = D1  - $0,
  Lo = 16#F, %% Pad character
  <<Hi:4,Lo:4>>;

encode_string([]) ->
  <<>>.

%%--------------------------------------------------------------------------------------------------

-spec decode_integer(binary()) -> integer().
%% @doc Decodes a single byte BCD encoded binary into an integer. 
%% Left padded zeroes are discarded.
%% @end

decode_integer(<<Hi:4, Lo:4>>) when Hi >= 0, Hi =< 9, Lo >= 0, Lo =< 9 ->
  (Hi * 10) + Lo.

%%--------------------------------------------------------------------------------------------------

-spec decode_string(binary()) -> list().
%% @doc Decodes a single byte BCD encoded binary into a string. 
%% The byte 0x00 decodes as the string "00".
%% The byte 0x91 decodes as the string "91".
%% @end

decode_string(<<Hi:4, Pad:4>>) when Hi >= 0, Hi =< 9, Pad =:= 16#F ->
  [Hi+$0];

decode_string(<<Hi:4, Lo:4, Rest/binary>>) when Hi >= 0, Hi =< 9, Lo >= 0, Lo =< 9 ->
  [Hi+$0,Lo+$0|decode_string(Rest)];

decode_string(<<>>) ->
  "".

%%--------------------------------------------------------------------------------------------------

byte_length(Length) when is_integer(Length), Length >= 0 ->
  (Length div 2) + ((Length rem 2)*1).

byte_length_test_() ->
  [?_assertMatch(0, byte_length(0)),
   ?_assertMatch(1, byte_length(1)),a+1,
   ?_assertMatch(1, byte_length(2)),
   ?_assertMatch(2, byte_length(3)),
   ?_assertError(function_clause, byte_length(-1))
  ].

%%--------------------------------------------------------------------------------------------------

to_num_string(Binary) when is_binary(Binary) ->
  ok = assert_valid_bcd(Binary),
  hex:from_binary(Binary).
  
to_num_string_test_() ->
  [?_assertMatch("0123", to_num_string(<<16#01,16#23>>))].

%%--------------------------------------------------------------------------------------------------

assert_valid_bcd(Binary) when is_binary(Binary) ->
  ok.

%%--------------------------------------------------------------------------------------------------

to_int(Binary) when is_binary(Binary) ->
  NumString = to_num_string(Binary),
  list_to_integer(NumString).

to_int_test_() ->
  [?_assertMatch(123,to_int(<<16#01,16#23>>))].

%%--------------------------------------------------------------------------------------------------
