%%--------------------------------------------------------------------------------------------------
-module(hex).
%%--------------------------------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  from_binary/1,
  to_binary/1
]).
%%--------------------------------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------------------------------

-spec from_binary( Binary::binary() ) -> string().
%% @doc converts Binary into its hexadecimal representation
         
from_binary(<<Hi:4,Lo:4,Rest/binary>>) ->
  [int_to_char(Hi), int_to_char(Lo) | from_binary(Rest) ];

from_binary(<<>>) ->
  [].

%%--------------------------------------------------------------------------------------------------

-spec to_binary( HexString::string() ) -> binary().
%% @doc converts the hexadecimal string HexString into its binary representation

to_binary(Binary) when is_binary(Binary) ->
  to_binary( binary_to_list(Binary) );

to_binary([HiChar,LoChar|Rest]) ->
  Hi = char_to_int(HiChar),
  Lo = char_to_int(LoChar),
  ERest = to_binary(Rest),
  <<Hi:4, Lo:4, ERest/binary>>;

to_binary([]) ->
  <<>>.

%%--------------------------------------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------------------------------------

int_to_char(Int) when is_integer(Int), Int >= 0, Int =< 9 -> $0 + Int;

int_to_char(Int) when is_integer(Int), Int > 9, Int =< 15 ->
  (Int - 10) + $A.

%%--------------------------------------------------------------------------------------------------

char_to_int(Char) when is_integer(Char), Char >= $0, Char =< $9 ->
  Char - $0;

char_to_int(Char) when is_integer(Char), Char >= $A, Char =< $F ->
  (10 + (Char - $A));

char_to_int(Invalid) ->
  throw({invalid_hex_character, [Invalid] }).

%%--------------------------------------------------------------------------------------------------  
%% Test
%%--------------------------------------------------------------------------------------------------  

specs_test_() ->
  [?_test( proper:check_specs(?MODULE) )].

%%--------------------------------------------------------------------------------------------------