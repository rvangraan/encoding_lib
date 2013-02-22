%%--------------------------------------------------------------------------------------------------
-module(ebcdic).
%%--------------------------------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  to_ascii/1,
  from_ascii/1
]).
%%--------------------------------------------------------------------------------------------------   
%% API
%%--------------------------------------------------------------------------------------------------   

-spec from_ascii(string()) -> string
               ;(binary()) -> binary().
%% @doc converts an ASCII encoded list or binary to EBCDIC encoding

from_ascii(List) when is_list(List) ->
  binary_to_list( from_ascii( list_to_binary( lists:flatten(List) )));

from_ascii(<<Hi:4,Lo:4,Rest/binary>>) ->
  Tab   = a2e_tab(Hi),
  EChar = lists:nth(Lo+1,Tab),
  ERest = from_ascii(Rest),
  <<EChar:8,ERest/binary>>;

from_ascii(<<>>) ->
  <<>>.

%%--------------------------------------------------------------------------------------------------   

-spec to_ascii(string()) -> string
             ;(binary()) -> binary().
%% @doc converts an EBCDIC encoded list or binary to ASCII encoding

to_ascii(List) when is_list(List) ->
  binary_to_list( to_ascii( list_to_binary( lists:flatten(List) )));

to_ascii(<<Hi:4,Lo:4,Rest/binary>>) ->
  Tab   = e2a_tab(Hi),
  EChar = lists:nth(Lo+1,Tab),
  ERest = to_ascii(Rest),
  <<EChar:8,ERest/binary>>;

to_ascii(<<>>) ->
  <<>>.
  
%%--------------------------------------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------------------------------------

a2e_tab(16#0) -> [16#00,16#01,16#02,16#03,16#37,16#2D,16#2E,16#2F,16#16,16#05,16#25,16#0B,16#0C,16#0D,16#0E,16#0F];
a2e_tab(16#1) -> [16#10,16#11,16#12,16#13,16#3C,16#3D,16#32,16#26,16#18,16#19,16#3F,16#27,16#1C,16#1D,16#1E,16#1F];
a2e_tab(16#2) -> [16#40,16#5A,16#7F,16#7B,16#5B,16#6C,16#50,16#7D,16#4D,16#5D,16#5C,16#4E,16#6B,16#60,16#4B,16#61];
a2e_tab(16#3) -> [16#F0,16#F1,16#F2,16#F3,16#F4,16#F5,16#F6,16#F7,16#F8,16#F9,16#7A,16#5E,16#4C,16#7E,16#6E,16#6F];
a2e_tab(16#4) -> [16#7C,16#C1,16#C2,16#C3,16#C4,16#C5,16#C6,16#C7,16#C8,16#C9,16#D1,16#D2,16#D3,16#D4,16#D5,16#D6];
a2e_tab(16#5) -> [16#D7,16#D8,16#D9,16#E2,16#E3,16#E4,16#E5,16#E6,16#E7,16#E8,16#E9,16#AD,16#E0,16#BD,16#5F,16#6D];
a2e_tab(16#6) -> [16#79,16#81,16#82,16#83,16#84,16#85,16#86,16#87,16#88,16#89,16#91,16#92,16#93,16#94,16#95,16#96];
a2e_tab(16#7) -> [16#97,16#98,16#99,16#A2,16#A3,16#A4,16#A5,16#A6,16#A7,16#A8,16#A9,16#C0,16#4F,16#D0,16#A1,16#07];
a2e_tab(16#8) -> [16#20,16#21,16#22,16#23,16#24,16#15,16#06,16#17,16#28,16#29,16#2A,16#2B,16#2C,16#09,16#0A,16#1B];
a2e_tab(16#9) -> [16#30,16#31,16#1A,16#33,16#34,16#35,16#36,16#08,16#38,16#39,16#3A,16#3B,16#04,16#14,16#3E,16#E1];
a2e_tab(16#A) -> [16#41,16#42,16#43,16#44,16#45,16#46,16#47,16#48,16#49,16#51,16#52,16#53,16#54,16#55,16#56,16#57];
a2e_tab(16#B) -> [16#58,16#59,16#62,16#63,16#64,16#65,16#66,16#67,16#68,16#69,16#70,16#71,16#72,16#73,16#74,16#75];
a2e_tab(16#C) -> [16#76,16#77,16#78,16#80,16#8A,16#8B,16#8C,16#8D,16#8E,16#8F,16#90,16#9A,16#9B,16#9C,16#9D,16#9E];
a2e_tab(16#D) -> [16#9F,16#A0,16#AA,16#AB,16#AC,16#4A,16#AE,16#AF,16#B0,16#B1,16#B2,16#B3,16#B4,16#B5,16#B6,16#B7];
a2e_tab(16#E) -> [16#B8,16#B9,16#BA,16#BB,16#BC,16#6A,16#BE,16#BF,16#CA,16#CB,16#CC,16#CD,16#CE,16#CF,16#DA,16#DB];
a2e_tab(16#F) -> [16#DC,16#DD,16#DE,16#DF,16#EA,16#EB,16#EC,16#ED,16#EE,16#EF,16#FA,16#FB,16#FC,16#FD,16#FE,16#FF].

e2a_tab(16#0) -> [16#00,16#01,16#02,16#03,16#9C,16#09,16#86,16#7F,16#97,16#8D,16#8E,16#0B,16#0C,16#0D,16#0E,16#0F];
e2a_tab(16#1) -> [16#10,16#11,16#12,16#13,16#9D,16#85,16#08,16#87,16#18,16#19,16#92,16#8F,16#1C,16#1D,16#1E,16#1F];
e2a_tab(16#2) -> [16#80,16#81,16#82,16#83,16#84,16#0A,16#17,16#1B,16#88,16#89,16#8A,16#8B,16#8C,16#05,16#06,16#07];
e2a_tab(16#3) -> [16#90,16#91,16#16,16#93,16#94,16#95,16#96,16#04,16#98,16#99,16#9A,16#9B,16#14,16#15,16#9E,16#1A];
e2a_tab(16#4) -> [16#20,16#A0,16#A1,16#A2,16#A3,16#A4,16#A5,16#A6,16#A7,16#A8,16#D5,16#2E,16#3C,16#28,16#2B,16#7C];
e2a_tab(16#5) -> [16#26,16#A9,16#AA,16#AB,16#AC,16#AD,16#AE,16#AF,16#B0,16#B1,16#21,16#24,16#2A,16#29,16#3B,16#5E];
e2a_tab(16#6) -> [16#2D,16#2F,16#B2,16#B3,16#B4,16#B5,16#B6,16#B7,16#B8,16#B9,16#E5,16#2C,16#25,16#5F,16#3E,16#3F];
e2a_tab(16#7) -> [16#BA,16#BB,16#BC,16#BD,16#BE,16#BF,16#C0,16#C1,16#C2,16#60,16#3A,16#23,16#40,16#27,16#3D,16#22];
e2a_tab(16#8) -> [16#C3,16#61,16#62,16#63,16#64,16#65,16#66,16#67,16#68,16#69,16#C4,16#C5,16#C6,16#C7,16#C8,16#C9];
e2a_tab(16#9) -> [16#CA,16#6A,16#6B,16#6C,16#6D,16#6E,16#6F,16#70,16#71,16#72,16#CB,16#CC,16#CD,16#CE,16#CF,16#D0];
e2a_tab(16#A) -> [16#D1,16#7E,16#73,16#74,16#75,16#76,16#77,16#78,16#79,16#7A,16#D2,16#D3,16#D4,16#5B,16#D6,16#D7];
e2a_tab(16#B) -> [16#D8,16#D9,16#DA,16#DB,16#DC,16#DD,16#DE,16#DF,16#E0,16#E1,16#E2,16#E3,16#E4,16#5D,16#E6,16#E7];
e2a_tab(16#C) -> [16#7B,16#41,16#42,16#43,16#44,16#45,16#46,16#47,16#48,16#49,16#E8,16#E9,16#EA,16#EB,16#EC,16#ED];
e2a_tab(16#D) -> [16#7D,16#4A,16#4B,16#4C,16#4D,16#4E,16#4F,16#50,16#51,16#52,16#EE,16#EF,16#F0,16#F1,16#F2,16#F3];
e2a_tab(16#E) -> [16#5C,16#9F,16#53,16#54,16#55,16#56,16#57,16#58,16#59,16#5A,16#F4,16#F5,16#F6,16#F7,16#F8,16#F9];
e2a_tab(16#F) -> [16#30,16#31,16#32,16#33,16#34,16#35,16#36,16#37,16#38,16#39,16#FA,16#FB,16#FC,16#FD,16#FE,16#FF].

%%--------------------------------------------------------------------------------------------------