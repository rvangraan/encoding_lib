%%--------------------------------------------------------------------------------------------------
-module(ebcdic_tests).
%%--------------------------------------------------------------------------------------------------
-compile([export_all]).
%%--------------------------------------------------------------------------------------------------
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------

ascii_lower_case() ->
  list( range($a,$z) ).

ascii_upper_case() ->
  list( range($A,$Z) ).

ascii_numbers() ->
  list( range($0,$9) ).

ascii_symbols() ->
  list(
    union([
      range(32,32),   % space
      range(33,47),   % ! through to /
      range(58,64),   % : through @
      range(91,96),   % [ through `
      range(123,126)  % { through ~
  ])).

%%--------------------------------------------------------------------------------------------------
  
simple_ascii_generator() ->
  union([
    ascii_lower_case(),
    ascii_upper_case(),
    ascii_numbers(),
    ascii_symbols()
  ]).

%%--------------------------------------------------------------------------------------------------

ebcdic_lower_case() ->
  list(
    union([
      range(129,137),
      range(145,153),
      range(162,169)
  ])).

ebcdic_upper_case() ->
  list(
    union([
      range(193,201),
      range(209,217),
      range(226,233)
  ])).

ebcdic_numbers() ->
  list( range(240,249) ).

ebcdic_symbols() ->
  list(
    union([
      range(40,40),   % space
      range(75,80),   % .<(+|&
      range(90,97),   % !$*);¬-/
      range(106,111), % ¦,%_>?
      range(121,127), % :#@'="
      range(161,161), % ~
      range(176,176), % ^
      range(186,187), % []
      range(192,192), % {
      range(208,208), % }
      range(224,224)  % \
  ])).

%%--------------------------------------------------------------------------------------------------       

simple_ebcdic_generator() ->
  union([
    ebcdic_lower_case(),
    ebcdic_upper_case(),
    ebcdic_numbers(),
    ebcdic_symbols()
  ]).

%%--------------------------------------------------------------------------------------------------

prop_from_ascii_to_ebcdic() ->
  ?FORALL(String, simple_ascii_generator(),
    begin
      EBCDIC = ebcdic:from_ascii(String),
      String =:= ebcdic:to_ascii(EBCDIC)
    end).

prop_from_ebcdic_to_ascii() ->
  ?FORALL(String, simple_ascii_generator(),
    begin
      EBCDIC = ebcdic:from_ascii(String),
      String =:= ebcdic:to_ascii(EBCDIC)
    end).

ebcdic_test() ->
  [
   ?assertEqual(true, proper:quickcheck(prop_from_ascii_to_ebcdic()) ),
   ?assertEqual(true, proper:quickcheck(prop_from_ebcdic_to_ascii()) )
  ].

%%--------------------------------------------------------------------------------------------------