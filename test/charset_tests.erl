-module(charset_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BITS_PER_BYTE, 8).

-define(CHAR_SETS, [charset64, charset32, charset16, charset8, charset4, charset2]).

bits_per_char_test() ->
  lists:foreach(
    fun(CharSet) ->
        Actual = entropy_string:bits_per_char(CharSet),
        Expected = round(math:log2(byte_size(entropy_string:charset(CharSet)))),
        ?assertEqual(Expected, Actual)
    end, ?CHAR_SETS).

bytes_needed_test() ->
  lists:foreach(
    fun(Bits) ->
        lists:foreach(
          fun(CharSet) ->
              BytesNeeded = entropy_string:bytes_needed(Bits, CharSet),
              AtLeast = ceil(Bits / ?BITS_PER_BYTE),
              ?assert(AtLeast =< BytesNeeded),
              ?assert(BytesNeeded =< AtLeast+1)
          end,
          ?CHAR_SETS)
    end,
    lists:seq(0,17)).

charset_len_test() ->
  ?assertEqual(64, byte_size(entropy_string:charset(charset64))),
  ?assertEqual(32, byte_size(entropy_string:charset(charset32))),
  ?assertEqual(16, byte_size(entropy_string:charset(charset16))),
  ?assertEqual( 8, byte_size(entropy_string:charset(charset8))),
  ?assertEqual( 4, byte_size(entropy_string:charset(charset4))),
  ?assertEqual( 2, byte_size(entropy_string:charset(charset2))).

invalid_charset_test() ->
  lists:foreach(
    fun(N) ->
        {error, _} = entropy_string:random_string(16, binary:list_to_bin(lists:seq(1,N)))
    end,
    lists:filter(fun(N) -> round(math:log2(N)) /= math:log2(N) end, lists:seq(1,65))).

%% Prevent use of erlang:ceil which is not available until OTP 20
-compile({no_auto_import,[ceil/1]}).

%%--------------------------------------------------------------------------------------------------
%%
%% ceil/1
%% 
%%   Needed for OTP < 20
%%
%%--------------------------------------------------------------------------------------------------
ceil(X) when X < 0 ->
  trunc(X);
ceil(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.
