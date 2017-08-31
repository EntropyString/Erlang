-module(charset_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BITS_PER_BYTE, 8).
-define(CHAR_SET_64, entropy_string:charset64()).
-define(CHAR_SET_32, entropy_string:charset32()).
-define(CHAR_SET_16, entropy_string:charset16()).
-define(CHAR_SET_8,  entropy_string:charset8()).
-define(CHAR_SET_4,  entropy_string:charset4()).
-define(CHAR_SET_2,  entropy_string:charset2()).
-define(CHAR_SETS,
        [?CHAR_SET_64, ?CHAR_SET_32, ?CHAR_SET_16, ?CHAR_SET_8, ?CHAR_SET_4, ?CHAR_SET_2]).

%% charset_test() ->
%%   ?assertEqual(?CHAR_SET_64, entropy_string:charset64()),
%%   ?assertEqual(?CHAR_SET_32, entropy_string:charset32()),
%%   ?assertEqual(?CHAR_SET_16, entropy_string:charset16()),
%%   ?assertEqual(?CHAR_SET_8,  entropy_string:charset8()),
%%   ?assertEqual(?CHAR_SET_4,  entropy_string:charset4()),
%%   ?assertEqual(?CHAR_SET_2,  entropy_string:charset2()).

bits_per_char_test() ->
  lists:foreach(
    fun(CharSet) ->
        Actual = entropy_string:bits_per_char(CharSet),
        Expected = round(math:log2(byte_size(CharSet))),
        ?assertEqual(Expected, Actual)
    end, ?CHAR_SETS).

bytes_needed_test() ->
  lists:foreach(
    fun(Bits) ->
        lists:foreach(
          fun(CharSet) ->
              BytesNeeded = entropy_string:bytes_needed(Bits, CharSet),
              AtLeast = math:ceil(Bits / ?BITS_PER_BYTE),
              ?assert(AtLeast =< BytesNeeded),
              ?assert(BytesNeeded =< AtLeast+1)
          end,
          ?CHAR_SETS)
    end,
    lists:seq(0,17)).

charset_len_test() ->
  ?assertEqual(64, byte_size(?CHAR_SET_64)),
  ?assertEqual(32, byte_size(?CHAR_SET_32)),
  ?assertEqual(16, byte_size(?CHAR_SET_16)),
  ?assertEqual( 8, byte_size(?CHAR_SET_8)),
  ?assertEqual( 4, byte_size(?CHAR_SET_4)),
  ?assertEqual( 2, byte_size(?CHAR_SET_2)).

invalid_charset_test() ->
  lists:foreach(
    fun(N) ->
        {error, _} = entropy_string:random_string(16, binary:list_to_bin(lists:seq(1,N)))
    end,
    lists:filter(fun(N) -> round(math:log2(N)) /= math:log2(N) end, lists:seq(1,65))).
