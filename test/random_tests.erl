-module(random_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHAR_SET_64, entropy_string:charset64()).
-define(CHAR_SET_32, entropy_string:charset32()).
-define(CHAR_SET_16, entropy_string:charset16()).
-define(CHAR_SET_8,  entropy_string:charset8()).
-define(CHAR_SET_4,  entropy_string:charset4()).
-define(CHAR_SET_2,  entropy_string:charset2()).

base_64_string_test() ->
  ?assertEqual(<<"3">>,            entropy_string:random_string( 6, ?CHAR_SET_64, <<16#dd:8>>)),
  ?assertEqual(<<"eP">>,           entropy_string:random_string(12, ?CHAR_SET_64, <<16#78fc:16>>)),
  ?assertEqual(<<"xW8">>,          entropy_string:random_string(18, ?CHAR_SET_64,
                                                                <<16#c56f21:24>>)),
  ?assertEqual(<<"yWjH">>,         entropy_string:random_string(24, ?CHAR_SET_64,
                                                                <<16#c968c7:24>>)),
  ?assertEqual(<<"pWIgh">>,        entropy_string:random_string(30, ?CHAR_SET_64,
                                                                <<16#a5622087:32>>)),
  ?assertEqual(<<"OVHKzI">>,       entropy_string:random_string(36, ?CHAR_SET_64,
                                                                <<16#3951cacc8b:40>>)),
  ?assertEqual(<<"g4kAx_Q">>,      entropy_string:random_string(42, ?CHAR_SET_64,
                                                                <<16#838900c7f402:48>>)),
  ?assertEqual(<<"Ubyox8kX">>,     entropy_string:random_string(48, ?CHAR_SET_64,
                                                                <<16#51bca8c7c917:48>>)),
  ?assertEqual(<<"0uPp2hmXU">>,    entropy_string:random_string(54, ?CHAR_SET_64,
                                                                <<16#d2e3e9da199752:56>>)),
  ?assertEqual(<<"2TnBrx4uaU">>,   entropy_string:random_string(60, ?CHAR_SET_64,
                                                                <<16#d939c1af1e2e6948:64>>)),
  ?assertEqual(<<"eD_9k9EGkEv">>,  entropy_string:random_string(66, ?CHAR_SET_64,
                                                                <<16#783ffd93d106904bd6:72>>)),
  ?assertEqual(<<"nZlOpdI_jIaA">>, entropy_string:random_string(72, ?CHAR_SET_64,
                                                                <<16#9d994ea5d23f8c8680:72>>)).
base_32_string_test() ->
  ?assertEqual(<<"N">>,           entropy_string:random_string( 5, ?CHAR_SET_32, <<16#dd:8>>)),
  ?assertEqual(<<"p6">>,          entropy_string:random_string(10, ?CHAR_SET_32, <<16#78fc:16>>)),
  ?assertEqual(<<"p6R">>,         entropy_string:random_string(15, ?CHAR_SET_32, <<16#78fc:16>>)),
  ?assertEqual(<<"JFHt">>,        entropy_string:random_string(20, ?CHAR_SET_32, <<16#c56f21:24>>)),
  ?assertEqual(<<"DFr43">>,       entropy_string:random_string(25, ?CHAR_SET_32,
                                                               <<16#a5622087:32>>)),
  ?assertEqual(<<"DFr433">>,      entropy_string:random_string(30, ?CHAR_SET_32,
                                                               <<16#a5622087:32>>)),
  ?assertEqual(<<"b8dPFB7">>,     entropy_string:random_string(35, ?CHAR_SET_32,
                                                               <<16#3951cacc8b:40>>)),
  ?assertEqual(<<"b8dPFB7h">>,    entropy_string:random_string(40, ?CHAR_SET_32,
                                                               <<16#3951cacc8b:40>>)),
  ?assertEqual(<<"qn7q3rTD2">>,   entropy_string:random_string(45, ?CHAR_SET_32,
                                                               <<16#838900c7f402:48>>)),
  ?assertEqual(<<"MhrRBGqLtQ">>,  entropy_string:random_string(50, ?CHAR_SET_32,
                                                               <<16#d2e3e9da199752:56>>)),
  ?assertEqual(<<"MhrRBGqLtQf">>, entropy_string:random_string(55, ?CHAR_SET_32,
                                                               <<16#d2e3e9da199752:56>>)).

base_16_string_test() ->
  ?assertEqual(<<"9">>,          entropy_string:random_string( 4, ?CHAR_SET_16, <<16#9d:8>>)),
  ?assertEqual(<<"ae">>,         entropy_string:random_string( 8, ?CHAR_SET_16, <<16#ae:8>>)),
  ?assertEqual(<<"01f">>,        entropy_string:random_string(12, ?CHAR_SET_16, <<16#01f2:16>>)),
  ?assertEqual(<<"c7c9">>,       entropy_string:random_string(16, ?CHAR_SET_16, <<16#c7c9:16>>)),
  ?assertEqual(<<"c7c90">>,      entropy_string:random_string(20, ?CHAR_SET_16, <<16#c7c900:24>>)).

base_8_string_test() ->
  ?assertEqual(<<"2">>,          entropy_string:random_string( 3, ?CHAR_SET_8, <<16#5a:8>>)),
  ?assertEqual(<<"26">>,         entropy_string:random_string( 6, ?CHAR_SET_8, <<16#5a:8>>)),
  ?assertEqual(<<"103">>,        entropy_string:random_string( 9, ?CHAR_SET_8, <<16#21a4:16>>)),
  ?assertEqual(<<"1032">>,       entropy_string:random_string(12, ?CHAR_SET_8, <<16#21a4:16>>)),
  ?assertEqual(<<"66414">>,      entropy_string:random_string(15, ?CHAR_SET_8, <<16#da19:16>>)),
  ?assertEqual(<<"773117">>,     entropy_string:random_string(18, ?CHAR_SET_8, <<16#fd93d1:24>>)),
  ?assertEqual(<<"7731172">>,    entropy_string:random_string(21, ?CHAR_SET_8, <<16#fd93d1:24>>)),
  ?assertEqual(<<"77311721">>,   entropy_string:random_string(24, ?CHAR_SET_8, <<16#fd93d1:24>>)),
  ?assertEqual(<<"617444076">>,  entropy_string:random_string(27, ?CHAR_SET_8, <<16#c7c907c9:32>>)),
  ?assertEqual(<<"6174440762">>, entropy_string:random_string(30, ?CHAR_SET_8, <<16#c7c907c9:32>>)).  
  
base_4_string_test() ->
  ?assertEqual(<<"T">>,        entropy_string:random_string( 2, ?CHAR_SET_4, <<16#5a:8>>)),
  ?assertEqual(<<"TT">>,       entropy_string:random_string( 4, ?CHAR_SET_4, <<16#5a:8>>)),
  ?assertEqual(<<"CTA">>,      entropy_string:random_string( 6, ?CHAR_SET_4, <<16#93:8>>)),
  ?assertEqual(<<"CTAG">>,     entropy_string:random_string( 8, ?CHAR_SET_4, <<16#93:8>>)),
  ?assertEqual(<<"ACAAG">>,    entropy_string:random_string(10, ?CHAR_SET_4, <<16#20f1:16>>)),
  ?assertEqual(<<"ACAAGG">>,   entropy_string:random_string(12, ?CHAR_SET_4, <<16#20f1:16>>)),
  ?assertEqual(<<"ACAAGGA">>,  entropy_string:random_string(14, ?CHAR_SET_4, <<16#20f1:16>>)),
  ?assertEqual(<<"ACAAGGAT">>, entropy_string:random_string(16, ?CHAR_SET_4, <<16#20f1:16>>)).

base_2_string_test() ->
  ?assertEqual(<<"0">>,                entropy_string:random_string( 1, ?CHAR_SET_2, <<16#27:8>>)),
  ?assertEqual(<<"00">>,               entropy_string:random_string( 2, ?CHAR_SET_2, <<16#27:8>>)),
  ?assertEqual(<<"001">>,              entropy_string:random_string( 3, ?CHAR_SET_2, <<16#27:8>>)),
  ?assertEqual(<<"0010">>,             entropy_string:random_string( 4, ?CHAR_SET_2, <<16#27:8>>)),
  ?assertEqual(<<"00100">>,            entropy_string:random_string( 5, ?CHAR_SET_2, <<16#27:8>>)),
  ?assertEqual(<<"001001">>,           entropy_string:random_string( 6, ?CHAR_SET_2, <<16#27:8>>)),
  ?assertEqual(<<"0010011">>,          entropy_string:random_string( 7, ?CHAR_SET_2, <<16#27:8>>)),
  ?assertEqual(<<"00100111">>,         entropy_string:random_string( 8, ?CHAR_SET_2, <<16#27:8>>)),
  ?assertEqual(<<"111000111">>,        entropy_string:random_string( 9, ?CHAR_SET_2,
                                                                     <<16#e3e9:16>>)),
  ?assertEqual(<<"1110001111101001">>, entropy_string:random_string(16, ?CHAR_SET_2,
                                                                    <<16#e3e9:16>>)).

small_id_test() ->
  ?assertEqual( 6, byte_size(entropy_string:small_id())),
  
  ?assertEqual( 5, byte_size(entropy_string:small_id(?CHAR_SET_64))),
  ?assertEqual( 6, byte_size(entropy_string:small_id(?CHAR_SET_32))),
  ?assertEqual( 8, byte_size(entropy_string:small_id(?CHAR_SET_16))),
  ?assertEqual(10, byte_size(entropy_string:small_id(?CHAR_SET_8))),
  ?assertEqual(15, byte_size(entropy_string:small_id(?CHAR_SET_4))),
  ?assertEqual(29, byte_size(entropy_string:small_id(?CHAR_SET_2))).

medium_id_test() ->
  ?assertEqual(14, byte_size(entropy_string:medium_id())),
  
  ?assertEqual(12, byte_size(entropy_string:medium_id(?CHAR_SET_64))),
  ?assertEqual(14, byte_size(entropy_string:medium_id(?CHAR_SET_32))),
  ?assertEqual(18, byte_size(entropy_string:medium_id(?CHAR_SET_16))),
  ?assertEqual(23, byte_size(entropy_string:medium_id(?CHAR_SET_8))),
  ?assertEqual(35, byte_size(entropy_string:medium_id(?CHAR_SET_4))),
  ?assertEqual(69, byte_size(entropy_string:medium_id(?CHAR_SET_2))).

large_id_test() ->
  ?assertEqual(20, byte_size(entropy_string:large_id())),
  
  ?assertEqual(17, byte_size(entropy_string:large_id(?CHAR_SET_64))),
  ?assertEqual(20, byte_size(entropy_string:large_id(?CHAR_SET_32))),
  ?assertEqual(25, byte_size(entropy_string:large_id(?CHAR_SET_16))),
  ?assertEqual(33, byte_size(entropy_string:large_id(?CHAR_SET_8))),
  ?assertEqual(50, byte_size(entropy_string:large_id(?CHAR_SET_4))),
  ?assertEqual(99, byte_size(entropy_string:large_id(?CHAR_SET_2))).

session_id_test() ->
  ?assertEqual( 26, byte_size(entropy_string:session_id())),

  ?assertEqual( 22, byte_size(entropy_string:session_id(?CHAR_SET_64))),
  ?assertEqual( 26, byte_size(entropy_string:session_id(?CHAR_SET_32))),
  ?assertEqual( 32, byte_size(entropy_string:session_id(?CHAR_SET_16))),
  ?assertEqual( 43, byte_size(entropy_string:session_id(?CHAR_SET_8))),
  ?assertEqual( 64, byte_size(entropy_string:session_id(?CHAR_SET_4))),
  ?assertEqual(128, byte_size(entropy_string:session_id(?CHAR_SET_2))).

token_test() ->
  ?assertEqual( 52, byte_size(entropy_string:token())),

  ?assertEqual( 43, byte_size(entropy_string:token(?CHAR_SET_64))),
  ?assertEqual( 52, byte_size(entropy_string:token(?CHAR_SET_32))),
  ?assertEqual( 64, byte_size(entropy_string:token(?CHAR_SET_16))),
  ?assertEqual( 86, byte_size(entropy_string:token(?CHAR_SET_8))),
  ?assertEqual(128, byte_size(entropy_string:token(?CHAR_SET_4))),
  ?assertEqual(256, byte_size(entropy_string:token(?CHAR_SET_2))).

invalid_bytes_test() -> 
  {error, _} = entropy_string:random_string( 7, ?CHAR_SET_64, <<16#12:8>>),
  {error, _} = entropy_string:random_string(13, ?CHAR_SET_64, <<16#1234:16>>),
  {error, _} = entropy_string:random_string(25, ?CHAR_SET_64, <<16#123456:24>>),
  {error, _} = entropy_string:random_string(31, ?CHAR_SET_64, <<16#12345678:32>>),

  {error, _} = entropy_string:random_string( 6, ?CHAR_SET_32, <<16#12:8>>),
  {error, _} = entropy_string:random_string(16, ?CHAR_SET_32, <<16#1234:16>>),
  {error, _} = entropy_string:random_string(21, ?CHAR_SET_32, <<16#123456:24>>),
  {error, _} = entropy_string:random_string(31, ?CHAR_SET_32, <<16#12345678:32>>),
  {error, _} = entropy_string:random_string(32, ?CHAR_SET_32, <<16#12345678:32>>),
  {error, _} = entropy_string:random_string(41, ?CHAR_SET_32, <<16#1234567890:40>>),
  {error, _} = entropy_string:random_string(46, ?CHAR_SET_32, <<16#123456789012:48>>),

  {error, _} = entropy_string:random_string( 9, ?CHAR_SET_16, <<16#12:8>>),
  {error, _} = entropy_string:random_string(17, ?CHAR_SET_16, <<16#1234:16>>),

  {error, _} = entropy_string:random_string( 7, ?CHAR_SET_8,  <<16#12:8>>),
  {error, _} = entropy_string:random_string(16, ?CHAR_SET_8,  <<16#1234:16>>),
  {error, _} = entropy_string:random_string(25, ?CHAR_SET_8,  <<16#123456:24>>),
  {error, _} = entropy_string:random_string(31, ?CHAR_SET_8,  <<16#12345678:32>>),

  {error, _} = entropy_string:random_string( 9, ?CHAR_SET_4,  <<16#12:8>>),
  {error, _} = entropy_string:random_string(17, ?CHAR_SET_4,  <<16#1234:16>>),

  {error, _} = entropy_string:random_string( 9, ?CHAR_SET_2,  <<16#12:8>>),
  {error, _} = entropy_string:random_string(17, ?CHAR_SET_2,  <<16#1234:16>>).

custom_chars_test() ->
  CharSet64 = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ9876543210_-">>,
  ?assertEqual(<<"NzLoPDi-JiAa">>,  entropy_string:random_string(72, CharSet64,
                                                                 <<16#9d994ea5d23f8c8680:72>>)),
  
  CharSet32 = <<"2346789BDFGHJMNPQRTbdfghjlmnpqrt">>,
  ?assertEqual(<<"mHRrbgQlTqF">>,  entropy_string:random_string(55, CharSet32,
                                                                <<16#d2e3e9da199752:56>>)),
  
  CharSet16 = <<"0123456789ABCDEF">>,
  ?assertEqual(<<"C7C90">>,  entropy_string:random_string(20, CharSet16,
                                                          <<16#c7c900:24>>)),
  
  CharSet8 = <<"abcdefgh">>,
  ?assertEqual(<<"gbheeeahgc">>,  entropy_string:random_string(30, CharSet8,
                                                               <<16#c7c907c9:32>>)),
  
  CharSet4 = <<"atcg">>,
  ?assertEqual(<<"acaaggat">>,  entropy_string:random_string(16, CharSet4,
                                                             <<16#20f1:16>>)),
  
  CharSet2 = <<"HT">>,
  ?assertEqual(<<"TTTHHHTTTTTHTHHT">>,  entropy_string:random_string(16, CharSet2,
                                                                     <<16#e3e9:16>>)).
  
  
