-module(bits_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEN(N), math:pow(10, N)).

zero_bits_test() ->
  ?assertEqual(0, entropy_string:bits(  0,   0)),
  ?assertEqual(0, entropy_string:bits(100,   0)),
  ?assertEqual(0, entropy_string:bits(  0, 100)),
  ?assertEqual(0, entropy_string:bits( -1,   0)),
  ?assertEqual(0, entropy_string:bits(  0,  -1)).

bits_test() ->
  ?assertEqual(15, round(entropy_string:bits(    10,   1000))),
  ?assertEqual(19, round(entropy_string:bits(    10,  10000))),
  ?assertEqual(22, round(entropy_string:bits(    10, 100000))),

  ?assertEqual(22, round(entropy_string:bits(   100,   1000))),
  ?assertEqual(26, round(entropy_string:bits(   100,  10000))),
  ?assertEqual(29, round(entropy_string:bits(   100, 100000))),
  
  ?assertEqual(29, round(entropy_string:bits(  1000,   1000))),
  ?assertEqual(32, round(entropy_string:bits(  1000,  10000))),
  ?assertEqual(36, round(entropy_string:bits(  1000, 100000))),

  ?assertEqual(36, round(entropy_string:bits( 10000,   1000))),
  ?assertEqual(39, round(entropy_string:bits( 10000,  10000))),
  ?assertEqual(42, round(entropy_string:bits( 10000, 100000))),
  
  ?assertEqual(42, round(entropy_string:bits(100000,   1000))),
  ?assertEqual(46, round(entropy_string:bits(100000,  10000))),
  ?assertEqual(49, round(entropy_string:bits(100000, 100000))).

bits_power_test() ->
  ?assertEqual(15, round(entropy_string:bits(?TEN(1), ?TEN(3)))),
  ?assertEqual(19, round(entropy_string:bits(?TEN(1), ?TEN(4)))),
  ?assertEqual(22, round(entropy_string:bits(?TEN(1), ?TEN(5)))),

  ?assertEqual(22, round(entropy_string:bits(?TEN(2), ?TEN(3)))),
  ?assertEqual(26, round(entropy_string:bits(?TEN(2), ?TEN(4)))),
  ?assertEqual(29, round(entropy_string:bits(?TEN(2), ?TEN(5)))),
  
  ?assertEqual(29, round(entropy_string:bits(?TEN(3), ?TEN(3)))),
  ?assertEqual(32, round(entropy_string:bits(?TEN(3), ?TEN(4)))),
  ?assertEqual(36, round(entropy_string:bits(?TEN(3), ?TEN(5)))),

  ?assertEqual(36, round(entropy_string:bits(?TEN(4), ?TEN(3)))),
  ?assertEqual(39, round(entropy_string:bits(?TEN(4), ?TEN(4)))),
  ?assertEqual(42, round(entropy_string:bits(?TEN(4), ?TEN(5)))),
  
  ?assertEqual(42, round(entropy_string:bits(?TEN(5), ?TEN(3)))),
  ?assertEqual(46, round(entropy_string:bits(?TEN(5), ?TEN(4)))),
  ?assertEqual(49, round(entropy_string:bits(?TEN(5), ?TEN(5)))).

bits_preshing_32_bit_test() ->
  ?assertEqual(32, round(entropy_string:bits(30084, ?TEN(1)))),
  ?assertEqual(32, round(entropy_string:bits( 9292, ?TEN(2)))),
  ?assertEqual(32, round(entropy_string:bits( 2932, ?TEN(3)))),
  ?assertEqual(32, round(entropy_string:bits(  927, ?TEN(4)))),
  ?assertEqual(32, round(entropy_string:bits(  294, ?TEN(5)))),
  ?assertEqual(32, round(entropy_string:bits(   93, ?TEN(6)))),
  ?assertEqual(32, round(entropy_string:bits(   30, ?TEN(7)))),
  ?assertEqual(32, round(entropy_string:bits(   10, ?TEN(8)))).

bits_preshing_65_bit_test() ->
  ?assertEqual(64, round(entropy_string:bits(1970000000, ?TEN(1)))),
  ?assertEqual(64, round(entropy_string:bits( 609000000, ?TEN(2)))),
  ?assertEqual(64, round(entropy_string:bits( 192000000, ?TEN(3)))),
  ?assertEqual(64, round(entropy_string:bits(  60700000, ?TEN(4)))),
  ?assertEqual(64, round(entropy_string:bits(  19200000, ?TEN(5)))),
  ?assertEqual(64, round(entropy_string:bits(   6070000, ?TEN(6)))),
  ?assertEqual(64, round(entropy_string:bits(   1920000, ?TEN(7)))),
  ?assertEqual(64, round(entropy_string:bits(    607401, ?TEN(8)))),
  ?assertEqual(64, round(entropy_string:bits(    192077, ?TEN(9)))),
  ?assertEqual(64, round(entropy_string:bits(     60704, ?TEN(10)))),
  ?assertEqual(64, round(entropy_string:bits(     19208, ?TEN(11)))),
  ?assertEqual(64, round(entropy_string:bits(      6074, ?TEN(12)))),
  ?assertEqual(64, round(entropy_string:bits(      1921, ?TEN(13)))),
  ?assertEqual(64, round(entropy_string:bits(       608, ?TEN(14)))),
  ?assertEqual(64, round(entropy_string:bits(       193, ?TEN(15)))),
  ?assertEqual(64, round(entropy_string:bits(        61, ?TEN(16)))),
  ?assertEqual(64, round(entropy_string:bits(        20, ?TEN(17)))),
  ?assertEqual(64, round(entropy_string:bits(         7, ?TEN(18)))).
  
bits_preshing_160_bit_test() ->
  ?assertEqual(162, round(entropy_string:bits(?TEN(24), ?TEN(1)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(23), ?TEN(1)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(24), ?TEN(2)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(23), ?TEN(2)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(23), ?TEN(3)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(22), ?TEN(3)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(23), ?TEN(4)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(22), ?TEN(4)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(22), ?TEN(5)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(21), ?TEN(5)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(22), ?TEN(6)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(21), ?TEN(6)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(21), ?TEN(7)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(20), ?TEN(7)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(21), ?TEN(8)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(20), ?TEN(8)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(20), ?TEN(9)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(19), ?TEN(9)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(20), ?TEN(10)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(19), ?TEN(10)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(19), ?TEN(11)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(18), ?TEN(11)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(19), ?TEN(12)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(18), ?TEN(12)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(18), ?TEN(13)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(17), ?TEN(13)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(18), ?TEN(14)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(17), ?TEN(14)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(17), ?TEN(15)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(16), ?TEN(15)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(17), ?TEN(16)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(16), ?TEN(16)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(16), ?TEN(17)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(15), ?TEN(17)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(16), ?TEN(18)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(15), ?TEN(18)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(15), ?TEN(19)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(14), ?TEN(19)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(15), ?TEN(20)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(14), ?TEN(20)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(14), ?TEN(21)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(13), ?TEN(21)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(14), ?TEN(22)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(13), ?TEN(22)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(13), ?TEN(23)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(12), ?TEN(23)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(13), ?TEN(24)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(12), ?TEN(24)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(12), ?TEN(25)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(11), ?TEN(25)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(12), ?TEN(26)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(11), ?TEN(26)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(11), ?TEN(27)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(10), ?TEN(27)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(11), ?TEN(28)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(10), ?TEN(28)))),
  ?assertEqual(162, round(entropy_string:bits(?TEN(10), ?TEN(29)))),
  ?assertEqual(155, round(entropy_string:bits(?TEN(9),  ?TEN(29)))),
  ?assertEqual(165, round(entropy_string:bits(?TEN(10), ?TEN(30)))),
  ?assertEqual(158, round(entropy_string:bits(?TEN(9),  ?TEN(30)))).

nan_bits_test() ->
  ?assertEqual(nan, entropy_string:bits(  -1,  -1)),
  ?assertEqual(nan, entropy_string:bits(  -1, 100)),
  ?assertEqual(nan, entropy_string:bits( 100,  -1)).
