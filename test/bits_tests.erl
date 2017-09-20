-module(bits_tests).

-include_lib("eunit/include/eunit.hrl").

zero_bits_test() ->
  ?assertEqual(0, entropy_string:bits(  0,   0)),
  ?assertEqual(0, entropy_string:bits(100,   0)),
  ?assertEqual(0, entropy_string:bits(  0, 100)),
  ?assertEqual(0, entropy_string:bits( -1,   0)),
  ?assertEqual(0, entropy_string:bits(  0,  -1)).

bits_int_int_test() ->
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

bits_float_float_test() ->
  ?assertEqual(15, round(entropy_string:bits(1.0e1, 1.0e3))),
  ?assertEqual(19, round(entropy_string:bits(1.0e1, 1.0e4))),
  ?assertEqual(22, round(entropy_string:bits(1.0e1, 1.0e5))),

  ?assertEqual(22, round(entropy_string:bits(1.0e2, 1.0e3))),
  ?assertEqual(26, round(entropy_string:bits(1.0e2, 1.0e4))),
  ?assertEqual(29, round(entropy_string:bits(1.0e2, 1.0e5))),
  
  ?assertEqual(29, round(entropy_string:bits(1.0e3, 1.0e3))),
  ?assertEqual(32, round(entropy_string:bits(1.0e3, 1.0e4))),
  ?assertEqual(36, round(entropy_string:bits(1.0e3, 1.0e5))),

  ?assertEqual(36, round(entropy_string:bits(1.0e4, 1.0e3))),
  ?assertEqual(39, round(entropy_string:bits(1.0e4, 1.0e4))),
  ?assertEqual(42, round(entropy_string:bits(1.0e4, 1.0e5))),
  
  ?assertEqual(42, round(entropy_string:bits(1.0e5, 1.0e3))),
  ?assertEqual(46, round(entropy_string:bits(1.0e5, 1.0e4))),
  ?assertEqual(49, round(entropy_string:bits(1.0e5, 1.0e5))).

bits_preshing_32_bit_test() ->
  ?assertEqual(32, round(entropy_string:bits(30084, 1.0e1))),
  ?assertEqual(32, round(entropy_string:bits( 9292, 1.0e2))),
  ?assertEqual(32, round(entropy_string:bits( 2932, 1.0e3))),
  ?assertEqual(32, round(entropy_string:bits(  927, 1.0e4))),
  ?assertEqual(32, round(entropy_string:bits(  294, 1.0e5))),
  ?assertEqual(32, round(entropy_string:bits(   93, 1.0e6))),
  ?assertEqual(32, round(entropy_string:bits(   30, 1.0e7))),
  ?assertEqual(32, round(entropy_string:bits(   10, 1.0e8))).

bits_preshing_64_bit_test() ->
  ?assertEqual(64, round(entropy_string:bits(1.97e09, 1.0e1))),
  ?assertEqual(64, round(entropy_string:bits(6.09e08, 1.0e2))),
  ?assertEqual(64, round(entropy_string:bits(1.92e08, 1.0e3))),
  ?assertEqual(64, round(entropy_string:bits(6.07e07, 1.0e4))),
  ?assertEqual(64, round(entropy_string:bits(1.92e07, 1.0e5))),
  ?assertEqual(64, round(entropy_string:bits(6.07e06, 1.0e6))),
  ?assertEqual(64, round(entropy_string:bits(1.92e06, 1.0e7))),
  ?assertEqual(64, round(entropy_string:bits( 607401, 1.0e8))),
  ?assertEqual(64, round(entropy_string:bits( 192077, 1.0e9))),
  ?assertEqual(64, round(entropy_string:bits(  60704, 1.0e10))),
  ?assertEqual(64, round(entropy_string:bits(  19208, 1.0e11))),
  ?assertEqual(64, round(entropy_string:bits(   6074, 1.0e12))),
  ?assertEqual(64, round(entropy_string:bits(   1921, 1.0e13))),
  ?assertEqual(64, round(entropy_string:bits(    608, 1.0e14))),
  ?assertEqual(64, round(entropy_string:bits(    193, 1.0e15))),
  ?assertEqual(64, round(entropy_string:bits(     61, 1.0e16))),
  ?assertEqual(64, round(entropy_string:bits(     20, 1.0e17))),
  ?assertEqual(64, round(entropy_string:bits(      7, 1.0e18))).
  
bits_preshing_160_bit_test() ->
    ?assertEqual(160, round(entropy_string:bits(1.42e24,      2))),
    ?assertEqual(160, round(entropy_string:bits(5.55e23,     10))),
    ?assertEqual(160, round(entropy_string:bits(1.71e23,    100))),
    ?assertEqual(160, round(entropy_string:bits(5.41e22,   1000))),
    ?assertEqual(160, round(entropy_string:bits(1.71e22, 1.0e04))),
    ?assertEqual(160, round(entropy_string:bits(5.41e21, 1.0e05))),
    ?assertEqual(160, round(entropy_string:bits(1.71e21, 1.0e06))),
    ?assertEqual(160, round(entropy_string:bits(5.41e20, 1.0e07))),
    ?assertEqual(160, round(entropy_string:bits(1.71e20, 1.0e08))),
    ?assertEqual(160, round(entropy_string:bits(5.41e19, 1.0e09))),
    ?assertEqual(160, round(entropy_string:bits(1.71e19, 1.0e10))),
    ?assertEqual(160, round(entropy_string:bits(5.41e18, 1.0e11))),
    ?assertEqual(160, round(entropy_string:bits(1.71e18, 1.0e12))),
    ?assertEqual(160, round(entropy_string:bits(5.41e17, 1.0e13))),
    ?assertEqual(160, round(entropy_string:bits(1.71e17, 1.0e14))),
    ?assertEqual(160, round(entropy_string:bits(5.41e16, 1.0e15))),
    ?assertEqual(160, round(entropy_string:bits(1.71e16, 1.0e16))),
    ?assertEqual(160, round(entropy_string:bits(5.41e15, 1.0e17))),
    ?assertEqual(160, round(entropy_string:bits(1.71e15, 1.0e18))).

nan_bits_test() ->
  ?assertEqual(nan, entropy_string:bits(  -1,  -1)),
  ?assertEqual(nan, entropy_string:bits(  -1, 100)),
  ?assertEqual(nan, entropy_string:bits( 100,  -1)).
