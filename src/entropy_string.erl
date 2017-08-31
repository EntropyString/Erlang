%%==================================================================================================
%%
%% @doc
%% Efficiently generate cryptographically strong random strings of specified entropy from various
%% character sets.
%% 
%% @author Paul Rogers
%% 
%%==================================================================================================
-module(entropy_string).
-author("paul@knoxen.com").

%% Prevent use of erlang:ceil which is not available until OTP 20
-compile({no_auto_import,[ceil/1]}).

-define(CHAR_SET_64, <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_">>).
-define(CHAR_SET_32, <<"2346789bdfghjmnpqrtBDFGHJLMNPQRT">>).
-define(CHAR_SET_16, <<"0123456789abcdef">>).
-define(CHAR_SET_8,  <<"01234567">>).
-define(CHAR_SET_4,  <<"ATCG">>).
-define(CHAR_SET_2,  <<"01">>).

-define(BITS_PER_BYTE, 8).

%%==================================================================================================
%% API
%%==================================================================================================
-export([charset64/0
        ,charset32/0
        ,charset16/0
        ,charset8/0
        ,charset4/0
        ,charset2/0
        ,small_id/0
        ,small_id/1
        ,medium_id/0
        ,medium_id/1
        ,large_id/0
        ,large_id/1
        ,session_id/0
        ,session_id/1
        ,token/0
        ,token/1
        ,random_string/1
        ,random_string/2
        ,random_string/3
        ,ten_p/1
        ,bits/2
        ,bits_per_char/1
        ,bytes_needed/2
        ,valid_charset/1
        ,valid_bytes/3
        ]).

%%==================================================================================================
%% Types
%%==================================================================================================
-type charset() :: binary().
-type reason()  :: binary().

%%==================================================================================================
%% Public Functions
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc
%% URl and file system safe CharSet from <a href="https://tools.ietf.org/html/rfc4648#section-5">RFC 4648</a>.
%%
%%   <b>ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_</b>
%%
-spec charset64() -> String when
    String :: charset().
%%--------------------------------------------------------------------------------------------------
charset64() ->
  ?CHAR_SET_64.

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Base 32 character set.
%%
%%   <b>2346789bdfghjmnpqrtBDFGHJLMNPQRT</b>
%%
-spec charset32() -> String when
    String :: charset().
%%--------------------------------------------------------------------------------------------------
charset32() ->
  ?CHAR_SET_32.

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Hexadecimal character set.
%%
%%   <b>0123456789abcdef</b>
%%
-spec charset16() -> String when
    String :: charset().
%%--------------------------------------------------------------------------------------------------
charset16() ->
  ?CHAR_SET_16.

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Octal character set.
%%
%%   <b>01234567</b>
%%
-spec charset8() -> String when
    String :: charset().
%%--------------------------------------------------------------------------------------------------
charset8() ->
  ?CHAR_SET_8.

%%--------------------------------------------------------------------------------------------------
%% @doc
%% DNA alphabet.
%%
%%   <b>ATCG</b>
%%
-spec charset4() -> String when
    String :: charset().
%%--------------------------------------------------------------------------------------------------
charset4() ->
  ?CHAR_SET_4.

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Binary character set.
%%
%%   <b>01</b>
%%
-spec charset2() -> String when
    String :: charset().
%%--------------------------------------------------------------------------------------------------
charset2() ->
  ?CHAR_SET_2.

%%--------------------------------------------------------------------------------------------------
%% @doc Small ID
%% Returns random string with a 1 in a million chance of repeat in 30 strings using the predefined
%% base 32 characters.
-spec small_id() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
small_id() ->
  small_id(?CHAR_SET_32).

%%--------------------------------------------------------------------------------------------------
%% @doc Small ID
%%   <ul>
%%     <li><b>CharSet</b> - Characters to use</li>
%%   </ul>
%%
%% Returns random string with a 1 in a million chance of repeat in 30 strings
-spec small_id(CharSet) -> String when
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
small_id(CharSet) ->
  random_string(29, CharSet).

%%--------------------------------------------------------------------------------------------------
%% @doc Medium ID
%% Returns random string with a 1 in a billion chance of repeat in a million strings using
%% the predefined base 32 characters.
-spec medium_id() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
medium_id() ->
  medium_id(?CHAR_SET_32).

%%--------------------------------------------------------------------------------------------------
%% @doc Medium ID
%%   <ul>
%%     <li><b>CharSet</b> - Characters to use</li>
%%   </ul>
%%
%% Returns random string with a 1 in a billion chance of repeat in a million strings.
-spec medium_id(CharSet) -> String when
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
medium_id(CharSet) ->
  random_string(69, CharSet).

%%--------------------------------------------------------------------------------------------------
%% @doc Large ID
%% Returns random string with a 1 in a trillion chance of repeat in a billion strings using
%% the predefined base 32 characters.
-spec large_id() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
large_id() ->
  large_id(?CHAR_SET_32).

%%--------------------------------------------------------------------------------------------------
%% @doc Large ID
%%   <ul>
%%     <li><b>CharSet</b> - Characters to use</li>
%%   </ul>
%%
%% Returns random string with a 1 in a trillion chance of repeat in a billion strings.
-spec large_id(CharSet) -> String when
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
large_id(CharSet) ->
  random_string(99, CharSet).

%%--------------------------------------------------------------------------------------------------
%% @doc Session ID
%% Returns random string suitable for 128-bit OWASP Session ID using predefined base 32 characters.
-spec session_id() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
session_id() ->
  session_id(?CHAR_SET_32).

%%--------------------------------------------------------------------------------------------------
%% @doc Session ID
%%   <ul>
%%     <li><b>CharSet</b> - Characters to use</li>
%%   </ul>
%%
%% Returns random string suitable for 128-bit OWASP Session ID.
-spec session_id(CharSet) -> String when
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
session_id(CharSet) ->
  random_string(128, CharSet).

%%--------------------------------------------------------------------------------------------------
%% @doc Token
%% Returns random string with 256 bits of entropy using predefined URL and file system safe
%% characters.
-spec token() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
token() ->
  token(?CHAR_SET_32).

%%--------------------------------------------------------------------------------------------------
%% @doc Token
%%   <ul>
%%     <li><b>CharSet</b> - Characters to use</li>
%%   </ul>
%%
%% Returns random string with 256 bits of entropy.
-spec token(CharSet) -> String when
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
token(CharSet) ->
  random_string(256, CharSet).

%%==================================================================================================
%%
%% random_string(bits)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc String of Bits entropy from predefined base 32 charset
%%   <ul>
%%     <li><b>Bits</b> - Entropy bits for string</li>
%%   </ul>
-spec random_string(Bits) -> String when
    Bits    :: integer(),
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
random_string(Bits) ->
  random_string(Bits, charset32()).

%%==================================================================================================
%%
%% random_string(bits, charset)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc String of Bits entropy from CharSet
%%   <ul>
%%     <li><b>Bits</b> - Entropy bits for string</li>
%%     <li><b>CharSet</b> - Characters to use</li>
%%   </ul>
-spec random_string(Bits, CharSet) -> String when
    Bits    :: integer(),
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
random_string(Bits, CharSet) 
  when CharSet == ?CHAR_SET_64;
       CharSet == ?CHAR_SET_32;
       CharSet == ?CHAR_SET_16;
       CharSet == ?CHAR_SET_8;
       CharSet == ?CHAR_SET_4;
       CharSet == ?CHAR_SET_2 ->
  es_string(Bits, CharSet);
random_string(Bits, CharSet) ->
  case valid_charset(CharSet) of
    true ->
      es_string(Bits, CharSet);
    Error ->
      Error
  end.

%%--------------------------------------------------------------------------------------------------
%% Generate Bytes and forward
%%--------------------------------------------------------------------------------------------------
es_string(Bits, CharSet) ->
  ByteCount = bytes_needed(Bits, CharSet),
  Bytes = crypto:strong_rand_bytes(ByteCount),
  es_string_bytes(Bits, CharSet, Bytes).

%%==================================================================================================
%%
%% string(bits, charset, bytes)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc String of Bits entropy from CharSet using Bytes
%%   <ul>
%%     <li><b>Bits</b> - Entropy bits for string</li>
%%     <li><b>CharSet</b> - Characters to use</li>
%%     <li><b>Bytes</b> - Bytes to use</li>
%%   </ul>
-spec random_string(Bits, CharSet, Bytes) -> String when
    Bits    :: integer(),
    CharSet :: charset(),
    Bytes   :: binary(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
random_string(Bits, CharSet, Bytes)
  when CharSet == ?CHAR_SET_64;
       CharSet == ?CHAR_SET_32;
       CharSet == ?CHAR_SET_16;
       CharSet == ?CHAR_SET_8;
       CharSet == ?CHAR_SET_4;
       CharSet == ?CHAR_SET_2 ->
  es_string(Bits, CharSet, Bytes);
random_string(Bits, CharSet, Bytes) ->
  case valid_charset(CharSet) of
    true -> es_string(Bits, CharSet, Bytes);
    Error -> Error
  end.

%%--------------------------------------------------------------------------------------------------
%% Validate Bytes and forward
%%--------------------------------------------------------------------------------------------------
es_string(Bits, CharSet, Bytes) ->
  case valid_bytes(Bits, CharSet, Bytes) of
    true -> es_string_bytes(Bits, CharSet, Bytes);
    Error -> Error
  end.

%%--------------------------------------------------------------------------------------------------
%% Calc number of characters and forward
%%--------------------------------------------------------------------------------------------------
es_string_bytes(Bits, CharSet, Bytes) ->
  BitsPerChar = bits_per_char(CharSet),
  NdxFn = ndx_fn(CharSet),
  CharCount = ceil(Bits / BitsPerChar),
  es_string_count(CharCount, NdxFn, CharSet, Bytes, <<>>).

%%--------------------------------------------------------------------------------------------------
%% Tail-recursive string generator
%%--------------------------------------------------------------------------------------------------
es_string_count(0, _NdxFn, _CharSet, _Bytes, Chars) ->
  Chars;
es_string_count(CharCount, NdxFn, CharSet, Bytes, Chars) ->
  Slice = CharCount - 1,
  Ndx = NdxFn(Slice, Bytes),
  Char = binary:part(CharSet, Ndx, 1),
  es_string_count(Slice, NdxFn, CharSet, Bytes, <<Char/binary, Chars/binary>>).

%%==================================================================================================
%%
%% ten_p(power)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc Power of ten
%%
%% Returns 10 raised to power
%%
%% Convenience function for specifying total number of strings and  acceptable associated risk
-spec ten_p(Power) -> TenPower when
    Power    :: number(),
    TenPower :: number().
%%--------------------------------------------------------------------------------------------------
ten_p(Power) ->
  math:pow(10, Power).

%%==================================================================================================
%%
%% bits(Total, Risk)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc Entropy bits required for total number of strings with given risk.
%%   <ul>
%%     <li><b>Total</b> - Total number of strings</li>
%%     <li><b>Risk</b> - Risk of repeat in Total strings</li>
%%   </ul>
-spec bits(Total, Risk) -> Bits when
    Total :: number(),
    Risk  :: number(),
    Bits  :: nan | integer().
%%--------------------------------------------------------------------------------------------------
bits(0, _Risk) ->
  0;
bits(_Total, 0) ->
  0;
bits(Total, Risk) when (is_number(Total) andalso 0 < Total),
                       (is_number(Risk)  andalso 0 < Risk) ->
  N = case Total of
        Total when Total < 1000 ->
          math:log2(Total) + math:log2(Total-1);
        Total ->
          2 * math:log2(Total)
      end,
  N + math:log2(Risk) - 1;
bits(_, _) ->
  nan.

%%==================================================================================================
%%
%% bits_per_char(CharSet)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc Entropy bits per character.
%%   <ul>
%%     <li><b>CharSet</b> - Characters in use</li>
%%   </ul>
-spec bits_per_char(CharSet) -> Bits when
    CharSet :: charset(),
    Bits    :: integer().
%%--------------------------------------------------------------------------------------------------
bits_per_char(?CHAR_SET_64) -> 6;
bits_per_char(?CHAR_SET_32) -> 5;
bits_per_char(?CHAR_SET_16) -> 4;
bits_per_char(?CHAR_SET_8)  -> 3;
bits_per_char(?CHAR_SET_4)  -> 2;
bits_per_char(?CHAR_SET_2)  -> 1;
bits_per_char(CharSet) when is_binary(CharSet) ->
  round(math:log2(byte_size(CharSet))).

%%==================================================================================================
%%
%% bytes_needed(Bits, CharSet)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc Bytes needed to form string of entropy <b>Bits</b> from characters in <b>CharSet</b>
%%   <ul>
%%     <li><b>Bits</b> - Entropy bits for string</li>
%%     <li><b>CharSet</b> - Characters in use</li>
%%   </ul>
-spec bytes_needed(Bits, CharSet) -> ByteCount when
    Bits      :: integer(),
    CharSet   :: charset(),
    ByteCount :: integer().
%%--------------------------------------------------------------------------------------------------
bytes_needed(Bits, CharSet) ->
  BitsPerChar = bits_per_char(CharSet),
  CharCount = round(ceil(Bits / BitsPerChar)),
  round(ceil(CharCount * BitsPerChar / ?BITS_PER_BYTE)).

%%==================================================================================================
%%
%% valid_charset(CharSet)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc Validate CharSet
%%   <ul>
%%     <li><b>CharSet</b> - Characters to use</li>
%%   </ul>
%%   Validates:
%%   <ul>
%%     <li>CharSet must have 2, 4, 8, 16, 32, or 64 characters</li>
%%     <li>Characters must by unique</li>
%%   </ul>
-spec valid_charset(CharSet) -> Result when
    CharSet :: charset(),
    Reason  :: reason(),
    Result  :: true | {error, Reason}.
%%--------------------------------------------------------------------------------------------------
valid_charset(CharSet) when is_binary(CharSet) ->
  Length = byte_size(CharSet),
  case lists:member(Length, [64,32,16,8,4,2]) of
    true -> unique(CharSet);
    false -> {error, <<"Invalid char count: must be one of 2,4,8,16,32,64">>}
  end.

%%==================================================================================================
%%
%% valid_bytes(Bits, CharSet, Bytes)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc Validate Bytes
%%   <ul>
%%     <li><b>Bits</b> - Entropy bits for string</li>
%%     <li><b>CharSet</b> - Characters in use</li>
%%     <li><b>Bytes</b> - Bytes in use</li>
%%   </ul>
%%   Validates:
%%   <ul>
%%     <li>Bytes must be sufficient to generate entropy Bits string from CharSet</li>
%%   </ul>
-spec valid_bytes(Bits, CharSet, Bytes) -> Result when
    Bits    :: integer(),
    CharSet :: charset(),
    Bytes   :: binary(),
    Reason  :: reason(),
    Result  :: true | {error, Reason}.
%%--------------------------------------------------------------------------------------------------
valid_bytes(Bits, CharSet, Bytes) when is_binary(Bytes) ->
  Need = bytes_needed(Bits, CharSet),
  Got = byte_size(Bytes),
  case Need =< Got of
    true -> true;
    _ ->
      Reason = io_lib:format("Insufficient bytes: need ~p and got ~p", [Need, Got]),
      {error, binary:list_to_bin(Reason)}
  end.
      

%%====================================================================
%% Internal functions
%%====================================================================
%%
%% Determine if CharSet elements unique
%%
unique(CharSet) ->
  unique(true, CharSet).

%%
%% Recursive test for unique/1
%%
unique(Unique, <<>>) ->
  Unique;
unique(true, <<H, T/binary>>) ->
  case binary:match(T, [<<H>>]) of
    nomatch -> unique(true, T);
    _ -> {error, <<"Chars not unique">>}
  end;
unique(Error, _) ->
  Error.

%%--------------------------------------------------------------------------------------------------
%%
%%
%%
-spec ndx_fn(CharSet) -> NdxFn when
    CharSet :: charset(),
    NdxFn   :: function().
%%--------------------------------------------------------------------------------------------------
ndx_fn(CharSet) ->
  BitsPerChar = bits_per_char(CharSet),
  fun(Slice, Bytes) ->
      Offset = Slice*BitsPerChar,
      <<_Skip:Offset, Ndx:BitsPerChar, _Rest/bitstring>> = Bytes,
      Ndx
  end.

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
