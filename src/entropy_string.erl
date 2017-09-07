%% MIT License

%% Copyright (c) 2017 Knoxen

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
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

-define(BITS_PER_BYTE, 8).

-define(SMALL_ID_BITS,    29).
-define(MEDIUM_ID_BITS,   69).
-define(LARGE_ID_BITS,    99).
-define(SESSION_ID_BITS, 128).
-define(TOKEN_BITS,      256).

%%==================================================================================================
%% API
%%==================================================================================================
-export([charset/1
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
-type charset() :: charset64 | charset32 | charset16 | charset8 | charset4 | charset2 | binary().
-type reason()  :: binary().

%%==================================================================================================
%% Public Functions
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc Predefined <em>CharSet</em>s
%%
%%   <ul>
%%     <li><code>charset64</code> : <b>ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_</b></li>
%%     <li><code>charset32</code> : <b>2346789bdfghjmnpqrtBDFGHJLMNPQRT</b></li>
%%     <li><code>charset16</code> : <b>0123456789abcdef</b></li>
%%     <li><code>charset8</code> : <b>01234567</b></li>
%%     <li><code>charset4</code> : <b>ATCG</b></li>
%%     <li><code>charset2</code> : <b>01</b></li>
%%   </ul>
%%
-spec charset(CharSet) -> String when
    CharSet :: charset64 | charset32 | charset16 | charset8 | charset4 | charset2,
    String :: binary().
%%--------------------------------------------------------------------------------------------------
charset(charset64) ->
  <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_">>;
charset(charset32) ->
  <<"2346789bdfghjmnpqrtBDFGHJLMNPQRT">>;
charset(charset16) ->
  <<"0123456789abcdef">>;
charset(charset8) ->
  <<"01234567">>;
charset(charset4) ->
  <<"ATCG">>;
charset(charset2) ->
  <<"01">>.

%%--------------------------------------------------------------------------------------------------
%% @doc Small ID using <code>charset32</code>.
%%
%% Returns random string with a 1 in a million chance of repeat in 30 strings using
%% <code>charset32</code>.
-spec small_id() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
small_id() ->
  small_id(charset32).

%%--------------------------------------------------------------------------------------------------
%% @doc Small ID using <em>CharSet</em>
%%
%% Returns random string with a 1 in a million chance of repeat in 30 strings
-spec small_id(CharSet) -> String when
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
small_id(CharSet) ->
  random_string(?SMALL_ID_BITS, CharSet).

%%--------------------------------------------------------------------------------------------------
%% @doc Medium ID using <code>charset32</code>.
%%
%% Returns random string with a 1 in a billion chance of repeat in a million strings using
%% <code>charset32</code>.
-spec medium_id() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
medium_id() ->
  medium_id(charset32).

%%--------------------------------------------------------------------------------------------------
%% @doc Medium ID using <em>CharSet</em>
%%
%% Returns random string with a 1 in a billion chance of repeat in a million strings.
-spec medium_id(CharSet) -> String when
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
medium_id(CharSet) ->
  random_string(?MEDIUM_ID_BITS, CharSet).

%%--------------------------------------------------------------------------------------------------
%% @doc Large ID using <code>charset32</code>.
%%
%% Returns random string with a 1 in a trillion chance of repeat in a billion strings using
%% <code>charset32</code>.
-spec large_id() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
large_id() ->
  large_id(charset32).

%%--------------------------------------------------------------------------------------------------
%% @doc Large ID using <em>CharSet</em>
%%
%% Returns random string with a 1 in a trillion chance of repeat in a billion strings.
-spec large_id(CharSet) -> String when
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
large_id(CharSet) ->
  random_string(?LARGE_ID_BITS, CharSet).

%%--------------------------------------------------------------------------------------------------
%% @doc Session ID using <code>charset32</code>.
%%
%% Returns random string suitable for 128-bit OWASP Session ID using <code>charset32</code>.
-spec session_id() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
session_id() ->
  session_id(charset32).

%%--------------------------------------------------------------------------------------------------
%% @doc Session ID using <em>CharSet</em>
%%
%% Returns random string suitable for 128-bit OWASP Session ID.
-spec session_id(CharSet) -> String when
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
session_id(CharSet) ->
  random_string(128, CharSet).

%%--------------------------------------------------------------------------------------------------
%% @doc Token using <code>charset32</code>.
%%
%% Returns random string with 256 bits of entropy using <code>charset32</code>
%% characters.
-spec token() -> String when
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
token() ->
  token(charset32).

%%--------------------------------------------------------------------------------------------------
%% @doc Token using <em>CharSet</em>
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
%% @doc String of entropy <em>Bits</em> using <code>charset32</code>.
%%
-spec random_string(Bits) -> String when
    Bits    :: number(),
    String  :: binary().
%%--------------------------------------------------------------------------------------------------
random_string(Bits) ->
  random_string(Bits, charset32).

%%==================================================================================================
%%
%% random_string(bits, charset)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc String of entropy <em>Bits</em> using <em>CharSet</em>
%%
-spec random_string(Bits, CharSet) -> String when
    Bits    :: number(),
    CharSet :: charset(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
random_string(Bits, CharSet) when
    CharSet =:= charset64;
    CharSet =:= charset32;
    CharSet =:= charset16;
    CharSet =:= charset8;
    CharSet =:= charset4;
    CharSet =:= charset2 ->
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
%% random_string(bits, charset, bytes)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc String of entropy <em>Bits</em> using <em>CharSet</em> and <em>Bytes</em>
%%
-spec random_string(Bits, CharSet, Bytes) -> String when
    Bits    :: integer(),
    CharSet :: charset(),
    Bytes   :: binary(),
    String  :: binary() | {error, reason()}.
%%--------------------------------------------------------------------------------------------------
random_string(Bits, CharSet, Bytes) when
    CharSet =:= charset64;
    CharSet =:= charset32;
    CharSet =:= charset16;
    CharSet =:= charset8;
    CharSet =:= charset4;
    CharSet =:= charset2 ->
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
es_string_bytes(Bits, CharSet, Bytes) when
    CharSet =:= charset64;
    CharSet =:= charset32;
    CharSet =:= charset16;
    CharSet =:= charset8;
    CharSet =:= charset4;
    CharSet =:= charset2 ->
  es_string_bytes(Bits, charset(CharSet), Bytes);
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
%% @deprecated Use 1.0eNN instead.
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
%% @doc Entropy bits required for <em>Total</em> number of strings with given <em>Risk</em>.
%%
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
%%
-spec bits_per_char(CharSet) -> Bits when
    CharSet :: charset(),
    Bits    :: integer().
%%--------------------------------------------------------------------------------------------------
bits_per_char(charset64) -> 6;
bits_per_char(charset32) -> 5;
bits_per_char(charset16) -> 4;
bits_per_char(charset8)  -> 3;
bits_per_char(charset4)  -> 2;
bits_per_char(charset2)  -> 1;
bits_per_char(CharSet) when is_binary(CharSet) ->
  round(math:log2(byte_size(CharSet))).

%%==================================================================================================
%%
%% bytes_needed(Bits, CharSet)
%%
%%==================================================================================================
%%--------------------------------------------------------------------------------------------------
%% @doc Bytes needed to form string of entropy <em>Bits</em> using <em>CharSet</em>
%%
-spec bytes_needed(Bits, CharSet) -> ByteCount when
    Bits      :: number(),
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
%%
%%   Validates:
%%   <ul>
%%     <li>CharSet must have 2, 4, 8, 16, 32, or 64 characters</li>
%%     <li>Characters must by unique</li>
%%   </ul>
-spec valid_charset(CharSet) -> Result when
    CharSet :: binary(),
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
%%
%%   Validates:
%%   <ul>
%%     <li>Bytes must be sufficient to generate entropy <em>Bits</em> using <em>CharSet</em></li>
%%   </ul>
-spec valid_bytes(Bits, CharSet, Bytes) -> Result when
    Bits    :: number(),
    CharSet :: binary(),
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
%% Determine if <em>CharSet</em> elements unique
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

%% Prevent use of erlang:ceil which is not available until OTP 20. Remove when possible.
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
