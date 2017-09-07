# EntropyString for Erlang

Efficiently generate cryptographically strong random strings of specified entropy from various character sets.

[![Build Status](https://travis-ci.org/EntropyString/Erlang.svg?branch=master)](https://travis-ci.org/EntropyString/Erlang) &nbsp; [![Hex Version](https://img.shields.io/hexpm/v/entropy_string_erl.svg "Hex Version")](https://hex.pm/packages/entropy_string_erl) &nbsp; [![License: MIT](https://img.shields.io/npm/l/express.svg)]()

### <a name="TOC"></a>TOC
 - [Installation](#Installation)
 - [Usage](#Usage)
 - [Overview](#Overview)
 - [Real Need](#RealNeed)
 - [Character Sets](#CharacterSets)
 - [Custom Characters](#CustomCharacters)
 - [Efficiency](#Efficiency)
 - [Custom Bytes](#CustomBytes)
 - [Take Away](#TakeAway)

### Installation

Add to `rebar.config`

  ```erlang
  {deps, [
...
       {entropy_string, {git, "https://github.com/EntropyString/Erlang.git", {tag, "1.0.0"}}}
   ]}.
  ```

To build and run tests

  ```bash
  > rebar3 compile
  > rebar3 eunit
  ```

[TOC](#TOC)

### <a name="Usage"></a>Usage

To run code snippets in the Erlang shell

  ```bash
  > rebar3 compile
  > erl -pa _build/default/lib/entropy_string/ebin
  Erlang/OTP ...
  1> l(entropy_string).
  {module,entropy_string}
  2>
  ```

Generate a potential of _1 million_ random strings with _1 in a billion_ chance of repeat:

  ```erlang
  2> Bits = entropy_string:bits(1.0e6, 1.0e9).
  68.7604899926346
  3> entropy_string:random_string(Bits).
  <<"GhrB6fJbD6gTpT">>
  ```

There are six predefined character sets. By default, `random_string/1` uses `charset32`, a character set with 32 characters. To get a random hexadecimal string with the same entropy `Bits` as above (see [Real Need](#RealNeed) for description of what entropy `Bits` represents):

  ```erlang
  2> Bits = entropy_string:bits(1.0e6, 1.0e9).
  68.7604899926346
  3> entropy_string:random_string(Bits, charset16).
  <<"99d535fbcac884e875">>
  ```

Custom characters are also supported. Using uppercase hexadecimal characters:

  ```erlang
  2> Bits = entropy_string:bits(1.0e6, 1.0e9).
  68.7604899926346
  3> entropy_string:random_string(Bits, <<"0123456789ABCDEF">>).
  <<"6099EA0B59F9813D5F">>
  ```

Convenience functions are provided for common scenarios. For example, OWASP session ID using`charset32`:

  ```erlang
  2> entropy_string:session_id().
  <<"bQQjJrbJQ44j76hMPqrTtqGFrq">>
  ```

Session ID using [RFC 4648](https://tools.ietf.org/html/rfc4648#section-5) file system and URL safe characters:

  ```erlang
  2> entropy_string:session_id(charset64).
  <<"4VMiJmD23Aq2Px7vGnd8Fi">>
  ```

[TOC](#TOC)

### <a name="Overview"></a>Overview

`entropy_string` provides easy creation of randomly generated strings of specific entropy using various character sets. Such strings are needed when generating, for example, random IDs and you don't want the overkill of a GUID, or for ensuring that some number of items have unique identifiers.

A key concern when generating such strings is that they be unique. To truly guarantee uniqueness requires either deterministic generation (e.g., a counter) that is not random, or that each newly created random string be compared against all existing strings. When ramdoness is required, the overhead of storing and comparing all strings is often too onerous and a different tack is needed.

A common strategy is to replace the *guarantee of uniqueness* with a weaker but often sufficient *probabilistic uniqueness*. Specifically, rather than being absolutely sure of uniqueness, we settle for a statement such as *"there is less than a 1 in a billion chance that two of my strings are the same"*. This strategy requires much less overhead, but does require we have some manner of qualifying what we mean by, for example, *"there is less than a 1 in a billion chance that 1 million strings of this form will have a repeat"*.

Understanding probabilistic uniqueness requires some understanding of [*entropy*](https://en.wikipedia.org/wiki/Entropy_(information_theory)) and of estimating the probability of a [*collision*](https://en.wikipedia.org/wiki/Birthday_problem#Cast_as_a_collision_problem) (i.e., the probability that two strings in a set of randomly generated strings might be the same).  Happily, you can use `entropy_string` without a deep understanding of these topics.

We'll begin investigating `entropy_string` by considering our [Real Need](Read%20Need) when generating random strings.

[TOC](#TOC)

### <a name="RealNeed"></a>Real Need

Let's start by reflecting on a common statement of need for developers, who might say:

*I need random strings 16 characters long.*

Okay. There are libraries available that address that exact need. But first, there are some questions that arise from the need as stated, such as:

  1. What characters do you want to use?
  2. How many of these strings do you need?
  3. Why do you need these strings?

The available libraries often let you specify the characters to use. So we can assume for now that question 1 is answered with:

*Hexadecimal will do fine*.

As for question 2, the developer might respond:

*I need 10,000 of these things*.

Ah, now we're getting somewhere. The answer to question 3 might lead to the further qualification:

*I need to generate 10,000 random, unique IDs*.

And the cat's out of the bag. We're getting at the real need, and it's not the same as the original statement. The developer needs *uniqueness* across some potential number of strings. The length of the string is a by-product of the uniqueness, not the goal, and should not be the primary specification for the random string.

As noted in the [Overview](#Overview), guaranteeing uniqueness is difficult, so we'll replace that declaration with one of *probabilistic uniqueness* by asking:

  - What risk of a repeat are you willing to accept?

Probabilistic uniqueness contains risk. That's the price we pay for giving up on the stronger declaration of strict uniqueness. But the developer can quantify an appropriate risk for a particular scenario with a statement like:

*I guess I can live with a 1 in a million chance of a repeat*.

So now we've gotten to the developer's real need:

*I need 10,000 random hexadecimal IDs with less than 1 in a million chance of any repeats*.

Not only is this statement more specific, there is no mention of string length. The developer needs probabilistic uniqueness, and strings are to be used to capture randomness for this purpose. As such, the length of the string is simply a by-product of the encoding used to represent the required uniqueness as a string.

How do you address this need using a library designed to generate strings of specified length?  Well, you don't directly, because that library was designed to answer the originally stated need, not the real need we've uncovered. We need a library that deals with probabilistic uniqueness of a total number of some strings. And that's exactly what `entropy_string` does.

Let's use `entropy_string` to help this developer by generating 5 IDs:

  ```erlang
  2> Bits = entropy_string:bits(10000, 1000000).
  45.50699332842307
  3> lists:map(fun(_) -> entropy_string:random_string(Bits, charset16) end, lists:seq(1,5)).
  [<<"9fd4090d336f">>,<<"692c599701c9">>,<<"175a5f34bb89">>,<<"144cc6119460">>,<<"dd61e0a66605">>]
  ```

To generate the IDs, we first use

  ```erlang
  Bits = entropy_string:bits(10000, 1000000).
  ```

to determine how much entropy is needed to generate a potential of _10000 strings_ while satisfy the probabilistic uniqueness of a _1 in a million risk_ of repeat. We can see from the output of the Erland shell it's about **45.51** bits. Inside the list comprehension we used

  ```erlang
  entropy_string:random_string(Bits, charset16)
  ```

to actually generate a random string of the specified entropy using hexadecimal (charset16) characters. Looking at the IDs, we can see each is 12 characters long. Again, the string length is a by-product of the characters used to represent the entropy we needed. And it seems the developer didn't really need 16 characters after all.

Finally, given that the strings are 12 hexadecimals long, each string actually has an information carrying capacity of 12 * 4 = 48 bits of entropy (a hexadecimal character carries 4 bits). That's fine. Assuming all characters are equally probable, a string can only carry entropy equal to a multiple of the amount of entropy represented per character. `entropy_string` produces the smallest strings that *exceed* the specified entropy.

[TOC](#TOC)

### <a name="CharacterSets"></a>Character Sets

As we\'ve seen in the previous sections, `entropy_string` provides predefined characters for each of the supported character set lengths. Let\'s see what\'s under the hood. The predefined character sets are *charset64*, *charset32*, *charset16*, *charset8*, *charset4* and *charset2*. The characters for each were chosen as follows:

  - CharSet 64: **ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_**
      * The file system and URL safe char set from [RFC 4648](https://tools.ietf.org/html/rfc4648#section-5).
      &nbsp;
  - CharSet 32: **2346789bdfghjmnpqrtBDFGHJLMNPQRT**
      * Remove all upper and lower case vowels (including y)
      * Remove all numbers that look like letters
      * Remove all letters that look like numbers
      * Remove all letters that have poor distinction between upper and lower case values.
      The resulting strings don't look like English words and are easy to parse visually.
      &nbsp;
  - CharSet 16: **0123456789abcdef**
      * Hexadecimal
      &nbsp;
  - CharSet  8: **01234567**
      * Octal
      &nbsp;
  - CharSet  4: **ATCG**
      * DNA alphabet. No good reason; just wanted to get away from the obvious.
      &nbsp;
  - CharSet  2: **01**
      * Binary

You may, of course, want to choose the characters used, which is covered next in [Custom Characters](#CustomCharacters).

[TOC](#TOC)

### <a name="CustomCharacters"></a>Custom Characters

Being able to easily generate random strings is great, but what if you want to specify your own characters? For example, suppose you want to visualize flipping a coin to produce 10 bits of entropy.

  ```erlang
  2> entropy_string:random_string(10, charset2).
  <<"0100111101">>
  ```

The resulting string of __0__'s and __1__'s doesn't look quite right. Perhaps you want to use the characters __H__ and __T__ instead.

  ```erlang
  2> entropy_string:random_string(10, <<"HT">>).
  <<"HTTTHHTTHH">>
  ```

As another example, we saw in [Character Sets](#CharacterSets) the predefined hex characters for `charSet16` are lowercase. Suppose you like uppercase hexadecimal letters instead.

  ```erlang
  2> entropy_string:random_string(48, <<"0123456789ABCDEF">>).
  <<"3BD881733687">>
  ```

To facilitate [efficient](#Efficiency) generation of strings, `entropy_string` limits character set lengths to powers of 2. Attempting to use a character set of an invalid length returns an error.

  ```erlang
  2> entropy_string:random_string(48, <<"123456789ABCDEF">>).
  {error,<<"Invalid char count: must be one of 2,4,8,16,32,64">>}
  ```

Likewise, since calculating entropy requires specification of the probability of each symbol, `entropy_string` requires all characters in a set be unique. (This maximize entropy per string as well).

  ```erlang
  2> entropy_string:random_string(48, <<"123456789ABCDEF1">>).
  {error,<<"Chars not unique">>}
  ```

[TOC](#TOC)

### <a name="Efficiency"></a>Efficiency

To efficiently create random strings, `entropy_string` generates the necessary number of random bytes needed for each string and uses those bytes in a binary pattern matching scheme to index into a character set. For example, to generate strings from the __32__ characters in the *charSet32* character set, each index needs to be an integer in the range `[0,31]`. Generating a random string of *charSet32* characters is thus reduced to generating random indices in the range `[0,31]`.

To generate the indices, `entropy_string` slices just enough bits from the random bytes to create each index. In the example at hand, 5 bits are needed to create an index in the range `[0,31]`. `entropy_string` processes the random bytes 5 bits at a time to create the indices. The first index comes from the first 5 bits of the first byte, the second index comes from the last 3 bits of the first byte combined with the first 2 bits of the second byte, and so on as the bytes are systematically sliced to form indices into the character set. And since binary pattern matching is really efficient, this scheme is quite fast.

The `entropy_string` scheme is also efficient with regard to the amount of randomness used. Consider the following possible Erlang solution to generating random strings. To generated a character, an index into the available characters is created using `rand.uniform/1`. The code looks something like:

  ```erlang
  -module (random_string).

  -export([len/1]).

  -define(CHARS, <<"abcdefghijklmnopqrstuvwxyz0123456">>).
  -define(LEN, 32).

  char(Ndx) ->
    Offset = Ndx * 8,
    <<_Skip:Offset, Char:8, _Rest/binary>> = ?CHARS,
    Char.

  ndx(_) ->
    rand:uniform(?LEN) - 1.

  len(Len) ->
    list_to_binary([char(Ndx) || Ndx <- lists:map(fun ndx/1, lists:seq(1,Len))]).
  ```

  ```bash
  2> c(random_string).
  {ok,random_string}
  3> random_string:len(16).
  <<"0kgl2ha6pmuzjknf">>
  ```

In the code above, `rand:uniform/1` generates a value used to index into the hexadecimal character set. The Erlang docs indicate that each returned random value has 58 bits of precision. Suppose we're creating strings with **Len = 16**. Generating each string character consumes 58 bits of randomness while only injecting 5 bits (`log2(32)`) of entropy into the resulting random string. The resulting string has an information carrying capacity of 16 * 5 = 80 bits, so creating each string requires a *total* of 928 bits of randomness while only actually *carrying* 80 bits of that entropy forward in the string itself. That means 848 bits (91%) of the generated randomness is simply wasted.

Compare that to the `entropy_string` scheme. For the example above, plucking 5 bits at a time requires a total of 80 bits (10 bytes) by available. Creating the same strings as above, `entropy_string` uses 80 bits of randomness per string with no wasted bits. In general, the `entropy_string` scheme can waste up to 7 bits per string, but that's the worst case scenario and that's *per string*, not *per character*!

There is, however, a potentially bigger issue at play in the above code. Erlang `rand` does not use a cryptographically strong psuedo random number generator. So the above code should not be used for session IDs or any other purpose that requires secure properties.

There are certainly other popular ways to create random strings, including secure ones. For example, if you created an appropriate `bin_to_hex/1` function, generating secure random hex strings can be done by

  ```elixir
  2> bin_to_hex(:crypto.strong_rand_bytes(8))
  <<"389B363BB7FD6227">>
  ```

Or you could use `base64` like this

  ```erlang
  2> l(base64).
  {module,base64}
  2> base64.encode(crypto:strong_rand_bytes(8)).
  <<"+z6Nose3J54=">>
  ```

Since Base64 encoding is concerned with decoding as well, you would have to strip any padding characters. And the characters used are not URL or file system safe. You could do subsequent character substitution, but you're going down a rabbit hole from co-opting a function for purposes it wasn't designed for.

These two solutions each have the limitations. You can't alter the characters, but more importantly, each lacks a clear specification of how random the resulting strings actually are. Each specifies byte length as opposed to specifying the entropy bits sufficient to represent some total number of strings with an explicit declaration of an associated risk of repeat using whatever encoding characters you want.

Fortunately you don't need to really understand how secure random bytes are efficiently sliced and diced to use `entropy_string`. But you may want to provide your own [Custom Bytes](#CustomBytes), which is the next topic.

[TOC](#TOC)

### <a name="CustomBytes"></a>Custom Bytes

As previously described, `entropy_string` automatically generates cryptographically strong random bytes to generate strings. You may, however, have a need to provide your own bytes, for deterministic testing or perhaps to use a specialized random byte generator.

Suppose we want a string capable of 30 bits of entropy using 32 characters. We can specify the bytes to use during string generation by

  ```erlang
  2> Bytes = <<16#fac89664:32>>.
  <<250,200,150,100>>
  3> entropy_string:random_string(30, entropy_string:charset(charset32), Bytes).
  <<"Th7fjL">>
  ```
 
The `Bytes` provided can come from any source. However, an error is returned if the number of bytes is insufficient to generate the string as described in the [Efficiency](#Efficiency) section:

  ```erlang
  2> Bytes = <<16#fac89664:32>>.
  <<250,200,150,100>>
  3> entropy_string:random_string(32, charset32, Bytes).
  {error,<<"Insufficient bytes: need 5 and got 4">>}
  ```

Note the number of bytes needed is dependent on the number of characters in the character set. For a string representation of entropy, we can only have multiples of the entropy bits per character. In the example above, each character represents 5 bits of entropy. So we can't get exactly 32 bits and we round up by the bits per character to a total 35 bits. We need 5 bytes (40 bits), not 4 (32 bits).

`entropy_string:bytes_needed/2` can be used to determine the number of bytes needed to cover a specified amount of entropy for a given character set.

  ```erlang
  2> entropy_string:bytes_needed(32, charset32).
  5
  ```

[TOC](#TOC)

### <a name="TakeAway"></a>Take Away

  - You don't need random strings of length L.
    - String length is a by-product, not a goal.
  - You don't need truly unique strings.
    - Uniqueness is too onerous. You'll do fine with probabilistically unique strings.
  - Probabilistic uniqueness involves measured risk.
    - Risk is measured as *"1 in __n__ chance of generating a repeat"*
    - Bits of entropy gives you that measure.
  - You need to a total of **_N_** strings with a risk **_1/n_** of repeat.
    - The characters are arbitrary.
  - You need `entropy_string`.
  
##### A million potential IDs with a 1 in a billion chance of a repeat:

  ```erlang
  2> entropy_string:random_string(entropy_string:bits(1.0e6, 1.0e9)).
  <<"GpFQ8TFLh27fnq">>
  ```
  
[TOC](#TOC)
