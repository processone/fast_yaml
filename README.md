# Fast YAML

[![CI](https://github.com/processone/fast_yaml/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/processone/fast_yaml/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/processone/fast_yaml/badge.svg?branch=master&service=github)](https://coveralls.io/github/processone/fast_yaml?branch=master)
[![Hex version](https://img.shields.io/hexpm/v/fast_yaml.svg "Hex version")](https://hex.pm/packages/fast_yaml)

Fast YAML is an Erlang wrapper for
[libyaml](http://pyyaml.org/wiki/LibYAML) "C" library.

It is designed to be fast and efficient.

## Installation

### Dependencies

Fast YAML depends on native LibYaml library. You need development
headers for LibYaml library to build it.

The minimum required Erlang/OTP version is 18.0

### Generic build

You can trigger build with:

    ./configure && make

### OSX build example

You can install LibYaml and with Homebrew:

    brew install libyaml

You can then export environment variable to use LibYaml as installed
by Homebrew, before issuing compilation commands:

    export LDFLAGS="-L/usr/local/lib"
    export CFLAGS="-I/usr/local/include"
    export CPPFLAGS="-I/usr/local/include"

    ./configure && make

## Example usage

```erlang
erl -pa ebin -pa deps/*/ebin
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.1  (abort with ^G)

1> application:start(fast_yaml).
ok

2> fast_yaml:decode(<<"a: 1\nb: -3.0">>).
{ok,[[{<<"a">>,1},{<<"b">>,-3.0}]]}
```

## Option `plain_as_atom`

Converts all unquoted YAML values to atoms

```erlang
3> fast_yaml:decode(<<"a: 1\nb: -3.0">>, [{plain_as_atom, true}]).
{ok,[[{a,1},{b,-3.0}]]}

4> fast_yaml:decode(<<"a: b\nc">>).
{error,{scanner_error,<<"could not find expected ':'">>,2,
                      0}}.

5> fast_yaml:decode_from_file("test/test2.yml", [plain_as_atom]).
{ok,[[[{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.4},
              {pulseDuration,12},
              {repetition,1000},
              {spotSize,<<"1mm">>}]}],
      [{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.0},
              {pulseDuration,10},
              {repetition,500},
              {spotSize,<<"2mm">>}]}],
      [{step,<<"id001">>}],
      [{step,<<"id002">>}],
      [{step,<<"id001">>}],
      [{step,<<"id002">>}]]]}
```

## Option `sane_scalars`

Converts the following scalar values to their Erlang-native data type:

`"null"` → `undefined`
`"true"` → `true`
`"false"` → `false`

Integer and float values also get converted. All other scalar values
stay binaries. Key in mappings also stay binary and never get coerced
into int / float / atom .

An unquoted mapping value that is an empty string gets converted into
`undefined`. (e.g. the string `"foo:"` decodes as `[{<<"foo">>, undefined}]`)

## Option `maps`

Convert YAML mappings into Erlang maps.


```erlang
7> fast_yaml:decode(<<"a: true\nb: -3.0\nc: string">>, [{maps, true}]).
{ok, [#{"a" => "true", "b" => -3.0, "c" => "string"}]}
```


> For compatibility with the `yamerl` and `YamlElixir` libraries, use the `[sane_scalars, maps]` options.


## Development

### Test

#### Unit test

You can run eunit test with the command:

    make test
