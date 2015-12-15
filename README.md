# Fast YAML

[![Build Status](https://travis-ci.org/processone/fast_yaml.svg?branch=master)](https://travis-ci.org/processone/fast_yaml) [![Coverage Status](https://coveralls.io/repos/processone/fast_yaml/badge.svg?branch=master&service=github)](https://coveralls.io/github/processone/fast_yaml?branch=master) [![Hex version](https://img.shields.io/hexpm/v/fast_yaml.svg "Hex version")](https://hex.pm/packages/fast_yaml)

Fast YAML is an Erlang wrapper for
[libyaml](http://pyyaml.org/wiki/LibYAML) "C" library.

It is designed to be fast and efficient.

## Installation

### Dependencies

Fast YAML depends on native LibYaml library. You need development
headers for LibYaml library to build it.

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

## Development

### Test

#### Unit test

You can run eunit test with the command:

    make test
