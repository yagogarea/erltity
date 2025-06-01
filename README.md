# Erltity
Erltity is an Erlang library that simplifies database interaction by generating Erlang code from entity definitions. It provides a set of functions to easily communicate with your database.

Erltity parses your entity definitions and generates Erlang code that allows you to interact with your database.

## Setup

Add `erltity` to your `rebar.config` dependencies:

```erlang
{deps, [
    {erltity, {git, "https://github.com/yagogarea/erltity.git", {branch, "main"}}}
]}.
```

## Supported Features

Currently, `erltity` supports:
- **Databases**:
      
  - [PostgreSQL](https://www.postgresql.org/)

- **Entity definition formats**:

  - [JSON Schema Draft-07](http://json-schema.org/draft-07/schema#)

## Usage
```erlang
1> erltity:start_link(epgsql, Opts). % Provide your database connection options

2> erltity:register(<<"test/schemas/dog.json">>, #{}).

3> dog:create(#{<<"name">> => <<"Nala">>, <<"breed">> => <<"Chihuahua">>}).
{ok,#{<<"breed">> => <<"Chihuahua">>,<<"id">> => 1,
      <<"name">> => <<"Nala">>}}

4> dog:find(1).
{ok,#{<<"breed">> => <<"Chihuahua">>,<<"id">> => 1,
      <<"name">> => <<"Nala">>}}
```
For practical examples, check out the examples directory.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing
Contributions are welcome! If you find a bug or have a feature request, feel free to open an issue or submit a pull request. See our [contributing guide](CONTRIBUTING.md) for more details.