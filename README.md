# Erltity
![erltity status](https://github.com/yagogarea/erltity/actions/workflows/ci.yml/badge.svg)
![Docs Status](https://github.com/yagogarea/erltity/workflows/Docs/badge.svg)

Erltity is an Erlang library that simplifies database interaction by generating Erlang code from entity definitions. It provides a set of functions to easily communicate with your database.

Erltity parses your entity definitions and generates Erlang code that allows you to interact with your database.

## Setup

Add `erltity`, the database driver and the parser that you want to use as dependencies in your `rebar.config` file. For example, to use PostgreSQL with the `epgsql` driver, using the JSON Schema parser, your `rebar.config` should look like this:

```erlang
{deps, [
  {erltity, {git_subdir, "https://github.com/yagogarea/erltity.git", {branch, "main"}, "apps/erltity"}},
  {erltity_epgsql, {git_subdir, "https://github.com/yagogarea/erltity.git", {branch, "main"}, "apps/erltity_epgsql"}},
  {erltity_json_schema_parser, {git_subdir, "https://github.com/yagogarea/erltity.git", {branch, "main"}, "apps/erltity_json_schema_parser"}}
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

### Register Function and Module Generation

The `register` function generates a module named after the entity definition with functions to interact with the database. This module is loaded into the system for immediate use.

Optionally, the module can be saved to disk with the `#{save_module => Path}` option.

When using a SQL database, if the target table does not exist at command execution, Erltity attempts to create it by inferring the table structure from the entity definition types.

Saving the generated table creation command to a file is possible with the `#{save_create_table => Path}` option.

The generated module includes the following functions for interacting with the database:

| Function   | Description                                          |
|------------|------------------------------------------------------|
| `create/1` | Inserts a new record into the database               |
| `update/2` | Updates an existing record identified by its ID     |
| `delete/1` | Deletes a record identified by its ID                |
| `find/1`   | Retrieves a single record by its ID                   |
| `find/2`   | Retrieves records filtered according to specified fields |

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing
Contributions are welcome! If you find a bug or have a feature request, feel free to open an issue or submit a pull request. See our [contributing guide](CONTRIBUTING.md) for more details.