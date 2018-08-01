# config-joiner-json


A library and binary to manage JSON config files for multiple configurations. 

## Building

This package is built using [`stack`](https://docs.haskellstack.org/en/stable/README/). 

```sh
stack build
```

will build this project.

## Setup

`config-joiner-json` has very few expectations for setup. In particular, the only
requirements are:
- A common JSON file to combine specific configurations with
- A directory of source JSON files that will be joined against this common file
    - Note that `config-joiner-json` does not yet support recursively generating joined files.
- A _different_ target directory to write the generated and completed files to. 

## Usage

### Binary

Once built the binary can be executed with

```sh
stack exec config-joiner-json -- --common-file <common-file-path> --source-directory <source-directory-path> --target-directory <target-directory-path>
```

or, if the binary is in your path, just 

```sh
config-joiner-json --common-file <common-file-path> --source-directory <source-directory-path> --target-directory <target-directory-path>
```

`config-join-json` also supports short options and include `--help`

### Library

The `config-joiner-json` library is not yet released on [`Hackage`](https://hackage.haskell.org/), but the library itself is documented and the site can be built with

```sh
stack haddock
```

This will drop the site in the local `./.stack-work` directory, at the time of writing 
the index is landing in `./.stack-work/install/x86_64-osx/lts-12.4/8.4.3/doc/index.html`.

Check out the site for the modules available for reading JSON files + generating and writing files.


## Example

Check out the [example directory](./data/example) for a simple example.

## TODOs

This is currently still in WIP (not yet released to Hackage or anything), this project still needs more tests (in particular for the libary) and could probably use some small features for the `join` binary.

- Test Coverage
- Features:
    - Search directories recursively for config files
