# Skärgården

## Live version

The game can be played at https://skargarden.simonolander.com

## Game description

The Swedish state has hired you to chart the archipelago. You contracted people to scout out all the islands, but they took off with the money leaving you with only loose clues as to where the islands are. Maybe you can somehow use these hints to finalize the sea charts?

## Tutorial

See the [tutorial document](docs/tutorial.md).

## Installation

First, clone the repository:

```sh
git clone https://github.com/simonolander/skargarden.git
cd skargarden
```

Install the javascript and purescript dependencies:

```sh
yarn install
```

Next, build the project (this command will run `spago build`; see the [`package.json`](package.json) file to see
all helper scripts for the project):

```sh
yarn build
```

Finally, bundle the javascript code and run a local server (defaults to [port 1234](http://localhost:1234)):

```sh
yarn serve
```

## Contributing

I'm happy to receive submit issues for bugs, feature requests, and typos!
