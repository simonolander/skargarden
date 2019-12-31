## Installation

First, clone the repository:

```sh
git clone https://github.com/thomashoneyman/purescript-halogen-realworld
cd purescript-halogen-realworld
```

Install the JavaScript and PureScript dependencies:

```sh
yarn install
```

Next, build the project (this command will run `spago build`; see the [`package.json`](package.json) file to see
all helper scripts for the project):

```sh
yarn build
```

Finally, bundle the JS and run a local server (defaults to [port 8080](http://127.0.0.1:8080), but if this port is already in use it will increment to 8081, etc.):

```sh
yarn serve
```

You can also run `yarn bundle` to create a distribution-ready bundle of JavaScript without starting a server.

## Learning PureScript

This project is intended to give non-PureScript developers a taste of what a small application in the language looks like, and to give advanced beginners in PureScript a resource to feel comfortable building reliable applications of their own.

PureScript is a delightful language that becomes only more interesting and rewarding the more you use it and the larger your application becomes; if you haven’t yet tried it out, I encourage you to do so. Not convinced? [Kris Jenkins has a lovely talk about PureScript which might change your mind](https://www.youtube.com/watch?time_continue=22&v=5AtyWgQ3vv0).

### Resources

The PureScript community is overwhelmingly warm and helpful. If you would like some help getting started, please consider joining the [official Discourse](https://discourse.purescript.org) and [functional programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com)). You may also want to check out:

1. [PureScript by Example](https://github.com/dwhitney/purescript-book), which will teach you PureScript from scratch and was written by the language’s creator, Phil Freeman. While the official book has not been updated for the latest version of the compiler, this link is to an up-to-date fork of the book.
2. Jordan Martinez’s [PureScript reference](https://github.com/JordanMartinez/purescript-jordans-reference), which has a broad overview of dozens of topics in PureScript and functional programming.
3. The (warning: currently in rough draft form) [handbook which explains the theory & principles at work in this application in-depth](https://thomashoneyman.com/guides/real-world-halogen), which will be useful to help take you from advanced beginner to advanced intermediate in the language.
4. The [official Halogen guide](https://github.com/slamdata/purescript-halogen), which will teach you how to use the Halogen framework to write components.

## Contributing

PRs are welcome! Any functional changes will need to remain compliant with the [RealWorld](https://github.com/gothinkster/realworld) spec, and I may re-word documentation changes to fit with the voice used throughout the repository.
