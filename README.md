# kj

Run scripts from a nested directory without navigating up to project root.

## usage

Say you have the following directory tree for a project you are developing,

```
~/project/
├── scripts
│   ├── bar.sh
│   ├── baz.py
│   ├── foo.sh
│   └── qux
└── src
    └── subpackage
```

where the contents of scripts are all executable.

You can execute these scripts, with or without file extensions, from any directory under `~/project/`:
```
[~/project/src/subpackage]$ kj -l
bar
baz
foo
qux
[~/project/src/subpackage]$ kj bar
# ... executes bar.sh
[~/project/src/subpackage]$ kj bar.sh
# ... executes bar.sh

```

## installation

kj is easy to install using [stack](http://docs.haskellstack.org/en/stable/README/#how-to-install), which is available for all major platforms.

After installing stack, run the following:
```
$ git clone http://github.com/steventlamb/kj.git
$ cd kj
$ stack setup
$ stack build
$ stack install
```

Stack will create an executable in its configured bin directory. Either add that directory to your `PATH` or copy that executable somewhere on your `PATH`.

Binaries are provided on the releases page for GNU/Linux. More platforms may later be supported.

## configuration

### custom directories

Instead of looking for a `scripts` dir, you can set `kjDir` in a `.kj.json` file at the root level where the scripts folder is, to a folder, without slashes, to look for scripts in.

Example `.kj.json`:
```json
{"kjDir": "kj_scripts"}
```

### shell completions

Execute one of the following snippets to add completion for your favorite shell. If you downloaded the the binary only, you'll want to use the following technique to download the completion script instead of `cp`ing it.

 ```shell
curl https://raw.githubusercontent.com/steventlamb/kj/master/completions/kj.sh > ~/.bash_completion.d/kj
```

#### bash

```shell
cp completions/kj.sh ~/.bash_completion.d/kj
```

#### fish

```shell
cp completions/kj.fish ~/.config/fish/completions/kj.fish
```

## misc

### name

Kj is chosen to be short and simple to type from the home row. It also also a nod to karaoke jockeys everywhere. You know who you are.

Kj used to be called `scripty`. This name led to too much difficulty with completion, and users were aliasing scripty to something like `s` as a workaround.

### motivation

Kj is inspired by the behavior of modern tools like fabric, vagrant, etc.
These tools allow you to run commands from any subdirectory of a project, which is very
convenient. I had gotten used to using fabric, and after switching to a folder full of
scripts for simplicity, I missed this functionality.

