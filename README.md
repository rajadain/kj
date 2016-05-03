# kj

kj allows you to use your `scripts/` folder as if it were part of a task running framework. You can:

* Run scripts from a nested directory without navigating up to project root
* Provide docstrings in your scripts as comments and view them from the command line
* Enjoy the niceties of a modern task runner like grunt or fabric without a separate syntax: the rest of your team is able to run the same scripts the old-fashioned way

## usage

Say you have the following directory tree for a project you are developing:

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

### docstrings

The first comment found in your script after a shebang line will be read by kj as quick documentation. Simply run `kj -d` from within a `kj` project to see scripts and their documentation.

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

Kj is chosen to be short and simple to type from the home row. It is also a nod to the [http://en.wikipedia.org/wiki/Karaoke#Karaoke_terms](karaoke jockey).

Kj used to be called `scripty`. This name led to too much difficulty with completion, and users were aliasing scripty to something like `s` as a workaround. Kj removes this burden.

### motivation

Kj is inspired by the behavior of modern tools like fabric, vagrant, etc.
These tools allow you to run commands from any subdirectory of a project, which is very
convenient.

I was working on a team that switched from using fabric to regular shell scripts. This turned out to be a good decision for reasons beyond the scope of this document, but I missed the flexibility of fabric. Kj adds some of those features to regular shell scripts.

## supported scripts

Kj current supports python and bash scripts only. Support for other scripts could be added if the userbase requested them, particlarly scripts whose comment prefix syntax is `#`. Otherwise, kj will be kept small.

