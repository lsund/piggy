# piggy

This haskell program + some shell magic will help you navigate the command
line.

## Why?

I am a heavy user of Ctrl-R and [fzf](https://github.com/junegunn/fzf). While
this is nice, I want to be able to have more control over

* The commands I execute the most
* The directories I most frequently visit

I want to do this by assigning mental "tags" to each command/directory. Instead
of writing

```
cd ~/Documents/tech/scripts/evilcorp/acme-missiles-utilities
```

I want to be able to type something shorter like

```
p cd acme
```

And it should have the same effect. This tool lets me do that.

## Install

0. Make sure you have [fzf](https://github.com/junegunn/fzf) installed

1. Call the install script from the piggy root directory.

```
./install
```

This builds the haskell program and populates `~/.piggy/scripts`.

2. put the following line in your zshrc (TRUST ME).

```
source $HOME/.piggy/scripts/functions
```

This will setup the alias `p` for you.

## Usage

#### Adding directories

Piggy associates tags with commands or directories. A tag is just a string to
match on.

To add a directory, navigate to a directory and call the add command.

```
# Syntax: p ad <path> <tag>
#         p ad <path>
cd $HOME/path/to/foo-repo
p ad . foo-repo
# By omitting the third argument, the current base directory is used
# as tag
p ad .
```

#### Changing directory

```
# Syntax: p cd <tag>
# The tag does not have to be fully specified, a substring suffices
p cd foo-repo
p cd foo
# The following will open up fzf with the available options
p cd
```

#### Adding a command

Adding a command is done similar to adding a directory

```
# Syntax: p ar <command> <tag>
p ar edit-zshrc 'vim /home/lsund/.zshrc'
```

#### Running a command

```
# Syntax: p r <tag>
p r zsh
# The following will open up fzf with the available options
p r
```
