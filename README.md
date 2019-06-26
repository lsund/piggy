# piggy

This is a command line utility for faster command line navigation

## Why?

I am a heavy user of Ctrl-R. While the history is nice, it is sometimes hard to
find the right commands. Also, different shells might have different history
listings. I wanted something robuster and more configurable, with a similar feel of backwards history search.

## Install

0. Make sure you have `fzf` installed

1. Call the install script from the piggy root directory.

```
./install
```

This builds the haskell program and populates `~/.piggy/scripts`.

2. Put this bad-boy in your `~/.aliases` or similar.

```
function p() {
    if [[ $1 == "cd" && $# == "1" ]]; then
        temp_file=$(mktemp)
        piggy_dirs=$($HOME/.local/bin/piggy-exe "dlt")
        dir=$(echo $piggy_dirs | fzf)
        echo "#!/bin/zsh\ncd $($HOME/.local/bin/piggy-exe cd $dir)" > $temp_file
        . $temp_file
        /usr/bin/rm $temp_file
    elif [[ $1 == "cd" ]]; then
        temp_file=$(mktemp)
        echo "#!/bin/zsh\ncd $($HOME/.local/bin/piggy-exe $@)" > $temp_file
        . $temp_file $@
        /usr/bin/rm $temp_file
    elif [[ $1 == "r" && $# == "1" ]]; then
        piggy_cmds=$($HOME/.local/bin/piggy-exe "rlt")
        cmd=$(echo $piggy_cmds | fzf)
        $($HOME/.local/bin/piggy-exe r $cmd)
    elif [[ $1 == "r" ]]; then
        $($HOME/.local/bin/piggy-exe $@)
    else
        $HOME/.local/bin/piggy-exe $@
    fi
}
```

3. Source it

```
source ~/.aliases
```

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
```
