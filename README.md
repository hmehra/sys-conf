# README -
Configuration files for the environment

## File paths
* tmux.conf   -> ~/.tmux.conf
* gitconfig   -> ~/.gitconfig
* emacs       -> ~/.emacs
* ssh.config  -> ~/.ssh/config

## Related Shortcuts:

### GDB
* gdb followed by load or gdb <program>
* print <variable>
* list filename:line#/function name
* backtrace - Stack
* break filename:line#/function name
* next
* watch <variable>

### Tmux
##### Binding Cntrl + a
* tmux -> new session
* Binding & c -> new window
* Binding & h -> Horizontal split
* Binding & v -> Vertical split
* Cntrl + arrow key -> Change windows
* Shift + arrow key -> Chang panes
* Binding + { -> Move pane left
* Binding + } -> Move pane right
* Binding + d -> Detach session

### Git
* origin - point of origin on the web  
* .git/config -> Configuration how the connection is made
* git clone path
* git add <files> -u for modified files
* git status
* git log
* git commit
* git push
* git checkout hash -> view/revert an old version of a commit
* git checkout master -> view/revert to the current state
* git checkout HEAD file -> revert to the most recent file status
* git revert hash -> permanent revert to earlier commit
* git branch -> Find out the branch
* git blame <filename>  -> find out who made the changes

Git via SSH
    Add the public key to the host
    Edit ssh config file. Add username, public key, hostname
    Edit git config file. Change url to ssh path

### Emacs -
* Control + Meta + f/b  -> Match braces
* Control + S <TAB key> highlight tabs
* Meta + Shift + < "Start of the buffer"
* M + x + set-variable case-fold-search nil   -> Disable case sensitive search
* Control + k  Delete line
* Meta + ; Comment multiple lines -> comment-dwim
* Meta + x set-variable show-trailing-whitespace t
* Alt + g and g -> Go to line
* C-s C-w Search selected region under cursor


### CTags for emacs
* Generate tag file
* find . -type f -iname "*.[chS]" | xargs etags -a

### Password less SSH
* cat .ssh/id_rsa.pub | ssh b@B 'cat >> .ssh/authorized_keys'
