# README -
Configuration files for the environment

File paths :
1. tmux.conf   -> ~/.tmux.conf
2. gitconfig   -> ~/.gitconfig
3. emacs       -> ~/.emacs
4. ssh.config  -> ~/.ssh/config

## Related Shortcuts:

### GDB
1. gdb followed by load or gdb <program>
2. print <variable>
3. list filename:line#/function name
4. backtrace - Stack
5. break filename:line#/function name
6. next
7. watch <variable>

### Tmux
Binding Cntrl + a
1. tmux -> new session
2. Binding & c -> new window
3. Binding & h -> Horizontal split
4. Binding & v -> Vertical split
5. Cntrl + arrow key -> Change windows
6. Shift + arrow key -> Chang panes
7. Binding + { -> Move pane left
8. Binding + } -> Move pane right
9. Binding + d -> Detach session

### Git
origin - point of origin on the web
1. git clone path
2. git add <files> -u for modified files
3. git status
4. git log
5. git commit
6. git push
7. git checkout hash -> view/revert an old version of a commit
   git checkout master -> view/revert to the current state
   git checkout HEAD file -> revert to the most recent file status
8. git revert hash -> permanent revert to earlier commit
9. git branch -> Find out the branch
.git/config -> Configuration how the connection is made
10. git blame <filename>  -> find out who made the changes
11. Git via SSH
    Add the public key to the host
    Edit ssh config file. Add username, public key, hostname
    Edit git config file. Change url to ssh path

### Emacs -
1. Control + Meta + f/b  -> Match braces
2. Control + S <TAB key> highlight tabs
3. Meta + Shift + < "Start of the buffer"
4. M + x + set-variable case-fold-search nil   -> Disable case sensitive search
5. Control + k  Delete line
6. Meta + ; Comment multiple lines -> comment-dwim
7. Trailing whitespacs
   Meta + x set-variable show-trailing-whitespace t
8. Alt + g and g -> Go to line
9. C-s C-w Search selected region under cursor


### CTags for emacs
1) Generate tag file
find . -type f -iname "*.[chS]" | xargs etags -a

Password less ssh
cat .ssh/id_rsa.pub | ssh b@B 'cat >> .ssh/authorized_keys'

### Emacs Packages -
1. Auto complete mode
2. Magit
3. Flycheck