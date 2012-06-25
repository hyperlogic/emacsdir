# On MacOS explictly set TERM, so we can get ls colors.
if [ $(uname)="Darwin" ]; then
	export TERM=ansi
fi
# On Linux, the PROMPT_COMMAND is set which mangles the PS1 prompt.
if [ $(uname)="Linux" ]; then
	unset PROMPT_COMMAND
fi
export EDITOR=emacsclient
