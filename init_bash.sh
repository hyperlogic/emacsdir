# On MacOS explictly set TERM, so we can get ls colors.
if [ $(uname)="Darwin" ]; then
	export TERM=ansi
fi
