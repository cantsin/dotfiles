ID=`/run/current-system/sw/bin/id -u`
RESULT=`TMPDIR=/run/user/$ID ~/.nix-profile/bin/emacsclient --eval '(progn (load-file "~/.config/nixpkgs/org-task/org-task.el") (get-current-task))' 2>/dev/null`
if [ $? -ne 0 ]
then
    echo "emacs server is not running"
else
    echo ${RESULT:1:-1}
fi
