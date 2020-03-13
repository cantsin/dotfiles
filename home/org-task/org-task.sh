ID=`/run/current-system/sw/bin/id -u`
RESULT=`TMPDIR=/run/user/$ID ~/.nix-profile/bin/emacsclient --eval '(progn (load-file "~/.config/nixpkgs/org-task/org-task.el") (get-current-task))'`
echo ${RESULT:1:-1}
