[core]
    hooksPath = "/home/benedikt.kristinsson/.git/hooks"


[user]
    email = benedikt.kristinsson@care.com
    name = Ben Kristinsson
    signingkey = A17F259BC2C05742

[alias]
    bradd = !sh -c 'git checkout master && git pull && git checkout -b "$1" master && git push -u origin "$1"' -
    brdel = !sh -c 'git checkout master && git pull && git branch --delete "$1" && git push origin --delete "$1"' -
    brpub = !sh -c 'git checkout master && git pull && git checkout "$1" && git rebase master && git push --force origin HEAD' -
    brlint = !sh -c 'al -t $(git diff --name-only origin | egrep -i ya?ml$)'

[blame]
    coloring = highlightRecent

[commit]
    gpgsign = true

[credential "https://github.com"]
    useHttpPath = true
    helper = /usr/local/bin/credential-github-app

[github-app-helper]
    key = ~/.caredevopsro.2021-12-01.private-key
    app-id = 155699





