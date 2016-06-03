_kj_commands () {
    local _IFS scripts
    _IFS=$IFS
    IFS=$'\n'
    scripts=($(kj -d | sed -r 's/^([a-zA-Z0-9_-]+) +?/\1:/'))
    IFS=$_IFS
    _describe -t scripts "scripts" scripts
}

compdef _kj_commands kj
