_mailctl()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(/usr/bin/mailctl "${CMDLINE[@]}") )
}

complete -o filenames -F _mailctl mailctl
