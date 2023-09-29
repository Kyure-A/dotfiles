existp() { type "$1" > /dev/null 2>&1; }
update_message() { printf "\033[32mUpdating \033[m\"$1\"\n\n" }
install_message() { printf "\033[32mInstalling \033[m\"$1\"\n\n" }
