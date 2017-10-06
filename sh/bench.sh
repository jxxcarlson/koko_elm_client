color=`tput setaf 48`
reset=`tput setaf 7`

echo
echo "${color}Compile benchmarks to bench.html${reset}"
echo

elm-make $1  --output bench.html
