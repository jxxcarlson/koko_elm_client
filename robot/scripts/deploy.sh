

# Color       #define       Value       RGB
# black     COLOR_BLACK       0     0, 0, 0
# red       COLOR_RED         1     max,0,0
# green     COLOR_GREEN       2     0,max,0
# yellow    COLOR_YELLOW      3     max,max,0
# blue      COLOR_BLUE        4     0,0,max
# magenta   COLOR_MAGENTA     5     max,0,max
# cyan      COLOR_CYAN        6     0,max,max
# white     COLOR_WHITE       7     max,max,max

# red=`tput setaf 1`
# green=`tput setaf 2`
# reset=`tput sgr0`
# echo "${red}red text ${green}green text${reset}"

color=`tput setaf 48`
reset=`tput setaf 7`

echo "${color}Commit changes to git${reset}"

git ci -a

echo "${color}push to GitHub${reset}"
git push origin master

echo
echo "${color}use src/deployment configuration${reset}"
cp ./robot/src/deploy/Configuration.elm ./src/
cp ./robot/src/deploy/webpack.config.js ./src/

echo
echo "${color}build app ...${reset}";
tput setaf 6;time npm run build; tput setaf 7;

echo
echo "${color}upload to cloud ...${reset}"
scp -r ./dist/* root@138.197.81.6:/var/www/html/

echo
tput setaf 2; echo "${color}Done${reset}"

echo
echo "${color}... Now use dev configuration.${reset}"
cp ./robot/src/dev/Configuration.elm ./src/
cp ./robot/src/dev/webpack.config.js ./src/

echo
echo "${color}Start webpack${reset}"
yarn start
