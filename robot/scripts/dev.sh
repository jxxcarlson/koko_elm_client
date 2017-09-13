color=`tput setaf 48`
reset=`tput setaf 7`

echo
echo "${color}Use dev configuration.${reset}"
cp ./robot/src/dev/Configuration.elm ./src/
cp ./robot/src/dev/webpack.config.js ./src/

echo
echo "${color}Start webpack${reset}"
yarn start
