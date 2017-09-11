
echo "push to GitHub"
git push

echo
echo "use src/deployment configuration"
cp ./robot/src/deploy/Configuration.elm ./src/
cp ./robot/src/deploy/webpack.config.js ./src/

echo
echo "build app ..."
npm run build

echo
echo "upload to cloud ..."
scp -r ./dist/* root@138.197.81.6:/var/www/html/
